use std::sync::{Mutex as StdMutex, RwLock};
use std::collections::BTreeMap;
use std::task::{Poll, Context};
use std::sync::Arc;
use std::pin::Pin;
use std::{error, io};
use tokio_util::codec::{FramedRead, FramedWrite};
use tokio::io::{AsyncRead, AsyncWrite, AsyncReadExt};
use bytes::BytesMut;
use futures::{Future, Stream, SinkExt, FutureExt};
use futures::channel::oneshot;
use xproto::{Message, FromMessage, Request, XString};
use xproto::protocol::xcore::{self, SetupRequest, Setup, QueryExtensionRequest, QueryExtensionReply};
use xproto::protocol::{ExtensionRequest, ExtensionKind, ExtensionEventCode};
use crate::codec::{XEncoder, XDecoder, XReply};
use crate::xauth::Family;

pub type Sequence = u16;

pub struct XStream<R> {
    read: FramedRead<R, XDecoder>,
}

#[derive(Debug, Default)]
pub struct Extensions {
    extensions: BTreeMap<ExtensionKind, QueryExtensionReply>,
}

impl Extensions {
    pub fn insert(&mut self, kind: ExtensionKind, query: QueryExtensionReply) {
        self.extensions.insert(kind, query);
    }

    pub fn get_extension(&self, kind: ExtensionKind) -> Option<&QueryExtensionReply> {
        self.extensions.get(&kind)
    }

    pub fn decode_event_code(&self, event_code: u8) -> Option<(ExtensionKind, u8)> {
        let (&kind, ext) = self.extensions.iter().filter(|(_, ext)| ext.first_event >= event_code)
            .min_by_key(|(_, ext)| ext.first_event)?;
        Some((kind, event_code - ext.first_event))
    }

    pub fn get_opcode(&self, opcode: u8) -> Option<(ExtensionKind, &QueryExtensionReply)> {
        self.extensions.iter()
            .find(|(_, e)| e.major_opcode == opcode)
            .map(|(&k, e)| (k, e))
    }

    pub fn decode_error_code(&self, error_code: u8) -> Option<u8> {
        unimplemented!()
    }
}

#[derive(Debug)]
pub struct XShared {
    commands: StdMutex<BTreeMap<Sequence, oneshot::Sender<Result<BytesMut, BytesMut>>>>,
    extensions: RwLock<Extensions>,
    setup: Setup,
}

impl XShared {
    pub fn new(setup: Setup) -> Self {
        Self {
            commands: Default::default(),
            extensions: Default::default(),
            setup,
        }
    }
}

pub struct XSink<W> {
    shared: Arc<XShared>,
    write: FramedWrite<W, XEncoder>,
    xid: crate::XidRange,
    sequence: u16,
}

impl<W: AsyncWrite> XSink<W> {
    fn with_parts(write: FramedWrite<W, XEncoder>, shared: Arc<XShared>) -> Self {
        Self {
            sequence: 1,
            xid: crate::XidRange::new(shared.setup.resource_id_base, shared.setup.resource_id_mask),
            shared,
            write,
        }
    }

    fn new(w: W, shared: Arc<XShared>) -> Self {
        Self::with_parts(FramedWrite::new(w, XEncoder), shared)
    }
}

impl<W> XSink<W> {
    pub fn next_sequence(&mut self) -> u16 {
        let sequence = self.sequence;
        self.sequence = self.sequence.wrapping_add(1);
        sequence
    }
}

impl<W: AsyncWrite + Unpin> XSink<W> {
    pub async fn execute<'a, 'c, C: Request + 'a>(&'a mut self, command: C) -> impl Future<Output=Result<C::Reply, io::Error>> + 'static where
        C::Reply: FromMessage<Context=()>,
        <C::Reply as FromMessage>::Error: error::Error + Sync + Send + 'static,
    {
        let sequence = self.next_sequence();
        let receiver = if <C::Reply as Message>::SIZE.size() != Some(0) {
            let (sender, receiver) = oneshot::channel();
            {
                let mut commands = self.shared.commands.lock().unwrap();
                commands.insert(sequence, sender);
            }
            Some(receiver)
        } else {
            None
        };
        let res = self.write.send(command).await;
        let context = ();
        async move {
            res?;
            if let Some(receiver) = receiver {
                let res = receiver.await
                    .map_err(|e| io::Error::new(io::ErrorKind::UnexpectedEof, e))?;
                let mut res = match res {
                    Ok(bytes) => bytes,
                    Err(bytes) =>
                        return Err(io::Error::new(io::ErrorKind::InvalidData, format!("received error {:?}", bytes))),
                };
                <<C as Request>::Reply as FromMessage>::decode(context, &mut res)
                    .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
                    .and_then(|reply| match reply {
                        /*Some(..) if res.remaining() > 0 => // TODO: trailing data isn't a real problem, and you need to align events to 32 anyway
                            Err(io::Error::new(io::ErrorKind::InvalidData, format!("unexpected trailing data for reply {}", sequence))),*/
                        Some(reply) => Ok(reply),
                        None =>
                            Err(io::Error::new(io::ErrorKind::UnexpectedEof, format!("expected more data for reply {}", sequence))),
                    })
            } else {
                // void reply expects no response, but what about errors? maybe we can wait for the next event, if seq says this was processed, return ok? but what if no events occur...
                Ok(FromMessage::decode((), &mut &[][..]).unwrap().unwrap())
            }
        }
    }

    /*pub async fn execute_ext<'a, 'c, C: ExtensionRequest + 'a>(&'a mut self, command: C) -> impl Future<Output=Result<C::Reply, io::Error>> + 'static where
        C::Reply: FromMessage<Context=()>,
        <C::Reply as FromMessage>::Error: error::Error + Sync + Send + 'static,
    {
        let ext_info = self.extension(C::EXTENSION_INFO.extension.kind).await.await?;
        if ext_info.is_none() {
            Err(io::Error::new(io::ErrorKind::BrokenPipe, format!("Extension {:?} not supported", ext_info.name))),
        }
        self.execute(command).await
    }*/

    pub fn try_generate_id<X: From<xproto::Xid>>(&mut self) -> Option<X> {
        self.xid.next().map(From::from)
    }

    pub async fn generate_id<X: From<xproto::Xid>>(&mut self) -> Result<X, io::Error> {
        match self.try_generate_id() {
            Some(xid) => Ok(xid),
            #[cfg(feature = "xc_misc")]
            None => {
                let more = self.execute(xproto::xc_misc::GetXIDRangeRequest::default())
                    .await.await?;
                self.xid += more;
                match self.try_generate_id() {
                    Some(xid) => Ok(xid),
                    None =>
                        Err(io::Error::new(io::ErrorKind::BrokenPipe, "server is out of XIDs")),
                }
            },
            #[cfg(not(feature = "xc_misc"))]
            None =>
                Err(io::Error::new(io::ErrorKind::BrokenPipe, "server is out of XIDs")),
        }
    }

    pub async fn intern_atom<I: Into<XString>>(&mut self, atom: I) -> impl Future<Output=io::Result<xcore::Atom>> {
        self.execute(xcore::InternAtomRequest {
            only_if_exists: false,
            name: atom.into(),
        }).await.map(|res| res.map(|res| res.atom.value()))
    }

    pub async fn extension(&mut self, kind: ExtensionKind) -> impl Future<Output=Result<Option<QueryExtensionReply>, io::Error>> {
        let cached = {
            let r = self.shared.extensions.read().unwrap();
            r.get_extension(kind).cloned()
        };
        let ext = match cached {
            Some(ext) => Ok(ext),
            None => {
                let req = QueryExtensionRequest {
                    name: kind.to_string().into(),
                };
                Err((self.execute(req).await, self.shared.clone()))
            },
        };

        async move {
            match ext {
                Ok(ext) => Ok(Some(ext)),
                Err((ext, shared)) => {
                    ext.await.map(|ext| {
                        let mut w = shared.extensions.write().unwrap();
                        w.insert(kind, ext.clone());
                        match ext.present {
                            true => Some(ext),
                            false => None,
                        }
                    })
                },
            }
        }
    }
}

pub struct XConnection<R> {
    shared: Arc<XShared>,
    stream: XStream<R>,
}

impl<R> XConnection<R> {
    pub fn setup(&self) -> &Setup {
        &self.shared.setup
    }
}

fn tcp_peer_addr(stream: &tokio::net::TcpStream) -> io::Result<(Family, Vec<u8>)> {
    use std::net::{SocketAddr, Ipv4Addr};

    let ip = match stream.peer_addr()? {
        SocketAddr::V4(addr) => *addr.ip(),
        SocketAddr::V6(addr) => {
            let ip = addr.ip();
            if ip.is_loopback() {
                Ipv4Addr::LOCALHOST
            } else if let Some(ip) = ip.to_ipv4() {
                ip
            } else {
                return Ok((Family::Internet6, ip.octets().to_vec()));
            }
        }
    };

    Ok(if !ip.is_loopback() {
        (Family::Internet, ip.octets().to_vec())
    } else {
        local_peer_addr()
    })
}

fn local_peer_addr() -> (Family, Vec<u8>) {
    let hostname = gethostname::gethostname()
        .to_str()
        .map(|name| name.as_bytes().to_vec())
        .unwrap_or_else(Vec::new);
    (Family::Local, hostname)
}

pub type IoStream<'a> = (Pin<Box<dyn AsyncRead + Send + 'a>>, Pin<Box<dyn AsyncWrite + Send + 'a>>);

fn splitstream<'a, RW: AsyncRead + AsyncWrite + Send + 'a>(rw: RW) -> IoStream<'a> {
    let (r, w) = tokio::io::split(rw);
    (Box::pin(r), Box::pin(w))
}

pub async fn open_display(display: &crate::Display) -> io::Result<(IoStream<'static>, crate::AuthInfo)> {
    const TCP_PORT_BASE: u16 = 6000;

    let protocol = display.protocol.as_ref().map(|s| &s[..]);
    let ((family, address), stream) = if (protocol.is_none() || protocol != Some("unix")) && !display.host.is_empty() && display.host != "unix" {
        let stream = tokio::net::TcpStream::connect((&display.host[..], TCP_PORT_BASE + display.display)).await?;
        (tcp_peer_addr(&stream)?, splitstream(stream))
    } else if protocol.is_none() || protocol == Some("unix") {
        let file_name = format!("/tmp/.X11-unix/X{}", display.display);

        // TODO: Try abstract socket (file name with prepended '\0')
        // Not supported on Rust right now: https://github.com/rust-lang/rust/issues/42048

        match tokio::net::UnixStream::connect(file_name).await {
            Ok(stream) =>
                (local_peer_addr(), splitstream(stream)),
            Err(err) => if protocol.is_none() && display.host.is_empty() {
                let stream = tokio::net::TcpStream::connect(("localhost", TCP_PORT_BASE + display.display)).await?;
                (tcp_peer_addr(&stream)?, splitstream(stream))
            } else {
                return Err(err.into())
            },
        }
    } else {
        unimplemented!()
    };

    let auth =
        crate::get_auth(family, &address, display.display)
        .unwrap_or(None)
        .unwrap_or_else(|| (Vec::new(), Vec::new()));
    Ok((stream, auth))
}

impl<R: AsyncRead> XConnection<R> {
    /*fn new(r: R, shared: Arc<XShared>) -> Self {
        Self {
            shared,
            stream: XStream {
                read: FramedRead::new(r, XDecoder::default()),
            },
        }
    }*/

    pub async fn connect<W: AsyncWrite + Unpin>(auth: crate::AuthInfo, mut r: R, w: W) -> Result<(Self, XSink<W>), io::Error> where
        R: Unpin,
    {
        let mut write = FramedWrite::new(w, XEncoder);
        let (authorization_protocol_name, authorization_protocol_data) = auth;
        let setup = SetupRequest {
            protocol_major_version: 11,
            protocol_minor_version: 0,
            byte_order: xproto::BYTE_ORDER,
            authorization_protocol_data: authorization_protocol_data.into(),
            authorization_protocol_name: authorization_protocol_name.into(),
        };
        write.send(setup).await?;

        let mut header = vec![0u8; 8];
        r.read_exact(&mut header).await?;
        let rest = u16::from_ne_bytes([header[6], header[7]]) as usize * 4;
        header.resize(header.len() + rest, 0);
        r.read_exact(&mut header[8..]).await?;
        let mut packet = &header[..];

        /*let mut packet = read.next().await
            .ok_or_else(|| io::Error::new(io::ErrorKind::UnexpectedEof, "X11 setup response expected"))
            .and_then(|r| r)?;*/
        let setup = Setup::decode((), &mut packet)?
            .ok_or_else(|| io::Error::new(io::ErrorKind::UnexpectedEof, "X11 setup response expected"))?;

        /* Check that we got a valid screen number
        if screen >= setup.roots.len() {
            return Err(ConnectError::InvalidScreen);
        }*/

        let shared = Arc::new(XShared::new(setup));

        let read = FramedRead::new(r, XDecoder::default());
        Ok((
            Self {
                shared: shared.clone(),
                stream: XStream {
                    read,
                },
            }, 
            XSink::with_parts(write, shared),
        ))
    }
}

/*pub fn new<R: AsyncRead, W: AsyncWrite>(r: R, w: W) -> (XConnection<R>, XSink<W>) {
    let shared = Arc::new(XShared::default());

    (
        XConnection::new(r, shared.clone()),
        XSink::new(w, shared),
    )
}*/

/*impl<W> Sink<Request> for XSink<W> {
    type Error = ();

    fn start_send(self: Pin<&mut Self>, item: Request) -> Result<(), Self::Error> {
        unimplemented!()
    }

    fn poll_ready(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Result<(), Self::Error>> {
        unimplemented!()
    }

    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Result<(), Self::Error>> {
        unimplemented!()
    }

    fn poll_close(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Result<(), Self::Error>> {
        unimplemented!()
    }
}*/

/*impl<R> XConnection<R> {
    fn ext_info_provider(&self) -> &dyn x11rb::x11_utils::ExtInfoProvider {
        unimplemented!()
    }
}*/

impl<R: AsyncRead> Stream for XConnection<R> {
    type Item = Result<xproto::ExtensionEvent, io::Error>;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<Option<Self::Item>> {
        let res = {
            let this = unsafe { self.as_mut().get_unchecked_mut() };
            let stream = unsafe { Pin::new_unchecked(&mut this.stream) };
            stream.poll_next(cx)
        };

        Poll::Ready(match futures::ready!(res) {
            None => None,
            Some(Err(e)) => Some(Err(e)),
            Some(Ok(reply)) => Some({
                let header = reply.parse();
                let sequence = header.sequence();
                let result = header.as_reply().map(|r| r.is_ok());
                match result {
                    Some(is_ok) => {
                        let res = {
                            let mut commands = self.shared.commands.lock().unwrap();
                            commands.remove(&sequence)
                        };
                        match res {
                            Some(sender) => {
                                let data = reply.into_bytes();
                                let data = if is_ok {
                                    Ok(data)
                                } else {
                                    Err(data)
                                };
                                let _ = sender.send(data); // can fail if receiver is dropped, but who cares
                                return self.poll_next(cx)
                            },
                            None => Err(io::Error::new(io::ErrorKind::InvalidData, format!("Sequence {} unexpected", sequence))),
                        }
                    },
                    None => {
                        // TODO  ext provider?
                        let opcode = match header {
                            xproto::ParsedReplyHeader::Event(e) if xproto::EventCode::is_core(e.event_code()) =>
                                xproto::EventCode::Core { code: e.event_code() },
                            xproto::ParsedReplyHeader::Event(e) => {
                                let (extension, code) = self.shared.extensions.read().unwrap()
                                    .decode_event_code(e.event_code())
                                    .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("unknown extension event {:?}", e)))?;
                                xproto::EventCode::Extension {
                                    extension,
                                    code: ExtensionEventCode::Basic {
                                        code,
                                    },
                                }
                            },
                            xproto::ParsedReplyHeader::GenericEvent(e) => {
                                let (extension, _) = self.shared.extensions.read().unwrap()
                                    .get_opcode(e.extension_opcode())
                                    .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, format!("unknown generic extension event {:?}", e)))?;
                                xproto::EventCode::Extension {
                                    extension,
                                    code: ExtensionEventCode::Generic {
                                        code: e.event_type(),
                                    },
                                }
                            },
                            _ => unreachable!(),
                        };
                        FromMessage::decode(opcode, &mut reply.into_bytes())
                            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
                            .and_then(|res| res.ok_or_else(||
                                io::Error::new(io::ErrorKind::UnexpectedEof, "Incomplete event")
                            ))
                    },
                }
            }),
        })
    }
}

impl<R: tokio::io::AsyncRead> Stream for XStream<R> {
    type Item = Result<XReply, io::Error>;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Option<Self::Item>> {
        let this = unsafe { self.get_unchecked_mut() };
        let read = unsafe { Pin::new_unchecked(&mut this.read) };
        /*Poll::Ready(match futures::ready!(read.poll_next(cx)) {
            None => None,
            Some(Err(e)) => Some(Err(e)),
            Some(Ok(data)) => Some(Ok(match ReplyInfo::parse(&data) {
                None => XEvent::Event(data),
                Some(ReplyInfo { sequence }) => XEvent::Reply {
                    sequence,
                    data,
                },
            })),
        })*/
        read.poll_next(cx)
    }
}

pub struct ReplyInfo {
    pub sequence: Sequence,
}

pub enum XEvent {
    Event(BytesMut),
    Reply {
        sequence: Sequence,
        data: BytesMut,
    },
}

impl ReplyInfo {
    pub fn parse(data: &[u8]) -> Option<Self> {
        match (data.get(0), data.get(2..4)) {
            (Some(1), Some(&[d0, d1])) => Some(ReplyInfo {
                sequence: u16::from_ne_bytes([d0, d1]),
            }),
            _ => None,
        }
    }
}
