use std::convert::TryFrom;
use std::{mem, io};
use tokio_util::codec::{Encoder, Decoder};
use bytes::BytesMut;
use xproto::{Message, ParsedReplyHeader};

pub type Sequence = u16;

#[derive(Debug, Copy, Clone, Default)]
pub struct XDecoder {
    length: Option<usize>,
}

pub struct XReply {
    data: BytesMut,
}

impl XReply {
    pub unsafe fn new_unchecked(data: BytesMut) -> Self {
        Self {
            data,
        }
    }

    pub fn into_bytes(self) -> BytesMut {
        self.data
    }

    pub fn as_bytes(&self) -> &BytesMut {
        &self.data
    }

    fn packet_header(&self) -> &[u8; 32] {
        unsafe {
            mem::transmute(self.as_bytes().as_ptr())
        }
    }

    pub fn parse(&self) -> ParsedReplyHeader {
        ParsedReplyHeader::new(self.packet_header())
    }
}

impl TryFrom<BytesMut> for XReply {
    type Error = io::ErrorKind;

    fn try_from(bytes: BytesMut) -> Result<Self, Self::Error> {
        if bytes.len() <= 32 {
            Err(io::ErrorKind::UnexpectedEof)
        } else {
            Ok(unsafe { Self::new_unchecked(bytes) })
        }
    }
}

impl Decoder for XDecoder {
    type Item = XReply;
    type Error = io::Error;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        //eprintln!("decoding {:?}", &src[..]);
        if src.len() < self.length.unwrap_or(32) {
            Ok(None)
        } else {
            let length = self.length.unwrap_or_else(|| {
                let header = unsafe {
                    ParsedReplyHeader::new_unchecked(&src)
                };
                header.packet_size()
            });

            let (length, res) = if src.len() < length {
                (Some(length), None)
            } else {
                (None, Some(src.split_to(length)))
            };
            self.length = length;
            Ok(res.map(|bytes| unsafe { XReply::new_unchecked(bytes) }))
        }
    }
}

pub struct XEncoder;

impl<T: Message> Encoder<T> for XEncoder {
    type Error = io::Error;

    fn encode(&mut self, item: T, dst: &mut BytesMut) -> Result<(), Self::Error> {
        let mut out = Vec::new();
        item.encode(&mut out);
        //eprintln!("encoded as {:?}", out);
        Ok(item.encode(dst))
    }
}
