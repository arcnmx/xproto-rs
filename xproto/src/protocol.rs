use core::{mem, slice};
use core::convert::TryInto;
use std::borrow::Cow;
use byteorder::NativeEndian;
use zerocopy::{AsBytes, FromBytes, Unaligned};
use crate::message::*;
use crate::DecodeError;

pub type U16 = zerocopy::byteorder::U16<NativeEndian>;
pub type U32 = zerocopy::byteorder::U32<NativeEndian>;

// https://code.woboq.org/gtk/include/X11/X.h.html

include!(concat!(env!("OUT_DIR"), "/xcbproto-rs/mod.rs"));

#[derive(Debug, Clone)]
pub struct ExtensionInfo<'a> {
    pub xname: Cow<'a, str>,
    pub name: Cow<'a, str>,
    pub multiword: bool,
    pub major_version: u16,
    pub minor_version: u16,
    pub kind: ExtensionKind,
}

pub trait Extension {
    const INFO: ExtensionInfo<'static>;

    fn opcode(&self) -> u8;
}

pub trait Event: FromMessage {
    const NUMBER: u16;
    const IS_PLAIN: bool; // otherwise has sequence number
    const IS_GENERIC: bool;
}

pub trait Error: FromMessage {
    const NUMBER: i8;
}

#[derive(Debug, Clone)]
pub enum RequestInfo {
    Core(CoreRequestInfo),
    Extension(ExtensionRequestInfo),
}

#[derive(Debug, Copy, Clone)]
pub struct CoreRequestInfo {
    pub opcode: u8,
}

#[derive(Debug, Clone)]
pub struct ExtensionRequestInfo {
    pub minor_opcode: u8,
    pub extension: ExtensionInfo<'static>,
}

impl RequestInfo {
    pub const fn request_opcode(&self) -> u8 {
        match self {
            RequestInfo::Core(core) => core.opcode,
            RequestInfo::Extension(ext) => ext.minor_opcode,
        }
    }
}

pub trait Request: Message {
    const INFO: RequestInfo;
    const COMBINE_ADJACENT: bool;

    type Reply: Reply;
}

pub trait CoreRequest: Request {
    const CORE_INFO: CoreRequestInfo;
}

pub trait ExtensionRequest: Request {
    const EXTENSION_INFO: ExtensionRequestInfo;
}

pub trait Reply: FromMessage {
//	type Request: Request;
}

impl Reply for () {
}


/*#[repr(C)]
pub struct Header {
    pub header: u8,
    pub data: u8,
}*/

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, AsBytes, FromBytes, Unaligned)]
#[repr(C)]
pub struct RequestHeader {
    pub major_opcode: u8,
    pub data: u8,
    pub length: U16,
}
impl MessageAsBytes for RequestHeader { }
impl MessageFromBytes for RequestHeader { }

impl RequestHeader {
    pub fn new<R: CoreRequest + FixedSizeMessage>(data: u8) -> Self {
        Self::with_length::<R>(data, R::FIXED_SIZE)
    }

    pub fn with_request<R: CoreRequest>(request: &R, data: u8) -> Self {
        Self::with_length::<R>(data, request.size())
    }

    pub fn with_length<R: CoreRequest>(data: u8, length: usize) -> Self {
        Self {
            major_opcode: R::CORE_INFO.opcode,
            data,
            length: U16::new((length / 4).try_into().expect("Request too large")),
        }
    }

    pub fn packet_size(&self) -> usize {
        32 + self.length.get() as usize * 4
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, AsBytes, FromBytes, Unaligned)]
#[repr(transparent)]
pub struct ExtensionRequestHeader {
    pub header: RequestHeader,
}
impl MessageAsBytes for ExtensionRequestHeader { }
impl MessageFromBytes for ExtensionRequestHeader { }

impl ExtensionRequestHeader {
    pub fn new<R: ExtensionRequest + FixedSizeMessage>(major_opcode: u8) -> Self {
        Self::with_length::<R>(major_opcode, R::FIXED_SIZE)
    }

    pub fn with_request<R: ExtensionRequest>(request: &R, major_opcode: u8) -> Self {
        Self::with_length::<R>(major_opcode, request.size())
    }

    pub fn with_length<R: ExtensionRequest>(major_opcode: u8, length: usize) -> Self {
        Self {
            header: RequestHeader {
                major_opcode,
                data: R::EXTENSION_INFO.minor_opcode,
                length: U16::new((length / 4).try_into().expect("Request too large")),
            },
        }
    }

    pub const fn minor_opcode(&self) -> u8 {
        self.header.data
    }

    pub fn packet_size(&self) -> usize {
        self.header.packet_size()
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, AsBytes, FromBytes, Unaligned)]
#[repr(C)]
pub struct GenericHeader {
    pub response_type: u8,
    pub data: u8,
    pub sequence: U16,
}
impl MessageAsBytes for GenericHeader { }
impl MessageFromBytes for GenericHeader { }

impl GenericHeader {
    pub fn sequence(&self) -> u16 {
        self.sequence.get()
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, AsBytes, FromBytes, Unaligned)]
#[repr(C)]
pub struct ReplyHeader {
    pub header: GenericHeader,
    pub length: U32,
}
impl MessageAsBytes for ReplyHeader { }
impl MessageFromBytes for ReplyHeader { }

impl ReplyHeader {
    pub const RESPONSE_TYPE: u8 = 1;

    pub const fn data(&self) -> u8 {
        self.header.data
    }

    pub fn packet_size(&self) -> u64 {
        32 + self.length.get() as u64 * 4
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, AsBytes, FromBytes, Unaligned)]
#[repr(transparent)]
pub struct EventHeader {
    pub header: GenericHeader,
}
impl MessageAsBytes for EventHeader { }
impl MessageFromBytes for EventHeader { }

impl EventHeader {
    pub const EVENT_CODE_MASK: u8 = 0x7f;

    /// Indicates whether event was generated by another client via SendEvent
    pub const fn is_generated(&self) -> bool {
        self.header.response_type & 0x80 != 0
    }

    pub const fn event_code(&self) -> u8 {
        self.header.response_type & Self::EVENT_CODE_MASK
    }

    pub const fn data(&self) -> u8 {
        self.header.data
    }

    pub const fn packet_size() -> usize {
        32
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, AsBytes, FromBytes, Unaligned)]
#[repr(C)]
pub struct GenericEventHeader {
    pub header: GenericHeader,
    pub length: U32,
    pub event_type: U16,
}
impl MessageAsBytes for GenericEventHeader { }
impl MessageFromBytes for GenericEventHeader { }

impl GenericEventHeader {
    pub const RESPONSE_TYPE: u8 = xcore::GeGenericEvent::NUMBER as u8;

    pub fn extension_opcode(&self) -> u8 {
        self.header.data
    }

    pub fn packet_size(&self) -> usize {
        32 + self.length.get() as usize * 4
    }

    pub fn event_type(&self) -> u16 {
        self.event_type.get()
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, AsBytes, FromBytes, Unaligned)]
#[repr(C)]
pub struct ErrorHeader {
    pub header: GenericHeader,
    pub data: U32,
    pub minor_opcode: U16,
    pub major_opcode: u8,
}
impl MessageAsBytes for ErrorHeader { }
impl MessageFromBytes for ErrorHeader { }

impl ErrorHeader {
    pub const RESPONSE_TYPE: u8 = 0;

    pub fn data(&self) -> u32 {
        self.data.get()
    }

    pub fn error_code(&self) -> u8 {
        self.header.data
    }

    pub fn packet_size() -> usize {
        32
    }
}

/*#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ErrorInfo {
    pub error_code: u8,
    pub major_opcode: u8,
    pub minor_opcode: u16,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EventInfo {
    pub major_opcode: u8,
    pub minor_opcode: Option<u16>,
    pub generic_length: Option<usize>,
}

pub enum ReplyKind {
    Reply {
        sequence: u16,
        result: Result<usize, u8>,
    },
    Event {
        major_opcode: u8,
        minor_opcode: Option<u8>,
        generic_length: Option<usize>,
    },
}*/

pub enum ParsedReplyHeader<'a> {
    Reply(&'a ReplyHeader),
    Event(&'a EventHeader),
    GenericEvent(&'a GenericEventHeader),
    Error(&'a ErrorHeader),
}

impl<'a> ParsedReplyHeader<'a> {
    pub unsafe fn new_unchecked(data: &'a [u8]) -> Self {
        debug_assert!(data.len() >= 32);
        let data = mem::transmute(data.as_ptr());
        Self::new(data)
    }

    pub fn new(data: &'a [u8; 32]) -> Self {
        match data[0] {
            ErrorHeader::RESPONSE_TYPE => ParsedReplyHeader::Error(unsafe { mem::transmute(data) }),
            ReplyHeader::RESPONSE_TYPE => ParsedReplyHeader::Reply(unsafe { mem::transmute(data) }),
            kind if kind & EventHeader::EVENT_CODE_MASK == GenericEventHeader::RESPONSE_TYPE =>
                ParsedReplyHeader::GenericEvent(unsafe { mem::transmute(data) }),
            _ => ParsedReplyHeader::Event(unsafe { mem::transmute(data) }),
        }
    }

    pub fn generic_header(&self) -> &'a GenericHeader {
        match self {
            ParsedReplyHeader::Reply(h) => &h.header,
            ParsedReplyHeader::Event(h) => &h.header,
            ParsedReplyHeader::GenericEvent(h) => &h.header,
            ParsedReplyHeader::Error(h) => &h.header,
        }
    }

    pub fn packet_size(&self) -> usize {
        match self {
            ParsedReplyHeader::Reply(h) => h.packet_size()
                .try_into().expect("Reply too large"),
            ParsedReplyHeader::Event(_) => EventHeader::packet_size(),
            ParsedReplyHeader::GenericEvent(h) => h.packet_size(),
            ParsedReplyHeader::Error(_) => ErrorHeader::packet_size(),
        }
    }

    pub fn sequence(&self) -> u16 {
        self.generic_header().sequence()
    }

    pub fn as_reply(&self) -> Option<Result<&'a ReplyHeader, &'a ErrorHeader>> {
        match self {
            ParsedReplyHeader::Reply(h) => Some(Ok(h)),
            ParsedReplyHeader::Error(h) => Some(Err(h)),
            _ => None,
        }
    }

    pub fn header_bytes(&self) -> &'a [u8] {
        match self {
            ParsedReplyHeader::Reply(h) => h.as_bytes(),
            ParsedReplyHeader::Event(h) => h.as_bytes(),
            ParsedReplyHeader::GenericEvent(h) => h.as_bytes(),
            ParsedReplyHeader::Error(h) => h.as_bytes(),
        }
    }

    pub unsafe fn packet(&self) -> &'a [u8] {
        let size = self.packet_size();
        slice::from_raw_parts(self.header_bytes().as_ptr(), size)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ExtensionEventCode {
    Basic {
        code: u8,
    },
    Generic {
        code: u16,
    },
}

impl ExtensionEventCode {
    pub fn disambiguate(&self) -> u32 {
        match self {
            &ExtensionEventCode::Basic { code } => code as u32,
            &ExtensionEventCode::Generic { code } => (code as u32 + 1) << 6,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum EventCode {
    Core {
        code: u8,
    },
    Extension {
        extension: ExtensionKind,
        code: ExtensionEventCode,
    },
}

impl EventCode {
    pub fn is_core(code: u8) -> bool {
        code < 64
    }
}
