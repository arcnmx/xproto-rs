use core::{mem, slice, fmt, str, ops, iter};
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
        Self::with_length::<R>(data, request.message_size())
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
        Self::with_length::<R>(major_opcode, request.message_size())
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

#[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct XString {
    pub data: Vec<u8>,
}

impl XString {
    pub fn new<I: Into<Vec<u8>>>(data: I) -> Self {
        Self {
            data: data.into(),
        }
    }

    pub fn into_inner(self) -> Vec<u8> {
        self.data
    }

    pub fn to_str(&self) -> Result<&str, str::Utf8Error> {
        str::from_utf8(self.data.as_bytes())
    }

    pub fn to_str_lossy(&self) -> Cow<str> {
        String::from_utf8_lossy(&self.data[..])
    }

    pub fn into_string(self) -> Result<String, std::string::FromUtf8Error> {
        String::from_utf8(self.data)
    }

    pub fn into_string_lossy(self) -> String {
        match self.to_str_lossy() {
            Cow::Borrowed(_) => unsafe {
                String::from_utf8_unchecked(self.data)
            },
            Cow::Owned(lossy) => lossy,
        }
    }
}

impl ops::Deref for XString {
    type Target = Vec<u8>;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl ops::DerefMut for XString {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

// TODO: more From impls

impl From<String> for XString {
    fn from(data: String) -> Self {
        Self::new(data)
    }
}

impl From<Vec<u8>> for XString {
    fn from(data: Vec<u8>) -> Self {
        Self::new(data)
    }
}

impl From<&'_ str> for XString {
    fn from(data: &str) -> Self {
        Self::new(data)
    }
}

impl From<XString> for Vec<u8> {
    fn from(str: XString) -> Self {
        str.into_inner()
    }
}

impl iter::FromIterator<u8> for XString {
    fn from_iter<T: IntoIterator<Item=u8>>(iter: T) -> Self {
        Self::new(iter.into_iter().collect::<Vec<_>>())
    }
}

impl IntoIterator for XString {
    type IntoIter = <Vec<u8> as IntoIterator>::IntoIter;
    type Item = u8;

    fn into_iter(self) -> Self::IntoIter {
        self.data.into_iter()
    }
}

impl fmt::Debug for XString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.to_str_lossy()[..], f)
    }
}

impl fmt::Display for XString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.to_str() {
            Ok(str) => fmt::Display::fmt(str, f),
            Err(..) => Err(fmt::Error),
        }
    }
}

impl Message for XString {
    const ALIGNMENT: MessageAlignment = <Vec<u8> as Message>::ALIGNMENT;
    const SIZE: MessageSize = <Vec<u8> as Message>::SIZE;

    fn message_size(&self) -> usize {
        self.data.message_size()
    }

    fn encode<B: bytes::BufMut>(&self, b: &mut B) {
        self.data.encode(b)
    }
}

impl FromMessage for XString {
    type Error = <Vec<u8> as FromMessage>::Error;
    type Context = <Vec<u8> as FromMessage>::Context;

    fn decode<B: bytes::Buf>(context: Self::Context, b: &mut B) -> Result<Option<Self>, Self::Error> {
        Vec::<u8>::decode(context, b)
            .map(|data| data.map(Self::new))
    }
}

#[cfg(feature = "xinput")]
mod xinput_impl {
    use crate::conversion::{AsPrimitive, iter_bits};
    use super::xinput::{Fp3232, Fp1616, ButtonPressEvent, RawButtonPressEvent};

    impl Fp3232 {
        pub fn new(fp3232: i64) -> Self {
            Self {
                integral: (fp3232 >> 32) as i32,
                frac: fp3232 as u32,
            }
        }

        pub fn with_integral(integral: i32) -> Self {
            Self {
                integral,
                frac: 0,
            }
        }

        pub fn with_parts(integral: i32, frac: u32) -> Self {
            Self {
                integral,
                frac,
            }
        }

        // 1.25 is encoded as (1, 0x40000000)
        // -1.25 is encoded as (-2, 0xc0000000)
        pub fn fixed_point(self) -> i64 {
            let int = i64::from(self.integral) << 32;
            int | self.frac as i64
        }

        pub fn to_f64(self) -> f64 {
            let int = self.integral as f64;
            int + self.frac as f64 / (1u64 << 32) as f64
        }

        pub fn to_f32(self) -> f32 {
            let int = self.integral as f32;
            int + self.frac as f32 / (1u64 << 32) as f32
        }
    }

    impl AsPrimitive<i32> for Fp3232 {
        fn as_(self) -> i32 {
            self.integral
        }
    }

    impl AsPrimitive<i16> for Fp3232 {
        fn as_(self) -> i16 {
            self.integral.as_()
        }
    }

    impl AsPrimitive<i8> for Fp3232 {
        fn as_(self) -> i8 {
            self.integral.as_()
        }
    }

    impl AsPrimitive<f32> for Fp3232 {
        fn as_(self) -> f32 {
            self.to_f32()
        }
    }

    impl AsPrimitive<f64> for Fp3232 {
        fn as_(self) -> f64 {
            self.to_f64()
        }
    }

    impl From<i32> for Fp3232 {
        fn from(v: i32) -> Self {
            Self::with_integral(v)
        }
    }

    impl Fp1616 {
        pub fn new(fp1616: i32) -> Self {
            Self {
                fp1616,
            }
        }

        pub fn with_integral(integral: i16) -> Self {
            Self::new((integral as i32) << 16)
        }

        pub fn with_parts(integral: i16, frac: u16) -> Self {
            Self::new(i32::from(integral) << 16 | i32::from(frac))
        }

        pub fn integral(self) -> i16 {
            (self.fixed_point() >> 16) as i16
        }

        pub fn frac(self) -> u16 {
            self.fixed_point() as u16
        }

        pub fn fixed_point(self) -> i32 {
            self.fp1616
        }

        pub fn to_f64(self) -> f64 {
            self.fixed_point() as f64 / (1u64 << 16) as f64
        }

        pub fn to_f32(self) -> f32 {
            self.fixed_point() as f32 / (1u32 << 16) as f32
        }
    }

    impl AsPrimitive<i16> for Fp1616 {
        fn as_(self) -> i16 {
            self.integral()
        }
    }

    impl AsPrimitive<i8> for Fp1616 {
        fn as_(self) -> i8 {
            self.integral().as_()
        }
    }

    impl AsPrimitive<f32> for Fp1616 {
        fn as_(self) -> f32 {
            self.to_f32()
        }
    }

    impl AsPrimitive<f64> for Fp1616 {
        fn as_(self) -> f64 {
            self.to_f64()
        }
    }

    impl From<i16> for Fp1616 {
        fn from(v: i16) -> Self {
            Self::with_integral(v)
        }
    }

    impl ButtonPressEvent {
        pub fn root_x(&self) -> i16 {
            self.root_x.integral()
        }

        pub fn root_y(&self) -> i16 {
            self.root_y.integral()
        }

        pub fn event_x(&self) -> i16 {
            self.event_x.integral()
        }

        pub fn event_y(&self) -> i16 {
            self.event_y.integral()
        }

        pub fn valuator_mask<'a>(&'a self) -> impl Iterator<Item=u16> + 'a {
            self.valuator_mask.iter().enumerate()
                .flat_map(|(i, &mask)| iter_bits(i * 32, mask))
                .map(|i| i as u16)
        }

        pub fn button_mask<'a>(&'a self) -> impl Iterator<Item=usize> + 'a {
            self.button_mask.iter().enumerate()
                .flat_map(|(i, &mask)| iter_bits(i * 32, mask))
        }

        pub fn axisvalues<'a>(&'a self) -> impl Iterator<Item=(u16, Fp3232)> + 'a {
            self.valuator_mask()
                .zip(self.axisvalues.iter().copied())
        }
    }

    impl RawButtonPressEvent {
        pub fn valuator_mask<'a>(&'a self) -> impl Iterator<Item=u16> + 'a {
            self.valuator_mask.iter().enumerate()
                .flat_map(|(i, &mask)| iter_bits(i * 32, mask))
                .map(|i| i as u16)
        }

        pub fn axisvalues<'a>(&'a self) -> impl Iterator<Item=(u16, Fp3232)> + 'a {
            self.valuator_mask()
                .zip(self.axisvalues.iter().copied())
        }

        pub fn axisvalues_raw<'a>(&'a self) -> impl Iterator<Item=(u16, Fp3232)> + 'a {
            self.valuator_mask()
                .zip(self.axisvalues_raw.iter().copied())
        }
    }
}
