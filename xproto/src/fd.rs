use crate::DecodeError;
use crate::message::*;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Fd(u32);

impl Message for Fd {
    const ALIGNMENT: MessageAlignment = MessageAlignment::new(1);
    const SIZE: MessageSize = MessageSize::new(0);

    fn message_size(&self) -> usize {
        0
    }

    fn encode<B: bytes::BufMut>(&self, _b: &mut B) { unimplemented!() }
}

impl FromMessage for Fd {
    type Error = DecodeError;
    type Context = ();

    fn decode<B: bytes::Buf>(context: Self::Context, _b: &mut B) -> Result<Option<Self>, Self::Error> {
        unimplemented!()
    }
}
