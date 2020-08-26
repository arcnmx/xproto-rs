use core::{mem, slice};
use core::convert::Infallible;
use bytes::{Buf, BufMut};
use zerocopy::{AsBytes, FromBytes};
use enumflags2::{BitFlags, RawBitFlags};
use crate::DecodeError;

// TODO: typenum

#[derive(Copy, Clone, Debug)]
pub struct MessageAlignment {
    pub alignment: usize,
    pub offset: usize,
}

impl MessageAlignment {
    pub const fn new(alignment: usize) -> Self {
        Self {
            alignment,
            offset: 0,
        }
    }

    pub const fn with_offset(alignment: usize, offset: usize) -> Self {
        Self {
            alignment,
            offset,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum MessageSize {
    Fixed {
        size: usize,
    },
    Variable {
        minimum: usize,
        alignment_end: MessageAlignment,
    },
}

impl MessageSize {
    pub const fn new(size: usize) -> Self {
        Self::Fixed { size }
    }

    pub const fn variable(minimum: usize, alignment_end: MessageAlignment) -> Self {
        Self::Variable {
            minimum,
            alignment_end,
        }
    }

    pub const fn to_zero_or_many(&self) -> Self {
        match self {
            &MessageSize::Fixed { size } => MessageSize::Variable {
                minimum: 0,
                alignment_end: MessageAlignment::new(size),
            },
            &MessageSize::Variable { minimum: _, alignment_end } => MessageSize::Variable {
                minimum: 0,
                alignment_end,
            },
        }
    }

    pub fn size(&self) -> Option<usize> {
        match self {
            &MessageSize::Fixed { size } => Some(size),
            _ => None,
        }
    }
}

pub trait Message {
    const ALIGNMENT: MessageAlignment;
    const SIZE: MessageSize;

    fn message_size(&self) -> usize;
    fn encode<B: BufMut>(&self, b: &mut B);
}

pub trait FixedSizeMessage: Message {
    const FIXED_SIZE: usize;
}

pub trait FromMessage: Message + Sized {
    type Error;
    type Context;

    fn decode<B: Buf>(context: Self::Context, b: &mut B) -> Result<Option<Self>, Self::Error>;
}

/*impl<T: Message> Message for &'_ T {
    const ALIGNMENT: MessageAlignment = <T as Message>::ALIGNMENT;
    const SIZE: MessageSize = <T as Message>::SIZE;

    fn message_size(&self) -> usize {
        (*self).message_size()
    }

    fn encode<B: BufMut>(&self, b: &mut B) {
        (*self).encode(b)
    }
}*/

pub trait MessageAsBytes: AsBytes { }
pub trait MessageFromBytes: MessageAsBytes + FromBytes { }
macro_rules! impl_message_for {
    ([$($l:tt,)+]) => {
        $(
            impl<T: MessageAsBytes> MessageAsBytes for [T; $l] { }
            impl<T: MessageFromBytes> MessageFromBytes for [T; $l] { }
        )+
    };
    ($($ty:ty),+) => {
        $(
            impl MessageAsBytes for $ty { }
            impl MessageFromBytes for $ty { }
        )+
    }
}

impl_message_for! { (), u8, u16, u32, u64, i8, i16, i32, i64, f32, f64 }
impl_message_for! { [
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
    10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
    21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
    31, 32, 64,
] }
impl MessageAsBytes for bool { }

impl<T: MessageAsBytes> Message for T {
    const ALIGNMENT: MessageAlignment = MessageAlignment::new(mem::align_of::<Self>());
    const SIZE: MessageSize = MessageSize::new(mem::size_of::<Self>());

    fn message_size(&self) -> usize {
        mem::size_of_val(self)
    }

    fn encode<B: BufMut>(&self, b: &mut B) {
        b.put(self.as_bytes())
    }
}

impl<T: MessageAsBytes> FixedSizeMessage for T {
    const FIXED_SIZE: usize = mem::size_of::<Self>();
}

impl<T: MessageFromBytes> FromMessage for T {
    type Error = Infallible;
    type Context = ();

    fn decode<B: Buf>(_context: Self::Context, b: &mut B) -> Result<Option<Self>, Self::Error> {
        Ok(if b.remaining() < mem::size_of::<Self>() {
            None
        } else {
            let res = mem::MaybeUninit::uninit();
            // TODO: if b.available <= size_of::<Self>(), that's a failure case :(
            unsafe {
                b.copy_to_slice(
                    slice::from_raw_parts_mut(res.as_ptr() as *mut u8, mem::size_of::<Self>())
                );
                Some(res.assume_init())
            }
        })
    }
}

impl<T: Message> Message for Option<T> {
    const ALIGNMENT: MessageAlignment = T::ALIGNMENT;
    const SIZE: MessageSize = T::SIZE.to_zero_or_many();

    fn message_size(&self) -> usize {
        match self {
            Some(m) => m.message_size(),
            None => 0,
        }
    }

    fn encode<B: BufMut>(&self, b: &mut B) {
        if let Some(m) = self {
            m.encode(b)
        }
    }
}

impl<T: FromMessage> FromMessage for Option<T> {
    type Error = T::Error;
    type Context = (bool, T::Context);

    fn decode<B: bytes::Buf>(context: Self::Context, b: &mut B) -> Result<Option<Self>, Self::Error> {
        if context.0 {
            T::decode(context.1, b).map(|res| res.map(Some))
        } else {
            Ok(Some(None))
        }
    }
}

impl<T: Message> Message for Vec<T> {
    const ALIGNMENT: MessageAlignment = T::ALIGNMENT;
    const SIZE: MessageSize = T::SIZE.to_zero_or_many();

    fn message_size(&self) -> usize {
        self.iter().map(|m| m.message_size()).sum()
    }

    fn encode<B: BufMut>(&self, b: &mut B) {
        for m in self {
            m.encode(b)
        }
    }
}

impl<T: FromMessage> FromMessage for Vec<T>
where
        T::Error: Into<DecodeError>,
        T::Context: Clone,
{
    type Error = DecodeError;
    type Context = (Option<usize>, T::Context);

    fn decode<B: bytes::Buf>(context: Self::Context, b: &mut B) -> Result<Option<Self>, Self::Error> {
        if let Some(len) = context.0 {
            (0..len).map(|_| T::decode(context.1.clone(), b)).collect::<Result<Option<_>, _>>()
                .map_err(Into::into)
        } else {
            let mut res = Vec::new();
            while b.remaining() > 0 {
                match T::decode(context.1.clone(), b).map_err(Into::into)? {
                    None => return Err(DecodeError::UnexpectedEof),
                    Some(t) => res.push(t),
                }
            }
            Ok(Some(res))
        }
    }
}

impl FromMessage for bool {
    type Error = DecodeError;
    type Context = <u8 as FromMessage>::Context;

    fn decode<B: Buf>(context: Self::Context, b: &mut B) -> Result<Option<Self>, Self::Error> {
        Ok(Some(match try_decode!(u8::decode(context, b)) {
            0 => false,
            1 => true,
            b => return Err(DecodeError::InvalidBits(b as _)),
        }))
    }
}

impl<E: RawBitFlags> Message for BitFlags<E> where E::Type: Message {
    const ALIGNMENT: MessageAlignment = MessageAlignment::new(mem::align_of::<Self>());
    const SIZE: MessageSize = MessageSize::Fixed { size: mem::size_of::<Self>() };

    fn message_size(&self) -> usize {
        self.bits().message_size()
    }

    fn encode<B: BufMut>(&self, b: &mut B) {
        self.bits().encode(b)
    }
}

impl<E: RawBitFlags> FromMessage for BitFlags<E>
where
    E::Type: FromMessage + Into<u32>,
    <E::Type as FromMessage>::Error: Into<DecodeError>,
{
    type Error = DecodeError;
    type Context = <E::Type as FromMessage>::Context;

    fn decode<B: Buf>(context: Self::Context, b: &mut B) -> Result<Option<Self>, Self::Error> {
        let bits = try_decode!(<E::Type>::decode(context, b));
        BitFlags::from_bits(bits)
            .map_err(Into::into).map(Some)
    }
}
