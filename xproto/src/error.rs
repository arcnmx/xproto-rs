use core::convert::Infallible;
use core::num::TryFromIntError;
use std::error::Error as StdError;
use std::{io, fmt};

#[derive(Debug, Clone)]
pub enum DecodeError {
    Generic,
    InvalidBits(u32),
    InvalidEnum(u32),
    UnexpectedEnum(u32),
    TryFromInt(TryFromIntError),
    UnexpectedEof,
    ConstMismatch,
}

impl fmt::Display for DecodeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Generic => write!(f, "unknown error"),
            Self::InvalidBits(bits) => write!(f, "invalid bits {}", bits),
            Self::InvalidEnum(value) => write!(f, "invalid enum value {}", value),
            Self::UnexpectedEnum(value) => write!(f, "unexpected enum value {}", value),
            Self::TryFromInt(e) => write!(f, "{}", e),
            Self::UnexpectedEof => write!(f, "unexpected EOF"),
            Self::ConstMismatch => write!(f, "expected const mismatch"),
        }
    }
}

impl From<DecodeError> for io::Error {
    fn from(e: DecodeError) -> Self {
        use std::io::ErrorKind;

        let kind = match e {
            DecodeError::Generic => ErrorKind::Other,
            DecodeError::InvalidBits(..) => ErrorKind::InvalidData,
            DecodeError::InvalidEnum(..) => ErrorKind::InvalidData,
            DecodeError::UnexpectedEnum(..) => ErrorKind::InvalidInput,
            DecodeError::TryFromInt(..) => ErrorKind::InvalidData,
            DecodeError::UnexpectedEof => ErrorKind::UnexpectedEof,
            DecodeError::ConstMismatch => ErrorKind::InvalidInput,
        };

        io::Error::new(kind, e)
    }
}

impl StdError for DecodeError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            Self::TryFromInt(e) => Some(e),
            _ => None,
        }
    }
}

impl From<Infallible> for DecodeError {
    #[inline]
    fn from(e: Infallible) -> Self {
        match e { }
    }
}

impl From<TryFromIntError> for DecodeError {
    #[inline]
    fn from(e: TryFromIntError) -> Self {
        Self::TryFromInt(e)
    }
}

impl From<()> for DecodeError {
    #[inline]
    fn from(_e: ()) -> Self {
        Self::Generic
    }
}

impl<T: enumflags2::RawBitFlags> From<enumflags2::FromBitsError<T>> for DecodeError
where
    T::Type: Into<u32>,
{
    #[inline]
    fn from(e: enumflags2::FromBitsError<T>) -> Self {
        Self::InvalidBits(e.invalid_bits().into())
    }
}

