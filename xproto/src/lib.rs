#![allow(unused_parens, non_camel_case_types, non_snake_case)]

#[cfg(target_endian = "little")]
pub const BYTE_ORDER: u8 = 0x6c;

#[cfg(target_endian = "big")]
pub const BYTE_ORDER: u8 = 0x42;

macro_rules! try_decode {
    ($expr:expr) => {
        match $expr {
            Err(e) => return Err(From::from(e)),
            Ok(None) => return Ok(None),
            Ok(Some(res)) => res,
        }
    };
}

mod codegen_prelude {
    pub(crate) use crate::message::*;
    pub(crate) use crate::{Xid, DecodeError};
    #[allow(unused_imports)]
    pub(crate) use crate::protocol;
}

#[macro_use]
mod xid;
#[macro_use]
pub mod enums;
pub mod conversion;
pub mod fd;
pub mod message;
pub mod protocol;
mod error;

pub use protocol::*;
pub use message::{Message, FromMessage};
pub use xid::Xid;
pub use error::DecodeError;
