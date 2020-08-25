use core::convert::{TryFrom, TryInto};
use core::marker::PhantomData;
use core::cmp::Ordering;
use core::{fmt, ops};
use enumflags2::{BitFlags, RawBitFlags};
use zerocopy::{AsBytes, FromBytes};
use crate::DecodeError;
use crate::protocol::xcore;
#[cfg(feature = "xfixes")]
use crate::protocol::xfixes;
#[cfg(feature = "render")]
use crate::protocol::render;
use crate::conversion::*;
use crate::message::*;

macro_rules! enum_backing {
    (@new $name:ident $bits:expr) => {
        //unsafe { enumflags2::BitFlags::new($bits) }
        //unsafe { enumflags2::BitFlags::new($bits) }
        unsafe { crate::enums::ResizeMask::<$name, <$name as enumflags2::_internal::RawBitFlags>::Type>::new_unchecked($bits) }
    };
    (@alts bool $name:ident) => {
        enum_backing! { @altbool $name => u8 }
        enum_backing! { @altbool $name => u16 }
        enum_backing! { @altbool $name => u32 }
    };
    (@alts u8 $name:ident) => {
        enum_backing! { @smallalt $name => bool }
        enum_backing! { @alt $name => u16 }
        enum_backing! { @alt $name => u32 }
    };
    (@alts u16 $name:ident) => {
        enum_backing! { @smallalt $name => u8 }
        enum_backing! { @alt $name => u32 }
    };
    (@alts u32 $name:ident) => {
        enum_backing! { @smallalt $name => bool }
        enum_backing! { @smallalt $name => u8 }
        enum_backing! { @smallalt $name => u16 }
    };
    (@smallalt $name:ident => $repr:ty) => {
        // TODO
    };
    (@altbool $name:ident => $repr:tt) => {
        impl crate::enums::WithRepresentation<$repr> for $name {
            type Output = crate::enums::ResizeEnum<Self, $repr>;
        }

        impl From<$name> for $repr {
            #[inline]
            fn from(v: $name) -> Self {
                v as _
            }
        }

        impl core::convert::TryFrom<$repr> for $name {
            type Error = DecodeError;

            fn try_from(v: $repr) -> Result<Self, Self::Error> {
                match v {
                    0 | 1 => core::convert::TryFrom::try_from(v != 0).map_err(From::from),
                    invalid => Err(DecodeError::InvalidEnum(invalid.into())),
                }
            }
        }
    };
    (@alt $name:ident => $repr:tt) => {
        impl crate::enums::WithRepresentation<$repr> for $name {
            type Output = crate::enums::ResizeEnum<Self, $repr>;
        }

        impl From<$name> for $repr {
            #[inline]
            fn from(v: $name) -> Self {
                v as _
            }
        }

        impl core::convert::TryFrom<$repr> for $name {
            type Error = DecodeError;

            fn try_from(v: $repr) -> Result<Self, Self::Error> {
                let repr = core::convert::TryFrom::try_from(v)?;
                match crate::conversion::CEnum::from_repr(repr) {
                    Some(e) => Ok(e),
                    None => Err(DecodeError::InvalidEnum(repr.into())),
                }
            }
        }
    };
    (@implfrom bool $name:ident) => {
        impl From<$name> for bool {
            #[inline]
            fn from(v: $name) -> Self {
                v as u8 != 0
            }
        }
    };
    (@implfrom $repr:tt $name:ident) => {
        impl From<$name> for $repr {
            #[inline]
            fn from(v: $name) -> Self {
                v as _
            }
        }
    };
    (@implcomplete bool $name:ident) => {
        impl FromMessage for $name {
            type Error = DecodeError;
            type Context = ();

            fn decode<B: bytes::Buf>(_context: Self::Context, b: &mut B) -> Result<Option<Self>, Self::Error> {
                match bool::decode(_context, b)? {
                    Some(repr) => crate::conversion::CEnum::from_repr(repr)
                        .map(Some).ok_or(DecodeError::InvalidEnum(repr.into())),
                    None => Ok(None),
                }
            }
        }
    };
    (@implcomplete $repr:tt $name:ident) => { };
    (@complete $name:ident => $repr:tt) => {
        enum_backing! { $name => $repr }
        enum_backing! { @implcomplete $repr $name }

        impl From<$repr> for $name {
            #[inline]
            fn from(v: $repr) -> Self {
                unsafe {
                    core::mem::transmute(v)
                }
            }
        }
    };
    (@incomplete $name:ident => $repr:tt) => {
        enum_backing! { $name => $repr }

        impl crate::conversion::UncheckedFrom<$repr> for $name {
            #[inline]
            unsafe fn unchecked_from(repr: $repr) -> Self {
                core::mem::transmute(repr)
            }
        }

        impl FromMessage for $name {
            type Error = DecodeError;
            type Context = ();

            fn decode<B: bytes::Buf>(_context: Self::Context, b: &mut B) -> Result<Option<Self>, Self::Error> {
                match <$repr>::decode(_context, b)? {
                    Some(repr) => crate::conversion::CEnum::from_repr(repr)
                        .map(Some).ok_or(DecodeError::InvalidEnum(repr.into())),
                    None => Ok(None),
                }
            }
        }
    };
    ($name:ident => $repr:tt) => {
        impl crate::enums::WithRepresentation<$repr> for $name {
            type Output = Self;
        }

        enum_backing! { @implfrom $repr $name }
        enum_backing! { @alts $repr $name }

        impl $name {
            #[inline]
            pub unsafe fn new_unchecked(v: $repr) -> Self {
                crate::conversion::CEnum::from_repr_unchecked(v)
            }

            #[allow(dead_code)]
            pub(crate) fn get(&self) -> Self {
                *self
            }
        }

        impl crate::conversion::CEnum for $name {
            type Repr = $repr;

            unsafe fn from_repr_unchecked(repr: Self::Repr) -> Self {
                crate::conversion::UncheckedFrom::unchecked_from(repr)
            }
        }
    };
}

macro_rules! from_xid {
    (@bool $xid:ty => $enum:ty) => {
        impl TryFrom<$xid> for $enum {
            type Error = DecodeError;

            fn try_from(value: $xid) -> Result<Self, Self::Error> {
                <$enum>::try_from(value.xid() != 0)
                    .map_err(From::from)
            }
        }

        impl From<$enum> for $xid {
            fn from(value: $enum) -> Self {
                Self::new(value.into())
            }
        }
    };
    ($xid:ty => $enum:ty) => {
        impl TryFrom<$xid> for $enum {
            type Error = DecodeError;

            fn try_from(value: $xid) -> Result<Self, Self::Error> {
                let repr: <$enum as CEnum>::Repr = value.xid().try_into()?;
                <$enum>::try_from(repr)
                    .map_err(From::from)
            }
        }

        impl From<$enum> for $xid {
            fn from(value: $enum) -> Self {
                Self::new(value.into())
            }
        }
    };
}

from_xid! { @bool xcore::Cursor => xcore::CursorEnum }
from_xid! { @bool xcore::Font => xcore::FontEnum }
from_xid! { @bool xcore::Pixmap => xcore::PixmapEnum }
from_xid! { @bool xcore::Colormap => xcore::ColormapEnum }
from_xid! { @bool xcore::Atom => xcore::GetPropertyType }
from_xid! { @bool xcore::Window => xcore::WindowEnum }
from_xid! { @bool xcore::Window => xcore::SendEventDest }
from_xid! { @bool xcore::Pixmap => xcore::BackPixmap }
#[cfg(feature = "render")]
from_xid! { @bool render::Picture => render::PictureEnum }
#[cfg(feature = "xfixes")]
from_xid! { @bool xfixes::Region => xfixes::RegionEnum }
from_xid! { xcore::Window => xcore::InputFocus }
from_xid! { xcore::Atom => xcore::AtomEnum }

/*
#[derive(Debug, Hash)]
#[repr(transparent)]
pub struct BitMask<E: RawBitFlags> {
    bits: E::Type,
    _flags: PhantomData<E>,
}*/

#[derive(Hash)]
#[repr(transparent)]
pub struct AltMask<E: Copy, T: Copy> {
    bits: T,
    _flags: PhantomData<E>,
}

impl<RHS: Copy, E: Copy, T: Copy + PartialEq<RHS>> PartialEq<AltMask<E, RHS>> for AltMask<E, T> {
    fn eq(&self, other: &AltMask<E, RHS>) -> bool {
        self.bits.eq(&other.bits)
    }
}

impl<RHS: Copy, E: Copy, T: Copy + PartialOrd<RHS>> PartialOrd<AltMask<E, RHS>> for AltMask<E, T> {
    fn partial_cmp(&self, other: &AltMask<E, RHS>) -> Option<Ordering> {
        self.bits.partial_cmp(&other.bits)
    }
}

impl<E: Copy, T: Copy> Copy for AltMask<E, T> { }

unsafe impl<E: Copy, T: Copy + AsBytes> AsBytes for AltMask<E, T> {
    fn only_derive_is_allowed_to_implement_this_trait() where Self: Sized { }
}

impl<E: Copy, T: Copy + AsBytes> MessageAsBytes for AltMask<E, T> { }
impl<E: Copy, T: Copy + AsBytes + FromBytes> MessageFromBytes for AltMask<E, T> { }

unsafe impl<E: Copy, T: Copy + FromBytes> FromBytes for AltMask<E, T> {
    fn only_derive_is_allowed_to_implement_this_trait() where Self: Sized { }
}

impl<E: Copy, T: Copy> Clone for AltMask<E, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<E: Copy, T: Copy + Ord> Ord for AltMask<E, T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.bits.cmp(&other.bits)
    }
}

impl<E: Copy, T: Copy + Eq> Eq for AltMask<E, T> { }

impl<E: Copy, T: Copy + Default> Default for AltMask<E, T> {
    fn default() -> Self {
        Self::with_bits(T::default())
    }
}

impl<E: RawBitFlags<Type=T> + fmt::Debug, T: Copy + enumflags2::_internal::BitFlagNum + fmt::Debug> fmt::Debug for AltMask<E, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AltMask")
            .field("flags", &self.flags())
            .field("invalid", &self.invalid_bits())
            .finish()
    }
}

impl<E: Copy, T: Copy> AltMask<E, T> {
    pub fn with_bits(bits: T) -> Self {
        Self {
            bits,
            _flags: PhantomData,
        }
    }

    pub fn bits(&self) -> T {
        self.bits
    }
}

impl<E: RawBitFlags<Type=T>, T: Copy + enumflags2::_internal::BitFlagNum> AltMask<E, T> {
    pub fn new<I: Into<BitFlags<E>>>(bits: I) -> Self {
        Self {
            bits: bits.into().bits(),
            _flags: PhantomData,
        }
    }

    pub fn flags(&self) -> BitFlags<E> {
        BitFlags::from_bits_truncate(self.bits)
    }

    pub fn invalid_bits(&self) -> T {
        match BitFlags::<E>::from_bits(self.bits) {
            Ok(_) => BitFlags::<E>::empty().bits(),
            Err(e) => e.invalid_bits()
        }
    }
}

#[derive(Hash)]
#[repr(transparent)]
pub struct ResizeMask<E: Copy, T: Copy> {
    bits: T,
    _flags: PhantomData<E>,
}

impl<RHS: Copy, E: Copy, T: Copy + PartialEq<RHS>> PartialEq<ResizeMask<E, RHS>> for ResizeMask<E, T> {
    fn eq(&self, other: &ResizeMask<E, RHS>) -> bool {
        self.bits.eq(&other.bits)
    }
}

impl<RHS: Copy, E: Copy, T: Copy + PartialOrd<RHS>> PartialOrd<ResizeMask<E, RHS>> for ResizeMask<E, T> {
    fn partial_cmp(&self, other: &ResizeMask<E, RHS>) -> Option<Ordering> {
        self.bits.partial_cmp(&other.bits)
    }
}

impl<T: Copy, E: Copy> Promote<T> for ResizeMask<E, T> {
    type Output = T;

    fn promote(self) -> Self::Output {
        self.bits()
    }
}

impl<T: Copy, E: Copy> Promote<ResizeMask<E, T>> for T {
    type Output = T;

    fn promote(self) -> Self::Output {
        self
    }
}

impl<E: Copy + RawBitFlags + fmt::Debug, T: Copy + AsPrimitive<E::Type>> fmt::Debug for ResizeMask<E, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.get(), f)
    }
}

impl<E: Copy, T: Copy> Copy for ResizeMask<E, T> { }

unsafe impl<E: Copy, T: Copy + AsBytes> AsBytes for ResizeMask<E, T> {
    fn only_derive_is_allowed_to_implement_this_trait() where Self: Sized { }
}

impl<E: Copy, T: Copy + AsBytes> MessageAsBytes for ResizeMask<E, T> { }

impl<E: Copy, T: Copy> Clone for ResizeMask<E, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<E: Copy, T: Copy + Ord> Ord for ResizeMask<E, T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.bits.cmp(&other.bits)
    }
}

impl<RHS: Copy, E: Copy, T: Copy + ops::BitOr<RHS>> ops::BitOr<ResizeMask<E, RHS>> for ResizeMask<E, T>
where
    T::Output: Copy,
{
    type Output = ResizeMask<E, T::Output>;

    fn bitor(self, rhs: ResizeMask<E, RHS>) -> Self::Output {
        unsafe {
            ResizeMask::new_unchecked(self.bits.bitor(rhs.bits))
        }
    }
}

impl<E: Copy, T: Copy + Eq> Eq for ResizeMask<E, T> { }

impl<E: Copy, T: Copy + Default> Default for ResizeMask<E, T> {
    fn default() -> Self {
        Self {
            bits: T::default(),
            _flags: PhantomData,
        }
    }
}

impl<I: Into<BitFlags<E>>, E: Copy + RawBitFlags, T: Copy + ops::BitOrAssign<T> + From<E::Type>> ops::BitOrAssign<I> for ResizeMask<E, T> {
    fn bitor_assign(&mut self, rhs: I) {
        self.bits |= rhs.into().bits().into();
    }
}

impl<I: Into<BitFlags<E>>, E: Copy + RawBitFlags, T: Copy + ops::BitAnd<T, Output=T> + 'static> ops::BitAnd<I> for ResizeMask<E, T>
where
    E::Type: AsPrimitive<T>,
{
    type Output = Self;

    fn bitand(self, rhs: I) -> Self::Output {
        unsafe {
            Self::with_bits_unchecked(
                self.bits & rhs.into().bits().as_()
            )
        }
    }
}

impl<E: Copy + RawBitFlags, T: Copy + ops::Not<Output=T> + ops::BitAnd<Output=T> + 'static> ops::Not for ResizeMask<E, T>
where
    E::Type: AsPrimitive<T>,
{
    type Output = Self;

    fn not(self) -> Self::Output {
        let bits = !self.bits;
        let all = BitFlags::<E>::all().bits().as_();
        unsafe {
            Self::with_bits_unchecked(bits & all)
        }
    }
}

impl<E: Copy, T: Copy> ResizeMask<E, T> {
    pub unsafe fn new_unchecked(value: T) -> Self {
        Self::with_bits_unchecked(value)
    }

    pub unsafe fn with_bits_unchecked(bits: T) -> Self {
        Self {
            bits,
            _flags: PhantomData,
        }
    }

    pub fn bits(&self) -> T {
        self.bits
    }

    pub fn count_ones(&self) -> u32 where T: Into<u64> {
        self.bits.into().count_ones()
    }
}

impl<E: Copy + RawBitFlags, T: Copy + AsPrimitive<E::Type>> ResizeMask<E, T> {
    pub fn get(&self) -> BitFlags<E> {
        unsafe {
            BitFlags::new(self.bits.as_())
        }
    }
}

impl<E: Copy + RawBitFlags, T: Copy + AsPrimitive<E::Type>> From<ResizeMask<E, T>> for BitFlags<E> {
    fn from(mask: ResizeMask<E, T>) -> Self {
        mask.get()
    }
}

impl<E: Copy + RawBitFlags, T: Copy + From<E::Type>> From<BitFlags<E>> for ResizeMask<E, T> {
    fn from(flags: BitFlags<E>) -> Self {
        unsafe {
            Self::with_bits_unchecked(flags.bits().into())
        }
    }
}

impl<E: Copy + RawBitFlags, T: Copy + From<E::Type>> From<E> for ResizeMask<E, T> {
    fn from(flag: E) -> Self {
        let flags = BitFlags::from(flag);
        unsafe {
            Self::with_bits_unchecked(flags.bits().into())
        }
    }
}

impl<E: Copy + RawBitFlags, T: Copy> ResizeMask<E, T> {
    pub fn with_bits(bits: T) -> Option<Self> where E::Type: TryFrom<T> {
        let ebits = TryFrom::try_from(bits).ok()?;
        BitFlags::<E>::from_bits(ebits)
            .ok()?;
        Some(unsafe { Self::with_bits_unchecked(bits) })
    }
}

impl<E: RawBitFlags, T: Copy + enumflags2::_internal::BitFlagNum> ResizeMask<E, T> {
    pub fn new<I: Into<BitFlags<E>>>(bits: I) -> Self where E::Type: Into<T> {
        Self {
            bits: bits.into().bits().into(),
            _flags: PhantomData,
        }
    }

    pub fn flags(&self) -> BitFlags<E> where T: AsPrimitive<E::Type> + 'static {
        BitFlags::from_bits_truncate(self.bits.as_())
    }
}

impl<E: Copy + RawBitFlags, T: Copy + AsBytes + FromMessage> FromMessage for ResizeMask<E, T>
where
    DecodeError: From<T::Error>,
    DecodeError: From<<E::Type as TryFrom<T>>::Error>,
    DecodeError: From<enumflags2::FromBitsError<E>>,
    E::Type: TryFrom<T>,
{
    type Error = DecodeError;
    type Context = T::Context;

    fn decode<B: bytes::Buf>(context: Self::Context, b: &mut B) -> Result<Option<Self>, Self::Error> {
        Ok(match T::decode(context, b)? {
            Some(repr) => Some({
                let bits: E::Type = TryFrom::try_from(repr)?;
                BitFlags::<E>::from_bits(bits)?;
                unsafe {
                    Self::with_bits_unchecked(repr)
                }
            }),
            None => None,
        })
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct AltEnum<E: Copy, T: Copy> {
    value: T,
    _enum: PhantomData<E>,
}

unsafe impl<E: Copy, T: Copy + AsBytes> AsBytes for AltEnum<E, T> {
    fn only_derive_is_allowed_to_implement_this_trait() where Self: Sized { }
}
impl<E: Copy, T: Copy + AsBytes> MessageAsBytes for AltEnum<E, T> { }
impl<E: Copy, T: Copy + AsBytes + FromBytes> MessageFromBytes for AltEnum<E, T> { }

unsafe impl<E: Copy, T: Copy + FromBytes> FromBytes for AltEnum<E, T> {
    fn only_derive_is_allowed_to_implement_this_trait() where Self: Sized { }
}

impl<E: Copy + TryFrom<T> + fmt::Debug, T: Copy + fmt::Debug> fmt::Debug for AltEnum<E, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.get(), f)
    }
}

impl<E: Copy, T: Copy + Default> Default for AltEnum<E, T> {
    fn default() -> Self {
        Self::with_value(T::default())
    }
}

impl<E: Copy, T: Copy> From<E> for AltEnum<E, T> where E: Into<T> {
    fn from(value: E) -> Self {
        Self::new(value)
    }
}


impl<E: Copy, T: Copy> AltEnum<E, T> {
    pub fn new(value: E) -> Self where E: Into<T> {
        Self {
            value: value.into(),
            _enum: PhantomData,
        }
    }

    pub fn with_value(value: T) -> Self {
        Self {
            value,
            _enum: PhantomData,
        }
    }

    pub fn value(&self) -> T {
        self.value
    }

    pub fn get(&self) -> Result<E, T> where E: TryFrom<T> {
        E::try_from(self.value).map_err(|_| self.value)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct ResizeEnum<E: Copy, T: Copy> {
    value: T,
    _enum: PhantomData<E>,
}

impl<E: Copy, T: Copy> ResizeEnum<E, T> {
    pub fn new(value: E) -> Self where E: Into<T> {
        Self {
            value: value.into(),
            _enum: PhantomData,
        }
    }

    pub unsafe fn new_unchecked(value: E) -> Self where E: AsPrimitive<T> {
        Self::with_value_unchecked(value.as_())
    }

    pub unsafe fn with_value_unchecked(value: T) -> Self {
        Self {
            value,
            _enum: PhantomData,
        }
    }

    pub fn value(&self) -> T {
        self.value
    }
}

impl<E: Copy + CEnum, T: Copy + AsBytes + FromMessage> FromMessage for ResizeEnum<E, T>
where
    DecodeError: From<T::Error>,
    DecodeError: From<<E as TryFrom<T>>::Error>,
    E: TryFrom<T>,
{
    type Error = DecodeError;
    type Context = T::Context;

    fn decode<B: bytes::Buf>(context: Self::Context, b: &mut B) -> Result<Option<Self>, Self::Error> {
        Ok(match T::decode(context, b)? {
            Some(value) => Some({
                E::try_from(value)?;
                unsafe {
                    Self::with_value_unchecked(value)
                }
            }),
            None => None,
        })
    }
}

impl<E: Copy + CEnum + fmt::Debug, T: Copy + AsPrimitive<E::Repr>> fmt::Debug for ResizeEnum<E, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.get(), f)
    }
}

unsafe impl<E: Copy, T: Copy + AsBytes> AsBytes for ResizeEnum<E, T> {
    fn only_derive_is_allowed_to_implement_this_trait() where Self: Sized { }
}
impl<E: Copy, T: Copy + AsBytes> MessageAsBytes for ResizeEnum<E, T> { }

impl<E: Copy, T: Copy> From<E> for ResizeEnum<E, T> where E: Into<T> {
    fn from(value: E) -> Self {
        Self::new(value)
    }
}

impl<E: Copy + CEnum, T: Copy> ResizeEnum<E, T> {
    pub fn with_value(value: T) -> Option<Self> where E::Repr: TryFrom<T> {
        let repr = TryFrom::try_from(value).ok()?;
        match E::try_from(repr) {
            Ok(_) => Some(unsafe { Self::with_value_unchecked(value) }),
            _ => None,
        }
    }

    pub fn get(&self) -> E where T: AsPrimitive<E::Repr> {
        unsafe {
            E::from_repr_unchecked(self.value.as_())
        }
    }
}

pub trait WithRepresentation<T> {
    type Output;
}

impl<E: RawBitFlags> WithRepresentation<E::Type> for BitFlags<E> {
    //type Output = Self;
    //type Output = BitMask<E>;
    type Output = ResizeMask<E, E::Type>;
}

pub type Mask<E, T> = <BitFlags<E> as WithRepresentation<T>>::Output;
pub type Enum<E, T> = <E as WithRepresentation<T>>::Output;
