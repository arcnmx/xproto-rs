use core::convert::TryFrom;
use core::ops;

pub(crate) trait UncheckedFrom<T>: Sized {
    unsafe fn unchecked_from(value: T) -> Self;
}

impl<F, T: From<F>> UncheckedFrom<F> for T {
    unsafe fn unchecked_from(value: F) -> Self {
        value.into()
    }
}

pub(crate) trait UncheckedInto<T>: Sized {
    unsafe fn unchecked_into(self) -> T;
}

impl<F, T: UncheckedFrom<F>> UncheckedInto<T> for F {
    unsafe fn unchecked_into(self) -> T {
        UncheckedFrom::unchecked_from(self)
    }
}

pub trait AsPrimitive<T> {
    fn as_(self) -> T;
}

impl AsPrimitive<bool> for u8 {
    fn as_(self) -> bool { self != 0 }
}

impl AsPrimitive<bool> for u16 {
    fn as_(self) -> bool { self != 0 }
}

impl AsPrimitive<bool> for u32 {
    fn as_(self) -> bool { self != 0 }
}

impl AsPrimitive<u8> for bool {
    fn as_(self) -> u8 { self as _ }
}

impl AsPrimitive<u8> for u16 {
    fn as_(self) -> u8 { self as _ }
}

impl AsPrimitive<u8> for u32 {
    fn as_(self) -> u8 { self as _ }
}

impl AsPrimitive<u16> for bool {
    fn as_(self) -> u16 { self as _ }
}

impl AsPrimitive<u16> for u8 {
    fn as_(self) -> u16 { self as _ }
}

impl AsPrimitive<u16> for u32 {
    fn as_(self) -> u16 { self as _ }
}

impl AsPrimitive<u32> for bool {
    fn as_(self) -> u32 { self as _ }
}

impl AsPrimitive<u32> for u8 {
    fn as_(self) -> u32 { self as _ }
}

impl AsPrimitive<u32> for u16 {
    fn as_(self) -> u32 { self as _ }
}

pub(crate) trait Promote<RHS> {
    type Output/*: AsPrimitive<Self>*/;

    fn promote(self) -> Self::Output /*{
        self.as_()
    }*/;
}

/*impl<LHS, RHS: From<LHS>> Promote<RHS> for LHS {
    type Output = RHS;

    fn promote(self) -> Self::Output {
        self.into()
    }
}*/

impl<T> Promote<T> for T {
    type Output = T;
    fn promote(self) -> Self::Output {
        self
    }
}

impl Promote<u8> for bool {
    type Output = u8;
    fn promote(self) -> Self::Output {
        self as _
    }
}

impl Promote<bool> for u8 {
    type Output = u8;
    fn promote(self) -> Self::Output {
        self
    }
}

/*impl Promote<bool> for usize {
    type Output = usize;

    fn promote(self) -> Self::Output {
        self.into()
    }
}*/

trait PromotedOp<RHS> {
    type Output;

    fn mul(self, rhs: RHS) -> Self::Output;
}

impl<OO, O: ops::Mul<Output=OO>, LHS: Promote<RHS, Output=O>, RHS: Promote<LHS, Output=O>> PromotedOp<RHS> for LHS {
    type Output = OO;

    fn mul(self, rhs: RHS) -> Self::Output {
        self.promote() * rhs.promote()
    }
}

pub(crate) struct Promoted<T>(pub(crate) T);

macro_rules! impl_op {
    ($op:ident $opfn:ident $operator:tt) => {
        impl<OO, O: ops::$op<Output=OO>, LHS: Promote<RHS, Output=O>, RHS: Promote<LHS, Output=O>> ops::$op<Promoted<RHS>> for Promoted<LHS> {
            type Output = OO;

            fn $opfn(self, rhs: Promoted<RHS>) -> Self::Output {
                self.0.promote() $operator rhs.0.promote()
            }
        }
    };
}

impl_op! { Mul mul * }
impl_op! { Div div / }
impl_op! { Add add + }
impl_op! { Sub sub - }
impl_op! { BitAnd bitand & }
impl_op! { BitOr bitor | }
impl_op! { Shl shl << }

pub trait CEnum: Copy + TryFrom<<Self as CEnum>::Repr> + Into<<Self as CEnum>::Repr> {
    type Repr: Copy + From<Self> + 'static;

    unsafe fn from_repr_unchecked(repr: Self::Repr) -> Self;

    #[inline]
    fn from_repr(repr: Self::Repr) -> Option<Self> {
        Self::try_from(repr).ok()
    }

    #[inline]
    fn repr(self) -> Self::Repr {
        self.into()
    }
}
