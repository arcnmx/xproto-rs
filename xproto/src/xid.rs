pub type Xid = u32;

macro_rules! xid {
    ($name:ident) => {
        #[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, zerocopy::AsBytes, zerocopy::FromBytes)]
        #[repr(transparent)]
        pub struct $name(Xid);
        impl MessageAsBytes for $name { }
        impl MessageFromBytes for $name { }

        impl $name {
            #[inline]
            pub const fn new(xid: Xid) -> Self {
                Self(xid)
            }

            #[inline]
            pub const fn xid(&self) -> Xid {
                self.0
            }
        }

        impl From<$name> for Xid {
            #[inline]
            fn from(xid: $name) -> Self {
                xid.xid()
            }
        }

        impl From<Xid> for $name {
            #[inline]
            fn from(xid: Xid) -> Self {
                Self::new(xid)
            }
        }
    };
}

macro_rules! xid_union {
    ($name:ident $kindname:ident => $($var:ident : $xid:ty,)+) => {
        #[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, zerocopy::AsBytes, zerocopy::FromBytes)]
        #[repr(transparent)]
        pub struct $name(Xid);
        impl MessageAsBytes for $name { }
        impl MessageFromBytes for $name { }

        impl $name {
            #[inline]
            pub const fn new(xid: Xid) -> Self {
                Self(xid)
            }

            $(
                #[inline]
                pub const fn $var(&self) -> $xid {
                    <$xid>::new(self.xid())
                }
            )+

            #[inline]
            pub const fn xid(&self) -> Xid {
                self.0
            }
        }

        $(
            impl From<$xid> for $name {
                #[inline]
                fn from(xid: $xid) -> Self {
                    Self::new(xid.xid())
                }
            }

            impl crate::conversion::UncheckedFrom<$name> for $xid {
                #[inline]
                unsafe fn unchecked_from(xid: $name) -> Self {
                    xid.$var()
                }
            }
        )+

        impl From<Xid> for $name {
            #[inline]
            fn from(xid: Xid) -> Self {
                Self::new(xid)
            }
        }
    };
}
