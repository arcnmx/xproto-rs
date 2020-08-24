use core::iter::FusedIterator;
#[cfg(feature = "xc_misc")]
use core::ops;
#[cfg(feature = "xc_misc")]
use xproto::xc_misc::GetXIDRangeReply;
use xproto::Xid;

#[derive(Debug, Copy, Clone, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct XidRange {
    shift: u32,
    end: u32,
    base: u32,
}

impl XidRange {
    pub fn new(start: u32, mask: u32) -> Self {
        let shift = mask.trailing_zeros();
        Self {
            base: start,
            end: (start | mask)
                .wrapping_add(1u32.checked_shl(shift).unwrap_or(0)),
            shift,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.base == self.end
    }

    pub fn peek(&self) -> Option<u32> {
        if self.is_empty() {
            None
        } else {
            Some(self.base)
        }
    }

    fn step(&self) -> u32 {
        1u32 << self.shift
    }

    pub fn increment(&mut self) {
        self.base = self.base.wrapping_add(self.step());
    }

    #[cfg(feature = "xc_misc")]
    pub fn extend(&self, range: &GetXIDRangeReply) -> Self {
        let count = if range.start_id == 0 {
            // indicates no IDs left
            0
        } else {
            range.count.checked_shl(self.shift).unwrap_or(0)
        };
        Self {
            base: range.start_id,
            end: range.start_id.wrapping_add(count),
            shift: self.shift,
        }
    }
}

impl Iterator for XidRange {
    type Item = Xid;

    fn next(&mut self) -> Option<Self::Item> {
        let res = self.peek()?;
        self.increment();
        Some(res.into())
    }
}

impl FusedIterator for XidRange { }

#[cfg(feature = "xc_misc")]
impl ops::Add<GetXIDRangeReply> for XidRange {
    type Output = XidRange;

    fn add(self, rhs: GetXIDRangeReply) -> Self::Output {
        self.extend(&rhs)
    }
}

#[cfg(feature = "xc_misc")]
impl ops::Add<&'_ GetXIDRangeReply> for XidRange {
    type Output = XidRange;

    fn add(self, rhs: &GetXIDRangeReply) -> Self::Output {
        self.extend(rhs)
    }
}

#[cfg(feature = "xc_misc")]
impl ops::Add<GetXIDRangeReply> for &'_ XidRange {
    type Output = XidRange;

    fn add(self, rhs: GetXIDRangeReply) -> Self::Output {
        self.extend(&rhs)
    }
}

#[cfg(feature = "xc_misc")]
impl ops::Add<&'_ GetXIDRangeReply> for &'_ XidRange {
    type Output = XidRange;

    fn add(self, rhs: &GetXIDRangeReply) -> Self::Output {
        self.extend(rhs)
    }
}

#[cfg(feature = "xc_misc")]
impl ops::AddAssign<GetXIDRangeReply> for XidRange {
    fn add_assign(&mut self, rhs: GetXIDRangeReply) {
        *self = self.extend(&rhs);
    }
}

#[cfg(feature = "xc_misc")]
impl ops::AddAssign<&'_ GetXIDRangeReply> for XidRange {
    fn add_assign(&mut self, rhs: &GetXIDRangeReply) {
        *self = self.extend(rhs);
    }
}

#[cfg(test)]
mod test {
    #[cfg(feature = "xc_misc")]
    use xproto::xc_misc::GetXIDRangeReply;
    use super::XidRange;

    #[test]
    fn exhaustive() {
        let mut allocator = XidRange::new(0x2800, 0x1ff);
        for expected in 0x2800..=0x29ff {
            assert_eq!(
                expected,
                allocator.next().unwrap()
            );
        }
        assert!(allocator.is_empty());
    }

    #[test]
    fn increment() {
        let mut allocator = XidRange::new(0, 0b1100);
        assert_eq!(0b0000, allocator.next().unwrap());
        assert_eq!(0b0100, allocator.next().unwrap());
        assert_eq!(0b1000, allocator.next().unwrap());
        assert_eq!(0b1100, allocator.next().unwrap());
        assert!(allocator.is_empty());
    }

    #[test]
    fn new_range() {
        #[cfg(feature = "xc_misc")]
        let reply = generate_get_xid_range_reply(0x13370, 3);
        let mut allocator = XidRange::new(0x420, 2);
        assert_eq!(0x420, allocator.next().unwrap());
        assert_eq!(0x422, allocator.next().unwrap());
        // At this point the range is exhausted and a GetXIDRange request is sent
        assert!(allocator.is_empty());
        #[cfg(feature = "xc_misc")] {
            allocator += reply;
            assert_eq!(0x13370, allocator.next().unwrap());
            assert_eq!(0x13372, allocator.next().unwrap());
            assert_eq!(0x13374, allocator.next().unwrap());
            // At this point the range is exhausted and a GetXIDRange request is sent
            assert!(allocator.is_empty());
            // Simulate the server running out of available XID ranges
            allocator += generate_get_xid_range_reply(0, 1);
            assert!(allocator.is_empty());
        }
    }

    #[test]
    fn invalid_arg() {
        let err = XidRange::new(1234, 0);
        assert!(err.is_empty());
    }

    #[cfg(feature = "xc_misc")]
    fn generate_get_xid_range_reply(start_id: u32, count: u32) -> GetXIDRangeReply {
        GetXIDRangeReply {
            sequence: 0,
            length: 0,
            start_id,
            count,
        }
    }
}
