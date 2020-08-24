mod parse_display;
pub use parse_display::{Display, ParseDisplayError};

mod xauth;
pub use xauth::{AuthInfo, get_auth};

mod id_allocator;
pub use id_allocator::XidRange;

#[cfg(feature = "tokio-util")]
pub mod codec;

#[cfg(feature = "with-tokio")]
pub mod stream;
