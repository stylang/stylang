//! An expression may have two roles: it always produces a value,
//! and it may have effects (otherwise known as “side effects”).
//!
//! More information see [`The Rust Reference`]
//!
//! [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions.html

mod lit;
pub use lit::*;

mod path;
pub use path::*;
