//! Syntax parser for `stylang`.

mod comments;
pub use comments::*;

mod lit;
pub use lit::*;

mod attr;
pub use attr::*;

mod item;
pub use item::*;

mod ty;
pub use ty::*;

mod vs;
pub use vs::*;
