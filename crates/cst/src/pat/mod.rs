//! A pattern in a local binding, function signature, match expression, or various other places.

mod ty;
pub use ty::*;

mod refer;
pub use refer::*;

mod ident;
pub use ident::*;

mod slice;
pub use slice::*;

mod tuple;
pub use tuple::*;

mod r#struct;
pub use r#struct::*;

mod pat;
pub use pat::*;
