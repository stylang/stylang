use parserc::syntax::Syntax;

use crate::{expr::LitExpr, input::CSTInput};

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/patterns.html#railroad-PatternNoTopAlt
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum PattNoTopAlt<I>
where
    I: CSTInput,
{
    Lit(LitExpr<I>),
}
