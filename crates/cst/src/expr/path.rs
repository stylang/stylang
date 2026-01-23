use parserc::syntax::Syntax;

use crate::{
    input::CSTInput,
    names::paths::{PathInExpr, QuilifiedPathInExpr},
};

/// A path used as an expression context denotes either a local variable or an item.
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions/path-expr.html#grammar-PathExpression
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum PathExpr<I>
where
    I: CSTInput,
{
    Path(PathInExpr<I>),
    Qualified(QuilifiedPathInExpr<I>),
}
