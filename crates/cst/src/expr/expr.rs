use parserc::syntax::Syntax;

use crate::{
    attr::OuterAttribute,
    expr::{BlockExpr, LitExpr, PathExpr},
    input::CSTInput,
};

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions.html#grammar-ExpressionWithoutBlock
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ExprWithoutBlockBody<I>
where
    I: CSTInput,
{
    LitExpr(LitExpr<I>),
    PathExpr(PathExpr<I>),
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions.html#grammar-ExpressionWithoutBlock
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ExprWithBlockBody<I>
where
    I: CSTInput,
{
    BlockExpr(Box<BlockExpr<I>>),
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions.html#grammar-ExpressionWithoutBlock
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprWithoutBlock<I>
where
    I: CSTInput,
{
    /// outer attribute list.
    pub attrs: Vec<OuterAttribute<I>>,
    /// expr body.
    pub body: ExprWithoutBlockBody<I>,
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions.html#grammar-ExpressionWithBlock
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprWithBlock<I>
where
    I: CSTInput,
{
    /// outer attribute list.
    pub attrs: Vec<OuterAttribute<I>>,
    /// expr body.
    pub body: ExprWithBlockBody<I>,
}
