use crate::{
    expr::Expr,
    input::CSTInput,
    lexical::punct::{DotDot, DotDotEq},
};

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions/range-expr.html#grammar-RangeExpression
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum RangeExpr<I>
where
    I: CSTInput,
{
    Range(Expr<I>, DotDot<I>, Expr<I>),
    RangeFrom(Expr<I>, DotDot<I>),
    RangeTo(DotDot<I>, Expr<I>),
    RangeFull(DotDot<I>),
    RangeInclusive(Expr<I>, DotDotEq<I>, Expr<I>),
    RangeToInclusive(DotDotEq<I>, Expr<I>),
}
