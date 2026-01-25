use parserc::syntax::{Punctuated, Syntax};

use crate::{
    expr::Expr,
    input::CSTInput,
    lexical::{
        delimiter::Bracket,
        punct::{BracketEnd, BracketStart, Comma, Semi},
    },
};

/// Array expressions construct arrays. Array expressions come in two forms.
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions/array-expr.html#grammar-ArrayExpression
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ArrayExpr<I>
where
    I: CSTInput,
{
    Repeat {
        delimiter_start: BracketStart<I>,
        value: Box<Expr<I>>,
        semi: Semi<I>,
        length: Box<Expr<I>>,
        delimiter_end: BracketEnd<I>,
    },
    List(Bracket<I, Punctuated<Expr<I>, Comma<I>>>),
}
