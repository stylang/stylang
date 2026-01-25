use parserc::syntax::{Punctuated, Syntax};

use crate::{
    attr::OuterAttribute,
    expr::{BlockExpr, Expr},
    input::CSTInput,
    lexical::{
        keywords::strict::{Async, Move},
        punct::{self, Colon, Comma, RArrow},
    },
    pat::PattNoTopAlt,
    types::{Type, TypeNoBounds},
};

/// A closure expression, also known as a lambda expression or a lambda
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions/closure-expr.html#grammar-ClosureExpression
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ClosureExpr<I>
where
    I: CSTInput,
{
    /// optional `async` keyword
    pub async_keyword: Option<Async<I>>,
    /// optional `move` keyword
    pub move_keyword: Option<Move<I>>,
    /// delimiter start punct `|`
    pub delimiter_start: punct::Or<I>,
    /// parameter-list
    pub params: Punctuated<ClosureParam<I>, Comma<I>>,
    /// delimiter end punct `|`
    pub delimiter_end: punct::Or<I>,
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions/closure-expr.html#railroad-ClosureParam
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ClosureParam<I>
where
    I: CSTInput,
{
    /// optional attribute-list.
    pub attrs: OuterAttribute<I>,
    /// parameter pattern
    pub patt: PattNoTopAlt<I>,
    /// explicit type declaration.
    pub ty: Option<(Colon<I>, Type<I>)>,
    /// body of this closure.
    pub body: ClosureBody<I>,
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions/closure-expr.html#railroad-ClosureExpression
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ClosureBody<I>
where
    I: CSTInput,
{
    Block {
        rarrow: RArrow<I>,
        output: TypeNoBounds<I>,
        block: BlockExpr<I>,
    },
    Expr(Expr<I>),
}
