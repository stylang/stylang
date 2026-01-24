use parserc::{
    ControlFlow,
    syntax::{Punctuated, Syntax},
};

use crate::{
    attr::OuterAttribute,
    errors::{CSTError, SyntaxKind},
    expr::{ArrayExpr, BlockExpr, LitExpr, PathExpr},
    input::CSTInput,
    lexical::{delimiter::Paren, punct::Comma},
    macros::invocation::MacroInvocation,
};

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions.html#grammar-ExpressionWithoutBlock
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Expr<I>
where
    I: CSTInput,
{
    WithBlock(ExprWithBlock<I>),
    WithoutBlock(ExprWithoutBlock<I>),
}

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
    TupleExpr(#[parserc(semantic = check_tuple_expr)] Paren<I, Punctuated<Expr<I>, Comma<I>>>),
    GroupedExpr(Paren<I, Box<Expr<I>>>),
    ArrayExpr(ArrayExpr<I>),
    MarcoInvocation(MacroInvocation<I>),
}

#[inline]
fn check_tuple_expr<I>(
    _: I,
    ty: Paren<I, Punctuated<Expr<I>, Comma<I>>>,
) -> Result<Paren<I, Punctuated<Expr<I>, Comma<I>>>, CSTError>
where
    I: CSTInput,
{
    if ty.body.pairs.len() == 0 && ty.body.tail.is_some() {
        return Err(CSTError::Syntax(
            SyntaxKind::TupleExpr,
            ControlFlow::Recovable,
            ty.to_span(),
        ));
    }

    Ok(ty)
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
