use parserc::syntax::{Delimiter, Punctuated};

use crate::{
    expr::Expr,
    input::CSTInput,
    misc::Ident,
    path::PathArguments,
    punct::{Comma, Dot, ParenEnd, ParenStart},
};

pub type CallArgs<I> = Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<Expr<I>, Comma<I>>>;

/// A function call expression: invoke(a, b).
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Call<I>(pub Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<Expr<I>, Comma<I>>>)
where
    I: CSTInput;

/// A function call expression: invoke(a, b).
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct MethodCall<I>
where
    I: CSTInput,
{
    /// punct `.`
    pub dot: Dot<I>,
    /// method name.
    pub ident: Ident<I>,
    /// syntax `::<...>`
    pub turbofish: Option<PathArguments<I>>,
    /// call arguments.
    pub args: CallArgs<I>,
}
