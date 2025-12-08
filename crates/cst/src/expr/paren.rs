use parserc::syntax::{Delimiter, Punctuated, Syntax};

use crate::{
    expr::Expr,
    input::CSTInput,
    punct::{Comma, ParenEnd, ParenStart},
};

/// A tuple expression: `(a, b, c, d)`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprTuple<I>(pub Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<Expr<I>, Comma<I>>>)
where
    I: CSTInput;
