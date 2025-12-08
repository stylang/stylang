use parserc::syntax::{Delimiter, Punctuated, Syntax};

use crate::{
    expr::Expr,
    input::CSTInput,
    punct::{BracketEnd, BracketStart, Comma},
};

/// A slice literal expression
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprArray<I>(
    pub Delimiter<BracketStart<I>, BracketEnd<I>, Punctuated<Expr<I>, Comma<I>>>,
)
where
    I: CSTInput;
