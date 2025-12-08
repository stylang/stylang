use parserc::syntax::{Delimiter, Syntax};

use crate::{
    expr::Expr,
    input::CSTInput,
    punct::{BracketEnd, BracketStart},
};

/// A square bracketed indexing expression: `vector[2]`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Index<I>(pub Delimiter<BracketStart<I>, BracketEnd<I>, Box<Expr<I>>>)
where
    I: CSTInput;
