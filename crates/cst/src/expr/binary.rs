use parserc::syntax::Syntax;

use crate::{
    input::CSTInput,
    punct::{AndAnd, Minus, OrOr, Plus, Rem, Slash, Star},
};

/// Binary operator.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum BinOp<I>
where
    I: CSTInput,
{
    Add(Plus<I>),
    Sub(Minus<I>),
    Mul(Star<I>),
    Div(Slash<I>),
    Rem(Rem<I>),
    And(AndAnd<I>),
    Or(OrOr<I>),
}
