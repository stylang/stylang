use parserc::syntax::Syntax;

use crate::{
    input::CSTInput,
    syntax::{OuterAttribute, OuterBlockDoc, OuterLineDoc},
    token::S,
};

/// directly child of one module.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Item<I>
where
    I: CSTInput,
{
    S(S<I>),
    OuterLineDoc(OuterLineDoc<I>),
    OuterBlockDoc(OuterBlockDoc<I>),
    OuterAttribute(OuterAttribute<I>),
}
