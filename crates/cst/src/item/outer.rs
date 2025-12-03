use parserc::syntax::Syntax;

use crate::{
    input::CSTInput,
    item::OuterAttr,
    misc::{OuterBlockDoc, OuterLineDoc},
};

/// misc items: outer documents or attributes.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Outer<I>
where
    I: CSTInput,
{
    OuterLineDoc(OuterLineDoc<I>),
    OuterBlockDoc(OuterBlockDoc<I>),
    OuterAttr(OuterAttr<I>),
}
