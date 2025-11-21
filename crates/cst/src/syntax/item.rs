use parserc::syntax::{Delimiter, Punctuated, Syntax};

use crate::{
    input::CSTInput,
    syntax::{OuterAttribute, OuterBlockDoc, OuterLineDoc, Type},
    token::{
        Ident, S,
        punct::{BraceEnd, BraceStart, Colon, Comma, ParenEnd, ParenStart},
    },
};

/// fields list of struct.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum StructFields<I>
where
    I: CSTInput,
{
    /// named fields: { xx:tt,... }
    Named(
        Delimiter<BraceStart<I>, BraceEnd<I>, Punctuated<(Ident<I>, Colon<I>, Type<I>), Comma<I>>>,
    ),

    /// unnamed fields: (tt,...);
    Unnamed(Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<Type<I>, Comma<I>>>),
}

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
