use parserc::syntax::Syntax;

use crate::{
    input::CSTInput,
    keyword::{Mut, Ref},
    misc::Ident,
    pat::Pat,
    punct::At,
};

/// Identifier patterns bind the value they match to a variable in the value namespace.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PatIdent<I>
where
    I: CSTInput,
{
    /// an optional keyword `ref`.
    pub by_ref: Option<Ref<I>>,
    /// an optional keyword `mut`
    pub mutability: Option<Mut<I>>,
    /// unique identifier within the pattern.
    pub ident: Ident<I>,
    /// Pattern no top alt.
    pub supat: Option<(At<I>, Box<Pat<I>>)>,
}
