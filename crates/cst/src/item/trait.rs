use parserc::syntax::Syntax;

use crate::{
    generics::Generics,
    input::CSTInput,
    keyword::{Mut, Self_, Trait},
    misc::Ident,
    punct::{And, Colon},
    ty::Type,
    vs::Visibility,
};

/// The self argument of an associated method.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum SelfParam<I>
where
    I: CSTInput,
{
    TypeSelf {
        /// optional mutability keyword `mut`
        mutability: Option<Mut<I>>,
        /// keyword `self` or `Self`
        keyword_self: Self_<I>,
        /// punct `:`
        #[parserc(crucial)]
        colon: Colon<I>,
        /// Self type declaration.
        ty: Box<Type<I>>,
    },
    ShortHand {
        /// optional reference char `&`
        reference: Option<And<I>>,
        /// optional mutability keyword `mut`
        mutability: Option<Mut<I>>,
        /// keyword `self` or `Self`
        keyword_self: Self_<I>,
    },
}

/// A trait definition: `pub trait Iterator { ... }`.
pub struct ItemTrait<I>
where
    I: CSTInput,
{
    /// optional visibility marker.
    pub visibility: Option<Visibility<I>>,
    /// leading keyword `trait`
    pub keyword: Trait<I>,
    /// trait name.
    pub ident: Ident<I>,
    /// optional generic parameters: <T,..>
    pub generics: Option<Generics<I>>,
}
