use parserc::syntax::{Delimiter, Punctuated, Syntax};

use crate::{
    And, ArrowRight, CSTInput, Comma, Extern, Fn_, Ident, Mut, ParenEnd, ParenStart, PatType,
    Self_, Semi, Type, Visibility,
};

/// The self argument of an associated method.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Receiver<I>
where
    I: CSTInput,
{
    /// optional reference char `&`
    pub reference: Option<And<I>>,
    /// optional mutability keyword `mut`
    pub mutability: Option<Mut<I>>,
    /// keyword `self` or `Self`
    pub keyword_self: Self_<I>,
}

/// A free-standing extern function: `extern fn process(n: usize) -> Result<()>;`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ItemExternFn<I>
where
    I: CSTInput,
{
    /// optional vs
    pub vs: Option<Visibility<I>>,
    /// required prefix `extern`
    pub keyword_extern: Extern<I>,
    /// keyword `fn`
    pub keyword_fn: Fn_<I>,
    /// name of this function.
    pub ident: Ident<I>,
    /// input parameters.
    pub inputs: Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<PatType<I>, Comma<I>>>,
    /// Return type of a function signature.
    pub output: Option<(ArrowRight<I>, Box<Type<I>>)>,
    /// end token `;`
    pub semi: Semi<I>,
}
