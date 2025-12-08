use parserc::syntax::{Delimiter, Punctuated, Syntax};

use crate::{
    input::CSTInput,
    keyword::Const,
    pat::Pat,
    punct::{ArrowRight, Comma, Or},
    ty::Type,
};

/// A closure expression: |a, b| a + b.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprClosure<I>
where
    I: CSTInput,
{
    /// optional `const` keyword.
    pub constness: Option<Const<I>>,
    /// input arguments of closure.
    pub inputs: Delimiter<Or<I>, Or<I>, Punctuated<Pat<I>, Comma<I>>>,
    /// Return type of a function signature.
    pub output: Option<(ArrowRight<I>, Box<Type<I>>)>,
}
