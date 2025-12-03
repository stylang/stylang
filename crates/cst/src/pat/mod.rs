//! A pattern in a local binding, function signature, match expression, or various other places.

use parserc::syntax::Syntax;

use crate::{input::CSTInput, misc::Ident, punct::Colon, ty::Type};

/// A type ascription pattern: foo: f64.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PatType<I>
where
    I: CSTInput,
{
    pub pat: Box<Pat<I>>,
    pub colon: Colon<I>,
    pub ty: Box<Type<I>>,
}

/// A pattern in a local binding, function signature, match expression, or various other places.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Pat<I>
where
    I: CSTInput,
{
    Type(PatType<I>),
    Ident(Ident<I>),
}
