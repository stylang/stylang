use parserc::syntax::Syntax;

use crate::{CSTInput, Colon, Eequal, Ident, Lit, Question, Type};
/// A trait used as a bound on a type parameter.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TraitBound<I>
where
    I: CSTInput,
{
    /// A modifier on a trait bound, currently only used for the ? in ?Sized.
    modifier: Option<Question<I>>,
}

/// An individual generic argument, like  T, or Item = T.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum GenericArgument<I>
where
    I: CSTInput,
{
    /// A type argument.
    Type(Type<I>),
    /// A binding (equality constraint) on an associated type: the Item = u8 in Iterator<Item = u8>.
    Associated(Ident<I>, Eequal<I>, Type<I>),
    /// An equality constraint on an associated constant: the PANIC = false in Trait<PANIC = false>.
    AssociatedConst(Ident<I>, Eequal<I>, Lit<I>),
}

/// A generic type parameter: `T: Into<String>`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TypeParam<I>
where
    I: CSTInput,
{
    /// Name of generic type parameter.
    pub name: Ident<I>,
    pub bounds: Option<Colon<I>>,
}
