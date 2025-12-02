use parserc::syntax::Syntax;

use crate::{CSTInput, Generics, Ident, Struct, Visibility};

/// A struct definition: `struct Foo<A> { x: A }`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ItemStruct<I>
where
    I: CSTInput,
{
    /// optional vs
    pub vs: Option<Visibility<I>>,
    /// leading keyword `struct`
    pub keyword: Struct<I>,
    /// name of this struct.
    pub ident: Ident<I>,
    /// optional type parameters of this struct.
    pub generics: Option<Generics<I>>,
}
