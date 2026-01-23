//! Every variable, item, and value in a Rust program has a type.
//! The type of a value defines the interpretation of the memory
//! holding it and the operations that may be performed on the value.
//!
//! More information see [`The Rust Reference`]
//!
//! [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/types.html#grammar-Type

use parserc::syntax::{Or, Punctuated, Syntax};

use crate::{
    attr::OuterAttribute,
    errors::CSTError,
    input::CSTInput,
    lexical::{
        delimiter::{Bracket, Paren},
        ident::Ident,
        keywords::strict::{Const, Dyn, Extern, For, Impl, Mut, SelfUpper, Unsafe, Use},
        label::LifeTime,
        lit::{LitDec, LitRawStr, LitStr},
        punct::{
            And, Colon, Comma, DotDotDot, Eq, Gt, Lt, Not, ParenEnd, ParenStart, Plus, Question,
            RArrow, Semi, Star, Underscore,
        },
    },
    macros::invocation::MacroInvocation,
    names::paths::{QuilifiedPathInType, TypePath},
};

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/types.html#railroad-Type
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Type<I>
where
    I: CSTInput,
{
    ImplTraitType(Box<ImplTraitType<I>>),
    TraitObjectType(Box<TraitObjectType<I>>),
    NoBounds(TypeNoBounds<I>),
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/types.html#railroad-TypeNoBounds
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum TypeNoBounds<I>
where
    I: CSTInput,
{
    /// More information see [`The Rust Reference`]
    ///
    /// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/paths.html#grammar-QualifiedPathInType
    QualifiedPathInType(Box<QuilifiedPathInType<I>>),

    /// More information see [`The Rust Reference`]
    ///
    /// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/types/impl-trait.html#grammar-ImplTraitTypeOneBound
    ImplTraitTypeOneBound(Box<ImplTraitTypeOneBound<I>>),

    /// More information see [`The Rust Reference`]
    ///
    /// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/types/trait-object.html#grammar-TraitObjectTypeOneBound
    TraitObjectTypeOneBound(Box<TraitObjectTypeOneBound<I>>),

    /// More information see [`The Rust Reference`]
    ///
    /// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/types.html#r-type.name.path
    TypePath(Box<TypePath<I>>),

    /// In some situations the combination of types may be ambiguous.
    /// Use parentheses around a type to avoid ambiguity.
    ///
    /// More information see [`The Rust Reference`]
    ///
    /// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/types.html#r-type.name.parenthesized.intro
    Paren(Paren<I, Box<Type<I>>>),

    /// More information see [`The Rust Reference`]
    ///
    /// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/types/tuple.html#grammar-TupleType
    TupleType(Paren<I, Punctuated<Type<I>, Comma<I>>>),
    /// More information see [`The Rust Reference`]
    ///
    /// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/types/never.html#grammar-NeverType
    Never(Not<I>),
    /// More information see [`The Rust Reference`]
    ///
    /// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/types/pointer.html#grammar-ReferenceType
    RawPointerType {
        star: Star<I>,
        modifier: Or<Mut<I>, Const<I>>,
        type_no_bounds: Box<TypeNoBounds<I>>,
    },
    /// More information see [`The Rust Reference`]
    ///
    /// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/types/pointer.html#grammar-ReferenceType
    Reference {
        leading_token: And<I>,
        lifetime: Option<LifeTime<I>>,
        mut_keyword: Option<Mut<I>>,
        type_no_bounds: Box<TypeNoBounds<I>>,
    },
    /// More information see [`The Rust Reference`]
    ///
    /// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/types/pointer.html#grammar-ReferenceType
    ///
    /// TODO: Replace `LitDec` with `constant expressions`
    ArrayType(Bracket<I, (Box<Type<I>>, Semi<I>, LitDec<I>)>),
    /// More information see [`The Rust Reference`]
    ///
    /// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/types/pointer.html#grammar-ReferenceType
    SliceType(Bracket<I, Box<Type<I>>>),
    /// More information see [`The Rust Reference`]
    ///
    /// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/types/inferred.html#grammar-InferredType
    InferredType(Underscore<I>),
    /// More information see [`The Rust Reference`]
    ///
    /// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/types/function-pointer.html#grammar-MaybeNamedFunctionParametersVariadic
    BareFunctionType(Box<BareFunctionType<I>>),

    /// [`Macros`](https://doc.rust-lang.org/stable/reference/macros.html) which expand to a type expression.
    MarcoInvocation(MacroInvocation<I>),
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/types/impl-trait.html#railroad-ImplTraitType
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ImplTraitType<I>
where
    I: CSTInput,
{
    /// keyword `impl`
    pub keyword: Impl<I>,
    /// type parameter bounds.
    pub bounds: TypeParamBounds<I>,
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/types/impl-trait.html#grammar-ImplTraitTypeOneBound
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ImplTraitTypeOneBound<I>
where
    I: CSTInput,
{
    /// keyword `dyn`
    pub keyword: Dyn<I>,
    /// type parameter bounds.
    pub bound: TraitBound<I>,
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/types/trait-object.html#railroad-TraitObjectType
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TraitObjectType<I>
where
    I: CSTInput,
{
    /// keyword `dyn`
    pub keyword: Dyn<I>,
    /// type parameter bounds.
    pub bounds: TypeParamBounds<I>,
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/types/trait-object.html#grammar-TraitObjectTypeOneBound
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TraitObjectTypeOneBound<I>
where
    I: CSTInput,
{
    /// keyword `impl`
    pub keyword: Impl<I>,
    /// type parameter bounds.
    pub bound: TraitBound<I>,
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/trait-bounds.html#grammar-TypeParamBounds
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TypeParamBounds<I>
where
    I: CSTInput,
{
    /// first bound.
    pub first: TypeParamBound<I>,
    /// rest bounds.
    pub rest: Vec<(Plus<I>, TypeParamBound<I>)>,
    /// optional tailing `+`
    pub tailing_plus: Option<Plus<I>>,
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/trait-bounds.html#railroad-TypeParamBound
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum TypeParamBound<I>
where
    I: CSTInput,
{
    LifeTime(LifeTime<I>),
    UseBound(UseBound<I>),
    TraitBound(TraitBound<I>),
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/trait-bounds.html#railroad-UseBound
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct UseBound<I>
where
    I: CSTInput,
{
    /// leading keyword `use`
    pub keyword: Use<I>,
    /// leading punct `<`
    pub leading_delimiter: Lt<I>,
    /// generic arguments.
    pub generic_args: Punctuated<UseBoundGenericArg<I>, Comma<I>>,
    /// leading punct `>`
    pub tailing_delimiter: Gt<I>,
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/trait-bounds.html#railroad-UseBound
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum UseBoundGenericArg<I>
where
    I: CSTInput,
{
    SelfUpper(SelfUpper<I>),
    LifeTime(LifeTime<I>),
    Ident(Ident<I>),
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/items/generics.html#grammar-GenericParams
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum GenericParam<I>
where
    I: CSTInput,
{
    LifetimeParam {
        lifetime: LifeTime<I>,
        bound: Option<(Colon<I>, Punctuated<LifeTime<I>, Plus<I>>)>,
    },
    TypeParam {
        ident: Ident<I>,
        bounds: Option<(Colon<I>, TypeParamBounds<I>)>,
        assign: Option<(Eq<I>, Type<I>)>,
    },
    ConstParam {
        keyword: Const<I>,
        ident: Ident<I>,
        colon: Colon<I>,
        ty: Type<I>,
        /// TODO: replace `Ident` to `const expr`
        assign: Option<(Eq<I>, Ident<I>)>,
    },
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/items/generics.html#grammar-GenericParams
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct GenericParams<I>
where
    I: CSTInput,
{
    /// leading punct `<`
    pub leading_delimiter: Lt<I>,
    /// generic arguments.
    pub generic_args: Punctuated<(Vec<OuterAttribute<I>>, GenericParam<I>), Comma<I>>,
    /// leading punct `>`
    pub tailing_delimiter: Gt<I>,
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/trait-bounds.html#grammar-ForLifetimes
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ForLifetimes<I>
where
    I: CSTInput,
{
    /// leading keyword `for`
    pub keyword: For<I>,
    /// generic parameter list: `<...>`
    pub generic_params: GenericParams<I>,
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/trait-bounds.html#railroad-TypeParamBound
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum TraitBound<I>
where
    I: CSTInput,
{
    TypePath {
        /// optional lifetime or `?`
        modifier: Option<Or<Question<I>, ForLifetimes<I>>>,
        /// trait bound type path.
        type_path: TypePath<I>,
    },
    Paren {
        /// leading delimiter `(`
        leading_delimiter: ParenStart<I>,
        /// optional lifetime or `?`
        modifier: Option<Or<Question<I>, ForLifetimes<I>>>,
        /// trait bound type path.
        type_path: TypePath<I>,
        /// tailing delimiter `)`
        tailing_delimiter: ParenEnd<I>,
    },
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/types/function-pointer.html#grammar-BareFunctionType
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct BareFunctionType<I>
where
    I: CSTInput,
{
    /// optional `for<...>`
    pub for_lifetime: ForLifetimes<I>,
    /// unsafe extern "C"...
    pub qualifiers: FunctionTypeQualifiers<I>,
    /// input parameters.
    pub inputs: Paren<I, FunctionParametersMaybeNamedVariadic<I>>,
    /// `-> TypeNoBounds`
    pub outputs: Option<(RArrow<I>, TypeNoBounds<I>)>,
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/types/function-pointer.html#grammar-BareFunctionType
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct FunctionTypeQualifiers<I>
where
    I: CSTInput,
{
    pub unsafe_keyword: Option<Unsafe<I>>,
    pub extern_keyword: Option<(Extern<I>, Option<Or<LitStr<I>, LitRawStr<I>>>)>,
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/types/function-pointer.html#grammar-BareFunctionType
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(semantic = check_variadic)]
pub struct FunctionParametersMaybeNamedVariadic<I>
where
    I: CSTInput,
{
    pub maybe_named_params: Punctuated<MaybeNamedParam<I>, Comma<I>>,
    pub variadic: Option<(Vec<OuterAttribute<I>>, DotDotDot<I>)>,
}

#[inline]
fn check_variadic<I>(
    _: I,
    parms: FunctionParametersMaybeNamedVariadic<I>,
) -> Result<FunctionParametersMaybeNamedVariadic<I>, CSTError>
where
    I: CSTInput,
{
    if parms.variadic.is_some() && parms.maybe_named_params.len() == 0 {
        return Err(CSTError::Semantics(
            crate::errors::SemanticsKind::FunctionParametersMaybeNamedVariadic,
            parms.to_span(),
        ));
    }

    Ok(parms)
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/types/function-pointer.html#grammar-BareFunctionType
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct MaybeNamedParam<I>
where
    I: CSTInput,
{
    pub attrs: Vec<OuterAttribute<I>>,
    pub ident: Option<(Or<Ident<I>, Underscore<I>>, Colon<I>)>,
    pub ty: Type<I>,
}

#[cfg(test)]
mod tests {

    use parserc::syntax::{Delimiter, SyntaxInput};

    use crate::{
        input::TokenStream,
        lexical::{S, ident::NonKeywordIdent},
        names::paths::{PathIdentSegment, TypePathSegment},
    };

    use super::*;

    #[test]
    fn test_type_path() {
        assert_eq!(
            TokenStream::from("i32").parse::<Type<_>>(),
            Ok(Type::NoBounds(TypeNoBounds::TypePath(Box::new(TypePath {
                leading_path_sep: None,
                first: TypePathSegment {
                    ident: PathIdentSegment::Ident(Ident::NonKeywordIdent(NonKeywordIdent(
                        TokenStream::from((0, "i32"))
                    ))),
                    args: None
                },
                rest: vec![]
            }))))
        );
    }

    #[test]
    fn test_paren_type() {
        assert_eq!(
            TokenStream::from("&'a (dyn Any + Send)").parse::<Type<_>>(),
            Ok(Type::NoBounds(TypeNoBounds::Reference {
                leading_token: And(None, TokenStream::from((0, "&")), None),
                lifetime: Some(LifeTime(TokenStream::from((1, "'a")))),
                mut_keyword: None,
                type_no_bounds: Box::new(TypeNoBounds::Paren(Delimiter {
                    start: ParenStart(
                        Some(S(TokenStream::from((3, " ")))),
                        TokenStream::from((4, "(")),
                        None
                    ),
                    end: ParenEnd(None, TokenStream::from((19, ")")), None),
                    body: Box::new(Type::TraitObjectType(Box::new(TraitObjectType {
                        keyword: Dyn(
                            TokenStream::from((5, "dyn")),
                            Some(S(TokenStream::from((8, " "))))
                        ),
                        bounds: TypeParamBounds {
                            first: TypeParamBound::TraitBound(TraitBound::TypePath {
                                modifier: None,
                                type_path: TypePath {
                                    leading_path_sep: None,
                                    first: TypePathSegment {
                                        ident: PathIdentSegment::Ident(Ident::NonKeywordIdent(
                                            NonKeywordIdent(TokenStream::from((9, "Any")))
                                        )),
                                        args: None
                                    },
                                    rest: vec![]
                                }
                            }),
                            rest: vec![(
                                Plus(
                                    Some(S(TokenStream::from((12, " ")))),
                                    TokenStream::from((13, "+")),
                                    Some(S(TokenStream::from((14, " "))))
                                ),
                                TypeParamBound::TraitBound(TraitBound::TypePath {
                                    modifier: None,
                                    type_path: TypePath {
                                        leading_path_sep: None,
                                        first: TypePathSegment {
                                            ident: PathIdentSegment::Ident(Ident::NonKeywordIdent(
                                                NonKeywordIdent(TokenStream::from((15, "Send")))
                                            )),
                                            args: None
                                        },
                                        rest: vec![]
                                    }
                                })
                            )],
                            tailing_plus: None
                        }
                    })))
                }))
            }))
        );
    }

    #[test]
    fn test_impl_trait_type() {
        assert_eq!(
            TokenStream::from("impl Trait ").parse::<TypeNoBounds<_>>(),
            Ok(TypeNoBounds::TraitObjectTypeOneBound(Box::new(
                TraitObjectTypeOneBound {
                    keyword: Impl(
                        TokenStream::from((0, "impl")),
                        Some(S(TokenStream::from((4, " "))))
                    ),
                    bound: TraitBound::TypePath {
                        modifier: None,
                        type_path: TypePath {
                            leading_path_sep: None,
                            first: TypePathSegment {
                                ident: PathIdentSegment::Ident(Ident::NonKeywordIdent(
                                    NonKeywordIdent(TokenStream::from((5, "Trait")))
                                )),
                                args: None
                            },
                            rest: vec![]
                        }
                    }
                }
            )))
        );

        assert_eq!(
            TokenStream::from("impl Trait + Debug").parse::<Type<_>>(),
            Ok(Type::ImplTraitType(Box::new(ImplTraitType {
                keyword: Impl(
                    TokenStream::from((0, "impl")),
                    Some(S(TokenStream::from((4, " "))))
                ),
                bounds: TypeParamBounds {
                    first: TypeParamBound::TraitBound(TraitBound::TypePath {
                        modifier: None,
                        type_path: TypePath {
                            leading_path_sep: None,
                            first: TypePathSegment {
                                ident: PathIdentSegment::Ident(Ident::NonKeywordIdent(
                                    NonKeywordIdent(TokenStream::from((5, "Trait")))
                                )),
                                args: None
                            },
                            rest: vec![]
                        }
                    }),
                    rest: vec![(
                        Plus(
                            Some(S(TokenStream::from((10, " ")))),
                            TokenStream::from((11, "+")),
                            Some(S(TokenStream::from((12, " "))))
                        ),
                        TypeParamBound::TraitBound(TraitBound::TypePath {
                            modifier: None,
                            type_path: TypePath {
                                leading_path_sep: None,
                                first: TypePathSegment {
                                    ident: PathIdentSegment::Ident(Ident::NonKeywordIdent(
                                        NonKeywordIdent(TokenStream::from((13, "Debug")))
                                    )),
                                    args: None
                                },
                                rest: vec![]
                            }
                        })
                    )],
                    tailing_plus: None
                }
            })))
        );

        assert_eq!(
            TokenStream::from("impl Sized + use<'a, T>").parse::<Type<_>>(),
            Ok(Type::ImplTraitType(Box::new(ImplTraitType {
                keyword: Impl(
                    TokenStream::from((0, "impl")),
                    Some(S(TokenStream::from((4, " "))))
                ),
                bounds: TypeParamBounds {
                    first: TypeParamBound::TraitBound(TraitBound::TypePath {
                        modifier: None,
                        type_path: TypePath {
                            leading_path_sep: None,
                            first: TypePathSegment {
                                ident: PathIdentSegment::Ident(Ident::NonKeywordIdent(
                                    NonKeywordIdent(TokenStream::from((5, "Sized")))
                                )),
                                args: None
                            },
                            rest: vec![]
                        }
                    }),
                    rest: vec![(
                        Plus(
                            Some(S(TokenStream::from((10, " ")))),
                            TokenStream::from((11, "+")),
                            Some(S(TokenStream::from((12, " "))))
                        ),
                        TypeParamBound::UseBound(UseBound {
                            keyword: Use(TokenStream::from((13, "use")), None),
                            leading_delimiter: Lt(None, TokenStream::from((16, "<")), None),
                            generic_args: Punctuated {
                                pairs: vec![(
                                    UseBoundGenericArg::LifeTime(LifeTime(TokenStream::from((
                                        17, "'a"
                                    )))),
                                    Comma(
                                        None,
                                        TokenStream::from((19, ",")),
                                        Some(S(TokenStream::from((20, " "))))
                                    )
                                )],
                                tail: Some(Box::new(UseBoundGenericArg::Ident(
                                    Ident::NonKeywordIdent(NonKeywordIdent(TokenStream::from((
                                        21, "T"
                                    ))))
                                )))
                            },
                            tailing_delimiter: Gt(None, TokenStream::from((22, ">")), None)
                        })
                    )],
                    tailing_plus: None
                }
            })))
        );
    }
}
