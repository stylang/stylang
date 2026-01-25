//! [`Paths`] are used to refer to an entity, possibly in another module or type.
//!
//! [`Paths`]: https://doc.rust-lang.org/reference/paths.html

use parserc::syntax::{Punctuated, Syntax};

use crate::{
    errors::SyntaxKind,
    expr::{BlockExpr, LitExpr},
    input::CSTInput,
    lexical::{
        delimiter::Paren,
        ident::Ident,
        keywords::strict::{As, Crate, SelfLower, SelfUpper, Super},
        label::LifeTime,
        punct::{Colon, Comma, Dollar, Eq, Gt, Lt, Minus, PathSep, RArrow},
    },
    types::{Type, TypeNoBounds, TypeParamBounds},
};

/// Segment of [`SimplePath`]
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/paths.html#railroad-SimplePathSegment
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::SimplePathSegment.map_unhandle())]
pub enum SimplePathSegment<I>
where
    I: CSTInput,
{
    /// keyword `super`
    Super(Super<I>),
    /// keyword `self`
    SelfLower(SelfLower<I>),
    /// keyword `crate`
    Crate(Crate<I>),
    /// segment `$crate`
    VariableCrate(#[parserc(keyword = "$")] I, Crate<I>),
    /// idenfitier
    Ident(Ident<I>),
}

/// Simple paths are used in `visibility markers`, `attributes`, `macros`, and `use` items.
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/paths.html#railroad-SimplePath
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::SimplePath.map_unhandle())]
pub struct SimplePath<I>
where
    I: CSTInput,
{
    /// optional leading path seperate `::`
    pub leading_sep: Option<PathSep<I>>,
    /// first segment
    pub first: SimplePathSegment<I>,
    /// rest segments
    pub rest: Vec<(PathSep<I>, SimplePathSegment<I>)>,
}

/// Paths in expressions allow for paths with generic arguments to be specified.
/// They are used in various places in expressions and patterns.
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/paths.html#paths-in-expressions
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PathInExpr<I>
where
    I: CSTInput,
{
    /// optional leading path seperate `::`
    pub leading_sep: Option<PathSep<I>>,
    /// first segment
    pub first: PathExprSegment<I>,
    /// rest segments
    pub rest: Vec<(PathSep<I>, PathExprSegment<I>)>,
}

/// Segement of [`PathInExpression`]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PathExprSegment<I>
where
    I: CSTInput,
{
    /// segment name.
    pub ident: PathIdentSegment<I>,
    /// optional generic arguments.
    pub generic_args: Option<(PathSep<I>, GenericArgs<I>)>,
}

/// See [`PathExprSegment`]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum PathIdentSegment<I>
where
    I: CSTInput,
{
    Super(Super<I>),
    SelfLower(SelfLower<I>),
    SelfUpper(SelfUpper<I>),
    Crate(Option<Dollar<I>>, Crate<I>),
    Ident(Ident<I>),
}

/// Optional generic args for `PathExprSegment`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct GenericArgs<I>
where
    I: CSTInput,
{
    /// punct `<`
    pub delimiter_start: Lt<I>,
    /// args punctuated by `,`
    pub args: Punctuated<GenericArg<I>, Comma<I>>,
    /// punct `>`
    pub delimiter_end: Gt<I>,
}

/// generic argument for `PathExprSegment`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum GenericArg<I>
where
    I: CSTInput,
{
    Lifetime(LifeTime<I>),
    Type(Type<I>),
    GenericArgsConst(GenericArgsConst<I>),
}

/// generic argument for `PathExprSegment`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum GenericArgsConst<I>
where
    I: CSTInput,
{
    BlockExpr(BlockExpr<I>),
    LitExpr(Option<Minus<I>>, LitExpr<I>),
    SimplePathSegment(SimplePathSegment<I>),
    GenericArgsBinding(GenericArgsBinding<I>),
    GenericArgsBounds(GenericArgsBounds<I>),
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/paths.html#railroad-GenericArgsBinding
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct GenericArgsBinding<I>
where
    I: CSTInput,
{
    /// binding target.
    pub ident: Ident<I>,
    /// optional generic argument list.
    pub generic_args: Option<GenericArgs<I>>,
    /// punct `=`
    pub eq: Eq<I>,
    /// assign type.
    pub ty: Type<I>,
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/paths.html#railroad-GenericArgsBounds
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct GenericArgsBounds<I>
where
    I: CSTInput,
{
    /// binding target.
    pub ident: Ident<I>,
    /// optional generic argument list.
    pub generic_args: Option<GenericArgs<I>>,
    /// punct `:`
    pub colon: Colon<I>,
    /// bounds for target.
    pub bounds: TypeParamBounds<I>,
}

/// Type paths are used within type definitions, trait bounds, type parameter bounds, and qualified paths.
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/paths.html#railroad-TypePath
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TypePath<I>
where
    I: CSTInput,
{
    /// optional leading path seperate punct `::`
    pub leading_path_sep: Option<PathSep<I>>,
    /// first segment.
    pub first: TypePathSegment<I>,
    /// rest segments.
    pub rest: Vec<(PathSep<I>, TypePathSegment<I>)>,
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/paths.html#railroad-TypePathSegment
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TypePathSegment<I>
where
    I: CSTInput,
{
    /// Name of this segment.
    pub ident: PathIdentSegment<I>,
    /// optional segument arguments.
    pub args: Option<TypePathArgs<I>>,
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/paths.html#railroad-TypePathSegment
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum TypePathArgs<I>
where
    I: CSTInput,
{
    GenericArgs(Option<PathSep<I>>, GenericArgs<I>),
    TypePathFn(Option<PathSep<I>>, TypePathFn<I>),
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/paths.html#railroad-TypePathFn
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TypePathFn<I>
where
    I: CSTInput,
{
    /// input parameters list, with delimiters `(` + `)`
    pub inputs: Paren<I, Punctuated<Type<I>, Comma<I>>>,
    /// output parameters.
    pub outputs: Option<(RArrow<I>, TypeNoBounds<I>)>,
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/paths.html#railroad-QualifiedPathType
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct QuilifiedPathType<I>
where
    I: CSTInput,
{
    /// punct `<`
    pub delimiter_start: Lt<I>,
    /// type name
    pub ty: Type<I>,
    /// optional as stmt.
    pub as_type_path: Option<(As<I>, TypePath<I>)>,
    /// punct `>`
    pub delimiter_end: Lt<I>,
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/paths.html#railroad-QualifiedPathInType
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct QuilifiedPathInType<I>
where
    I: CSTInput,
{
    pub ty: QuilifiedPathType<I>,
    /// first segment.
    pub first: (PathSep<I>, TypePathSegment<I>),
    /// rest segements
    pub rest: Vec<(PathSep<I>, TypePathSegment<I>)>,
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/paths.html#railroad-QualifiedPathInExpression
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct QuilifiedPathInExpr<I>
where
    I: CSTInput,
{
    pub ty: QuilifiedPathType<I>,
    /// first segment.
    pub first: (PathSep<I>, PathExprSegment<I>),
    /// rest segements
    pub rest: Vec<(PathSep<I>, PathExprSegment<I>)>,
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Span, syntax::SyntaxInput};

    use crate::{
        errors::{CSTError, SyntaxKind},
        input::TokenStream,
        lexical::{
            ident::{Ident, NonKeywordIdent},
            keywords::strict::Crate,
            punct::PathSep,
        },
        names::paths::{SimplePath, SimplePathSegment},
    };

    #[test]
    fn test_simple_path() {
        let non_simple_paths = [(
            "",
            CSTError::Syntax(
                SyntaxKind::SimplePathSegment,
                ControlFlow::Recovable,
                Span::Range(0..0),
            ),
        )];

        for (input, err) in non_simple_paths {
            assert_eq!(TokenStream::from(input).parse::<SimplePath<_>>(), Err(err));
        }

        let simple_paths = [
            (
                "$crate",
                SimplePath::<TokenStream<'_>> {
                    leading_sep: None,
                    first: SimplePathSegment::VariableCrate(
                        TokenStream::from("$"),
                        Crate(TokenStream::from((1, "crate")), None),
                    ),
                    rest: vec![],
                },
            ),
            (
                "crate",
                SimplePath::<TokenStream<'_>> {
                    leading_sep: None,
                    first: SimplePathSegment::Crate(Crate(TokenStream::from("crate"), None)),
                    rest: vec![],
                },
            ),
            (
                "::a",
                SimplePath::<TokenStream<'_>> {
                    leading_sep: Some(PathSep(None, TokenStream::from("::"), None)),
                    first: SimplePathSegment::Ident(Ident::NonKeywordIdent(NonKeywordIdent(
                        TokenStream::from((2, "a")),
                    ))),
                    rest: vec![],
                },
            ),
            (
                "::a::",
                SimplePath::<TokenStream<'_>> {
                    leading_sep: Some(PathSep(None, TokenStream::from("::"), None)),
                    first: SimplePathSegment::Ident(Ident::NonKeywordIdent(NonKeywordIdent(
                        TokenStream::from((2, "a")),
                    ))),
                    rest: vec![],
                },
            ),
        ];

        for (input, expect) in simple_paths {
            assert_eq!(
                TokenStream::from(input).parse::<SimplePath<_>>(),
                Ok(expect)
            );
        }
    }
}
