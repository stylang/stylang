//! The possible types that a `stylang` value could have.

use parserc::{
    ControlFlow,
    syntax::{Delimiter, Punctuated, Syntax},
};

use crate::{
    errors::{CSTError, SyntaxKind},
    expr::Digits,
    generics::GenericArgument,
    input::CSTInput,
    misc::Ident,
    punct::{
        AngleBracketEnd, AngleBracketStart, ArrowRight, BracketEnd, BracketStart, Comma, Or,
        ParenEnd, ParenStart, PathSep, Semi,
    },
};

/// Angle bracketed arguments of a path segment.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PathArguments<I>
where
    I: CSTInput,
{
    /// optional leading `::`
    pub leading_pathsep: Option<PathSep<I>>,
    /// angle bracketd arguments.
    pub args: Delimiter<
        AngleBracketStart<I>,
        AngleBracketEnd<I>,
        Punctuated<GenericArgument<I>, Comma<I>>,
    >,
}

/// A segment of a path together with any path arguments on that segment.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PathSegment<I>
where
    I: CSTInput,
{
    /// segment name.
    pub ident: Ident<I>,
    /// path arguments.
    pub arguments: Option<PathArguments<I>>,
}

/// A path at which a named item is exported (e.g. std::collections::HashMap).
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(semantic = check_path)]
pub struct Path<I>
where
    I: CSTInput,
{
    /// optional leading `::`
    pub leading_pathsep: Option<PathSep<I>>,
    /// segments of path.
    pub segments: Punctuated<PathSegment<I>, PathSep<I>>,
}

fn check_path<I>(path: Path<I>) -> Result<Path<I>, CSTError>
where
    I: CSTInput,
{
    if path.leading_pathsep.is_none() && path.segments.is_empty() {
        Err(CSTError::Syntax(
            SyntaxKind::Path,
            ControlFlow::Recovable,
            path.to_span(),
        ))
    } else {
        Ok(path)
    }
}

/// A bare function type: `fn(usize) -> bool`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::BareFn.map_unhandle())]
pub struct TypeBareFn<I>
where
    I: CSTInput,
{
    /// keyword `fn`
    #[parserc(crucial, keyword = "fn")]
    pub keyword: I,
    /// input base fn arguments.
    pub inputs: Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<Type<I>, Comma<I>>>,
    /// Return type of a function signature.
    pub output: Option<(ArrowRight<I>, Box<Type<I>>)>,
}

/// Type of `stylang` value.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Type<I>
where
    I: CSTInput,
{
    I8(#[parserc(keyword = "i8")] I),
    I16(#[parserc(keyword = "i16")] I),
    I32(#[parserc(keyword = "i32")] I),
    I64(#[parserc(keyword = "i64")] I),
    I128(#[parserc(keyword = "i128")] I),
    I256(#[parserc(keyword = "i256")] I),
    U8(#[parserc(keyword = "u8")] I),
    U16(#[parserc(keyword = "u16")] I),
    U32(#[parserc(keyword = "u32")] I),
    U64(#[parserc(keyword = "u64")] I),
    U128(#[parserc(keyword = "u128")] I),
    U256(#[parserc(keyword = "u256")] I),
    F16(#[parserc(keyword = "f16")] I),
    F32(#[parserc(keyword = "f32")] I),
    F64(#[parserc(keyword = "f64")] I),
    String(#[parserc(keyword = "string")] I),
    Time(#[parserc(keyword = "time")] I),
    Length(#[parserc(keyword = "length")] I),
    Color(#[parserc(keyword = "color")] I),
    Angle(#[parserc(keyword = "angle")] I),
    Freq(#[parserc(keyword = "freq")] I),
    /// draw
    Draw(#[parserc(keyword = "draw")] I),
    /// view
    View(#[parserc(keyword = "view")] I),
    /// ...type
    Variadic(#[parserc(crucial, keyword = "...")] I, Box<Type<I>>),
    /// Sequence, `[T] or [T;N]`
    Sequence(
        Delimiter<BracketStart<I>, BracketEnd<I>, (Box<Type<I>>, Option<(Semi<I>, Digits<I>)>)>,
    ),
    /// tuple: (T1,T2,...)
    Tuple(Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<Type<I>, Comma<I>>>),
    /// a mutable reference.
    Mut(#[parserc(crucial, keyword = "&mut")] I, Box<Type<I>>),
    /// a immutable reference.
    Ref(#[parserc(crucial, keyword = "&")] I, Box<Type<I>>),
    /// map
    Map(
        #[parserc(crucial, keyword = "map")] I,
        Delimiter<BracketStart<I>, BracketEnd<I>, (Box<Type<I>>, Comma<I>, Box<Type<I>>)>,
    ),
    /// set
    Set(
        #[parserc(crucial, keyword = "set")] I,
        Delimiter<BracketStart<I>, BracketEnd<I>, Box<Type<I>>>,
    ),
    /// bare function `fn(string) -> i32`
    BareFn(TypeBareFn<I>),
    /// A path like std::slice::Iter
    Path(Path<I>),
}

/// A type with extension `union` expr.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprType<I>
where
    I: CSTInput,
{
    pub first: Type<I>,
    pub rest: Vec<(Or<I>, Type<I>)>,
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, InputSyntaxExt};

    use crate::{input::TokenStream, misc::S};

    use super::*;

    #[test]
    fn test_ty() {
        assert_eq!(
            TokenStream::from("&...&mut[[i32;4]]").parse::<Type<_>>(),
            Ok(Type::Ref(
                TokenStream::from((0, "&")),
                Box::new(Type::Variadic(
                    TokenStream::from((1, "...")),
                    Box::new(Type::Mut(
                        TokenStream::from((4, "&mut")),
                        Box::new(Type::Sequence(Delimiter {
                            start: BracketStart(None, TokenStream::from((8, "[")), None),
                            end: BracketEnd(None, TokenStream::from((16, "]")), None),
                            body: (
                                Box::new(Type::Sequence(Delimiter {
                                    start: BracketStart(None, TokenStream::from((9, "[")), None),
                                    end: BracketEnd(None, TokenStream::from((15, "]")), None),
                                    body: (
                                        Box::new(Type::I32(TokenStream::from((10, "i32")),)),
                                        Some((
                                            Semi(None, TokenStream::from((13, ";")), None),
                                            Digits {
                                                input: TokenStream::from((14, "4")),
                                                value: 4
                                            }
                                        ))
                                    )
                                })),
                                None
                            )
                        }))
                    ))
                ))
            ))
        );

        assert_eq!(
            TokenStream::from("[fn() -> view]").parse::<Type<_>>(),
            Ok(Type::Sequence(Delimiter {
                start: BracketStart(None, TokenStream::from((0, "[")), None),
                end: BracketEnd(None, TokenStream::from((13, "]")), None),
                body: (
                    Box::new(Type::BareFn(TypeBareFn {
                        keyword: TokenStream::from((1, "fn")),
                        inputs: Delimiter {
                            start: ParenStart(None, TokenStream::from((3, "(")), None),
                            end: ParenEnd(
                                None,
                                TokenStream::from((4, ")")),
                                Some(S(TokenStream::from((5, " "))))
                            ),
                            body: Punctuated {
                                pairs: vec![],
                                tail: None
                            }
                        },
                        output: Some((
                            ArrowRight(
                                None,
                                TokenStream::from((6, "->")),
                                Some(S(TokenStream::from((8, " "))))
                            ),
                            Box::new(Type::View(TokenStream::from((9, "view"))))
                        ))
                    })),
                    None
                )
            }))
        );

        assert_eq!(
            TokenStream::from("map[string,draw]").parse(),
            Ok(Type::Map(
                TokenStream::from("map"),
                Delimiter {
                    start: BracketStart(None, TokenStream::from((3, "[")), None),
                    end: BracketEnd(None, TokenStream::from((15, "]")), None),
                    body: (
                        Box::new(Type::String(TokenStream::from((4, "string")))),
                        Comma(None, TokenStream::from((10, ",")), None),
                        Box::new(Type::Draw(TokenStream::from((11, "draw"))))
                    )
                }
            ))
        );

        assert_eq!(
            TokenStream::from("set[string]").parse(),
            Ok(Type::Set(
                TokenStream::from("set"),
                Delimiter {
                    start: BracketStart(None, TokenStream::from((3, "[")), None),
                    end: BracketEnd(None, TokenStream::from((10, "]")), None),
                    body: Box::new(Type::String(TokenStream::from((4, "string")))),
                }
            ))
        );

        assert_eq!(
            TokenStream::from("draw").parse(),
            Ok(Type::Draw(TokenStream::from("draw")))
        );

        assert_eq!(
            TokenStream::from("color").parse(),
            Ok(Type::Color(TokenStream::from("color")))
        );

        assert_eq!(
            TokenStream::from("angle").parse(),
            Ok(Type::Angle(TokenStream::from("angle")))
        );

        assert_eq!(
            TokenStream::from("length").parse(),
            Ok(Type::Length(TokenStream::from("length")))
        );

        assert_eq!(
            TokenStream::from("freq").parse(),
            Ok(Type::Freq(TokenStream::from("freq")))
        );
    }

    #[test]
    fn test_ty_union() {
        assert_eq!(
            TokenStream::from("i32 | u32 | length").parse::<ExprType<_>>(),
            Ok(ExprType {
                first: Type::I32(TokenStream::from((0, "i32"))),
                rest: vec![
                    (
                        Or(
                            Some(S(TokenStream::from((3, " ")))),
                            TokenStream::from((4, "|")),
                            Some(S(TokenStream::from((5, " "))))
                        ),
                        Type::U32(TokenStream::from((6, "u32")))
                    ),
                    (
                        Or(
                            Some(S(TokenStream::from((9, " ")))),
                            TokenStream::from((10, "|")),
                            Some(S(TokenStream::from((11, " "))))
                        ),
                        Type::Length(TokenStream::from((12, "length")))
                    )
                ]
            })
        );
    }
}
