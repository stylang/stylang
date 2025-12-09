use parserc::{
    ParseError, Parser, Span,
    syntax::{Punctuated, Syntax, SyntaxInput},
};

use crate::{
    errors::{CSTError, SyntaxKind},
    expr::Expr,
    input::CSTInput,
    punct::{BracketEnd, BracketStart, Comma, Semi},
};

/// A square bracketed indexing expression: `vector[2]`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Index<I>
where
    I: CSTInput,
{
    pub delimiter_start: BracketStart<I>,
    pub expr: Box<Expr<I>>,
    pub delimiter_end: BracketEnd<I>,
}

impl<I> Index<I>
where
    I: CSTInput,
{
    #[inline]
    pub fn to_span(&self) -> Span {
        self.delimiter_start.to_span() + self.delimiter_end.to_span()
    }
}

/// A slice literal expression
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprArray<I>
where
    I: CSTInput,
{
    /// leading delimiter punct `[`
    pub delimiter_start: BracketStart<I>,
    /// elment exprs.
    pub elems: Punctuated<Expr<I>, Comma<I>>,
    /// tailing delimiter punct `]`
    pub delimiter_end: BracketEnd<I>,
}

/// An array literal constructed from one repeated element: [0u8; N].
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprRepeat<I>
where
    I: CSTInput,
{
    /// leading delimiter punct `[`
    pub delimiter_start: BracketStart<I>,
    pub expr: Box<Expr<I>>,
    pub semi: Semi<I>,
    pub len: Box<Expr<I>>,
    /// tailing delimiter punct `]`
    pub delimiter_end: BracketEnd<I>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub(crate) enum ExprBracket<I>
where
    I: CSTInput,
{
    Index(Index<I>),
    Repeat(ExprRepeat<I>),
    Array(ExprArray<I>),
}

#[inline]
pub(super) fn parse_bracket<I>(input: &mut I) -> Result<ExprBracket<I>, CSTError>
where
    I: CSTInput,
{
    let delimiter_start: BracketStart<I> = input.parse()?;
    let elems: Punctuated<Expr<I>, Comma<I>> = input.parse()?;

    if (elems.len() == 1 && elems.tail.is_none()) || elems.len() > 1 {
        let delimiter_end: BracketEnd<I> = input.parse().map_err(ParseError::into_fatal)?;

        return Ok(ExprBracket::Array(ExprArray {
            delimiter_start,
            elems,
            delimiter_end,
        }));
    }

    if let Some(semi) = Semi::into_parser().ok().parse(input)? {
        let len = Expr::into_parser()
            .boxed()
            .parse(input)
            .map_err(SyntaxKind::RepeatLen.map_non_fatal())?;

        let delimiter_end: BracketEnd<I> = input.parse().map_err(ParseError::into_fatal)?;

        return Ok(ExprBracket::Repeat(ExprRepeat {
            delimiter_start,
            expr: elems.tail.unwrap(),
            semi,
            len,
            delimiter_end,
        }));
    }

    let delimiter_end: BracketEnd<I> = input.parse()?;

    Ok(ExprBracket::Index(Index {
        delimiter_start,
        expr: elems.tail.unwrap(),
        delimiter_end,
    }))
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, SyntaxInput};

    use crate::{
        expr::{Call, Digits, Expr, ExprLit, ExprPath, LitNumber},
        input::TokenStream,
        misc::Ident,
        path::{Path, PathSegment},
        punct::{ParenEnd, ParenStart},
    };

    use super::*;

    #[test]
    fn test_array() {
        assert_eq!(
            TokenStream::from("[1,3,4,f()]").parse::<Expr<_>>(),
            Ok(Expr::Array(ExprArray {
                delimiter_start: BracketStart(None, TokenStream::from((0, "[")), None),
                elems: Punctuated {
                    pairs: vec![
                        (
                            Expr::Lit(ExprLit::Number(LitNumber {
                                sign: None,
                                trunc: Some(Digits {
                                    input: TokenStream::from((1, "1")),
                                    value: 1
                                }),
                                fract: None,
                                exp: None
                            })),
                            Comma(None, TokenStream::from((2, ",")), None)
                        ),
                        (
                            Expr::Lit(ExprLit::Number(LitNumber {
                                sign: None,
                                trunc: Some(Digits {
                                    input: TokenStream::from((3, "3")),
                                    value: 3
                                }),
                                fract: None,
                                exp: None
                            })),
                            Comma(None, TokenStream::from((4, ",")), None)
                        ),
                        (
                            Expr::Lit(ExprLit::Number(LitNumber {
                                sign: None,
                                trunc: Some(Digits {
                                    input: TokenStream::from((5, "4")),
                                    value: 4
                                }),
                                fract: None,
                                exp: None
                            })),
                            Comma(None, TokenStream::from((6, ",")), None)
                        )
                    ],
                    tail: Some(Box::new(Expr::Call(
                        Box::new(Expr::Path(ExprPath {
                            qself: None,
                            path: Path {
                                leading_pathsep: None,
                                segments: Punctuated {
                                    pairs: vec![],
                                    tail: Some(Box::new(PathSegment {
                                        ident: Ident(TokenStream::from((7, "f"))),
                                        arguments: None
                                    }))
                                }
                            }
                        })),
                        vec![],
                        Call(Delimiter {
                            start: ParenStart(None, TokenStream::from((8, "(")), None),
                            end: ParenEnd(None, TokenStream::from((9, ")")), None),
                            body: Punctuated {
                                pairs: vec![],
                                tail: None
                            }
                        })
                    )))
                },
                delimiter_end: BracketEnd(None, TokenStream::from((10, "]")), None)
            }))
        );

        assert_eq!(
            TokenStream::from("[1,]").parse::<Expr<_>>(),
            Ok(Expr::Array(ExprArray {
                delimiter_start: BracketStart(None, TokenStream::from((0, "[")), None),
                elems: Punctuated {
                    pairs: vec![(
                        Expr::Lit(ExprLit::Number(LitNumber {
                            sign: None,
                            trunc: Some(Digits {
                                input: TokenStream::from((1, "1")),
                                value: 1
                            }),
                            fract: None,
                            exp: None
                        })),
                        Comma(None, TokenStream::from((2, ",")), None)
                    )],
                    tail: None
                },
                delimiter_end: BracketEnd(None, TokenStream::from((3, "]")), None)
            }))
        );
    }

    #[test]
    fn test_repeat() {
        assert_eq!(
            TokenStream::from("[f();g()][1]").parse::<Expr<_>>(),
            Ok(Expr::Index(
                Box::new(Expr::Repeat(ExprRepeat {
                    delimiter_start: BracketStart(None, TokenStream::from((0, "[")), None),
                    expr: Box::new(Expr::Call(
                        Box::new(Expr::Path(ExprPath {
                            qself: None,
                            path: Path {
                                leading_pathsep: None,
                                segments: Punctuated {
                                    pairs: vec![],
                                    tail: Some(Box::new(PathSegment {
                                        ident: Ident(TokenStream::from((1, "f"))),
                                        arguments: None
                                    }))
                                }
                            }
                        })),
                        vec![],
                        Call(Delimiter {
                            start: ParenStart(None, TokenStream::from((2, "(")), None),
                            end: ParenEnd(None, TokenStream::from((3, ")")), None),
                            body: Punctuated {
                                pairs: vec![],
                                tail: None
                            }
                        })
                    )),
                    semi: Semi(None, TokenStream::from((4, ";")), None),
                    len: Box::new(Expr::Call(
                        Box::new(Expr::Path(ExprPath {
                            qself: None,
                            path: Path {
                                leading_pathsep: None,
                                segments: Punctuated {
                                    pairs: vec![],
                                    tail: Some(Box::new(PathSegment {
                                        ident: Ident(TokenStream::from((5, "g"))),
                                        arguments: None
                                    }))
                                }
                            }
                        })),
                        vec![],
                        Call(Delimiter {
                            start: ParenStart(None, TokenStream::from((6, "(")), None),
                            end: ParenEnd(None, TokenStream::from((7, ")")), None),
                            body: Punctuated {
                                pairs: vec![],
                                tail: None
                            }
                        })
                    )),
                    delimiter_end: BracketEnd(None, TokenStream::from((8, "]")), None)
                })),
                vec![],
                Index {
                    delimiter_start: BracketStart(None, TokenStream::from((9, "[")), None),
                    expr: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                        sign: None,
                        trunc: Some(Digits {
                            input: TokenStream::from((10, "1")),
                            value: 1
                        }),
                        fract: None,
                        exp: None
                    }))),
                    delimiter_end: BracketEnd(None, TokenStream::from((11, "]")), None)
                }
            ))
        );
    }
}
