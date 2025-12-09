use parserc::syntax::Syntax;

use crate::{
    errors::SyntaxKind,
    expr::{Expr, ExprPath},
    input::CSTInput,
    misc::Ident,
    punct::{BraceEnd, BraceStart, Colon, Comma, DotDot},
};

/// A rest expr: `..Default::default()`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Rest<I>
where
    I: CSTInput,
{
    /// leading keyword `..`
    pub keyword: DotDot<I>,
    /// rest body expr.
    #[parserc(map_err = SyntaxKind::RestBody.map_non_fatal())]
    pub expr: Box<Expr<I>>,
}

/// struct field init expr.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct FiledInit<I>
where
    I: CSTInput,
{
    /// field name.
    pub ident: Ident<I>,
    /// split punct `:`
    pub colon: Colon<I>,
    /// init expr.
    pub expr: Box<Expr<I>>,
}

/// A struct literal expression: Point { x: 1, y: 1 }.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprStruct<I>
where
    I: CSTInput,
{
    /// struct type path.
    pub path: ExprPath<I>,
    /// delimiter start token `{`
    pub delimiter_start: BraceStart<I>,
    /// first line field init expr.
    #[parserc(crucial)]
    pub first: FiledInit<I>,
    /// rest lines of field init exprs.
    pub pairs: Vec<(Comma<I>, FiledInit<I>)>,
    /// tail comma punct `,`
    pub comma: Option<Comma<I>>,
    /// rest expr.
    pub rest: Option<Rest<I>>,
    /// delimiter end token `}`
    pub delimiter_end: BraceEnd<I>,
}

#[cfg(test)]
mod tests {

    use parserc::{
        ControlFlow, Span,
        syntax::{Delimiter, Punctuated, SyntaxInput},
    };

    use crate::{
        errors::{CSTError, PunctKind},
        expr::{
            Call, Digits, Expr, ExprLit, ExprPath, ExprRange, ExprStruct, FiledInit, LitNumber,
            RangeLimits, Rest,
        },
        input::TokenStream,
        misc::{Ident, S},
        path::{Path, PathSegment},
        punct::{BraceEnd, BraceStart, Colon, Comma, DotDot, ParenEnd, ParenStart, PathSep},
    };

    #[test]
    fn test_struct_expr() {
        assert_eq!(
            TokenStream::from("A {a: 1, b: 2, ..a1}").parse::<Expr<_>>(),
            Ok(Expr::Struct(ExprStruct {
                path: ExprPath {
                    qself: None,
                    path: Path {
                        leading_pathsep: None,
                        segments: Punctuated {
                            pairs: vec![],
                            tail: Some(Box::new(PathSegment {
                                ident: Ident(TokenStream::from((0, "A"))),
                                arguments: None
                            }))
                        }
                    }
                },
                delimiter_start: BraceStart(
                    Some(S(TokenStream::from((1, " ")))),
                    TokenStream::from((2, "{")),
                    None
                ),
                first: FiledInit {
                    ident: Ident(TokenStream::from((3, "a"))),
                    colon: Colon(
                        None,
                        TokenStream::from((4, ":")),
                        Some(S(TokenStream::from((5, " "))))
                    ),
                    expr: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                        sign: None,
                        trunc: Some(Digits {
                            input: TokenStream::from((6, "1")),
                            value: 1
                        }),
                        fract: None,
                        exp: None
                    })))
                },
                pairs: vec![(
                    Comma(
                        None,
                        TokenStream::from((7, ",")),
                        Some(S(TokenStream::from((8, " "))))
                    ),
                    FiledInit {
                        ident: Ident(TokenStream::from((9, "b"))),
                        colon: Colon(
                            None,
                            TokenStream::from((10, ":")),
                            Some(S(TokenStream::from((11, " "))))
                        ),
                        expr: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                            sign: None,
                            trunc: Some(Digits {
                                input: TokenStream::from((12, "2")),
                                value: 2
                            }),
                            fract: None,
                            exp: None
                        })))
                    }
                )],
                comma: Some(Comma(
                    None,
                    TokenStream::from((13, ",")),
                    Some(S(TokenStream::from((14, " "))))
                )),
                rest: Some(Rest {
                    keyword: DotDot(None, TokenStream::from((15, "..")), None),
                    expr: Box::new(Expr::Path(ExprPath {
                        qself: None,
                        path: Path {
                            leading_pathsep: None,
                            segments: Punctuated {
                                pairs: vec![],
                                tail: Some(Box::new(PathSegment {
                                    ident: Ident(TokenStream::from((17, "a1"))),
                                    arguments: None
                                }))
                            }
                        }
                    }))
                }),
                delimiter_end: BraceEnd(None, TokenStream::from((19, "}")), None)
            }))
        );
    }

    #[test]
    fn expr_struct_error_detect() {
        assert_eq!(
            TokenStream::from("A { a:1, ").parse::<Expr<_>>(),
            Err(CSTError::Punct(
                PunctKind::BraceEnd,
                ControlFlow::Fatal,
                Span::Range(9..9)
            ))
        );

        assert_eq!(
            TokenStream::from("{ A {a: 1,..use}").parse::<Expr<_>>(),
            Err(CSTError::Punct(
                PunctKind::BraceEnd,
                ControlFlow::Fatal,
                Span::Range(16..16)
            ))
        );
    }

    #[test]
    fn expr_struct_misparse() {
        assert_eq!(
            TokenStream::from("A { a:1..Default::default() }").parse::<Expr<_>>(),
            Ok(Expr::Struct(ExprStruct {
                path: ExprPath {
                    qself: None,
                    path: Path {
                        leading_pathsep: None,
                        segments: Punctuated {
                            pairs: vec![],
                            tail: Some(Box::new(PathSegment {
                                ident: Ident(TokenStream::from((0, "A"))),
                                arguments: None
                            }))
                        }
                    }
                },
                delimiter_start: BraceStart(
                    Some(S(TokenStream::from((1, " ")))),
                    TokenStream::from((2, "{")),
                    Some(S(TokenStream::from((3, " "))))
                ),
                first: FiledInit {
                    ident: Ident(TokenStream::from((4, "a"))),
                    colon: Colon(None, TokenStream::from((5, ":")), None),
                    expr: Box::new(Expr::Range(ExprRange {
                        start: Some(Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                            sign: None,
                            trunc: Some(Digits {
                                input: TokenStream::from((6, "1")),
                                value: 1
                            }),
                            fract: None,
                            exp: None
                        })))),
                        limits: RangeLimits::HalfOpen(DotDot(
                            None,
                            TokenStream::from((7, "..")),
                            None
                        )),
                        end: Some(Box::new(Expr::Call(
                            Box::new(Expr::Path(ExprPath {
                                qself: None,
                                path: Path {
                                    leading_pathsep: None,
                                    segments: Punctuated {
                                        pairs: vec![(
                                            PathSegment {
                                                ident: Ident(TokenStream::from((9, "Default"))),
                                                arguments: None
                                            },
                                            PathSep(None, TokenStream::from((16, "::")), None)
                                        )],
                                        tail: Some(Box::new(PathSegment {
                                            ident: Ident(TokenStream::from((18, "default"))),
                                            arguments: None
                                        }))
                                    }
                                }
                            })),
                            vec![],
                            Call(Delimiter {
                                start: ParenStart(None, TokenStream::from((25, "(")), None),
                                end: ParenEnd(
                                    None,
                                    TokenStream::from((26, ")")),
                                    Some(S(TokenStream::from((27, " "))))
                                ),
                                body: Punctuated {
                                    pairs: vec![],
                                    tail: None
                                }
                            })
                        )))
                    }))
                },
                pairs: vec![],
                comma: None,
                rest: None,
                delimiter_end: BraceEnd(None, TokenStream::from((28, "}")), None)
            }))
        );
    }
}
