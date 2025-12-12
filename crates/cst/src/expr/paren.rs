use parserc::syntax::{Delimiter, Punctuated, Syntax};

use crate::{
    expr::Expr,
    input::CSTInput,
    punct::{Comma, ParenEnd, ParenStart},
};

/// A tuple expression: `(a, b, c, d)`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprTuple<I>(pub Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<Expr<I>, Comma<I>>>)
where
    I: CSTInput;

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, Punctuated, SyntaxInput};

    use crate::{
        expr::{
            Call, Digits, Expr, ExprLet, ExprLit, ExprPath, ExprTuple, Field, LitNumber, Member,
        },
        generics::GenericArgument,
        input::TokenStream,
        keyword::Let,
        misc::{Ident, S},
        pat::{Pat, PatIdent},
        path::{Path, PathArguments, PathSegment},
        punct::{
            AngleBracketEnd, AngleBracketStart, Comma, Dot, Equal, ParenEnd, ParenStart, PathSep,
        },
    };

    #[test]
    fn test_tuple_struct() {
        assert_eq!(
            TokenStream::from("std::sync::Mutex::<i32>(10)").parse::<Expr<_>>(),
            Ok(Expr::Call(
                Box::new(Expr::Path(ExprPath {
                    qself: None,
                    path: Path {
                        leading_pathsep: None,
                        segments: Punctuated {
                            pairs: vec![
                                (
                                    PathSegment {
                                        ident: Ident(TokenStream::from((0, "std"))),
                                        arguments: None
                                    },
                                    PathSep(None, TokenStream::from((3, "::")), None)
                                ),
                                (
                                    PathSegment {
                                        ident: Ident(TokenStream::from((5, "sync"))),
                                        arguments: None
                                    },
                                    PathSep(None, TokenStream::from((9, "::")), None)
                                )
                            ],
                            tail: Some(Box::new(PathSegment {
                                ident: Ident(TokenStream::from((11, "Mutex"))),
                                arguments: Some(PathArguments {
                                    leading_pathsep: Some(PathSep(
                                        None,
                                        TokenStream::from((16, "::")),
                                        None
                                    )),
                                    delimiter_start: AngleBracketStart(
                                        None,
                                        TokenStream::from((18, "<")),
                                        None
                                    ),
                                    args: Punctuated {
                                        pairs: vec![],
                                        tail: Some(Box::new(GenericArgument::Const(Expr::Path(
                                            ExprPath {
                                                qself: None,
                                                path: Path {
                                                    leading_pathsep: None,
                                                    segments: Punctuated {
                                                        pairs: vec![],
                                                        tail: Some(Box::new(PathSegment {
                                                            ident: Ident(TokenStream::from((
                                                                19, "i32"
                                                            ))),
                                                            arguments: None
                                                        }))
                                                    }
                                                }
                                            }
                                        ))))
                                    },
                                    delimiter_end: AngleBracketEnd(
                                        None,
                                        TokenStream::from((22, ">")),
                                        None
                                    )
                                })
                            }))
                        }
                    }
                })),
                vec![],
                Call(Delimiter {
                    start: ParenStart(None, TokenStream::from((23, "(")), None),
                    end: ParenEnd(None, TokenStream::from((26, ")")), None),
                    body: Punctuated {
                        pairs: vec![],
                        tail: Some(Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                            sign: None,
                            trunc: Some(Digits {
                                input: TokenStream::from((24, "10")),
                                value: 10
                            }),
                            fract: None,
                            exp: None
                        }))))
                    }
                })
            ))
        );
    }

    #[test]
    fn test_tuple() {
        assert_eq!(
            TokenStream::from("let a = (1,2,3)").parse::<Expr<_>>(),
            Ok(Expr::Let(ExprLet {
                keyword: Let(
                    TokenStream::from((0, "let")),
                    Some(S(TokenStream::from((3, " "))))
                ),
                pat: Box::new(Pat::Ident(PatIdent {
                    by_ref: None,
                    mutability: None,
                    ident: Ident(TokenStream::from((4, "a"))),
                    subpat: None
                })),
                eq: Equal(
                    Some(S(TokenStream::from((5, " ")))),
                    TokenStream::from((6, "=")),
                    Some(S(TokenStream::from((7, " "))))
                ),
                expr: Box::new(Expr::Tuple(ExprTuple(Delimiter {
                    start: ParenStart(None, TokenStream::from((8, "(")), None),
                    end: ParenEnd(None, TokenStream::from((14, ")")), None),
                    body: Punctuated {
                        pairs: vec![
                            (
                                Expr::Lit(ExprLit::Number(LitNumber {
                                    sign: None,
                                    trunc: Some(Digits {
                                        input: TokenStream::from((9, "1")),
                                        value: 1
                                    }),
                                    fract: None,
                                    exp: None
                                })),
                                Comma(None, TokenStream::from((10, ",")), None)
                            ),
                            (
                                Expr::Lit(ExprLit::Number(LitNumber {
                                    sign: None,
                                    trunc: Some(Digits {
                                        input: TokenStream::from((11, "2")),
                                        value: 2
                                    }),
                                    fract: None,
                                    exp: None
                                })),
                                Comma(None, TokenStream::from((12, ",")), None)
                            )
                        ],
                        tail: Some(Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                            sign: None,
                            trunc: Some(Digits {
                                input: TokenStream::from((13, "3")),
                                value: 3
                            }),
                            fract: None,
                            exp: None
                        }))))
                    }
                })))
            }))
        );
    }

    #[test]
    fn test_tuple_field() {
        assert_eq!(
            TokenStream::from("(1,2,3).1").parse::<Expr<_>>(),
            Ok(Expr::Field(
                Box::new(Expr::Tuple(ExprTuple(Delimiter {
                    start: ParenStart(None, TokenStream::from((0, "(")), None),
                    end: ParenEnd(None, TokenStream::from((6, ")")), None),
                    body: Punctuated {
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
                                        input: TokenStream::from((3, "2")),
                                        value: 2
                                    }),
                                    fract: None,
                                    exp: None
                                })),
                                Comma(None, TokenStream::from((4, ",")), None)
                            )
                        ],
                        tail: Some(Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                            sign: None,
                            trunc: Some(Digits {
                                input: TokenStream::from((5, "3")),
                                value: 3
                            }),
                            fract: None,
                            exp: None
                        }))))
                    }
                }))),
                vec![],
                Field {
                    dot: Dot(None, TokenStream::from((7, ".")), None),
                    member: Member::Unamed(Digits {
                        input: TokenStream::from((8, "1")),
                        value: 1
                    })
                }
            ))
        );
    }
}
