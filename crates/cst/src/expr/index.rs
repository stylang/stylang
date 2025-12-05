use parserc::syntax::Delimiter;

use crate::{
    expr::{Expr, group::Composable},
    input::CSTInput,
    punct::{BracketEnd, BracketStart},
};

/// index sub-expr.
pub type Index<I> = Delimiter<BracketStart<I>, BracketEnd<I>, Box<Expr<I>>>;

/// A square bracketed indexing expression: `vector[2]`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprIndex<I>
where
    I: CSTInput,
{
    /// function body.
    pub expr: Box<Expr<I>>,
    /// index sub-expr.
    pub index: Index<I>,
}

impl<I> Composable<I> for ExprIndex<I>
where
    I: CSTInput,
{
    #[inline]
    fn priority(&self) -> usize {
        1
    }

    #[inline]
    fn compose<F>(self, _: usize, f: F) -> Expr<I>
    where
        F: FnOnce(Expr<I>) -> Expr<I>,
    {
        f(Expr::Index(self))
    }
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, InputSyntaxExt, Punctuated};

    use crate::{
        expr::{
            BinOp, Digits, Expr, ExprArray, ExprBinary, ExprIndex, ExprLit, ExprMethodCall,
            ExprPath, LitNumber,
        },
        input::TokenStream,
        misc::{Ident, S},
        path::{Path, PathSegment},
        punct::{BracketEnd, BracketStart, Caret, Comma, Dot, OrOr, ParenEnd, ParenStart},
    };

    #[test]
    fn test_index() {
        assert_eq!(
            TokenStream::from("[12,3][0].name()").parse::<Expr<_>>(),
            Ok(Expr::MethodCall(ExprMethodCall {
                receiver: Box::new(Expr::Index(ExprIndex {
                    expr: Box::new(Expr::Array(ExprArray(Delimiter {
                        start: BracketStart(None, TokenStream::from((0, "[")), None),
                        end: BracketEnd(None, TokenStream::from((5, "]")), None),
                        body: Punctuated {
                            pairs: vec![(
                                Expr::Lit(ExprLit::Number(LitNumber {
                                    sign: None,
                                    trunc: Some(Digits {
                                        input: TokenStream::from((1, "12")),
                                        value: 12,
                                    }),
                                    fract: None,
                                    exp: None,
                                })),
                                Comma(None, TokenStream::from((3, ",")), None),
                            )],
                            tail: Some(Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                                sign: None,
                                trunc: Some(Digits {
                                    input: TokenStream::from((4, "3")),
                                    value: 3,
                                }),
                                fract: None,
                                exp: None,
                            })))),
                        },
                    }))),
                    index: Delimiter {
                        start: BracketStart(None, TokenStream::from((6, "[")), None),
                        end: BracketEnd(None, TokenStream::from((8, "]")), None),
                        body: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                            sign: None,
                            trunc: Some(Digits {
                                input: TokenStream::from((7, "0")),
                                value: 0,
                            }),
                            fract: None,
                            exp: None,
                        }))),
                    },
                })),
                dot: Dot(None, TokenStream::from((9, ".")), None),
                ident: Ident(TokenStream::from((10, "name"))),
                turbofish: None,
                args: Delimiter {
                    start: ParenStart(None, TokenStream::from((14, "(")), None),
                    end: ParenEnd(None, TokenStream::from((15, ")")), None),
                    body: Punctuated {
                        pairs: vec![],
                        tail: None,
                    },
                },
            }),)
        );
    }

    #[test]
    fn test_index_priority() {
        assert_eq!(
            TokenStream::from("a[1] ^ b [2] || c [0]").parse::<Expr<_>>(),
            Ok(Expr::Binary(ExprBinary {
                left: Box::new(Expr::Binary(ExprBinary {
                    left: Box::new(Expr::Index(ExprIndex {
                        expr: Box::new(Expr::Path(ExprPath {
                            qself: None,
                            path: Path {
                                leading_pathsep: None,
                                segments: Punctuated {
                                    pairs: vec![],
                                    tail: Some(Box::new(PathSegment {
                                        ident: Ident(TokenStream::from((0, "a"))),
                                        arguments: None
                                    }))
                                }
                            }
                        })),
                        index: Delimiter {
                            start: BracketStart(None, TokenStream::from((1, "[")), None),
                            end: BracketEnd(
                                None,
                                TokenStream::from((3, "]")),
                                Some(S(TokenStream::from((4, " "))))
                            ),
                            body: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                                sign: None,
                                trunc: Some(Digits {
                                    input: TokenStream::from((2, "1")),
                                    value: 1
                                }),
                                fract: None,
                                exp: None
                            })))
                        }
                    })),
                    op: BinOp::BitXor(Caret(
                        None,
                        TokenStream::from((5, "^")),
                        Some(S(TokenStream::from((6, " "))))
                    )),
                    right: Box::new(Expr::Index(ExprIndex {
                        expr: Box::new(Expr::Path(ExprPath {
                            qself: None,
                            path: Path {
                                leading_pathsep: None,
                                segments: Punctuated {
                                    pairs: vec![],
                                    tail: Some(Box::new(PathSegment {
                                        ident: Ident(TokenStream::from((7, "b"))),
                                        arguments: None
                                    }))
                                }
                            }
                        })),
                        index: Delimiter {
                            start: BracketStart(
                                Some(S(TokenStream::from((8, " ")))),
                                TokenStream::from((9, "[")),
                                None
                            ),
                            end: BracketEnd(
                                None,
                                TokenStream::from((11, "]")),
                                Some(S(TokenStream::from((12, " "))))
                            ),
                            body: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                                sign: None,
                                trunc: Some(Digits {
                                    input: TokenStream::from((10, "2")),
                                    value: 2
                                }),
                                fract: None,
                                exp: None
                            })))
                        }
                    }))
                })),
                op: BinOp::Or(OrOr(
                    None,
                    TokenStream::from((13, "||")),
                    Some(S(TokenStream::from((15, " "))))
                )),
                right: Box::new(Expr::Index(ExprIndex {
                    expr: Box::new(Expr::Path(ExprPath {
                        qself: None,
                        path: Path {
                            leading_pathsep: None,
                            segments: Punctuated {
                                pairs: vec![],
                                tail: Some(Box::new(PathSegment {
                                    ident: Ident(TokenStream::from((16, "c"))),
                                    arguments: None
                                }))
                            }
                        }
                    })),
                    index: Delimiter {
                        start: BracketStart(
                            Some(S(TokenStream::from((17, " ")))),
                            TokenStream::from((18, "[")),
                            None
                        ),
                        end: BracketEnd(None, TokenStream::from((20, "]")), None),
                        body: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                            sign: None,
                            trunc: Some(Digits {
                                input: TokenStream::from((19, "0")),
                                value: 0
                            }),
                            fract: None,
                            exp: None
                        })))
                    }
                }))
            }))
        );
    }
}
