use parserc::syntax::{Delimiter, Punctuated, Syntax};

use crate::{
    expr::{Expr, group::Composable},
    input::CSTInput,
    punct::{Comma, ParenEnd, ParenStart},
};

/// A tuple expression: `(a, b, c, d)`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprTuple<I>(pub Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<Expr<I>, Comma<I>>>)
where
    I: CSTInput;

impl<I> Composable<I> for ExprTuple<I>
where
    I: CSTInput,
{
    fn priority(&self) -> usize {
        1
    }

    fn compose<F>(self, _: usize, f: F) -> Expr<I>
    where
        F: FnOnce(Expr<I>) -> Expr<I>,
    {
        f(Expr::Tuple(self))
    }
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, InputSyntaxExt, Punctuated};

    use crate::{
        expr::{BinOp, Expr, ExprBinary, ExprCall, ExprPath, ExprTuple},
        input::TokenStream,
        misc::{Ident, S},
        path::{Path, PathSegment},
        punct::{AndAnd, Caret, Comma, OrOr, ParenEnd, ParenStart, Plus},
    };

    #[test]
    fn test_paren() {
        assert_eq!(
            TokenStream::from("(a || b) && (c() + d) ").parse::<Expr<_>>(),
            Ok(Expr::Binary(ExprBinary {
                left: Box::new(Expr::Tuple(ExprTuple(Delimiter {
                    start: ParenStart(None, TokenStream::from((0, "(")), None),
                    end: ParenEnd(
                        None,
                        TokenStream::from((7, ")")),
                        Some(S(TokenStream::from((8, " "))))
                    ),
                    body: Punctuated {
                        pairs: vec![],
                        tail: Some(Box::new(Expr::Binary(ExprBinary {
                            left: Box::new(Expr::Path(ExprPath {
                                qself: None,
                                path: Path {
                                    leading_pathsep: None,
                                    segments: Punctuated {
                                        pairs: vec![],
                                        tail: Some(Box::new(PathSegment {
                                            ident: Ident(TokenStream::from((1, "a"))),
                                            arguments: None
                                        }))
                                    }
                                }
                            })),
                            op: BinOp::Or(OrOr(
                                Some(S(TokenStream::from((2, " ")))),
                                TokenStream::from((3, "||")),
                                Some(S(TokenStream::from((5, " "))))
                            )),
                            right: Box::new(Expr::Path(ExprPath {
                                qself: None,
                                path: Path {
                                    leading_pathsep: None,
                                    segments: Punctuated {
                                        pairs: vec![],
                                        tail: Some(Box::new(PathSegment {
                                            ident: Ident(TokenStream::from((6, "b"))),
                                            arguments: None
                                        }))
                                    }
                                }
                            }))
                        })))
                    }
                }))),
                op: BinOp::And(AndAnd(
                    None,
                    TokenStream::from((9, "&&")),
                    Some(S(TokenStream::from((11, " "))))
                )),
                right: Box::new(Expr::Tuple(ExprTuple(Delimiter {
                    start: ParenStart(None, TokenStream::from((12, "(")), None),
                    end: ParenEnd(
                        None,
                        TokenStream::from((20, ")")),
                        Some(S(TokenStream::from((21, " "))))
                    ),
                    body: Punctuated {
                        pairs: vec![],
                        tail: Some(Box::new(Expr::Binary(ExprBinary {
                            left: Box::new(Expr::Call(ExprCall {
                                func: Box::new(Expr::Path(ExprPath {
                                    qself: None,
                                    path: Path {
                                        leading_pathsep: None,
                                        segments: Punctuated {
                                            pairs: vec![],
                                            tail: Some(Box::new(PathSegment {
                                                ident: Ident(TokenStream::from((13, "c"))),
                                                arguments: None
                                            }))
                                        }
                                    }
                                })),
                                args: Delimiter {
                                    start: ParenStart(None, TokenStream::from((14, "(")), None),
                                    end: ParenEnd(
                                        None,
                                        TokenStream::from((15, ")")),
                                        Some(S(TokenStream::from((16, " "))))
                                    ),
                                    body: Punctuated {
                                        pairs: vec![],
                                        tail: None
                                    }
                                }
                            })),
                            op: BinOp::Add(Plus(
                                None,
                                TokenStream::from((17, "+")),
                                Some(S(TokenStream::from((18, " "))))
                            )),
                            right: Box::new(Expr::Path(ExprPath {
                                qself: None,
                                path: Path {
                                    leading_pathsep: None,
                                    segments: Punctuated {
                                        pairs: vec![],
                                        tail: Some(Box::new(PathSegment {
                                            ident: Ident(TokenStream::from((19, "d"))),
                                            arguments: None
                                        }))
                                    }
                                }
                            }))
                        })))
                    }
                })))
            }))
        );
    }

    #[test]
    fn test_tuple() {
        assert_eq!(
            TokenStream::from("(a,b,c(),d^ f)").parse::<Expr<_>>(),
            Ok(Expr::Tuple(ExprTuple(Delimiter {
                start: ParenStart(None, TokenStream::from((0, "(")), None),
                end: ParenEnd(None, TokenStream::from((13, ")")), None),
                body: Punctuated {
                    pairs: vec![
                        (
                            Expr::Path(ExprPath {
                                qself: None,
                                path: Path {
                                    leading_pathsep: None,
                                    segments: Punctuated {
                                        pairs: vec![],
                                        tail: Some(Box::new(PathSegment {
                                            ident: Ident(TokenStream::from((1, "a"))),
                                            arguments: None
                                        }))
                                    }
                                }
                            }),
                            Comma(None, TokenStream::from((2, ",")), None)
                        ),
                        (
                            Expr::Path(ExprPath {
                                qself: None,
                                path: Path {
                                    leading_pathsep: None,
                                    segments: Punctuated {
                                        pairs: vec![],
                                        tail: Some(Box::new(PathSegment {
                                            ident: Ident(TokenStream::from((3, "b"))),
                                            arguments: None
                                        }))
                                    }
                                }
                            }),
                            Comma(None, TokenStream::from((4, ",")), None)
                        ),
                        (
                            Expr::Call(ExprCall {
                                func: Box::new(Expr::Path(ExprPath {
                                    qself: None,
                                    path: Path {
                                        leading_pathsep: None,
                                        segments: Punctuated {
                                            pairs: vec![],
                                            tail: Some(Box::new(PathSegment {
                                                ident: Ident(TokenStream::from((5, "c"))),
                                                arguments: None
                                            }))
                                        }
                                    }
                                })),
                                args: Delimiter {
                                    start: ParenStart(None, TokenStream::from((6, "(")), None),
                                    end: ParenEnd(None, TokenStream::from((7, ")")), None),
                                    body: Punctuated {
                                        pairs: vec![],
                                        tail: None
                                    }
                                }
                            }),
                            Comma(None, TokenStream::from((8, ",")), None)
                        )
                    ],
                    tail: Some(Box::new(Expr::Binary(ExprBinary {
                        left: Box::new(Expr::Path(ExprPath {
                            qself: None,
                            path: Path {
                                leading_pathsep: None,
                                segments: Punctuated {
                                    pairs: vec![],
                                    tail: Some(Box::new(PathSegment {
                                        ident: Ident(TokenStream::from((9, "d"))),
                                        arguments: None
                                    }))
                                }
                            }
                        })),
                        op: BinOp::BitXor(Caret(
                            None,
                            TokenStream::from((10, "^")),
                            Some(S(TokenStream::from((11, " "))))
                        )),
                        right: Box::new(Expr::Path(ExprPath {
                            qself: None,
                            path: Path {
                                leading_pathsep: None,
                                segments: Punctuated {
                                    pairs: vec![],
                                    tail: Some(Box::new(PathSegment {
                                        ident: Ident(TokenStream::from((12, "f"))),
                                        arguments: None
                                    }))
                                }
                            }
                        }))
                    })))
                }
            })))
        );
    }
}
