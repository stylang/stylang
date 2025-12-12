use parserc::{Parser, syntax::Syntax};

use crate::{
    block::Block,
    errors::{CSTError, SyntaxKind},
    expr::{Expr, ExprLet, parse_binary},
    input::CSTInput,
    keyword::{For, In, Loop, While},
    misc::{Label, S},
    pat::Pat,
    punct::Colon,
};

/// A for loop: `for pat in expr { ... }`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprForLoop<I>
where
    I: CSTInput,
{
    /// optional label
    pub label: Option<(Label<I>, Colon<I>)>,
    /// keyword `for`
    #[parserc(crucial)]
    pub for_token: For<I>,
    /// match pattern.
    pub pat: (Box<Pat<I>>, Option<S<I>>),
    /// keyword `in`
    pub in_token: In<I>,
    /// target iterator.
    pub expr: Box<Expr<I>>,
    /// for block.
    pub body: Block<I>,
}

/// Conditionless loop: loop { ... }.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprLoop<I>
where
    I: CSTInput,
{
    /// optional label
    pub label: Option<(Label<I>, Colon<I>)>,
    /// leading keyword `loop`
    #[parserc(crucial)]
    pub keyword: Loop<I>,
    /// for block.
    pub body: Block<I>,
}

/// A while loop: while expr { ... }.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprWhile<I>
where
    I: CSTInput,
{
    /// optional label
    pub label: Option<(Label<I>, Colon<I>)>,
    /// leading keyword `loop`
    #[parserc(crucial)]
    pub keyword: While<I>,
    /// target iterator.
    #[parserc(parser = parse_while_expr)]
    pub expr: Box<Expr<I>>,
    /// for block.
    pub body: Block<I>,
}

#[inline]
fn parse_while_expr<I>(input: &mut I) -> Result<Box<Expr<I>>, CSTError>
where
    I: CSTInput,
{
    ExprLet::into_parser()
        .map(Expr::Let)
        .or(parse_binary)
        .boxed()
        .parse(input)
        .map_err(SyntaxKind::WhileExpr.map_non_fatal())
}

#[cfg(test)]
mod tests {
    use parserc::{
        ControlFlow, Span,
        syntax::{Delimiter, Punctuated, SyntaxInput},
    };

    use crate::{
        block::Stmt,
        errors::PunctKind,
        expr::{BinOp, Digits, Expr, ExprBinary, ExprLit, ExprPath, LitNumber},
        input::TokenStream,
        keyword::Let,
        misc::Ident,
        pat::PatIdent,
        path::{Path, PathSegment},
        punct::{BraceEnd, BraceStart, Equal, Lt, PlusEq, Semi},
    };

    use super::*;

    #[test]
    fn test_loop() {
        assert_eq!(
            TokenStream::from("loop { a += 1; }").parse::<Expr<_>>(),
            Ok(Expr::Loop(ExprLoop {
                label: None,
                keyword: Loop(
                    TokenStream::from((0, "loop")),
                    Some(S(TokenStream::from((4, " "))))
                ),
                body: Block(Delimiter {
                    start: BraceStart(
                        None,
                        TokenStream::from((5, "{")),
                        Some(S(TokenStream::from((6, " "))))
                    ),
                    end: BraceEnd(None, TokenStream::from((15, "}")), None),
                    body: vec![Stmt::Expr(
                        Expr::Binary(ExprBinary {
                            left: Box::new(Expr::Path(ExprPath {
                                qself: None,
                                path: Path {
                                    leading_pathsep: None,
                                    segments: Punctuated {
                                        pairs: vec![],
                                        tail: Some(Box::new(PathSegment {
                                            ident: Ident(TokenStream::from((7, "a"))),
                                            arguments: None
                                        }))
                                    }
                                }
                            })),
                            op: BinOp::AddAssign(PlusEq(
                                Some(S(TokenStream::from((8, " ")))),
                                TokenStream::from((9, "+=")),
                                Some(S(TokenStream::from((11, " "))))
                            )),
                            right: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                                sign: None,
                                trunc: Some(Digits {
                                    input: TokenStream::from((12, "1")),
                                    value: 1
                                }),
                                fract: None,
                                exp: None
                            })))
                        }),
                        Some(Semi(
                            None,
                            TokenStream::from((13, ";")),
                            Some(S(TokenStream::from((14, " "))))
                        ))
                    )]
                })
            }))
        );

        assert_eq!(
            TokenStream::from("for a in b { a += 1; }").parse::<Expr<_>>(),
            Ok(Expr::For(ExprForLoop {
                label: None,
                for_token: For(
                    TokenStream::from((0, "for")),
                    Some(S(TokenStream::from((3, " "))))
                ),
                pat: (
                    Box::new(Pat::Ident(PatIdent {
                        by_ref: None,
                        mutability: None,
                        ident: Ident(TokenStream::from((4, "a"))),
                        subpat: None
                    })),
                    Some(S(TokenStream::from((5, " "))))
                ),
                in_token: In(
                    TokenStream::from((6, "in")),
                    Some(S(TokenStream::from((8, " "))))
                ),
                expr: Box::new(Expr::Path(ExprPath {
                    qself: None,
                    path: Path {
                        leading_pathsep: None,
                        segments: Punctuated {
                            pairs: vec![],
                            tail: Some(Box::new(PathSegment {
                                ident: Ident(TokenStream::from((9, "b"))),
                                arguments: None
                            }))
                        }
                    }
                })),
                body: Block(Delimiter {
                    start: BraceStart(
                        Some(S(TokenStream::from((10, " ")))),
                        TokenStream::from((11, "{")),
                        Some(S(TokenStream::from((12, " "))))
                    ),
                    end: BraceEnd(None, TokenStream::from((21, "}")), None),
                    body: vec![Stmt::Expr(
                        Expr::Binary(ExprBinary {
                            left: Box::new(Expr::Path(ExprPath {
                                qself: None,
                                path: Path {
                                    leading_pathsep: None,
                                    segments: Punctuated {
                                        pairs: vec![],
                                        tail: Some(Box::new(PathSegment {
                                            ident: Ident(TokenStream::from((13, "a"))),
                                            arguments: None
                                        }))
                                    }
                                }
                            })),
                            op: BinOp::AddAssign(PlusEq(
                                Some(S(TokenStream::from((14, " ")))),
                                TokenStream::from((15, "+=")),
                                Some(S(TokenStream::from((17, " "))))
                            )),
                            right: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                                sign: None,
                                trunc: Some(Digits {
                                    input: TokenStream::from((18, "1")),
                                    value: 1
                                }),
                                fract: None,
                                exp: None
                            })))
                        }),
                        Some(Semi(
                            None,
                            TokenStream::from((19, ";")),
                            Some(S(TokenStream::from((20, " "))))
                        ))
                    )]
                })
            }))
        );
    }

    #[test]
    fn test_while() {
        assert_eq!(
            TokenStream::from("while a < b { a += 2; }").parse::<Expr<_>>(),
            Ok(Expr::While(ExprWhile {
                label: None,
                keyword: While(
                    TokenStream::from((0, "while")),
                    Some(S(TokenStream::from((5, " "))))
                ),
                expr: Box::new(Expr::Binary(ExprBinary {
                    left: Box::new(Expr::Path(ExprPath {
                        qself: None,
                        path: Path {
                            leading_pathsep: None,
                            segments: Punctuated {
                                pairs: vec![],
                                tail: Some(Box::new(PathSegment {
                                    ident: Ident(TokenStream::from((6, "a"))),
                                    arguments: None
                                }))
                            }
                        }
                    })),
                    op: BinOp::Lt(Lt(
                        Some(S(TokenStream::from((7, " ")))),
                        TokenStream::from((8, "<")),
                        Some(S(TokenStream::from((9, " "))))
                    )),
                    right: Box::new(Expr::Path(ExprPath {
                        qself: None,
                        path: Path {
                            leading_pathsep: None,
                            segments: Punctuated {
                                pairs: vec![],
                                tail: Some(Box::new(PathSegment {
                                    ident: Ident(TokenStream::from((10, "b"))),
                                    arguments: None
                                }))
                            }
                        }
                    }))
                })),
                body: Block(Delimiter {
                    start: BraceStart(
                        Some(S(TokenStream::from((11, " ")))),
                        TokenStream::from((12, "{")),
                        Some(S(TokenStream::from((13, " "))))
                    ),
                    end: BraceEnd(None, TokenStream::from((22, "}")), None),
                    body: vec![Stmt::Expr(
                        Expr::Binary(ExprBinary {
                            left: Box::new(Expr::Path(ExprPath {
                                qself: None,
                                path: Path {
                                    leading_pathsep: None,
                                    segments: Punctuated {
                                        pairs: vec![],
                                        tail: Some(Box::new(PathSegment {
                                            ident: Ident(TokenStream::from((14, "a"))),
                                            arguments: None
                                        }))
                                    }
                                }
                            })),
                            op: BinOp::AddAssign(PlusEq(
                                Some(S(TokenStream::from((15, " ")))),
                                TokenStream::from((16, "+=")),
                                Some(S(TokenStream::from((18, " "))))
                            )),
                            right: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                                sign: None,
                                trunc: Some(Digits {
                                    input: TokenStream::from((19, "2")),
                                    value: 2
                                }),
                                fract: None,
                                exp: None
                            })))
                        }),
                        Some(Semi(
                            None,
                            TokenStream::from((20, ";")),
                            Some(S(TokenStream::from((21, " "))))
                        ))
                    )]
                })
            }))
        );

        assert_eq!(
            TokenStream::from("while let b = b { } ").parse::<Expr<_>>(),
            Ok(Expr::While(ExprWhile {
                label: None,
                keyword: While(
                    TokenStream::from((0, "while")),
                    Some(S(TokenStream::from((5, " "))))
                ),
                expr: Box::new(Expr::Let(ExprLet {
                    keyword: Let(
                        TokenStream::from((6, "let")),
                        Some(S(TokenStream::from((9, " "))))
                    ),
                    pat: Box::new(Pat::Ident(PatIdent {
                        by_ref: None,
                        mutability: None,
                        ident: Ident(TokenStream::from((10, "b"))),
                        subpat: None
                    })),
                    eq: Equal(
                        Some(S(TokenStream::from((11, " ")))),
                        TokenStream::from((12, "=")),
                        Some(S(TokenStream::from((13, " "))))
                    ),
                    expr: Box::new(Expr::Path(ExprPath {
                        qself: None,
                        path: Path {
                            leading_pathsep: None,
                            segments: Punctuated {
                                pairs: vec![],
                                tail: Some(Box::new(PathSegment {
                                    ident: Ident(TokenStream::from((14, "b"))),
                                    arguments: None
                                }))
                            }
                        }
                    }))
                })),
                body: Block(Delimiter {
                    start: BraceStart(
                        Some(S(TokenStream::from((15, " ")))),
                        TokenStream::from((16, "{")),
                        Some(S(TokenStream::from((17, " "))))
                    ),
                    end: BraceEnd(
                        None,
                        TokenStream::from((18, "}")),
                        Some(S(TokenStream::from((19, " "))))
                    ),
                    body: vec![]
                })
            }))
        );
    }

    #[test]
    fn detect_while_expr_error() {
        assert_eq!(
            TokenStream::from("while {}").parse::<Expr<_>>(),
            Err(CSTError::Punct(
                PunctKind::BraceStart,
                parserc::ControlFlow::Fatal,
                Span::Range(8..8)
            ))
        );

        assert_eq!(
            TokenStream::from("while let = b {}").parse::<Expr<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::Pat,
                parserc::ControlFlow::Fatal,
                Span::Range(10..11)
            ))
        );

        assert_eq!(
            TokenStream::from("while a {").parse::<Expr<_>>(),
            Err(CSTError::Punct(
                PunctKind::BraceEnd,
                parserc::ControlFlow::Fatal,
                Span::Range(9..9)
            ))
        );
    }

    #[test]
    fn unclosed_conditioness_loop() {
        assert_eq!(
            TokenStream::from("loop { ").parse::<Expr<_>>(),
            Err(CSTError::Punct(
                PunctKind::BraceEnd,
                ControlFlow::Fatal,
                Span::Range(7..7)
            ))
        );
    }
}
