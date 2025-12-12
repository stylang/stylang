use parserc::syntax::{Delimiter, Punctuated, Syntax};

use crate::{
    expr::Expr,
    input::CSTInput,
    keyword::Const,
    pat::Pat,
    punct::{ArrowRight, Comma, Or},
    ty::Type,
};

/// A closure expression: |a, b| a + b.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprClosure<I>
where
    I: CSTInput,
{
    /// optional `const` keyword.
    pub constness: Option<Const<I>>,
    /// input arguments of closure.
    #[parserc(crucial)]
    pub inputs: Delimiter<Or<I>, Or<I>, Punctuated<Pat<I>, Comma<I>>>,
    /// Return type of a function signature.
    pub output: Option<(ArrowRight<I>, Box<Type<I>>)>,
    /// Closure body expr.
    pub body: Box<Expr<I>>,
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, Punctuated, SyntaxInput};

    use crate::{
        block::{Block, LocalInit, Stmt},
        expr::{BinOp, Expr, ExprBinary, ExprClosure, ExprConst, ExprPath},
        input::TokenStream,
        keyword::{Const, Let},
        misc::{Ident, S},
        pat::{Pat, PatIdent},
        path::{Path, PathSegment},
        punct::{BraceEnd, BraceStart, Comma, Equal, Or, Plus, Semi},
    };

    #[test]
    fn test_closure() {
        assert_eq!(
            TokenStream::from("const { let a = const |a,b| a + b; a}").parse::<Expr<_>>(),
            Ok(Expr::Const(ExprConst {
                keyword: Const(
                    TokenStream::from((0, "const")),
                    Some(S(TokenStream::from((5, " "))))
                ),
                block: Block(Delimiter {
                    start: BraceStart(
                        None,
                        TokenStream::from((6, "{")),
                        Some(S(TokenStream::from((7, " "))))
                    ),
                    end: BraceEnd(None, TokenStream::from((36, "}")), None),
                    body: vec![
                        Stmt::Local {
                            keyword: Let(
                                TokenStream::from((8, "let")),
                                Some(S(TokenStream::from((11, " "))))
                            ),
                            pat: Pat::Ident(PatIdent {
                                by_ref: None,
                                mutability: None,
                                ident: Ident(TokenStream::from((12, "a"))),
                                subpat: None
                            }),
                            init: Some(LocalInit {
                                eq: Equal(
                                    Some(S(TokenStream::from((13, " ")))),
                                    TokenStream::from((14, "=")),
                                    Some(S(TokenStream::from((15, " "))))
                                ),
                                expr: Box::new(Expr::Closure(ExprClosure {
                                    constness: Some(Const(
                                        TokenStream::from((16, "const")),
                                        Some(S(TokenStream::from((21, " "))))
                                    )),
                                    inputs: Delimiter {
                                        start: Or(None, TokenStream::from((22, "|")), None),
                                        end: Or(
                                            None,
                                            TokenStream::from((26, "|")),
                                            Some(S(TokenStream::from((27, " "))))
                                        ),
                                        body: Punctuated {
                                            pairs: vec![(
                                                Pat::Ident(PatIdent {
                                                    by_ref: None,
                                                    mutability: None,
                                                    ident: Ident(TokenStream::from((23, "a"))),
                                                    subpat: None
                                                }),
                                                Comma(None, TokenStream::from((24, ",")), None)
                                            )],
                                            tail: Some(Box::new(Pat::Ident(PatIdent {
                                                by_ref: None,
                                                mutability: None,
                                                ident: Ident(TokenStream::from((25, "b"))),
                                                subpat: None
                                            })))
                                        }
                                    },
                                    output: None,
                                    body: Box::new(Expr::Binary(ExprBinary {
                                        left: Box::new(Expr::Path(ExprPath {
                                            qself: None,
                                            path: Path {
                                                leading_pathsep: None,
                                                segments: Punctuated {
                                                    pairs: vec![],
                                                    tail: Some(Box::new(PathSegment {
                                                        ident: Ident(TokenStream::from((28, "a"))),
                                                        arguments: None
                                                    }))
                                                }
                                            }
                                        })),
                                        op: BinOp::Add(Plus(
                                            Some(S(TokenStream::from((29, " ")))),
                                            TokenStream::from((30, "+")),
                                            Some(S(TokenStream::from((31, " "))))
                                        )),
                                        right: Box::new(Expr::Path(ExprPath {
                                            qself: None,
                                            path: Path {
                                                leading_pathsep: None,
                                                segments: Punctuated {
                                                    pairs: vec![],
                                                    tail: Some(Box::new(PathSegment {
                                                        ident: Ident(TokenStream::from((32, "b"))),
                                                        arguments: None
                                                    }))
                                                }
                                            }
                                        }))
                                    }))
                                })),
                                diverge: None
                            }),
                            semi: Semi(
                                None,
                                TokenStream::from((33, ";")),
                                Some(S(TokenStream::from((34, " "))))
                            )
                        },
                        Stmt::Expr(
                            Expr::Path(ExprPath {
                                qself: None,
                                path: Path {
                                    leading_pathsep: None,
                                    segments: Punctuated {
                                        pairs: vec![],
                                        tail: Some(Box::new(PathSegment {
                                            ident: Ident(TokenStream::from((35, "a"))),
                                            arguments: None
                                        }))
                                    }
                                }
                            }),
                            None
                        )
                    ]
                })
            }))
        );
    }
}
