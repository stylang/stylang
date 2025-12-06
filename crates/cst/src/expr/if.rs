use parserc::syntax::{Or, Syntax};

use crate::{
    block::Block,
    expr::{Expr, group::Composable},
    input::CSTInput,
    keyword::{Else, If},
};
/// An if expression with an optional else block: if expr { ... } else { ... }.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprIf<I>
where
    I: CSTInput,
{
    /// leading keyword `if`
    #[parserc(crucial)]
    pub keyword: If<I>,
    /// if condition sub-expr.
    pub cond: Box<Expr<I>>,
    /// then branch.
    pub then_branch: Block<I>,
    /// Optional else branch.
    pub else_branch: Option<ElseBranch<I>>,
}

/// Else block.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ElseBranch<I>
where
    I: CSTInput,
{
    /// leading keyword `else`
    pub keyword: Else<I>,
    /// `if {}` or `{}`
    pub branch: Or<Box<ExprIf<I>>, Block<I>>,
}

impl<I> Composable<I> for ExprIf<I>
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
        f(Expr::If(self))
    }
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, InputSyntaxExt, Or, Punctuated};

    use crate::{
        block::{Block, Stmt},
        expr::{BinOp, ElseBranch, Expr, ExprBinary, ExprCall, ExprIf, ExprPath},
        input::TokenStream,
        keyword::{Else, If},
        misc::{Ident, S},
        path::{Path, PathSegment},
        punct::{BraceEnd, BraceStart, NotEq, ParenEnd, ParenStart},
    };

    #[test]
    fn test_nested_if() {
        assert_eq!(
            TokenStream::from("if a != b { c } else if d { e } else { f }() ").parse::<Expr<_>>(),
            Ok(Expr::Call(ExprCall {
                func: Box::new(Expr::If(ExprIf {
                    keyword: If(
                        TokenStream::from((0, "if")),
                        Some(S(TokenStream::from((2, " "))))
                    ),
                    cond: Box::new(Expr::Binary(ExprBinary {
                        left: Box::new(Expr::Path(ExprPath {
                            qself: None,
                            path: Path {
                                leading_pathsep: None,
                                segments: Punctuated {
                                    pairs: vec![],
                                    tail: Some(Box::new(PathSegment {
                                        ident: Ident(TokenStream::from((3, "a"))),
                                        arguments: None
                                    }))
                                }
                            }
                        })),
                        op: BinOp::NotEq(NotEq(
                            Some(S(TokenStream::from((4, " ")))),
                            TokenStream::from((5, "!=")),
                            Some(S(TokenStream::from((7, " "))))
                        )),
                        right: Box::new(Expr::Path(ExprPath {
                            qself: None,
                            path: Path {
                                leading_pathsep: None,
                                segments: Punctuated {
                                    pairs: vec![],
                                    tail: Some(Box::new(PathSegment {
                                        ident: Ident(TokenStream::from((8, "b"))),
                                        arguments: None
                                    }))
                                }
                            }
                        }))
                    })),
                    then_branch: Block(Delimiter {
                        start: BraceStart(
                            Some(S(TokenStream::from((9, " ")))),
                            TokenStream::from((10, "{")),
                            Some(S(TokenStream::from((11, " "))))
                        ),
                        end: BraceEnd(
                            Some(S(TokenStream::from((13, " ")))),
                            TokenStream::from((14, "}")),
                            Some(S(TokenStream::from((15, " "))))
                        ),
                        body: vec![Stmt::Expr(
                            Expr::Path(ExprPath {
                                qself: None,
                                path: Path {
                                    leading_pathsep: None,
                                    segments: Punctuated {
                                        pairs: vec![],
                                        tail: Some(Box::new(PathSegment {
                                            ident: Ident(TokenStream::from((12, "c"))),
                                            arguments: None
                                        }))
                                    }
                                }
                            }),
                            None
                        )]
                    }),
                    else_branch: Some(ElseBranch {
                        keyword: Else(
                            TokenStream::from((16, "else")),
                            Some(S(TokenStream::from((20, " "))))
                        ),
                        branch: Or::First(Box::new(ExprIf {
                            keyword: If(
                                TokenStream::from((21, "if")),
                                Some(S(TokenStream::from((23, " "))))
                            ),
                            cond: Box::new(Expr::Path(ExprPath {
                                qself: None,
                                path: Path {
                                    leading_pathsep: None,
                                    segments: Punctuated {
                                        pairs: vec![],
                                        tail: Some(Box::new(PathSegment {
                                            ident: Ident(TokenStream::from((24, "d"))),
                                            arguments: None
                                        }))
                                    }
                                }
                            })),
                            then_branch: Block(Delimiter {
                                start: BraceStart(
                                    Some(S(TokenStream::from((25, " ")))),
                                    TokenStream::from((26, "{")),
                                    Some(S(TokenStream::from((27, " "))))
                                ),
                                end: BraceEnd(
                                    Some(S(TokenStream::from((29, " ")))),
                                    TokenStream::from((30, "}")),
                                    Some(S(TokenStream::from((31, " "))))
                                ),
                                body: vec![Stmt::Expr(
                                    Expr::Path(ExprPath {
                                        qself: None,
                                        path: Path {
                                            leading_pathsep: None,
                                            segments: Punctuated {
                                                pairs: vec![],
                                                tail: Some(Box::new(PathSegment {
                                                    ident: Ident(TokenStream::from((28, "e"))),
                                                    arguments: None
                                                }))
                                            }
                                        }
                                    }),
                                    None
                                )]
                            }),
                            else_branch: Some(ElseBranch {
                                keyword: Else(
                                    TokenStream::from((32, "else")),
                                    Some(S(TokenStream::from((36, " "))))
                                ),
                                branch: Or::Second(Block(Delimiter {
                                    start: BraceStart(
                                        None,
                                        TokenStream::from((37, "{")),
                                        Some(S(TokenStream::from((38, " "))))
                                    ),
                                    end: BraceEnd(
                                        Some(S(TokenStream::from((40, " ")))),
                                        TokenStream::from((41, "}")),
                                        None
                                    ),
                                    body: vec![Stmt::Expr(
                                        Expr::Path(ExprPath {
                                            qself: None,
                                            path: Path {
                                                leading_pathsep: None,
                                                segments: Punctuated {
                                                    pairs: vec![],
                                                    tail: Some(Box::new(PathSegment {
                                                        ident: Ident(TokenStream::from((39, "f"))),
                                                        arguments: None
                                                    }))
                                                }
                                            }
                                        }),
                                        None
                                    )]
                                }))
                            })
                        }))
                    })
                })),
                args: Delimiter {
                    start: ParenStart(None, TokenStream::from((42, "(")), None),
                    end: ParenEnd(
                        None,
                        TokenStream::from((43, ")")),
                        Some(S(TokenStream::from((44, " "))))
                    ),
                    body: Punctuated {
                        pairs: vec![],
                        tail: None
                    }
                }
            }))
        );
    }
}
