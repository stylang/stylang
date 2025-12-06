use parserc::syntax::Syntax;

use crate::{
    block::Block,
    expr::{Expr, group::Composable},
    input::CSTInput,
    keyword::{For, In},
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

impl<I> Composable<I> for ExprForLoop<I>
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
        f(Expr::For(self))
    }
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, InputSyntaxExt, Punctuated};

    use crate::{
        block::{Block, Stmt},
        expr::{Expr, ExprCall, ExprForLoop, ExprIf, ExprMethodCall, ExprPath},
        input::TokenStream,
        keyword::{For, If, In},
        misc::{Ident, S},
        pat::Pat,
        path::{Path, PathSegment},
        punct::{BraceEnd, BraceStart, Dot, ParenEnd, ParenStart, Semi},
    };

    #[test]
    fn test_for() {
        assert_eq!(
            TokenStream::from("for a in b { if a.is_some() { a(); } } ").parse::<Expr<_>>(),
            Ok(Expr::For(ExprForLoop {
                label: None,
                for_token: For(
                    TokenStream::from((0, "for")),
                    Some(S(TokenStream::from((3, " "))))
                ),
                pat: (
                    Box::new(Pat::Ident(Ident(TokenStream::from((4, "a"))))),
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
                    end: BraceEnd(
                        None,
                        TokenStream::from((37, "}")),
                        Some(S(TokenStream::from((38, " "))))
                    ),
                    body: vec![Stmt::Expr(
                        Expr::If(ExprIf {
                            keyword: If(
                                TokenStream::from((13, "if")),
                                Some(S(TokenStream::from((15, " "))))
                            ),
                            cond: Box::new(Expr::MethodCall(ExprMethodCall {
                                receiver: Box::new(Expr::Path(ExprPath {
                                    qself: None,
                                    path: Path {
                                        leading_pathsep: None,
                                        segments: Punctuated {
                                            pairs: vec![],
                                            tail: Some(Box::new(PathSegment {
                                                ident: Ident(TokenStream::from((16, "a"))),
                                                arguments: None
                                            }))
                                        }
                                    }
                                })),
                                dot: Dot(None, TokenStream::from((17, ".")), None),
                                ident: Ident(TokenStream::from((18, "is_some"))),
                                turbofish: None,
                                args: Delimiter {
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
                                }
                            })),
                            then_branch: Block(Delimiter {
                                start: BraceStart(
                                    None,
                                    TokenStream::from((28, "{")),
                                    Some(S(TokenStream::from((29, " "))))
                                ),
                                end: BraceEnd(
                                    None,
                                    TokenStream::from((35, "}")),
                                    Some(S(TokenStream::from((36, " "))))
                                ),
                                body: vec![Stmt::Expr(
                                    Expr::Call(ExprCall {
                                        func: Box::new(Expr::Path(ExprPath {
                                            qself: None,
                                            path: Path {
                                                leading_pathsep: None,
                                                segments: Punctuated {
                                                    pairs: vec![],
                                                    tail: Some(Box::new(PathSegment {
                                                        ident: Ident(TokenStream::from((30, "a"))),
                                                        arguments: None
                                                    }))
                                                }
                                            }
                                        })),
                                        args: Delimiter {
                                            start: ParenStart(
                                                None,
                                                TokenStream::from((31, "(")),
                                                None
                                            ),
                                            end: ParenEnd(None, TokenStream::from((32, ")")), None),
                                            body: Punctuated {
                                                pairs: vec![],
                                                tail: None
                                            }
                                        }
                                    }),
                                    Some(Semi(
                                        None,
                                        TokenStream::from((33, ";")),
                                        Some(S(TokenStream::from((34, " "))))
                                    ))
                                )]
                            }),
                            else_branch: None
                        }),
                        None
                    )]
                })
            }))
        );
    }
}
