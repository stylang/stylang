use parserc::syntax::Syntax;

use crate::{
    block::Block,
    expr::{Expr, group::Composable},
    input::CSTInput,
    keyword::Const,
};

/// A const block: `const { ... }`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprConst<I>
where
    I: CSTInput,
{
    /// A keyword `const`
    pub keyword: Const<I>,
    /// A const block .
    pub block: Block<I>,
}

impl<I> Composable<I> for ExprConst<I>
where
    I: CSTInput,
{
    #[inline]
    fn priority(&self) -> usize {
        1
    }

    #[inline]
    fn compose<F>(self, _priority: usize, f: F) -> super::Expr<I>
    where
        F: FnOnce(super::Expr<I>) -> super::Expr<I>,
    {
        f(Expr::Const(self))
    }
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, InputSyntaxExt, Punctuated};

    use crate::{
        block::{LocalInit, Stmt},
        expr::{BinOp, Digits, Expr, ExprBinary, ExprLit, ExprPath, LitNumber},
        input::TokenStream,
        keyword::Let,
        misc::{Ident, S},
        pat::Pat,
        path::{Path, PathSegment},
        punct::{BraceEnd, BraceStart, Equal, Semi, Star},
    };

    use super::*;

    #[test]
    fn test_const() {
        assert_eq!(
            TokenStream::from("const { let i = 1; let j = 2; i * j }").parse::<Expr<_>>(),
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
                    end: BraceEnd(
                        Some(S(TokenStream::from((35, " ")))),
                        TokenStream::from((36, "}")),
                        None
                    ),
                    body: vec![
                        Stmt::Local {
                            keyword: Let(
                                TokenStream::from((8, "let")),
                                Some(S(TokenStream::from((11, " "))))
                            ),
                            pat: Pat::Ident(Ident(TokenStream::from((12, "i")))),
                            init: Some(LocalInit {
                                eq: Equal(
                                    Some(S(TokenStream::from((13, " ")))),
                                    TokenStream::from((14, "=")),
                                    Some(S(TokenStream::from((15, " "))))
                                ),
                                expr: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                                    sign: None,
                                    trunc: Some(Digits {
                                        input: TokenStream::from((16, "1")),
                                        value: 1
                                    }),
                                    fract: None,
                                    exp: None
                                }))),
                                diverge: None
                            }),
                            semi: Semi(
                                None,
                                TokenStream::from((17, ";")),
                                Some(S(TokenStream::from((18, " "))))
                            )
                        },
                        Stmt::Local {
                            keyword: Let(
                                TokenStream::from((19, "let")),
                                Some(S(TokenStream::from((22, " "))))
                            ),
                            pat: Pat::Ident(Ident(TokenStream::from((23, "j")))),
                            init: Some(LocalInit {
                                eq: Equal(
                                    Some(S(TokenStream::from((24, " ")))),
                                    TokenStream::from((25, "=")),
                                    Some(S(TokenStream::from((26, " "))))
                                ),
                                expr: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                                    sign: None,
                                    trunc: Some(Digits {
                                        input: TokenStream::from((27, "2")),
                                        value: 2
                                    }),
                                    fract: None,
                                    exp: None
                                }))),
                                diverge: None
                            }),
                            semi: Semi(
                                None,
                                TokenStream::from((28, ";")),
                                Some(S(TokenStream::from((29, " "))))
                            )
                        },
                        Stmt::Expr(
                            Expr::Binary(ExprBinary {
                                left: Box::new(Expr::Path(ExprPath {
                                    qself: None,
                                    path: Path {
                                        leading_pathsep: None,
                                        segments: Punctuated {
                                            pairs: vec![],
                                            tail: Some(Box::new(PathSegment {
                                                ident: Ident(TokenStream::from((30, "i"))),
                                                arguments: None
                                            }))
                                        }
                                    }
                                })),
                                op: BinOp::Mul(Star(
                                    Some(S(TokenStream::from((31, " ")))),
                                    TokenStream::from((32, "*")),
                                    Some(S(TokenStream::from((33, " "))))
                                )),
                                right: Box::new(Expr::Path(ExprPath {
                                    qself: None,
                                    path: Path {
                                        leading_pathsep: None,
                                        segments: Punctuated {
                                            pairs: vec![],
                                            tail: Some(Box::new(PathSegment {
                                                ident: Ident(TokenStream::from((34, "j"))),
                                                arguments: None
                                            }))
                                        }
                                    }
                                }))
                            }),
                            None
                        )
                    ]
                })
            }))
        );
    }
}
