use parserc::syntax::{Or, Syntax};

use crate::{
    block::Block,
    expr::Expr,
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

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, Punctuated, SyntaxInput};

    use crate::{
        block::Stmt,
        expr::{Digits, Expr, ExprLit, ExprPath, ExprRange, Field, LitNumber, Member, RangeLimits},
        input::TokenStream,
        misc::{Ident, S},
        path::{Path, PathSegment},
        punct::{BraceEnd, BraceStart, Dot, DotDotEq},
    };

    use super::*;

    #[test]
    fn test_if() {
        assert_eq!(
            TokenStream::from("if a { b } else if c { c } else { d }.name ..= 19")
                .parse::<Expr<_>>(),
            Ok(Expr::Range(ExprRange {
                start: Some(Box::new(Expr::Field(
                    Box::new(Expr::If(ExprIf {
                        keyword: If(
                            TokenStream::from((0, "if")),
                            Some(S(TokenStream::from((2, " "))))
                        ),
                        cond: Box::new(Expr::Path(ExprPath {
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
                        then_branch: Block(Delimiter {
                            start: BraceStart(
                                Some(S(TokenStream::from((4, " ")))),
                                TokenStream::from((5, "{")),
                                Some(S(TokenStream::from((6, " "))))
                            ),
                            end: BraceEnd(
                                Some(S(TokenStream::from((8, " ")))),
                                TokenStream::from((9, "}")),
                                Some(S(TokenStream::from((10, " "))))
                            ),
                            body: vec![Stmt::Expr(
                                Expr::Path(ExprPath {
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
                                }),
                                None
                            )]
                        }),
                        else_branch: Some(ElseBranch {
                            keyword: Else(
                                TokenStream::from((11, "else")),
                                Some(S(TokenStream::from((15, " "))))
                            ),
                            branch: Or::First(Box::new(ExprIf {
                                keyword: If(
                                    TokenStream::from((16, "if")),
                                    Some(S(TokenStream::from((18, " "))))
                                ),
                                cond: Box::new(Expr::Path(ExprPath {
                                    qself: None,
                                    path: Path {
                                        leading_pathsep: None,
                                        segments: Punctuated {
                                            pairs: vec![],
                                            tail: Some(Box::new(PathSegment {
                                                ident: Ident(TokenStream::from((19, "c"))),
                                                arguments: None
                                            }))
                                        }
                                    }
                                })),
                                then_branch: Block(Delimiter {
                                    start: BraceStart(
                                        Some(S(TokenStream::from((20, " ")))),
                                        TokenStream::from((21, "{")),
                                        Some(S(TokenStream::from((22, " "))))
                                    ),
                                    end: BraceEnd(
                                        Some(S(TokenStream::from((24, " ")))),
                                        TokenStream::from((25, "}")),
                                        Some(S(TokenStream::from((26, " "))))
                                    ),
                                    body: vec![Stmt::Expr(
                                        Expr::Path(ExprPath {
                                            qself: None,
                                            path: Path {
                                                leading_pathsep: None,
                                                segments: Punctuated {
                                                    pairs: vec![],
                                                    tail: Some(Box::new(PathSegment {
                                                        ident: Ident(TokenStream::from((23, "c"))),
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
                                        TokenStream::from((27, "else")),
                                        Some(S(TokenStream::from((31, " "))))
                                    ),
                                    branch: Or::Second(Block(Delimiter {
                                        start: BraceStart(
                                            None,
                                            TokenStream::from((32, "{")),
                                            Some(S(TokenStream::from((33, " "))))
                                        ),
                                        end: BraceEnd(
                                            Some(S(TokenStream::from((35, " ")))),
                                            TokenStream::from((36, "}")),
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
                                                            ident: Ident(TokenStream::from((
                                                                34, "d"
                                                            ))),
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
                    vec![],
                    Field {
                        dot: Dot(None, TokenStream::from((37, ".")), None),
                        member: Member::Named(Ident(TokenStream::from((38, "name"))))
                    }
                ))),
                limits: RangeLimits::Closed(DotDotEq(
                    Some(S(TokenStream::from((42, " ")))),
                    TokenStream::from((43, "..=")),
                    Some(S(TokenStream::from((46, " "))))
                )),
                end: Some(Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                    sign: None,
                    trunc: Some(Digits {
                        input: TokenStream::from((47, "19")),
                        value: 19
                    }),
                    fract: None,
                    exp: None
                }))))
            }))
        );
    }
}
