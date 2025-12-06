use parserc::syntax::Syntax;

use crate::{
    errors::SyntaxKind,
    expr::{Expr, group::Composable, parse_rhs},
    input::CSTInput,
    punct::{Minus, Not, Star},
};

/// Unary operator.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::ExprBinaryOp.map())]
pub enum UnaryOp<I>
where
    I: CSTInput,
{
    Deref(Star<I>),
    Not(Not<I>),
    Neg(Minus<I>),
}

/// A unary operator: `*`, `!`, `-`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprUnary<I>
where
    I: CSTInput,
{
    /// leading unary operator char.
    #[parserc(crucial)]
    pub op: UnaryOp<I>,
    /// right operand.
    #[parserc(
        parser = parse_rhs.boxed(),
        map_err = SyntaxKind::ExprUnaryRightOprand.map_unhandle()
    )]
    pub right: Box<Expr<I>>,
}

impl<I> Composable<I> for ExprUnary<I>
where
    I: CSTInput,
{
    fn priority(&self) -> usize {
        1
    }

    fn compose<F>(self, _: usize, f: F) -> super::Expr<I>
    where
        F: FnOnce(super::Expr<I>) -> super::Expr<I>,
    {
        f(Expr::Unary(self))
    }
}

#[cfg(test)]
mod test {
    use parserc::syntax::{InputSyntaxExt, Punctuated};

    use crate::{
        expr::{BinOp, ExprBinary, ExprPath},
        input::TokenStream,
        misc::{Ident, S},
        path::{Path, PathSegment},
        punct::{Caret, OrOr},
    };

    use super::*;

    #[test]
    fn test_unary() {
        assert_eq!(
            TokenStream::from("!a || - b || c ^ d").parse::<Expr<_>>(),
            Ok(Expr::Binary(ExprBinary {
                left: Box::new(Expr::Binary(ExprBinary {
                    left: Box::new(Expr::Unary(ExprUnary {
                        op: UnaryOp::Not(Not(None, TokenStream::from((0, "!")), None)),
                        right: Box::new(Expr::Path(ExprPath {
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
                        }))
                    })),
                    op: BinOp::Or(OrOr(
                        Some(S(TokenStream::from((2, " ")))),
                        TokenStream::from((3, "||")),
                        Some(S(TokenStream::from((5, " "))))
                    )),
                    right: Box::new(Expr::Unary(ExprUnary {
                        op: UnaryOp::Neg(Minus(
                            None,
                            TokenStream::from((6, "-")),
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
                    }))
                })),
                op: BinOp::Or(OrOr(
                    Some(S(TokenStream::from((9, " ")))),
                    TokenStream::from((10, "||")),
                    Some(S(TokenStream::from((12, " "))))
                )),
                right: Box::new(Expr::Binary(ExprBinary {
                    left: Box::new(Expr::Path(ExprPath {
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
                    op: BinOp::BitXor(Caret(
                        Some(S(TokenStream::from((14, " ")))),
                        TokenStream::from((15, "^")),
                        Some(S(TokenStream::from((16, " "))))
                    )),
                    right: Box::new(Expr::Path(ExprPath {
                        qself: None,
                        path: Path {
                            leading_pathsep: None,
                            segments: Punctuated {
                                pairs: vec![],
                                tail: Some(Box::new(PathSegment {
                                    ident: Ident(TokenStream::from((17, "d"))),
                                    arguments: None
                                }))
                            }
                        }
                    }))
                }))
            }))
        );
    }

    #[test]
    fn test_nested_unary() {
        assert_eq!(
            TokenStream::from("!*a").parse::<Expr<_>>(),
            Ok(Expr::Unary(ExprUnary {
                op: UnaryOp::Not(Not(None, TokenStream::from((0, "!")), None)),
                right: Box::new(Expr::Unary(ExprUnary {
                    op: UnaryOp::Deref(Star(None, TokenStream::from((1, "*")), None)),
                    right: Box::new(Expr::Path(ExprPath {
                        qself: None,
                        path: Path {
                            leading_pathsep: None,
                            segments: Punctuated {
                                pairs: vec![],
                                tail: Some(Box::new(PathSegment {
                                    ident: Ident(TokenStream::from((2, "a"))),
                                    arguments: None
                                }))
                            }
                        }
                    }))
                }))
            }))
        );
    }
}
