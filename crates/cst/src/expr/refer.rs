use parserc::syntax::Syntax;

use crate::{
    expr::{Expr, group::Composable, parse_lhs},
    input::CSTInput,
    keyword::Mut,
    punct::And,
};

// A referencing operation: &a or &mut a.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprReference<I>
where
    I: CSTInput,
{
    /// leading token `&`
    pub leading_token: And<I>,
    /// optional keyword `mut`
    pub mutability: Option<Mut<I>>,
    /// target expr.
    #[parserc(parser = parse_lhs.boxed())]
    pub expr: Box<Expr<I>>,
}

impl<I> Composable<I> for ExprReference<I>
where
    I: CSTInput,
{
    #[inline]
    fn priority(&self) -> usize {
        2
    }

    #[inline]
    fn compose<F>(mut self, priority: usize, f: F) -> Expr<I>
    where
        F: FnOnce(Expr<I>) -> Expr<I>,
    {
        if self.priority() > priority {
            self.expr = Box::new((*self.expr).compose(priority, f));
            Expr::Reference(self)
        } else {
            f(Expr::Reference(self))
        }
    }
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, InputSyntaxExt, Punctuated};

    use crate::{
        expr::{
            BinOp, Digits, Expr, ExprBinary, ExprCall, ExprIndex, ExprLit, ExprPath, ExprReference,
            LitNumber,
        },
        input::TokenStream,
        misc::Ident,
        path::{Path, PathSegment},
        punct::{And, BracketEnd, BracketStart, ParenEnd, ParenStart, Star},
    };

    #[test]
    fn test_ref() {
        assert_eq!(
            TokenStream::from("&a()*2").parse::<Expr<_>>(),
            Ok(Expr::Binary(ExprBinary {
                left: Box::new(Expr::Reference(ExprReference {
                    leading_token: And(None, TokenStream::from((0, "&")), None),
                    mutability: None,
                    expr: Box::new(Expr::Call(ExprCall {
                        func: Box::new(Expr::Path(ExprPath {
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
                        args: Delimiter {
                            start: ParenStart(None, TokenStream::from((2, "(")), None),
                            end: ParenEnd(None, TokenStream::from((3, ")")), None),
                            body: Punctuated {
                                pairs: vec![],
                                tail: None
                            }
                        }
                    }))
                })),
                op: BinOp::Mul(Star(None, TokenStream::from((4, "*")), None)),
                right: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                    sign: None,
                    trunc: Some(Digits {
                        input: TokenStream::from((5, "2")),
                        value: 2
                    }),
                    fract: None,
                    exp: None
                })))
            }))
        );
    }

    #[test]
    fn test_refref() {
        assert_eq!(
            TokenStream::from("&&a[1]").parse::<Expr<_>>(),
            Ok(Expr::Reference(ExprReference {
                leading_token: And(None, TokenStream::from((0, "&")), None),
                mutability: None,
                expr: Box::new(Expr::Reference(ExprReference {
                    leading_token: And(None, TokenStream::from((1, "&")), None),
                    mutability: None,
                    expr: Box::new(Expr::Index(ExprIndex {
                        expr: Box::new(Expr::Path(ExprPath {
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
                        })),
                        index: Delimiter {
                            start: BracketStart(None, TokenStream::from((3, "[")), None),
                            end: BracketEnd(None, TokenStream::from((5, "]")), None),
                            body: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                                sign: None,
                                trunc: Some(Digits {
                                    input: TokenStream::from((4, "1")),
                                    value: 1
                                }),
                                fract: None,
                                exp: None
                            })))
                        }
                    }))
                }))
            }))
        );
    }
}
