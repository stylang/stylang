use parserc::syntax::{Delimiter, Punctuated};

use crate::{
    expr::{Expr, group::Composable},
    input::CSTInput,
    punct::{Comma, ParenEnd, ParenStart},
};

pub type CallArgs<I> = Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<Expr<I>, Comma<I>>>;

/// A function call expression: invoke(a, b).
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprCall<I>
where
    I: CSTInput,
{
    /// function body.
    pub func: Box<Expr<I>>,
    /// call arguments.
    pub args: CallArgs<I>,
}

impl<I> Composable<I> for ExprCall<I>
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
        f(Expr::Call(self))
    }
}

#[cfg(test)]
mod tests {
    use parserc::syntax::InputSyntaxExt;

    use super::*;
    use crate::{
        expr::{Digits, Expr, ExprLit, ExprPath, LitNumber},
        input::TokenStream,
        misc::Ident,
        path::{Path, PathSegment},
        punct::PathSep,
    };

    #[test]
    fn test_call() {
        assert_eq!(
            TokenStream::from("hello()(1,World::Char)").parse::<Expr<_>>(),
            Ok(Expr::Call(ExprCall {
                func: Box::new(Expr::Call(ExprCall {
                    func: Box::new(Expr::Path(ExprPath {
                        qself: None,
                        path: Path {
                            leading_pathsep: None,
                            segments: Punctuated {
                                pairs: vec![],
                                tail: Some(Box::new(PathSegment {
                                    ident: Ident(TokenStream::from((0, "hello"))),
                                    arguments: None
                                }))
                            }
                        }
                    })),
                    args: Delimiter {
                        start: ParenStart(None, TokenStream::from((5, "(")), None),
                        end: ParenEnd(None, TokenStream::from((6, ")")), None),
                        body: Punctuated {
                            pairs: vec![],
                            tail: None
                        }
                    }
                })),
                args: Delimiter {
                    start: ParenStart(None, TokenStream::from((7, "(")), None),
                    end: ParenEnd(None, TokenStream::from((21, ")")), None),
                    body: Punctuated {
                        pairs: vec![(
                            Expr::Lit(ExprLit::Number(LitNumber {
                                sign: None,
                                trunc: Some(Digits {
                                    input: TokenStream::from((8, "1")),
                                    value: 1
                                }),
                                fract: None,
                                exp: None
                            })),
                            Comma(None, TokenStream::from((9, ",")), None)
                        )],
                        tail: Some(Box::new(Expr::Path(ExprPath {
                            qself: None,
                            path: Path {
                                leading_pathsep: None,
                                segments: Punctuated {
                                    pairs: vec![(
                                        PathSegment {
                                            ident: Ident(TokenStream::from((10, "World"))),
                                            arguments: None
                                        },
                                        PathSep(None, TokenStream::from((15, "::")), None)
                                    )],
                                    tail: Some(Box::new(PathSegment {
                                        ident: Ident(TokenStream::from((17, "Char"))),
                                        arguments: None
                                    }))
                                }
                            }
                        })))
                    }
                }
            }))
        );
    }
}
