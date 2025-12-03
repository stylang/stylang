use parserc::{
    Parser,
    syntax::{Delimiter, InputSyntaxExt, Punctuated, Syntax},
};

use crate::{
    expr::{Expr, ExprPath},
    input::CSTInput,
    punct::{Comma, ParenEnd, ParenStart},
};

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
    pub args: Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<Expr<I>, Comma<I>>>,
}

impl<I> Syntax<I> for ExprCall<I>
where
    I: CSTInput,
{
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let mut func = ExprPath::into_parser()
            .map(|expr| Expr::Path(expr))
            .boxed()
            .parse(input)?;

        let mut args = input.parse()?;

        while let Some(next_call) = input.parse()? {
            func = Box::new(Expr::Call(Self { func, args }));
            args = next_call;
        }

        Ok(Self { func, args })
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.func.to_span() + self.args.to_span()
    }
}

#[cfg(test)]
mod tests {
    use parserc::syntax::InputSyntaxExt;

    use super::*;
    use crate::{
        expr::{Digits, Expr, ExprLit, LitNumber},
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
