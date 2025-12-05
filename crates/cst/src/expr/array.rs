use parserc::syntax::{Delimiter, Punctuated, Syntax};

use crate::{
    expr::{Expr, group::Composable},
    input::CSTInput,
    punct::{BracketEnd, BracketStart, Comma},
};

/// A slice literal expression
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprArray<I>(
    pub Delimiter<BracketStart<I>, BracketEnd<I>, Punctuated<Expr<I>, Comma<I>>>,
)
where
    I: CSTInput;

impl<I> Composable<I> for ExprArray<I>
where
    I: CSTInput,
{
    #[inline]
    fn priority(&self) -> usize {
        1
    }

    #[inline]
    fn compose<F>(self, _: usize, f: F) -> super::Expr<I>
    where
        F: FnOnce(super::Expr<I>) -> super::Expr<I>,
    {
        f(Expr::Array(self))
    }
}

#[cfg(test)]
mod tests {
    use parserc::syntax::InputSyntaxExt;

    use super::*;
    use crate::{
        expr::{Digits, ExprArray, ExprLit, LitNumber},
        input::TokenStream,
        misc::S,
    };

    #[test]
    fn test_array() {
        assert_eq!(
            TokenStream::from("[[1,3],[1,2], [3,4]]").parse::<ExprArray<_>>(),
            Ok(ExprArray(Delimiter {
                start: BracketStart(None, TokenStream::from((0, "[")), None),
                end: BracketEnd(None, TokenStream::from((19, "]")), None),
                body: Punctuated {
                    pairs: vec![
                        (
                            Expr::Array(ExprArray(Delimiter {
                                start: BracketStart(None, TokenStream::from((1, "[")), None),
                                end: BracketEnd(None, TokenStream::from((5, "]")), None),
                                body: Punctuated {
                                    pairs: vec![(
                                        Expr::Lit(ExprLit::Number(LitNumber {
                                            sign: None,
                                            trunc: Some(Digits {
                                                input: TokenStream::from((2, "1")),
                                                value: 1
                                            }),
                                            fract: None,
                                            exp: None
                                        })),
                                        Comma(None, TokenStream::from((3, ",")), None)
                                    )],
                                    tail: Some(Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                                        sign: None,
                                        trunc: Some(Digits {
                                            input: TokenStream::from((4, "3")),
                                            value: 3
                                        }),
                                        fract: None,
                                        exp: None
                                    }))))
                                }
                            })),
                            Comma(None, TokenStream::from((6, ",")), None)
                        ),
                        (
                            Expr::Array(ExprArray(Delimiter {
                                start: BracketStart(None, TokenStream::from((7, "[")), None),
                                end: BracketEnd(None, TokenStream::from((11, "]")), None),
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
                                    tail: Some(Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                                        sign: None,
                                        trunc: Some(Digits {
                                            input: TokenStream::from((10, "2")),
                                            value: 2
                                        }),
                                        fract: None,
                                        exp: None
                                    }))))
                                }
                            })),
                            Comma(
                                None,
                                TokenStream::from((12, ",")),
                                Some(S(TokenStream::from((13, " "))))
                            )
                        )
                    ],
                    tail: Some(Box::new(Expr::Array(ExprArray(Delimiter {
                        start: BracketStart(None, TokenStream::from((14, "[")), None),
                        end: BracketEnd(None, TokenStream::from((18, "]")), None),
                        body: Punctuated {
                            pairs: vec![(
                                Expr::Lit(ExprLit::Number(LitNumber {
                                    sign: None,
                                    trunc: Some(Digits {
                                        input: TokenStream::from((15, "3")),
                                        value: 3
                                    }),
                                    fract: None,
                                    exp: None
                                })),
                                Comma(None, TokenStream::from((16, ",")), None)
                            )],
                            tail: Some(Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                                sign: None,
                                trunc: Some(Digits {
                                    input: TokenStream::from((17, "4")),
                                    value: 4
                                }),
                                fract: None,
                                exp: None
                            }))))
                        }
                    }))))
                }
            }))
        );
    }
}
