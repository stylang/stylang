use parserc::syntax::{Punctuated, Syntax};

use crate::{
    expr::Expr,
    input::CSTInput,
    lexical::{
        delimiter::Bracket,
        punct::{BracketEnd, BracketStart, Comma, Semi},
    },
};

/// Array expressions construct arrays. Array expressions come in two forms.
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions/array-expr.html#grammar-ArrayExpression
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ArrayExpr<I>
where
    I: CSTInput,
{
    Repeat {
        delimiter_start: BracketStart<I>,
        value: Box<Expr<I>>,
        semi: Semi<I>,
        length: Box<Expr<I>>,
        delimiter_end: BracketEnd<I>,
    },
    List(Bracket<I, Punctuated<Expr<I>, Comma<I>>>),
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, SyntaxInput};

    use crate::{
        expr::{Expr, ExprWithoutBlock, ExprWithoutBlockBody, LitExpr, PathExpr},
        input::TokenStream,
        lexical::{
            S,
            ident::{Ident, NonKeywordIdent},
            lit::{LitDec, LitInt},
        },
        names::paths::{PathInExpr, SimplePathSegment},
    };

    use super::*;

    #[test]
    fn test_array() {
        assert_eq!(
            TokenStream::from("[1, 2, 3, 4,]").parse::<Expr<_>>(),
            Ok(Expr::WithoutBlock(ExprWithoutBlock {
                attrs: vec![],
                body: ExprWithoutBlockBody::ArrayExpr(ArrayExpr::List(Delimiter {
                    start: BracketStart(None, TokenStream::from((0, "[")), None),
                    end: BracketEnd(None, TokenStream::from((12, "]")), None),
                    body: Punctuated {
                        pairs: vec![
                            (
                                Expr::WithoutBlock(ExprWithoutBlock {
                                    attrs: vec![],
                                    body: ExprWithoutBlockBody::LitExpr(LitExpr::Int(
                                        LitInt::Dec(LitDec(TokenStream::from((1, "1")))),
                                        None
                                    ))
                                }),
                                Comma(
                                    None,
                                    TokenStream::from((2, ",")),
                                    Some(S(TokenStream::from((3, " "))))
                                )
                            ),
                            (
                                Expr::WithoutBlock(ExprWithoutBlock {
                                    attrs: vec![],
                                    body: ExprWithoutBlockBody::LitExpr(LitExpr::Int(
                                        LitInt::Dec(LitDec(TokenStream::from((4, "2")))),
                                        None
                                    ))
                                }),
                                Comma(
                                    None,
                                    TokenStream::from((5, ",")),
                                    Some(S(TokenStream::from((6, " "))))
                                )
                            ),
                            (
                                Expr::WithoutBlock(ExprWithoutBlock {
                                    attrs: vec![],
                                    body: ExprWithoutBlockBody::LitExpr(LitExpr::Int(
                                        LitInt::Dec(LitDec(TokenStream::from((7, "3")))),
                                        None
                                    ))
                                }),
                                Comma(
                                    None,
                                    TokenStream::from((8, ",")),
                                    Some(S(TokenStream::from((9, " "))))
                                )
                            ),
                            (
                                Expr::WithoutBlock(ExprWithoutBlock {
                                    attrs: vec![],
                                    body: ExprWithoutBlockBody::LitExpr(LitExpr::Int(
                                        LitInt::Dec(LitDec(TokenStream::from((10, "4")))),
                                        None
                                    ))
                                }),
                                Comma(None, TokenStream::from((11, ",")), None)
                            )
                        ],
                        tail: None
                    }
                }))
            }))
        );
        assert_eq!(
            TokenStream::from("[0; 128]").parse::<Expr<_>>(),
            Ok(Expr::WithoutBlock(ExprWithoutBlock {
                attrs: vec![],
                body: ExprWithoutBlockBody::ArrayExpr(ArrayExpr::Repeat {
                    delimiter_start: BracketStart(None, TokenStream::from((0, "[")), None),
                    value: Box::new(Expr::WithoutBlock(ExprWithoutBlock {
                        attrs: vec![],
                        body: ExprWithoutBlockBody::LitExpr(LitExpr::Int(
                            LitInt::Dec(LitDec(TokenStream::from((1, "0")))),
                            None
                        ))
                    })),
                    semi: Semi(
                        None,
                        TokenStream::from((2, ";")),
                        Some(S(TokenStream::from((3, " "))))
                    ),
                    length: Box::new(Expr::WithoutBlock(ExprWithoutBlock {
                        attrs: vec![],
                        body: ExprWithoutBlockBody::LitExpr(LitExpr::Int(
                            LitInt::Dec(LitDec(TokenStream::from((4, "128")))),
                            None
                        ))
                    })),
                    delimiter_end: BracketEnd(None, TokenStream::from((7, "]")), None)
                })
            }))
        );
        assert_eq!(
            TokenStream::from("[[1, 0, 0], [0, 1, 0], [0, 0, 1]]").parse::<Expr<_>>(),
            Ok(Expr::WithoutBlock(ExprWithoutBlock {
                attrs: vec![],
                body: ExprWithoutBlockBody::ArrayExpr(ArrayExpr::List(Delimiter {
                    start: BracketStart(None, TokenStream::from((0, "[")), None),
                    end: BracketEnd(None, TokenStream::from((32, "]")), None),
                    body: Punctuated {
                        pairs: vec![
                            (
                                Expr::WithoutBlock(ExprWithoutBlock {
                                    attrs: vec![],
                                    body: ExprWithoutBlockBody::ArrayExpr(ArrayExpr::List(
                                        Delimiter {
                                            start: BracketStart(
                                                None,
                                                TokenStream::from((1, "[")),
                                                None
                                            ),
                                            end: BracketEnd(
                                                None,
                                                TokenStream::from((9, "]")),
                                                None
                                            ),
                                            body: Punctuated {
                                                pairs: vec![
                                                    (
                                                        Expr::WithoutBlock(ExprWithoutBlock {
                                                            attrs: vec![],
                                                            body: ExprWithoutBlockBody::LitExpr(
                                                                LitExpr::Int(
                                                                    LitInt::Dec(LitDec(
                                                                        TokenStream::from((2, "1"))
                                                                    )),
                                                                    None
                                                                )
                                                            )
                                                        }),
                                                        Comma(
                                                            None,
                                                            TokenStream::from((3, ",")),
                                                            Some(S(TokenStream::from((4, " "))))
                                                        )
                                                    ),
                                                    (
                                                        Expr::WithoutBlock(ExprWithoutBlock {
                                                            attrs: vec![],
                                                            body: ExprWithoutBlockBody::LitExpr(
                                                                LitExpr::Int(
                                                                    LitInt::Dec(LitDec(
                                                                        TokenStream::from((5, "0"))
                                                                    )),
                                                                    None
                                                                )
                                                            )
                                                        }),
                                                        Comma(
                                                            None,
                                                            TokenStream::from((6, ",")),
                                                            Some(S(TokenStream::from((7, " "))))
                                                        )
                                                    )
                                                ],
                                                tail: Some(Box::new(Expr::WithoutBlock(
                                                    ExprWithoutBlock {
                                                        attrs: vec![],
                                                        body: ExprWithoutBlockBody::LitExpr(
                                                            LitExpr::Int(
                                                                LitInt::Dec(LitDec(
                                                                    TokenStream::from((8, "0"))
                                                                )),
                                                                None
                                                            )
                                                        )
                                                    }
                                                )))
                                            }
                                        }
                                    ))
                                }),
                                Comma(
                                    None,
                                    TokenStream::from((10, ",")),
                                    Some(S(TokenStream::from((11, " "))))
                                )
                            ),
                            (
                                Expr::WithoutBlock(ExprWithoutBlock {
                                    attrs: vec![],
                                    body: ExprWithoutBlockBody::ArrayExpr(ArrayExpr::List(
                                        Delimiter {
                                            start: BracketStart(
                                                None,
                                                TokenStream::from((12, "[")),
                                                None
                                            ),
                                            end: BracketEnd(
                                                None,
                                                TokenStream::from((20, "]")),
                                                None
                                            ),
                                            body: Punctuated {
                                                pairs: vec![
                                                    (
                                                        Expr::WithoutBlock(ExprWithoutBlock {
                                                            attrs: vec![],
                                                            body: ExprWithoutBlockBody::LitExpr(
                                                                LitExpr::Int(
                                                                    LitInt::Dec(LitDec(
                                                                        TokenStream::from((
                                                                            13, "0"
                                                                        ))
                                                                    )),
                                                                    None
                                                                )
                                                            )
                                                        }),
                                                        Comma(
                                                            None,
                                                            TokenStream::from((14, ",")),
                                                            Some(S(TokenStream::from((15, " "))))
                                                        )
                                                    ),
                                                    (
                                                        Expr::WithoutBlock(ExprWithoutBlock {
                                                            attrs: vec![],
                                                            body: ExprWithoutBlockBody::LitExpr(
                                                                LitExpr::Int(
                                                                    LitInt::Dec(LitDec(
                                                                        TokenStream::from((
                                                                            16, "1"
                                                                        ))
                                                                    )),
                                                                    None
                                                                )
                                                            )
                                                        }),
                                                        Comma(
                                                            None,
                                                            TokenStream::from((17, ",")),
                                                            Some(S(TokenStream::from((18, " "))))
                                                        )
                                                    )
                                                ],
                                                tail: Some(Box::new(Expr::WithoutBlock(
                                                    ExprWithoutBlock {
                                                        attrs: vec![],
                                                        body: ExprWithoutBlockBody::LitExpr(
                                                            LitExpr::Int(
                                                                LitInt::Dec(LitDec(
                                                                    TokenStream::from((19, "0"))
                                                                )),
                                                                None
                                                            )
                                                        )
                                                    }
                                                )))
                                            }
                                        }
                                    ))
                                }),
                                Comma(
                                    None,
                                    TokenStream::from((21, ",")),
                                    Some(S(TokenStream::from((22, " "))))
                                )
                            )
                        ],
                        tail: Some(Box::new(Expr::WithoutBlock(ExprWithoutBlock {
                            attrs: vec![],
                            body: ExprWithoutBlockBody::ArrayExpr(ArrayExpr::List(Delimiter {
                                start: BracketStart(None, TokenStream::from((23, "[")), None),
                                end: BracketEnd(None, TokenStream::from((31, "]")), None),
                                body: Punctuated {
                                    pairs: vec![
                                        (
                                            Expr::WithoutBlock(ExprWithoutBlock {
                                                attrs: vec![],
                                                body: ExprWithoutBlockBody::LitExpr(LitExpr::Int(
                                                    LitInt::Dec(LitDec(TokenStream::from((
                                                        24, "0"
                                                    )))),
                                                    None
                                                ))
                                            }),
                                            Comma(
                                                None,
                                                TokenStream::from((25, ",")),
                                                Some(S(TokenStream::from((26, " "))))
                                            )
                                        ),
                                        (
                                            Expr::WithoutBlock(ExprWithoutBlock {
                                                attrs: vec![],
                                                body: ExprWithoutBlockBody::LitExpr(LitExpr::Int(
                                                    LitInt::Dec(LitDec(TokenStream::from((
                                                        27, "0"
                                                    )))),
                                                    None
                                                ))
                                            }),
                                            Comma(
                                                None,
                                                TokenStream::from((28, ",")),
                                                Some(S(TokenStream::from((29, " "))))
                                            )
                                        )
                                    ],
                                    tail: Some(Box::new(Expr::WithoutBlock(ExprWithoutBlock {
                                        attrs: vec![],
                                        body: ExprWithoutBlockBody::LitExpr(LitExpr::Int(
                                            LitInt::Dec(LitDec(TokenStream::from((30, "1")))),
                                            None
                                        ))
                                    })))
                                }
                            }))
                        })))
                    }
                }))
            }))
        );

        assert_eq!(
            TokenStream::from("[EMPTY; 2]").parse::<Expr<_>>(),
            Ok(Expr::WithoutBlock(ExprWithoutBlock {
                attrs: vec![],
                body: ExprWithoutBlockBody::ArrayExpr(ArrayExpr::Repeat {
                    delimiter_start: BracketStart(None, TokenStream::from((0, "[")), None),
                    value: Box::new(Expr::WithoutBlock(ExprWithoutBlock {
                        attrs: vec![],
                        body: ExprWithoutBlockBody::PathExpr(PathExpr::Path(PathInExpr {
                            leading_sep: None,
                            first: SimplePathSegment::Ident(Ident::NonKeywordIdent(
                                NonKeywordIdent(TokenStream::from((1, "EMPTY")))
                            )),
                            rest: vec![]
                        }))
                    })),
                    semi: Semi(
                        None,
                        TokenStream::from((6, ";")),
                        Some(S(TokenStream::from((7, " "))))
                    ),
                    length: Box::new(Expr::WithoutBlock(ExprWithoutBlock {
                        attrs: vec![],
                        body: ExprWithoutBlockBody::LitExpr(LitExpr::Int(
                            LitInt::Dec(LitDec(TokenStream::from((8, "2")))),
                            None
                        ))
                    })),
                    delimiter_end: BracketEnd(None, TokenStream::from((9, "]")), None)
                })
            }))
        );
    }
}
