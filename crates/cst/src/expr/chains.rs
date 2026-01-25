use parserc::{
    ToSpan,
    syntax::{Punctuated, Syntax},
};

use crate::{
    expr::Expr,
    input::CSTInput,
    lexical::{
        delimiter::{Bracket, Paren},
        ident::Ident,
        keywords::strict::Await,
        lit::TupleIndex,
        punct::{Comma, Dot},
    },
    names::paths::PathExprSegment,
};

/// A block expression, or block, is a control flow expression and anonymous namespace scope
/// for items and variable declarations.
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions/block-expr.html
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct AwaitExpr<I>
where
    I: CSTInput,
{
    /// Target expression
    pub expr: Box<Expr<I>>,
    /// period `.`
    pub period: Dot<I>,
    /// `await` keyword.
    pub predicate: Await<I>,
}

impl<I> ToSpan for AwaitExpr<I>
where
    I: CSTInput,
{
    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.expr.to_span() + self.predicate.to_span()
    }
}

/// A tuple indexing expression accesses fields of tuples and tuple structs.
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions/tuple-expr.html#grammar-TupleIndexingExpression
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TupleIndexExpr<I>
where
    I: CSTInput,
{
    /// Target expression
    pub expr: Box<Expr<I>>,
    /// period `.`
    pub period: Dot<I>,
    /// index token.
    pub predicate: TupleIndex<I>,
}

impl<I> ToSpan for TupleIndexExpr<I>
where
    I: CSTInput,
{
    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.expr.to_span() + self.predicate.to_span()
    }
}

/// A parenthesized comma-separated list of expression, called the argument operands.
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions/method-call-expr.html#grammar-MethodCallExpression
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct CallParams<I>(Paren<I, Punctuated<Expr<I>, Comma<I>>>)
where
    I: CSTInput;

/// A method call consists of an expression (the receiver) followed by a single dot,
/// an expression path segment, and a parenthesized expression-list.
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions/field-expr.html#grammar-FieldExpression
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct FieldExpr<I>
where
    I: CSTInput,
{
    /// Target expression
    pub expr: Box<Expr<I>>,
    /// period `.`
    pub period: Dot<I>,
    /// field name
    pub ident: Ident<I>,
}

impl<I> ToSpan for FieldExpr<I>
where
    I: CSTInput,
{
    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.expr.to_span() + self.ident.to_span()
    }
}

/// A method call consists of an expression (the receiver) followed by a single dot,
/// an expression path segment, and a parenthesized expression-list.
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions/method-call-expr.html#grammar-MethodCallExpression
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct CallExpr<I>
where
    I: CSTInput,
{
    /// Target expression
    pub expr: Box<Expr<I>>,
    /// parenthesized expression-list
    pub call_params: CallParams<I>,
}

impl<I> ToSpan for CallExpr<I>
where
    I: CSTInput,
{
    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.expr.to_span() + self.call_params.to_span()
    }
}

/// A method call consists of an expression (the receiver) followed by a single dot,
/// an expression path segment, and a parenthesized expression-list.
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions/method-call-expr.html#grammar-MethodCallExpression
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct MethodCallExpr<I>
where
    I: CSTInput,
{
    /// Target expression
    pub expr: Box<Expr<I>>,
    /// period `.`
    pub period: Dot<I>,
    /// an expression path segment.
    pub path_expr_segment: PathExprSegment<I>,
    /// parenthesized expression-list
    pub call_params: CallParams<I>,
}

impl<I> ToSpan for MethodCallExpr<I>
where
    I: CSTInput,
{
    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.expr.to_span() + self.call_params.to_span()
    }
}

/// A method call consists of an expression (the receiver) followed by a single dot,
/// an expression path segment, and a parenthesized expression-list.
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions/method-call-expr.html#grammar-MethodCallExpression
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct IndexExpr<I>
where
    I: CSTInput,
{
    /// Target expression
    pub expr: Box<Expr<I>>,
    /// a square-bracket-enclosed expression of type usize
    pub index: Bracket<I, Box<Expr<I>>>,
}

impl<I> ToSpan for IndexExpr<I>
where
    I: CSTInput,
{
    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.expr.to_span() + self.index.to_span()
    }
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, Punctuated, SyntaxInput};

    use crate::{
        expr::{ArrayExpr, Expr, ExprWithoutBlock, LitExpr, PathExpr},
        input::TokenStream,
        lexical::{
            ident::NonKeywordIdent,
            lit::{LitDec, LitInt},
            punct::{BracketEnd, BracketStart, Comma, Gt, Lt, ParenEnd, ParenStart, PathSep},
        },
        names::paths::{
            GenericArg, GenericArgs, PathIdentSegment, PathInExpr, TypePath, TypePathSegment,
        },
        types::{Type, TypeNoBounds},
    };

    use super::*;

    #[test]
    fn test_await() {
        assert_eq!(
            TokenStream::from("[[1],[2,3],].await").parse::<Expr<_>>(),
            Ok(Expr::WithoutBlock(ExprWithoutBlock::AwaitExpr(AwaitExpr {
                expr: Box::new(Expr::WithoutBlock(ExprWithoutBlock::ArrayExpr(
                    vec![],
                    ArrayExpr::List(Delimiter {
                        start: BracketStart(None, TokenStream::from((0, "[")), None),
                        end: BracketEnd(None, TokenStream::from((11, "]")), None),
                        body: Punctuated {
                            pairs: vec![
                                (
                                    Expr::WithoutBlock(ExprWithoutBlock::ArrayExpr(
                                        vec![],
                                        ArrayExpr::List(Delimiter {
                                            start: BracketStart(
                                                None,
                                                TokenStream::from((1, "[")),
                                                None
                                            ),
                                            end: BracketEnd(
                                                None,
                                                TokenStream::from((3, "]")),
                                                None
                                            ),
                                            body: Punctuated {
                                                pairs: vec![],
                                                tail: Some(Box::new(Expr::WithoutBlock(
                                                    ExprWithoutBlock::LitExpr(
                                                        vec![],
                                                        LitExpr::Int(
                                                            LitInt::Dec(LitDec(TokenStream::from(
                                                                (2, "1")
                                                            ))),
                                                            None
                                                        )
                                                    )
                                                )))
                                            }
                                        })
                                    )),
                                    Comma(None, TokenStream::from((4, ",")), None)
                                ),
                                (
                                    Expr::WithoutBlock(ExprWithoutBlock::ArrayExpr(
                                        vec![],
                                        ArrayExpr::List(Delimiter {
                                            start: BracketStart(
                                                None,
                                                TokenStream::from((5, "[")),
                                                None
                                            ),
                                            end: BracketEnd(
                                                None,
                                                TokenStream::from((9, "]")),
                                                None
                                            ),
                                            body: Punctuated {
                                                pairs: vec![(
                                                    Expr::WithoutBlock(ExprWithoutBlock::LitExpr(
                                                        vec![],
                                                        LitExpr::Int(
                                                            LitInt::Dec(LitDec(TokenStream::from(
                                                                (6, "2")
                                                            ))),
                                                            None
                                                        )
                                                    )),
                                                    Comma(None, TokenStream::from((7, ",")), None)
                                                )],
                                                tail: Some(Box::new(Expr::WithoutBlock(
                                                    ExprWithoutBlock::LitExpr(
                                                        vec![],
                                                        LitExpr::Int(
                                                            LitInt::Dec(LitDec(TokenStream::from(
                                                                (8, "3")
                                                            ))),
                                                            None
                                                        )
                                                    )
                                                )))
                                            }
                                        })
                                    )),
                                    Comma(None, TokenStream::from((10, ",")), None)
                                )
                            ],
                            tail: None
                        }
                    })
                ))),
                period: Dot(None, TokenStream::from((12, ".")), None),
                predicate: Await(TokenStream::from((13, "await")), None)
            })))
        );
    }

    #[test]
    fn test_chain_expr() {
        assert_eq!(
            TokenStream::from("a[0].call::<i32>(1,3).1()").parse::<Expr<_>>(),
            Ok(Expr::WithoutBlock(ExprWithoutBlock::CallExpr(CallExpr {
                expr: Box::new(Expr::WithoutBlock(ExprWithoutBlock::TupleIndexExpr(
                    TupleIndexExpr {
                        expr: Box::new(Expr::WithoutBlock(ExprWithoutBlock::MethodCallExpr(
                            MethodCallExpr {
                                expr: Box::new(Expr::WithoutBlock(ExprWithoutBlock::IndexExpr(
                                    IndexExpr {
                                        expr: Box::new(Expr::WithoutBlock(
                                            ExprWithoutBlock::PathExpr(
                                                vec![],
                                                PathExpr::Path(PathInExpr {
                                                    leading_sep: None,
                                                    first: PathExprSegment {
                                                        ident: PathIdentSegment::Ident(
                                                            Ident::NonKeywordIdent(
                                                                NonKeywordIdent(TokenStream::from(
                                                                    (0, "a")
                                                                ))
                                                            )
                                                        ),
                                                        generic_args: None
                                                    },
                                                    rest: vec![]
                                                })
                                            )
                                        )),
                                        index: Delimiter {
                                            start: BracketStart(
                                                None,
                                                TokenStream::from((1, "[")),
                                                None
                                            ),
                                            end: BracketEnd(
                                                None,
                                                TokenStream::from((3, "]")),
                                                None
                                            ),
                                            body: Box::new(Expr::WithoutBlock(
                                                ExprWithoutBlock::LitExpr(
                                                    vec![],
                                                    LitExpr::Int(
                                                        LitInt::Dec(LitDec(TokenStream::from((
                                                            2, "0"
                                                        )))),
                                                        None
                                                    )
                                                )
                                            ))
                                        }
                                    }
                                ))),
                                period: Dot(None, TokenStream::from((4, ".")), None),
                                path_expr_segment: PathExprSegment {
                                    ident: PathIdentSegment::Ident(Ident::NonKeywordIdent(
                                        NonKeywordIdent(TokenStream::from((5, "call")))
                                    )),
                                    generic_args: Some((
                                        PathSep(None, TokenStream::from((9, "::")), None),
                                        GenericArgs {
                                            delimiter_start: Lt(
                                                None,
                                                TokenStream::from((11, "<")),
                                                None
                                            ),
                                            args: Punctuated {
                                                pairs: vec![],
                                                tail: Some(Box::new(GenericArg::Type(
                                                    Type::NoBounds(TypeNoBounds::TypePath(
                                                        Box::new(TypePath {
                                                            leading_path_sep: None,
                                                            first: TypePathSegment {
                                                                ident: PathIdentSegment::Ident(
                                                                    Ident::NonKeywordIdent(
                                                                        NonKeywordIdent(
                                                                            TokenStream::from((
                                                                                12, "i32"
                                                                            ))
                                                                        )
                                                                    )
                                                                ),
                                                                args: None
                                                            },
                                                            rest: vec![]
                                                        })
                                                    ))
                                                )))
                                            },
                                            delimiter_end: Gt(
                                                None,
                                                TokenStream::from((15, ">")),
                                                None
                                            )
                                        }
                                    ))
                                },
                                call_params: CallParams(Delimiter {
                                    start: ParenStart(None, TokenStream::from((16, "(")), None),
                                    end: ParenEnd(None, TokenStream::from((20, ")")), None),
                                    body: Punctuated {
                                        pairs: vec![(
                                            Expr::WithoutBlock(ExprWithoutBlock::LitExpr(
                                                vec![],
                                                LitExpr::Int(
                                                    LitInt::Dec(LitDec(TokenStream::from((
                                                        17, "1"
                                                    )))),
                                                    None
                                                )
                                            )),
                                            Comma(None, TokenStream::from((18, ",")), None)
                                        )],
                                        tail: Some(Box::new(Expr::WithoutBlock(
                                            ExprWithoutBlock::LitExpr(
                                                vec![],
                                                LitExpr::Int(
                                                    LitInt::Dec(LitDec(TokenStream::from((
                                                        19, "3"
                                                    )))),
                                                    None
                                                )
                                            )
                                        )))
                                    }
                                })
                            }
                        ))),
                        period: Dot(None, TokenStream::from((21, ".")), None),
                        predicate: TupleIndex::Dec(LitDec(TokenStream::from((22, "1"))))
                    }
                ))),
                call_params: CallParams(Delimiter {
                    start: ParenStart(None, TokenStream::from((23, "(")), None),
                    end: ParenEnd(None, TokenStream::from((24, ")")), None),
                    body: Punctuated {
                        pairs: vec![],
                        tail: None
                    }
                })
            })))
        );
    }
}
