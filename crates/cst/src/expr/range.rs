use parserc::{ControlFlow, Parser, syntax::Syntax};

use crate::{
    errors::{CSTError, SemanticsKind, SyntaxKind},
    expr::{Expr, parse_binary},
    input::CSTInput,
    punct::{DotDot, DotDotEq},
};

/// Limit types of a range, inclusive or exclusive.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::RangeLimits.map())]
pub enum RangeLimits<I>
where
    I: CSTInput,
{
    Closed(DotDotEq<I>),
    HalfOpen(DotDot<I>),
}

/// A range expression: 1..2, 1.., ..2, 1..=2, ..=2.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprRange<I>
where
    I: CSTInput,
{
    /// range start expr
    pub start: Option<Box<Expr<I>>>,
    /// range limits punct `..` or `..=`
    pub limits: RangeLimits<I>,
    /// range end expr.
    pub end: Option<Box<Expr<I>>>,
}

#[inline]
pub(crate) fn parse_range<I>(input: &mut I) -> Result<Expr<I>, CSTError>
where
    I: CSTInput,
{
    let start = parse_binary.boxed().ok().parse(input)?;

    let Some(limits) = RangeLimits::into_parser().ok().parse(input)? else {
        return start.map(|expr| *expr).ok_or(CSTError::Syntax(
            SyntaxKind::Expr,
            ControlFlow::Recovable,
            input.to_span_at(1),
        ));
    };

    let end = parse_binary.boxed().ok().parse(input)?;

    if let RangeLimits::Closed(limits) = &limits {
        if start.is_none() || end.is_none() {
            return Err(CSTError::Semantics(SemanticsKind::Range, limits.to_span()));
        }
    } else {
        if start.is_none() && end.is_none() {
            return Err(CSTError::Semantics(
                SemanticsKind::RangeLimits,
                limits.to_span(),
            ));
        }
    }

    Ok(Expr::Range(ExprRange { start, limits, end }))
}

#[cfg(test)]
mod tests {
    use parserc::{Span, syntax::SyntaxInput};

    use crate::{
        errors::{CSTError, SemanticsKind},
        expr::{BinOp, Digits, Expr, ExprBinary, ExprLit, ExprRange, LitNumber, RangeLimits},
        input::TokenStream,
        punct::{Caret, DotDot, DotDotEq, Plus},
    };

    #[test]
    fn range_semantic() {
        assert_eq!(
            TokenStream::from("..=1").parse::<Expr<_>>(),
            Err(CSTError::Semantics(SemanticsKind::Range, Span::Range(0..3)))
        );

        assert_eq!(
            TokenStream::from("1..=").parse::<Expr<_>>(),
            Err(CSTError::Semantics(SemanticsKind::Range, Span::Range(1..4)))
        );

        assert_eq!(
            TokenStream::from("..").parse::<Expr<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::RangeLimits,
                Span::Range(0..2)
            ))
        );
    }

    #[test]
    fn test_range() {
        assert_eq!(
            TokenStream::from("1..").parse::<Expr<_>>(),
            Ok(Expr::Range(ExprRange {
                start: Some(Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                    sign: None,
                    trunc: Some(Digits {
                        input: TokenStream::from((0, "1")),
                        value: 1
                    }),
                    fract: None,
                    exp: None
                })))),
                limits: RangeLimits::HalfOpen(DotDot(None, TokenStream::from((1, "..")), None)),
                end: None
            }))
        );
        assert_eq!(
            TokenStream::from("..1^4").parse::<Expr<_>>(),
            Ok(Expr::Range(ExprRange {
                start: None,
                limits: RangeLimits::HalfOpen(DotDot(None, TokenStream::from((0, "..")), None)),
                end: Some(Box::new(Expr::Binary(ExprBinary {
                    left: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                        sign: None,
                        trunc: Some(Digits {
                            input: TokenStream::from((2, "1")),
                            value: 1
                        }),
                        fract: None,
                        exp: None
                    }))),
                    op: BinOp::BitXor(Caret(None, TokenStream::from((3, "^")), None)),
                    right: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                        sign: None,
                        trunc: Some(Digits {
                            input: TokenStream::from((4, "4")),
                            value: 4
                        }),
                        fract: None,
                        exp: None
                    })))
                })))
            }))
        );
        assert_eq!(
            TokenStream::from("1..").parse::<Expr<_>>(),
            Ok(Expr::Range(ExprRange {
                start: Some(Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                    sign: None,
                    trunc: Some(Digits {
                        input: TokenStream::from((0, "1")),
                        value: 1
                    }),
                    fract: None,
                    exp: None
                })))),
                limits: RangeLimits::HalfOpen(DotDot(None, TokenStream::from((1, "..")), None)),
                end: None
            }))
        );
        assert_eq!(
            TokenStream::from("1..1+2").parse::<Expr<_>>(),
            Ok(Expr::Range(ExprRange {
                start: Some(Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                    sign: None,
                    trunc: Some(Digits {
                        input: TokenStream::from((0, "1")),
                        value: 1
                    }),
                    fract: None,
                    exp: None
                })))),
                limits: RangeLimits::HalfOpen(DotDot(None, TokenStream::from((1, "..")), None)),
                end: Some(Box::new(Expr::Binary(ExprBinary {
                    left: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                        sign: None,
                        trunc: Some(Digits {
                            input: TokenStream::from((3, "1")),
                            value: 1
                        }),
                        fract: None,
                        exp: None
                    }))),
                    op: BinOp::Add(Plus(None, TokenStream::from((4, "+")), None)),
                    right: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                        sign: None,
                        trunc: Some(Digits {
                            input: TokenStream::from((5, "2")),
                            value: 2
                        }),
                        fract: None,
                        exp: None
                    })))
                })))
            }))
        );
        assert_eq!(
            TokenStream::from("1..=1+2").parse::<Expr<_>>(),
            Ok(Expr::Range(ExprRange {
                start: Some(Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                    sign: None,
                    trunc: Some(Digits {
                        input: TokenStream::from((0, "1")),
                        value: 1
                    }),
                    fract: None,
                    exp: None
                })))),
                limits: RangeLimits::Closed(DotDotEq(None, TokenStream::from((1, "..=")), None)),
                end: Some(Box::new(Expr::Binary(ExprBinary {
                    left: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                        sign: None,
                        trunc: Some(Digits {
                            input: TokenStream::from((4, "1")),
                            value: 1
                        }),
                        fract: None,
                        exp: None
                    }))),
                    op: BinOp::Add(Plus(None, TokenStream::from((5, "+")), None)),
                    right: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                        sign: None,
                        trunc: Some(Digits {
                            input: TokenStream::from((6, "2")),
                            value: 2
                        }),
                        fract: None,
                        exp: None
                    })))
                })))
            }))
        );
    }
}
