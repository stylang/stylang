use parserc::{ControlFlow, Parser, syntax::Syntax};

use crate::{
    errors::{CSTError, SemanticsKind, SyntaxKind},
    expr::{ExprConst, ExprInfer, ExprLit, ExprPath, RangeLimits},
    input::CSTInput,
    pat::{PatReference, PatSlice, PatStruct, PatTuple, PatTupleStruct, PatType, ident::PatIdent},
    punct::{DotDot, Or},
};

#[inline]
fn parse_range_operand<I>(input: &mut I) -> Result<PatNoTopAlt<I>, CSTError>
where
    I: CSTInput,
{
    ExprLit::into_parser()
        .map(PatNoTopAlt::Lit)
        .or(ExprPath::into_parser().map(PatNoTopAlt::Path))
        .parse(input)
        .map_err(SyntaxKind::Pat.map_unhandle())
}

#[inline]
fn parse_range<I>(input: &mut I) -> Result<PatNoTopAlt<I>, CSTError>
where
    I: CSTInput,
{
    let start = parse_range_operand.ok().parse(input)?;

    let Some(limits) = RangeLimits::into_parser().ok().parse(input)? else {
        return start.ok_or(CSTError::Syntax(
            SyntaxKind::Pat,
            ControlFlow::Recovable,
            input.to_span_at(1),
        ));
    };

    let end = parse_range_operand.ok().parse(input)?;

    let limits = match limits {
        RangeLimits::Closed(dot_dot_eq) => {
            if start.is_none() || end.is_none() {
                return Err(CSTError::Semantics(
                    SemanticsKind::Range,
                    dot_dot_eq.to_span(),
                ));
            }

            RangeLimits::Closed(dot_dot_eq)
        }
        RangeLimits::HalfOpen(dot_dot) => {
            if start.is_none() && end.is_none() {
                return Ok(PatNoTopAlt::Rest(dot_dot));
            }

            RangeLimits::HalfOpen(dot_dot)
        }
    };

    Ok(PatNoTopAlt::Range {
        start: start.map(|pat| Box::new(Pat::from(pat))),
        limits,
        end: end.map(|pat| Box::new(Pat::from(pat))),
    })
}

/// A pattern in a local binding, function signature, match expression, or various other places.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum PatNoTopAlt<I>
where
    I: CSTInput,
{
    Range {
        start: Option<Box<Pat<I>>>,
        limits: RangeLimits<I>,
        end: Option<Box<Pat<I>>>,
    },
    Lit(ExprLit<I>),
    Const(ExprConst<I>),
    Ref(PatReference<I>),
    Type(PatType<I>),
    Ident(PatIdent<I>),
    Wild(ExprInfer<I>),
    Path(ExprPath<I>),
    Rest(DotDot<I>),
    Slice(PatSlice<I>),
    Tuple(PatTuple<I>),
    TupleStruct(PatTupleStruct<I>),
    Struct(PatStruct<I>),
}

impl<I> Syntax<I> for PatNoTopAlt<I>
where
    I: CSTInput,
{
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        PatType::into_parser()
            .map(PatNoTopAlt::Type)
            .or(PatReference::into_parser().map(PatNoTopAlt::Ref))
            .or(ExprInfer::into_parser().map(PatNoTopAlt::Wild))
            .or(PatSlice::into_parser().map(PatNoTopAlt::Slice))
            .or(PatTuple::into_parser().map(PatNoTopAlt::Tuple))
            .or(PatStruct::into_parser().map(PatNoTopAlt::Struct))
            .or(PatTupleStruct::into_parser().map(PatNoTopAlt::TupleStruct))
            .or(PatIdent::into_parser().map(PatNoTopAlt::Ident))
            .or(parse_range)
            .parse(input)
    }

    fn to_span(&self) -> parserc::Span {
        match self {
            PatNoTopAlt::Range { start, limits, end } => {
                start.to_span() + limits.to_span() + end.to_span()
            }
            PatNoTopAlt::Lit(pat) => pat.to_span(),
            PatNoTopAlt::Const(pat) => pat.to_span(),
            PatNoTopAlt::Ref(pat) => pat.to_span(),
            PatNoTopAlt::Type(pat) => pat.to_span(),
            PatNoTopAlt::Ident(pat) => pat.to_span(),
            PatNoTopAlt::Wild(pat) => pat.to_span(),
            PatNoTopAlt::Path(pat) => pat.to_span(),
            PatNoTopAlt::Rest(pat) => pat.to_span(),
            PatNoTopAlt::Slice(pat) => pat.to_span(),
            PatNoTopAlt::Tuple(pat) => pat.to_span(),
            PatNoTopAlt::TupleStruct(pat) => pat.to_span(),
            PatNoTopAlt::Struct(pat) => pat.to_span(),
        }
    }
}

impl<I> From<PatNoTopAlt<I>> for Pat<I>
where
    I: CSTInput,
{
    fn from(value: PatNoTopAlt<I>) -> Self {
        match value {
            PatNoTopAlt::Range { start, limits, end } => Pat::Range { start, limits, end },
            PatNoTopAlt::Lit(pat) => Pat::Lit(pat),
            PatNoTopAlt::Const(pat) => Pat::Const(pat),
            PatNoTopAlt::Ref(pat) => Pat::Ref(pat),
            PatNoTopAlt::Type(pat) => Pat::Type(pat),
            PatNoTopAlt::Ident(pat) => Pat::Ident(pat),
            PatNoTopAlt::Wild(pat) => Pat::Wild(pat),
            PatNoTopAlt::Path(pat) => Pat::Path(pat),
            PatNoTopAlt::Rest(pat) => Pat::Rest(pat),
            PatNoTopAlt::Slice(pat) => Pat::Slice(pat),
            PatNoTopAlt::Tuple(pat) => Pat::Tuple(pat),
            PatNoTopAlt::TupleStruct(pat) => Pat::TupleStruct(pat),
            PatNoTopAlt::Struct(pat) => Pat::Struct(pat),
        }
    }
}

/// A pattern in a local binding, function signature, match expression, or various other places.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Pat<I>
where
    I: CSTInput,
{
    Range {
        start: Option<Box<Pat<I>>>,
        limits: RangeLimits<I>,
        end: Option<Box<Pat<I>>>,
    },
    Lit(ExprLit<I>),
    Const(ExprConst<I>),
    Ref(PatReference<I>),
    Type(PatType<I>),
    Ident(PatIdent<I>),
    Wild(ExprInfer<I>),
    Path(ExprPath<I>),
    Rest(DotDot<I>),
    Slice(PatSlice<I>),
    Tuple(PatTuple<I>),
    TupleStruct(PatTupleStruct<I>),
    Struct(PatStruct<I>),
    Or(Box<Pat<I>>, Or<I>, Box<Pat<I>>),
}

impl<I> Syntax<I> for Pat<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let mut left = PatNoTopAlt::into_parser().map(Pat::from).parse(input)?;

        while let Some(or) = Or::into_parser().ok().parse(input)? {
            let right = PatNoTopAlt::into_parser()
                .map(Pat::from)
                .parse(input)
                .map_err(SyntaxKind::PatOrRightOperand.map_non_fatal())?;
            left = Pat::Or(Box::new(left), or, Box::new(right));
        }

        Ok(left)
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        match self {
            Pat::Type(expr) => expr.to_span(),
            Pat::Ident(expr) => expr.to_span(),
            Pat::Ref(pat) => pat.to_span(),
            Pat::Lit(pat) => pat.to_span(),
            Pat::Const(pat) => pat.to_span(),
            Pat::Wild(pat) => pat.to_span(),
            Pat::Path(pat) => pat.to_span(),
            Pat::Rest(pat) => pat.to_span(),
            Pat::Slice(pat) => pat.to_span(),
            Pat::Tuple(pat) => pat.to_span(),
            Pat::TupleStruct(pat) => pat.to_span(),
            Pat::Struct(pat) => pat.to_span(),
            Pat::Or(left, _, right) => left.to_span() + right.to_span(),
            Pat::Range {
                start: left,
                limits,
                end: right,
            } => left.to_span() + limits.to_span() + right.to_span(),
        }
    }
}

#[cfg(test)]
mod tests {
    use parserc::syntax::SyntaxInput;

    use crate::{
        expr::{Digits, ExprLit, LitNumber, RangeLimits},
        input::TokenStream,
        misc::{Ident, S},
        pat::{Pat, PatType, ident::PatIdent},
        punct::{At, Colon, DotDot, DotDotEq, Or},
        ty::Type,
    };

    #[test]
    fn test_pat_type() {
        assert_eq!(
            TokenStream::from("a: u8").parse(),
            Ok(Pat::Type(PatType {
                pat: Box::new(Pat::Ident(PatIdent {
                    by_ref: None,
                    mutability: None,
                    ident: Ident(TokenStream::from("a")),
                    subpat: None
                })),
                colon: Colon(
                    None,
                    TokenStream::from((1, ":")),
                    Some(S(TokenStream::from((2, " "))))
                ),
                ty: Box::new(Type::U8(TokenStream::from((3, "u8"))))
            }))
        );
    }

    #[test]
    fn test_pat_or() {
        assert_eq!(
            TokenStream::from("1..2 | 3..=9 | a @ ..").parse::<Pat<_>>(),
            Ok(Pat::Or(
                Box::new(Pat::Or(
                    Box::new(Pat::Range {
                        start: Some(Box::new(Pat::Lit(ExprLit::Number(LitNumber {
                            sign: None,
                            trunc: Some(Digits {
                                input: TokenStream::from((0, "1")),
                                value: 1
                            }),
                            fract: None,
                            exp: None
                        })))),
                        limits: RangeLimits::HalfOpen(DotDot(
                            None,
                            TokenStream::from((1, "..")),
                            None
                        )),
                        end: Some(Box::new(Pat::Lit(ExprLit::Number(LitNumber {
                            sign: None,
                            trunc: Some(Digits {
                                input: TokenStream::from((3, "2")),
                                value: 2
                            }),
                            fract: None,
                            exp: None
                        }))))
                    }),
                    Or(
                        Some(S(TokenStream::from((4, " ")))),
                        TokenStream::from((5, "|")),
                        Some(S(TokenStream::from((6, " "))))
                    ),
                    Box::new(Pat::Range {
                        start: Some(Box::new(Pat::Lit(ExprLit::Number(LitNumber {
                            sign: None,
                            trunc: Some(Digits {
                                input: TokenStream::from((7, "3")),
                                value: 3
                            }),
                            fract: None,
                            exp: None
                        })))),
                        limits: RangeLimits::Closed(DotDotEq(
                            None,
                            TokenStream::from((8, "..=")),
                            None
                        )),
                        end: Some(Box::new(Pat::Lit(ExprLit::Number(LitNumber {
                            sign: None,
                            trunc: Some(Digits {
                                input: TokenStream::from((11, "9")),
                                value: 9
                            }),
                            fract: None,
                            exp: None
                        }))))
                    })
                )),
                Or(
                    Some(S(TokenStream::from((12, " ")))),
                    TokenStream::from((13, "|")),
                    Some(S(TokenStream::from((14, " "))))
                ),
                Box::new(Pat::Ident(PatIdent {
                    by_ref: None,
                    mutability: None,
                    ident: Ident(TokenStream::from((15, "a"))),
                    subpat: Some((
                        At(
                            Some(S(TokenStream::from((16, " ")))),
                            TokenStream::from((17, "@")),
                            Some(S(TokenStream::from((18, " "))))
                        ),
                        Box::new(Pat::Rest(DotDot(None, TokenStream::from((19, "..")), None)))
                    ))
                }))
            ))
        );
    }
}
