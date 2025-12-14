use parserc::{ControlFlow, Parser, syntax::Syntax};

use crate::{
    errors::{CSTError, SemanticsKind, SyntaxKind},
    expr::{ExprConst, ExprInfer, ExprLit, ExprPath, RangeLimits},
    input::CSTInput,
    pat::{PatReference, PatSlice, PatStruct, PatTuple, PatTupleStruct, PatType, ident::PatIdent},
    punct::{DotDot, Or},
};

#[inline]
fn parse_range_operand<I>(input: &mut I) -> Result<Pat<I>, CSTError>
where
    I: CSTInput,
{
    ExprLit::into_parser()
        .map(Pat::Lit)
        .or(ExprPath::into_parser().map(Pat::Path))
        .parse(input)
        .map_err(SyntaxKind::Pat.map_unhandle())
}

#[inline]
fn parse_range<I>(input: &mut I) -> Result<Pat<I>, CSTError>
where
    I: CSTInput,
{
    let start = parse_range_operand.boxed().ok().parse(input)?;

    let Some(limits) = RangeLimits::into_parser().ok().parse(input)? else {
        return start.map(|expr| *expr).ok_or(CSTError::Syntax(
            SyntaxKind::Pat,
            ControlFlow::Recovable,
            input.to_span_at(1),
        ));
    };

    let end = parse_range_operand.boxed().ok().parse(input)?;

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
                return Ok(Pat::Rest(dot_dot));
            }

            RangeLimits::HalfOpen(dot_dot)
        }
    };

    Ok(Pat::Range { start, limits, end })
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
        if let Some(pat) = PatType::into_parser()
            .map(Pat::Type)
            .or(PatReference::into_parser().map(Pat::Ref))
            .or(ExprInfer::into_parser().map(Pat::Wild))
            .or(PatSlice::into_parser().map(Pat::Slice))
            .or(PatTuple::into_parser().map(Pat::Tuple))
            .or(PatStruct::into_parser().map(Pat::Struct))
            .or(PatTupleStruct::into_parser().map(Pat::TupleStruct))
            .or(PatIdent::into_parser().map(Pat::Ident))
            .ok()
            .parse(input)?
        {
            return Ok(pat);
        }

        parse_range(input)
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
        input::TokenStream,
        misc::{Ident, S},
        pat::{Pat, PatType, ident::PatIdent},
        punct::Colon,
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
}
