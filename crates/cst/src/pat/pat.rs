use parserc::{ControlFlow, Parser, syntax::Syntax};

use crate::{
    errors::{CSTError, SemanticsKind, SyntaxKind},
    expr::{Expr, ExprConst, ExprInfer, ExprLit, ExprPath, ExprRange, parse_range},
    input::CSTInput,
    pat::{PatReference, PatSlice, PatStruct, PatTuple, PatTupleStruct, PatType, ident::PatIdent},
    punct::{DotDot, Or},
};

#[inline]
fn parse_expr<I>(input: &mut I) -> Result<Pat<I>, CSTError>
where
    I: CSTInput,
{
    match parse_range(input).map_err(|err| match err {
        CSTError::Syntax(SyntaxKind::Expr, control_flow, span) => {
            CSTError::Syntax(SyntaxKind::Pat, control_flow, span)
        }
        err => err,
    })? {
        Expr::Range(range) => Ok(Pat::Range(range)),
        Expr::Lit(lit) => Ok(Pat::Lit(lit)),
        Expr::Const(expr) => Ok(Pat::Const(expr)),
        Expr::Path(expr) => Ok(Pat::Path(expr)),
        expr => Err(CSTError::Syntax(
            SyntaxKind::Pat,
            ControlFlow::Recovable,
            expr.to_span(),
        )),
    }
}

/// A pattern in a local binding, function signature, match expression, or various other places.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Pat<I>
where
    I: CSTInput,
{
    Range(ExprRange<I>),
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
        let mut cloned = input.clone();

        match parse_expr(input) {
            Ok(pat) => Ok(pat),
            Err(CSTError::Semantics(SemanticsKind::RangeLimits, _)) => DotDot::into_parser()
                .map(Self::Rest)
                .parse(&mut cloned)
                .map_err(SyntaxKind::Pat.map_unhandle())
                .inspect(|_| *input = cloned)
                .inspect_err(|err| {
                    println!("{}", err);
                }),
            Err(err) => Err(err),
        }
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        match self {
            Pat::Range(expr) => expr.start.to_span() + expr.limits.to_span() + expr.end.to_span(),
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
