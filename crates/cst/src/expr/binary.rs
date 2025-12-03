use parserc::{
    ParseError,
    syntax::{InputSyntaxExt, Syntax},
};

use crate::{
    errors::SyntaxKind,
    expr::{Expr, parse_left_operand},
    input::CSTInput,
    punct::{
        AndAnd, Caret, CaretEq, EqEq, Gt, GtEq, Lt, LtEq, Minus, MinusEq, OrOr, Plus, Rem, RemEq,
        Shl, ShlEq, Shr, ShrEq, Slash, SlashEq, Star, StarEq, StarStar, StarStarEq,
    },
};

/// Binary operator.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::ExprBinaryOp.map())]
pub enum BinOp<I>
where
    I: CSTInput,
{
    AddAssign(Plus<I>),
    Add(Plus<I>),
    SubAssign(MinusEq<I>),
    Sub(Minus<I>),
    MulAssign(StarEq<I>),
    SqrtAssign(StarStarEq<I>),
    Sqrt(StarStar<I>),
    Mul(Star<I>),
    DivAssign(SlashEq<I>),
    Div(Slash<I>),
    RemAssign(RemEq<I>),
    Rem(Rem<I>),
    Eq(EqEq<I>),
    And(AndAnd<I>),
    Or(OrOr<I>),
    BitXorEq(CaretEq<I>),
    BitXor(Caret<I>),
    Le(LtEq<I>),
    ShlAssign(ShlEq<I>),
    Shl(Shl<I>),
    Lt(Lt<I>),
    ShrAssign(ShrEq<I>),
    Shr(Shr<I>),
    Ge(GtEq<I>),
    Gt(Gt<I>),
}

/// A binary operation: `a + b`, `a += b`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprBinary<I>
where
    I: CSTInput,
{
    /// left operand
    pub left: Box<Expr<I>>,
    /// operator punct.
    pub op: BinOp<I>,
    /// right operand.
    pub right: Box<Expr<I>>,
}

impl<I> Syntax<I> for ExprBinary<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let mut left =
            parse_left_operand(input).map_err(SyntaxKind::ExprBinaryLeftOperand.map_unhandle())?;

        let mut op = input.parse()?;

        let mut right = input
            .parse()
            .map_err(|err| SyntaxKind::ExprBinaryRightOperand.map_unhandle()(err).into_fatal())?;

        while let Some(next_call) = input.parse()? {
            left = Box::new(Expr::Binary(Self { left, op, right }));
            op = next_call;
            right = input.parse().map_err(|err| {
                SyntaxKind::ExprBinaryRightOperand.map_unhandle()(err).into_fatal()
            })?;
        }

        Ok(Self { left, op, right })
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.left.to_span() + self.right.to_span()
    }
}

#[cfg(test)]
mod tests {
    use parserc::{
        ControlFlow, Span,
        syntax::{InputSyntaxExt, Punctuated},
    };

    use crate::{
        errors::{CSTError, SyntaxKind},
        expr::{BinOp, Expr, ExprBinary, ExprPath},
        input::TokenStream,
        misc::{Ident, S},
        path::{Path, PathSegment},
        punct::Plus,
    };

    #[test]
    fn test_binary() {
        assert_eq!(
            TokenStream::from("a + b").parse::<Expr<_>>(),
            Ok(Expr::Binary(ExprBinary {
                left: Box::new(Expr::Path(ExprPath {
                    qself: None,
                    path: Path {
                        leading_pathsep: None,
                        segments: Punctuated {
                            pairs: vec![],
                            tail: Some(Box::new(PathSegment {
                                ident: Ident(TokenStream::from((0, "a"))),
                                arguments: None
                            }))
                        }
                    }
                })),
                op: BinOp::AddAssign(Plus(
                    Some(S(TokenStream::from((1, " ")))),
                    TokenStream::from((2, "+")),
                    Some(S(TokenStream::from((3, " "))))
                )),
                right: Box::new(Expr::Path(ExprPath {
                    qself: None,
                    path: Path {
                        leading_pathsep: None,
                        segments: Punctuated {
                            pairs: vec![],
                            tail: Some(Box::new(PathSegment {
                                ident: Ident(TokenStream::from((4, "b"))),
                                arguments: None
                            }))
                        }
                    }
                }))
            }))
        );
        assert_eq!(
            TokenStream::from("a ^  ").parse::<Expr<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::ExprBinaryRightOperand,
                ControlFlow::Fatal,
                Span::Range(5..5)
            ))
        );
    }
}
