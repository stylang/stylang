use parserc::{Parser, syntax::Syntax};

use crate::{
    errors::{CSTError, SyntaxKind},
    expr::{Expr, ExprChain, ExprReference, ExprUnary},
    input::CSTInput,
    punct::*,
};

/// Binary operator.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::ExprBinaryOp.map())]
pub enum BinOp<I>
where
    I: CSTInput,
{
    /// operator `+=`, level 12,
    AddAssign(PlusEq<I>),
    /// operator `-=`, level 12,
    SubAssign(MinusEq<I>),
    /// operator `*=`, level 12,
    MulAssign(StarEq<I>),
    /// operator `**=`, level 12,
    SqrtAssign(StarStarEq<I>),
    /// operator `/=`, level 12,
    DivAssign(SlashEq<I>),
    /// operator `|=`, level 12,
    BitOrAssign(OrEq<I>),
    /// operator `&=`, level 12,
    BitAndAssign(AndEq<I>),
    /// operator `%=`, level 12,
    RemAssign(RemEq<I>),
    /// operator `<<=`, level 12,
    ShlAssign(ShlEq<I>),
    /// operator `>>=`, level 12,
    ShrAssign(ShrEq<I>),
    /// operator `^=`, level 12,
    BitXorAssign(CaretEq<I>),

    /// operator `**`, level 2
    Sqrt(StarStar<I>),

    /// operator `*`, level 3,
    Mul(Star<I>),
    /// operator `/`, level 3,
    Div(Slash<I>),
    /// operator `%`, level 3,
    Rem(Rem<I>),

    /// operator `+`, level 4,
    Add(Plus<I>),
    /// operator `-`, level 4,
    Sub(Minus<I>),

    /// operator `<<`, level 5,
    Shl(Shl<I>),
    /// operator `<<`, level 5,
    Shr(Shr<I>),

    /// operator `&&`, level 10,
    And(AndAnd<I>),

    /// operator `||`, level 11,
    Or(OrOr<I>),

    /// operator `&`, level 6,
    BitAnd(And<I>),
    /// operator `^`, level 7,
    BitXor(Caret<I>),
    /// operator `|`, level 8,
    BitOr(Or<I>),

    /// operator `==`, level 9,
    Eq(EqEq<I>),
    /// operator `!=`, level 9,
    NotEq(NotEq<I>),
    /// operator `<=`, level 9,
    Le(LtEq<I>),
    /// operator `<`, level 9,
    Lt(Lt<I>),
    /// operator `>=`, level 9,
    Ge(GtEq<I>),
    /// operator `>`, level 9,
    Gt(Gt<I>),
    /// operator `=`, level 12,
    Assign(Equal<I>),
}

#[allow(unused)]
impl<I> BinOp<I>
where
    I: CSTInput,
{
    pub(super) fn priority(&self) -> usize {
        match self {
            BinOp::Sqrt(_) => 2,
            BinOp::Mul(_) => 3,
            BinOp::Div(_) => 3,
            BinOp::Rem(_) => 3,
            BinOp::Add(_) => 4,
            BinOp::Sub(_) => 4,
            BinOp::Shl(_) => 5,
            BinOp::Shr(_) => 5,
            BinOp::BitAnd(_) => 6,
            BinOp::BitXor(_) => 7,
            BinOp::BitOr(_) => 8,
            BinOp::Eq(_) => 9,
            BinOp::NotEq(_) => 9,
            BinOp::Le(_) => 9,
            BinOp::Lt(_) => 9,
            BinOp::Ge(_) => 9,
            BinOp::Gt(_) => 9,
            BinOp::And(_) => 10,
            BinOp::Or(_) => 11,
            BinOp::Assign(_) => 12,
            BinOp::AddAssign(_) => 12,
            BinOp::SubAssign(_) => 12,
            BinOp::MulAssign(_) => 12,
            BinOp::SqrtAssign(_) => 12,
            BinOp::DivAssign(_) => 12,
            BinOp::BitOrAssign(_) => 12,
            BinOp::BitAndAssign(_) => 12,
            BinOp::RemAssign(_) => 12,
            BinOp::ShlAssign(_) => 12,
            BinOp::ShrAssign(_) => 12,
            BinOp::BitXorAssign(_) => 12,
        }
    }
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

#[inline]
fn parse_binary_operand<I>(input: &mut I) -> Result<Expr<I>, CSTError>
where
    I: CSTInput,
{
    ExprReference::into_parser()
        .map(Expr::Ref)
        .or(ExprUnary::into_parser().map(Expr::Unary))
        .or(ExprChain::into_parser().map(ExprChain::into))
        .parse(input)
}

pub(super) fn parse_binary<I>(input: &mut I) -> Result<Expr<I>, CSTError>
where
    I: CSTInput,
{
    let left: Expr<_> = parse_binary_operand(input)?.into();

    let Some(op) = BinOp::into_parser().ok().parse(input)? else {
        return Ok(left);
    };

    let right =
        parse_binary_operand(input).map_err(SyntaxKind::ExprBinaryRightOperand.map_into_fatal())?;

    let mut binary = ExprBinary {
        left: Box::new(left),
        op,
        right: Box::new(right),
    };

    while let Some(op) = BinOp::into_parser().ok().parse(input)? {
        let right = parse_binary_operand(input)
            .map_err(SyntaxKind::ExprBinaryRightOperand.map_into_fatal())?;

        if binary.op.priority() > op.priority() {
            binary.right = Box::new(Expr::Binary(ExprBinary {
                left: binary.right,
                op,
                right: Box::new(right),
            }));
        } else {
            binary = ExprBinary {
                left: Box::new(Expr::Binary(binary)),
                op,
                right: Box::new(right),
            };
        }

        if binary.op.priority() == 12 {
            break;
        }
    }

    Ok(Expr::Binary(binary))
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        expr::{Digits, ExprLit, ExprPath, LitNumber},
        input::TokenStream,
        misc::{Ident, S},
        path::{Path, PathSegment},
    };
    use parserc::syntax::{Punctuated, SyntaxInput};

    #[test]
    fn test_binop() {
        macro_rules! make_test {
            ($ident:ident, $punct: ident, $input: literal ,$expect: literal) => {
                assert_eq!(
                    TokenStream::from($input).parse(),
                    Ok(BinOp::$ident($punct(
                        None,
                        TokenStream::from($expect),
                        None
                    )))
                )
            };
        }

        make_test!(AddAssign, PlusEq, "+==", "+=");
        make_test!(Add, Plus, "++", "+");
        make_test!(SubAssign, MinusEq, "-=+", "-=");
        make_test!(Sub, Minus, "--", "-");
        make_test!(MulAssign, StarEq, "*==", "*=");
        make_test!(SqrtAssign, StarStarEq, "**==", "**=");
        make_test!(Sqrt, StarStar, "**/", "**");
        make_test!(Mul, Star, "*", "*");
        make_test!(DivAssign, SlashEq, "/==", "/=");
        make_test!(Div, Slash, "/", "/");
        make_test!(RemAssign, RemEq, "%==", "%=");
        make_test!(Rem, Rem, "%", "%");
        make_test!(Eq, EqEq, "==/", "==");
        make_test!(And, AndAnd, "&&=", "&&");
        make_test!(Or, OrOr, "||=", "||");
        make_test!(BitXorAssign, CaretEq, "^==", "^=");
        make_test!(BitXor, Caret, "^", "^");
        make_test!(Le, LtEq, "<=/", "<=");
        make_test!(ShlAssign, ShlEq, "<<=<", "<<=");
        make_test!(Shl, Shl, "<</", "<<");
        make_test!(Lt, Lt, "</", "<");
        make_test!(ShrAssign, ShrEq, ">>=/", ">>=");
        make_test!(Shr, Shr, ">>/", ">>");
        make_test!(Ge, GtEq, ">=/", ">=");
        make_test!(Gt, Gt, ">/", ">");
    }

    #[test]
    fn test_binary() {
        assert_eq!(
            TokenStream::from("a + c - b * c%5 / 10").parse::<Expr<_>>(),
            Ok(Expr::Binary(ExprBinary {
                left: Box::new(Expr::Binary(ExprBinary {
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
                    op: BinOp::Add(Plus(
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
                                    ident: Ident(TokenStream::from((4, "c"))),
                                    arguments: None
                                }))
                            }
                        }
                    }))
                })),
                op: BinOp::Sub(Minus(
                    Some(S(TokenStream::from((5, " ")))),
                    TokenStream::from((6, "-")),
                    Some(S(TokenStream::from((7, " "))))
                )),
                right: Box::new(Expr::Binary(ExprBinary {
                    left: Box::new(Expr::Binary(ExprBinary {
                        left: Box::new(Expr::Binary(ExprBinary {
                            left: Box::new(Expr::Path(ExprPath {
                                qself: None,
                                path: Path {
                                    leading_pathsep: None,
                                    segments: Punctuated {
                                        pairs: vec![],
                                        tail: Some(Box::new(PathSegment {
                                            ident: Ident(TokenStream::from((8, "b"))),
                                            arguments: None
                                        }))
                                    }
                                }
                            })),
                            op: BinOp::Mul(Star(
                                Some(S(TokenStream::from((9, " ")))),
                                TokenStream::from((10, "*")),
                                Some(S(TokenStream::from((11, " "))))
                            )),
                            right: Box::new(Expr::Path(ExprPath {
                                qself: None,
                                path: Path {
                                    leading_pathsep: None,
                                    segments: Punctuated {
                                        pairs: vec![],
                                        tail: Some(Box::new(PathSegment {
                                            ident: Ident(TokenStream::from((12, "c"))),
                                            arguments: None
                                        }))
                                    }
                                }
                            }))
                        })),
                        op: BinOp::Rem(Rem(None, TokenStream::from((13, "%")), None)),
                        right: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                            sign: None,
                            trunc: Some(Digits {
                                input: TokenStream::from((14, "5")),
                                value: 5
                            }),
                            fract: None,
                            exp: None
                        })))
                    })),
                    op: BinOp::Div(Slash(
                        Some(S(TokenStream::from((15, " ")))),
                        TokenStream::from((16, "/")),
                        Some(S(TokenStream::from((17, " "))))
                    )),
                    right: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                        sign: None,
                        trunc: Some(Digits {
                            input: TokenStream::from((18, "10")),
                            value: 10
                        }),
                        fract: None,
                        exp: None
                    })))
                }))
            }))
        );
    }

    #[test]
    fn test_chain_assign() {
        assert_eq!(
            TokenStream::from("a = 10 * a = B").parse::<Expr<_>>(),
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
                op: BinOp::Assign(Equal(
                    Some(S(TokenStream::from((1, " ")))),
                    TokenStream::from((2, "=")),
                    Some(S(TokenStream::from((3, " "))))
                )),
                right: Box::new(Expr::Binary(ExprBinary {
                    left: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                        sign: None,
                        trunc: Some(Digits {
                            input: TokenStream::from((4, "10")),
                            value: 10
                        }),
                        fract: None,
                        exp: None
                    }))),
                    op: BinOp::Mul(Star(
                        Some(S(TokenStream::from((6, " ")))),
                        TokenStream::from((7, "*")),
                        Some(S(TokenStream::from((8, " "))))
                    )),
                    right: Box::new(Expr::Path(ExprPath {
                        qself: None,
                        path: Path {
                            leading_pathsep: None,
                            segments: Punctuated {
                                pairs: vec![],
                                tail: Some(Box::new(PathSegment {
                                    ident: Ident(TokenStream::from((9, "a"))),
                                    arguments: None
                                }))
                            }
                        }
                    }))
                }))
            }))
        );
    }

    #[test]
    fn test_cmp() {
        println!("{:?}", TokenStream::from("a < b").parse::<Expr<_>>());
    }
}
