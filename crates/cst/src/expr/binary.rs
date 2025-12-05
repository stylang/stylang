use parserc::syntax::Syntax;

use crate::{
    errors::SyntaxKind,
    expr::{Expr, group::Composable},
    input::CSTInput,
    punct::{
        And, AndAnd, AndEq, Caret, CaretEq, EqEq, Equal, Gt, GtEq, Lt, LtEq, Minus, MinusEq, NotEq,
        Or, OrEq, OrOr, Plus, PlusEq, Rem, RemEq, Shl, ShlEq, Shr, ShrEq, Slash, SlashEq, Star,
        StarEq, StarStar, StarStarEq,
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

impl<I> Composable<I> for ExprBinary<I>
where
    I: CSTInput,
{
    #[inline]
    fn priority(&self) -> usize {
        2
    }

    #[inline]
    fn compose<F>(mut self, priority: usize, f: F) -> super::Expr<I>
    where
        F: FnOnce(super::Expr<I>) -> super::Expr<I>,
    {
        if self.priority() > priority {
            self.right = Box::new((*self.right).compose(priority, f));
            Expr::Binary(self)
        } else {
            f(Expr::Binary(self))
        }
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

    use super::*;

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
