use parserc::syntax::Syntax;

use crate::{
    errors::SyntaxKind,
    expr::Expr,
    input::CSTInput,
    punct::{Minus, Not, Star},
};

/// Unary operator.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::ExprBinaryOp.map())]
pub enum UnaryOp<I>
where
    I: CSTInput,
{
    Deref(Star<I>),
    Not(Not<I>),
    Neg(Minus<I>),
}

/// A unary operator: `*`, `!`, `-`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprUnary<I>
where
    I: CSTInput,
{
    /// leading unary operator char.
    #[parserc(crucial)]
    pub op: UnaryOp<I>,
    /// right operand.
    pub right: Box<Expr<I>>,
}
