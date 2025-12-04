use parserc::{Parser, syntax::Syntax};

use crate::{
    errors::{CSTError, SyntaxKind},
    expr::{
        ExprArray, ExprAssgin, ExprBinary, ExprBlock, ExprCall, ExprLet, ExprLit, ExprPath,
        ExprUnary,
    },
    input::CSTInput,
};

/// A stylang expression.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Expr<I>
where
    I: CSTInput,
{
    Binary(ExprBinary<I>),
    Unary(ExprUnary<I>),
    Call(ExprCall<I>),
    Array(ExprArray<I>),
    Block(ExprBlock<I>),
    Assign(ExprAssgin<I>),
    Lit(ExprLit<I>),
    Let(ExprLet<I>),
    Path(ExprPath<I>),
}

#[inline]
pub(super) fn parse_op_lhs<I>(input: &mut I) -> Result<Box<Expr<I>>, CSTError>
where
    I: CSTInput,
{
    ExprPath::into_parser()
        .map(|expr| Expr::Path(expr))
        .boxed()
        .parse(input)
        .map_err(SyntaxKind::AssignLeftOperand.map_unhandle())
}

#[inline]
pub(super) fn parse_op_rhs<I>(input: &mut I) -> Result<Box<Expr<I>>, CSTError>
where
    I: CSTInput,
{
    ExprCall::into_parser()
        .map(|expr| Expr::Call(expr))
        .or(ExprArray::into_parser().map(|expr| Expr::Array(expr)))
        .or(ExprBlock::into_parser().map(|expr| Expr::Block(expr)))
        .or(ExprLit::into_parser().map(|expr| Expr::Lit(expr)))
        .or(ExprPath::into_parser().map(|expr| Expr::Path(expr)))
        .boxed()
        .parse(input)
        .map_err(SyntaxKind::AssignLeftOperand.map_unhandle())
}
