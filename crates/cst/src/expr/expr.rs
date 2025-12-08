use parserc::{Parser, syntax::Syntax};

use crate::{
    errors::{CSTError, PunctKind, SemanticsKind, SyntaxKind},
    expr::{
        Call, ExprArray, ExprBinary, ExprBlock, ExprBracket, ExprBreak, ExprConst, ExprContinue,
        ExprForLoop, ExprIf, ExprInfer, ExprLet, ExprLit, ExprLoop, ExprPath, ExprRange,
        ExprReference, ExprRepeat, ExprReturn, ExprTuple, ExprUnary, ExprWhile, Field, Index,
        MethodCall, parse_bracket, parse_range,
    },
    input::CSTInput,
};

/// operators of a stylang chain call.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ChainOp<I>
where
    I: CSTInput,
{
    MethodCall(MethodCall<I>),
    Field(Field<I>),
    Call(Call<I>),
    Index(#[parserc(parser = parse_index)] Index<I>),
}

#[inline]
fn parse_index<I>(input: &mut I) -> Result<Index<I>, CSTError>
where
    I: CSTInput,
{
    match parse_bracket(input)? {
        ExprBracket::Index(index) => Ok(index),
        ExprBracket::Repeat(expr_repeat) => Err(CSTError::Semantics(
            SemanticsKind::Repeat,
            expr_repeat.delimiter_start.to_span() + expr_repeat.delimiter_end.to_span(),
        )),
        ExprBracket::Array(expr_array) => Err(CSTError::Semantics(
            SemanticsKind::Array,
            expr_array.delimiter_start.to_span() + expr_array.delimiter_end.to_span(),
        )),
    }
}

/// A chain call expression
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub(crate) struct ExprChain<I>
where
    I: CSTInput,
{
    /// based expr.
    #[parserc(parser = parse_expr_chain_base)]
    pub base: Box<Expr<I>>,
    /// chain ops.
    pub ops: Vec<ChainOp<I>>,
}

#[inline]
fn parse_expr_chain_base<I>(input: &mut I) -> Result<Box<Expr<I>>, CSTError>
where
    I: CSTInput,
{
    ExprLit::into_parser()
        .map(Expr::Lit)
        .or(ExprPath::into_parser().map(Expr::Path))
        .or(ExprBlock::into_parser().map(Expr::Block))
        .or(ExprIf::into_parser().map(Expr::If))
        .or(ExprConst::into_parser().map(Expr::Const))
        .or(ExprTuple::into_parser().map(Expr::Tuple))
        .or(|input: &mut I| match parse_bracket(input)? {
            ExprBracket::Index(index) => Err(CSTError::Semantics(
                SemanticsKind::Index,
                index.delimiter_start.to_span() + index.delimiter_end.to_span(),
            )),
            ExprBracket::Repeat(expr_repeat) => Ok(Expr::Repeat(expr_repeat)),
            ExprBracket::Array(expr_array) => Ok(Expr::Array(expr_array)),
        })
        .boxed()
        .parse(input)
        .map_err(|err| {
            if let CSTError::Punct(PunctKind::ParenStart, control_flow, span) = err {
                CSTError::Syntax(SyntaxKind::Expr, control_flow, span)
            } else {
                err
            }
        })
}

impl<I> From<ExprChain<I>> for Expr<I>
where
    I: CSTInput,
{
    fn from(mut value: ExprChain<I>) -> Self {
        let Some(tail) = value.ops.pop() else {
            return *value.base;
        };

        match tail {
            ChainOp::MethodCall(tail) => Expr::MethodCall(value.base, value.ops, tail),
            ChainOp::Field(tail) => Expr::Field(value.base, value.ops, tail),
            ChainOp::Call(tail) => Expr::Call(value.base, value.ops, tail),
            ChainOp::Index(tail) => Expr::Index(value.base, value.ops, tail),
        }
    }
}

/// A stylang expression.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Expr<I>
where
    I: CSTInput,
{
    MethodCall(Box<Expr<I>>, Vec<ChainOp<I>>, MethodCall<I>),
    Field(Box<Expr<I>>, Vec<ChainOp<I>>, Field<I>),
    Call(Box<Expr<I>>, Vec<ChainOp<I>>, Call<I>),
    Index(Box<Expr<I>>, Vec<ChainOp<I>>, Index<I>),

    Lit(ExprLit<I>),
    Path(ExprPath<I>),
    Block(ExprBlock<I>),
    Array(ExprArray<I>),
    Repeat(ExprRepeat<I>),
    If(ExprIf<I>),
    Const(ExprConst<I>),
    Tuple(ExprTuple<I>),

    Ref(ExprReference<I>),
    Unary(ExprUnary<I>),
    Binary(ExprBinary<I>),
    Range(ExprRange<I>),

    Let(ExprLet<I>),
    Infer(ExprInfer<I>),
    Continue(ExprContinue<I>),
    For(ExprForLoop<I>),
    Loop(ExprLoop<I>),
    While(ExprWhile<I>),
    Break(ExprBreak<I>),
    Return(ExprReturn<I>),
}

impl<I> Syntax<I> for Expr<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        if let Some(expr) = ExprLet::into_parser()
            .map(Expr::Let)
            .or(ExprInfer::into_parser().map(Expr::Infer))
            .or(ExprForLoop::into_parser().map(Expr::For))
            .or(ExprLoop::into_parser().map(Expr::Loop))
            .or(ExprContinue::into_parser().map(Expr::Continue))
            .or(ExprWhile::into_parser().map(Expr::While))
            .or(ExprBreak::into_parser().map(Expr::Break))
            .or(ExprReturn::into_parser().map(Expr::Return))
            .ok()
            .parse(input)?
        {
            return Ok(expr);
        }

        parse_range(input)
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        match self {
            Expr::Lit(expr) => expr.to_span(),
            Expr::Path(expr) => expr.to_span(),
            Expr::Block(expr) => expr.to_span(),
            Expr::Array(expr) => expr.delimiter_start.to_span() + expr.delimiter_end.to_span(),
            Expr::If(expr) => expr.to_span(),
            Expr::Const(expr) => expr.to_span(),
            Expr::Tuple(expr) => expr.to_span(),
            Expr::Ref(expr) => expr.to_span(),
            Expr::Let(expr) => expr.to_span(),
            Expr::Infer(expr) => expr.to_span(),
            Expr::MethodCall(expr, _, tail) => expr.to_span() + tail.to_span(),
            Expr::Field(expr, _, tail) => expr.to_span() + tail.to_span(),
            Expr::Call(expr, _, tail) => expr.to_span() + tail.to_span(),
            Expr::Index(expr, _, tail) => expr.to_span() + tail.delimiter_end.to_span(),
            Expr::Unary(expr) => expr.to_span(),
            Expr::Continue(expr) => expr.to_span(),
            Expr::Binary(expr) => expr.left.to_span() + expr.right.to_span(),
            Expr::Range(expr) => expr.start.to_span() + expr.limits.to_span() + expr.end.to_span(),
            Expr::For(expr) => expr.to_span(),
            Expr::Loop(expr) => expr.to_span(),
            Expr::While(expr) => expr.to_span(),
            Expr::Break(expr) => expr.to_span(),
            Expr::Return(expr) => expr.to_span(),
            Expr::Repeat(expr) => expr.delimiter_start.to_span() + expr.delimiter_end.to_span(),
        }
    }
}
