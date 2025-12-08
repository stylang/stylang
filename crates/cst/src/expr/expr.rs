use parserc::{Parser, syntax::Syntax};

use crate::{
    errors::CSTError,
    expr::{
        Call, ExprArray, ExprBlock, ExprConst, ExprIf, ExprInfer, ExprLet, ExprLit, ExprPath,
        ExprReference, ExprTuple, Field, Index, MethodCall,
    },
    input::CSTInput,
};

/// operators of a stylang chain call.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ChainCall<I>
where
    I: CSTInput,
{
    MethodCall(MethodCall<I>),
    Field(Field<I>),
    Call(Call<I>),
    Index(Index<I>),
}

/// A chain call expression
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprChain<I>
where
    I: CSTInput,
{
    /// based expr.
    #[parserc(parser = parse_expr_chain_base)]
    pub base: Box<Expr<I>>,
    /// chain calls.
    pub calls: Vec<ChainCall<I>>,
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
        .or(ExprArray::into_parser().map(Expr::Array))
        .or(ExprIf::into_parser().map(Expr::If))
        .or(ExprConst::into_parser().map(Expr::Const))
        .or(ExprTuple::into_parser().map(Expr::Tuple))
        .boxed()
        .parse(input)
}

impl<I> From<ExprChain<I>> for Expr<I>
where
    I: CSTInput,
{
    fn from(value: ExprChain<I>) -> Self {
        if value.calls.is_empty() {
            match *value.base {
                Expr::Tuple(expr) => Expr::Tuple(expr),
                Expr::Const(expr) => Expr::Const(expr),
                Expr::Block(expr) => Expr::Block(expr),
                Expr::Array(expr) => Expr::Array(expr),
                Expr::If(expr) => Expr::If(expr),
                Expr::Lit(expr) => Expr::Lit(expr),
                Expr::Path(expr) => Expr::Path(expr),
                _ => unreachable!(""),
            }
        } else {
            Expr::Chain(value)
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
    Chain(ExprChain<I>),

    Lit(ExprLit<I>),
    Path(ExprPath<I>),
    Block(ExprBlock<I>),
    Array(ExprArray<I>),
    If(ExprIf<I>),
    Const(ExprConst<I>),
    Tuple(ExprTuple<I>),

    Ref(ExprReference<I>),
    Let(ExprLet<I>),
    Infer(ExprInfer<I>),
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
            .or(ExprReference::into_parser().map(Expr::Ref))
            .ok()
            .parse(input)?
        {
            return Ok(expr);
        }

        Ok(ExprChain::parse(input)?.into())
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        match self {
            Expr::Chain(expr) => expr.to_span(),
            Expr::Lit(expr) => expr.to_span(),
            Expr::Path(expr) => expr.to_span(),
            Expr::Block(expr) => expr.to_span(),
            Expr::Array(expr) => expr.to_span(),
            Expr::If(expr) => expr.to_span(),
            Expr::Const(expr) => expr.to_span(),
            Expr::Tuple(expr) => expr.to_span(),
            Expr::Ref(expr) => expr.to_span(),
            Expr::Let(expr) => expr.to_span(),
            Expr::Infer(expr) => expr.to_span(),
        }
    }
}
