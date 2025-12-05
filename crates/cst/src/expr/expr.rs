use parserc::{Parser, syntax::Syntax};

use crate::{
    errors::{CSTError, SemanticsKind, SyntaxKind},
    expr::{
        BinOp, CallArgs, ExprArray, ExprAssgin, ExprBinary, ExprBlock, ExprCall, ExprClosure,
        ExprConst, ExprFiled, ExprIndex, ExprInfer, ExprLet, ExprLit, ExprMethodCall, ExprParen,
        ExprPath, ExprUnary, Index, Member, group::Composable,
    },
    input::CSTInput,
    path::PathArguments,
    punct::Dot,
};

/// A stylang expression.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Expr<I>
where
    I: CSTInput,
{
    Infer(ExprInfer<I>),
    Field(ExprFiled<I>),
    Call(ExprCall<I>),
    Index(ExprIndex<I>),
    MethodCall(ExprMethodCall<I>),
    Binary(ExprBinary<I>),

    ///////////////////
    /// lhs
    Paren(ExprParen<I>),
    Const(ExprConst<I>),
    Unary(ExprUnary<I>),
    Block(ExprBlock<I>),
    Lit(ExprLit<I>),
    Path(ExprPath<I>),

    ///////////////
    /// rhs
    Closure(ExprClosure<I>),
    Assign(ExprAssgin<I>),
    Let(ExprLet<I>),
    Array(ExprArray<I>),
}

impl<I> Syntax<I> for Expr<I>
where
    I: CSTInput,
{
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        if let Some(expr) = ExprAssgin::into_parser()
            .map(Expr::Assign)
            .or(ExprLet::into_parser().map(Expr::Let))
            .or(ExprInfer::into_parser().map(Expr::Infer))
            .or(ExprClosure::into_parser().map(Expr::Closure))
            .ok()
            .parse(input)?
        {
            return Ok(expr);
        }

        let mut lhs = parse_lhs(input)?;

        loop {
            if let Some(index) = Index::into_parser().ok().parse(input)? {
                lhs = lhs.compose(1, |expr| {
                    Expr::Index(ExprIndex {
                        expr: Box::new(expr),
                        index,
                    })
                });

                continue;
            }

            if let Some(op) = BinOp::into_parser().ok().parse(input)? {
                let rhs =
                    parse_rhs(input).map_err(SyntaxKind::ExprBinaryRightOperand.map_fatal())?;

                lhs = lhs.compose(op.priority(), |expr| {
                    Expr::Binary(ExprBinary {
                        left: Box::new(expr),
                        op,
                        right: Box::new(rhs),
                    })
                });

                continue;
            }

            if let Some(dot) = Dot::into_parser().ok().parse(input)? {
                let member = Member::parse(input)?;

                lhs = lhs.compose(1, |expr| {
                    Expr::Field(ExprFiled {
                        base: Box::new(expr),
                        dot,
                        member,
                    })
                });

                continue;
            }

            let turbofish = PathArguments::into_parser().ok().parse(input)?;

            if let Some(args) = CallArgs::into_parser().ok().parse(input)? {
                match lhs {
                    Expr::Field(ExprFiled { base, dot, member }) => {
                        if let Member::Named(ident) = member {
                            lhs = Expr::MethodCall(ExprMethodCall {
                                receiver: base,
                                dot,
                                ident,
                                turbofish,
                                args,
                            });

                            continue;
                        }

                        // semantic check.
                        if let Some(turbofish) = turbofish {
                            return Err(CSTError::Semantics(
                                SemanticsKind::TurboFish,
                                turbofish.to_span(),
                            ));
                        }

                        lhs = Expr::Field(ExprFiled { base, dot, member });
                    }
                    _ => {}
                }

                lhs = lhs.compose(1, |expr| {
                    Expr::Call(ExprCall {
                        func: Box::new(expr),
                        args,
                    })
                });

                continue;
            }

            // semantic check.
            if let Some(turbofish) = turbofish {
                return Err(CSTError::Semantics(
                    SemanticsKind::TurboFish,
                    turbofish.to_span(),
                ));
            }

            break;
        }

        Ok(lhs)
    }

    fn to_span(&self) -> parserc::Span {
        match self {
            Expr::Field(expr) => expr.base.to_span() + expr.member.to_span(),
            Expr::Closure(expr) => expr.to_span(),
            Expr::Const(expr) => expr.to_span(),
            Expr::Binary(expr) => expr.left.to_span() + expr.right.to_span(),
            Expr::Unary(expr) => expr.to_span(),
            Expr::Call(expr) => expr.func.to_span() + expr.args.to_span(),
            Expr::MethodCall(expr) => expr.receiver.to_span() + expr.args.to_span(),
            Expr::Array(expr) => expr.to_span(),
            Expr::Block(expr) => expr.to_span(),
            Expr::Assign(expr) => expr.to_span(),
            Expr::Lit(expr) => expr.to_span(),
            Expr::Let(expr) => expr.to_span(),
            Expr::Path(expr) => expr.to_span(),
            Expr::Paren(expr) => expr.to_span(),
            Expr::Index(expr) => expr.expr.to_span() + expr.index.to_span(),
            Expr::Infer(expr) => expr.0.to_span(),
        }
    }
}

impl<I> Composable<I> for Expr<I>
where
    I: CSTInput,
{
    fn priority(&self) -> usize {
        match self {
            Expr::Field(expr) => expr.priority(),
            Expr::Call(expr) => expr.priority(),
            Expr::MethodCall(expr) => expr.priority(),
            Expr::Binary(expr) => expr.priority(),
            Expr::Const(expr) => expr.priority(),
            Expr::Unary(expr) => expr.priority(),
            Expr::Block(expr) => expr.priority(),
            Expr::Lit(expr) => expr.priority(),
            Expr::Path(expr) => expr.priority(),
            Expr::Closure(expr) => expr.priority(),
            Expr::Assign(expr) => expr.priority(),
            Expr::Let(expr) => expr.priority(),
            Expr::Array(expr) => expr.priority(),
            Expr::Paren(expr) => expr.priority(),
            Expr::Index(expr) => expr.priority(),
            Expr::Infer(expr) => expr.priority(),
        }
    }

    fn compose<F>(self, priority: usize, f: F) -> Expr<I>
    where
        F: FnOnce(Expr<I>) -> Expr<I>,
    {
        match self {
            Expr::Field(expr) => expr.compose(priority, f),
            Expr::Call(expr) => expr.compose(priority, f),
            Expr::MethodCall(expr) => expr.compose(priority, f),
            Expr::Binary(expr) => expr.compose(priority, f),
            Expr::Const(expr) => expr.compose(priority, f),
            Expr::Unary(expr) => expr.compose(priority, f),
            Expr::Block(expr) => expr.compose(priority, f),
            Expr::Lit(expr) => expr.compose(priority, f),
            Expr::Path(expr) => expr.compose(priority, f),
            Expr::Closure(expr) => expr.compose(priority, f),
            Expr::Assign(expr) => expr.compose(priority, f),
            Expr::Let(expr) => expr.compose(priority, f),
            Expr::Array(expr) => expr.compose(priority, f),
            Expr::Paren(expr) => expr.compose(priority, f),
            Expr::Index(expr) => expr.compose(priority, f),
            Expr::Infer(expr) => expr.compose(priority, f),
        }
    }
}

#[inline]
fn parse_lhs<I>(input: &mut I) -> Result<Expr<I>, CSTError>
where
    I: CSTInput,
{
    ExprConst::into_parser()
        .map(Expr::Const)
        .or(ExprArray::into_parser().map(Expr::Array))
        .or(ExprParen::into_parser().map(Expr::Paren))
        .or(ExprUnary::into_parser().map(Expr::Unary))
        .or(ExprBlock::into_parser().map(Expr::Block))
        .or(ExprLit::into_parser().map(Expr::Lit))
        .or(ExprPath::into_parser().map(Expr::Path))
        .parse(input)
}

#[inline]
pub(super) fn parse_rhs<I>(input: &mut I) -> Result<Expr<I>, CSTError>
where
    I: CSTInput,
{
    ExprArray::into_parser()
        .map(Expr::Array)
        .or(ExprParen::into_parser().map(Expr::Paren))
        .or(ExprArray::into_parser().map(Expr::Array))
        .or(ExprConst::into_parser().map(Expr::Const))
        .or(ExprUnary::into_parser().map(Expr::Unary))
        .or(ExprBlock::into_parser().map(Expr::Block))
        .or(ExprLit::into_parser().map(Expr::Lit))
        .or(ExprPath::into_parser().map(Expr::Path))
        .parse(input)
}
