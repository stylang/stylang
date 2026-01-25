use parserc::{
    ControlFlow, ToSpan,
    syntax::{Punctuated, Syntax, SyntaxInput},
};

use crate::{
    attr::OuterAttribute,
    errors::{CSTError, SyntaxKind},
    expr::{
        ArrayExpr, AwaitExpr, BlockExpr, CallExpr, CallParams, FieldExpr, IndexExpr, LitExpr,
        MethodCallExpr, PathExpr, StructExpr, StructExprFields, TupleIndexExpr,
    },
    input::CSTInput,
    lexical::{
        delimiter::{Brace, Bracket, Paren},
        keywords::strict::{Await, Break, Continue},
        label::LifeTimeOrLabel,
        lit::TupleIndex,
        punct::{Comma, Dot, Underscore},
    },
    macros::invocation::MacroInvocation,
    names::paths::{PathExprSegment, PathIdentSegment},
};

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions.html#grammar-ExpressionWithoutBlock
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Expr<I>
where
    I: CSTInput,
{
    WithBlock(ExprWithBlock<I>),
    WithoutBlock(ExprWithoutBlock<I>),
}

impl<I> Syntax<I> for Expr<I>
where
    I: CSTInput,
{
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let attrs: Vec<OuterAttribute<_>> = input.parse()?;

        let prefix = Prefix::parse(input)?;

        let mut expr = match prefix {
            Prefix::LitExpr(expr) => Expr::WithoutBlock(ExprWithoutBlock::LitExpr(attrs, expr)),
            Prefix::PathExpr(expr) => {
                if let PathExpr::Path(expr) = expr {
                    // test if this is a `StructExpr`
                    if let Some(fields) = input.parse::<Option<Brace<I, StructExprFields<I>>>>()? {
                        Expr::WithoutBlock(ExprWithoutBlock::StructExpr(StructExpr {
                            path: expr,
                            fields,
                        }))
                    } else {
                        Expr::WithoutBlock(ExprWithoutBlock::PathExpr(attrs, PathExpr::Path(expr)))
                    }
                } else {
                    Expr::WithoutBlock(ExprWithoutBlock::PathExpr(attrs, expr))
                }
            }
            Prefix::TupleExpr(expr) => Expr::WithoutBlock(ExprWithoutBlock::TupleExpr(attrs, expr)),
            Prefix::GroupedExpr(expr) => {
                Expr::WithoutBlock(ExprWithoutBlock::GroupedExpr(attrs, expr))
            }
            Prefix::ArrayExpr(expr) => Expr::WithoutBlock(ExprWithoutBlock::ArrayExpr(attrs, expr)),
            Prefix::UnderscoreExpr(expr) => {
                Expr::WithoutBlock(ExprWithoutBlock::UnderscoreExpr(attrs, expr))
            }
            Prefix::MarcoInvocation(expr) => {
                Expr::WithoutBlock(ExprWithoutBlock::MarcoInvocation(attrs, expr))
            }
            Prefix::BlockExpr(expr) => Expr::WithBlock(ExprWithBlock::BlockExpr(attrs, expr)),
            Prefix::ContinueExpr { keyword, label } => {
                return Ok(Expr::WithoutBlock(ExprWithoutBlock::ContinueExpr {
                    attrs,
                    keyword,
                    label,
                }));
            }
            Prefix::BreakExpr {
                keyword,
                label,
                expr,
            } => {
                return Ok(Expr::WithoutBlock(ExprWithoutBlock::BreakExpr {
                    attrs,
                    keyword,
                    label,
                    expr,
                }));
            }
        };

        loop {
            if let Some(dot) = input.parse::<Option<Dot<_>>>()? {
                if let Some(predicate) = input.parse::<Option<Await<_>>>()? {
                    expr = Expr::WithoutBlock(ExprWithoutBlock::AwaitExpr(AwaitExpr {
                        expr: Box::new(expr),
                        period: dot,
                        predicate,
                    }));

                    continue;
                } else if let Some(predicate) = input.parse::<Option<TupleIndex<_>>>()? {
                    expr = Expr::WithoutBlock(ExprWithoutBlock::TupleIndexExpr(TupleIndexExpr {
                        expr: Box::new(expr),
                        period: dot,
                        predicate,
                    }));

                    continue;
                } else if let Some(predicate) = input.parse::<Option<PathExprSegment<_>>>()? {
                    if let Some(call_params) = input.parse::<Option<CallParams<_>>>()? {
                        expr =
                            Expr::WithoutBlock(ExprWithoutBlock::MethodCallExpr(MethodCallExpr {
                                expr: Box::new(expr),
                                period: dot,
                                path_expr_segment: predicate,
                                call_params,
                            }));

                        continue;
                    }

                    if predicate.generic_args.is_none() {
                        if let PathIdentSegment::Ident(ident) = predicate.ident {
                            expr = Expr::WithoutBlock(ExprWithoutBlock::FieldExpr(FieldExpr {
                                expr: Box::new(expr),
                                period: dot,
                                ident,
                            }));

                            continue;
                        }

                        return Err(CSTError::Syntax(
                            SyntaxKind::FieldName,
                            ControlFlow::Fatal,
                            predicate.ident.to_span(),
                        ));
                    } else {
                        return Err(CSTError::Syntax(
                            SyntaxKind::CallParams,
                            ControlFlow::Fatal,
                            input.to_span_at(1),
                        ));
                    }
                }

                return Err(CSTError::Syntax(
                    SyntaxKind::ExprPredicate,
                    ControlFlow::Fatal,
                    input.to_span_at(1),
                ));
            } else if let Some(call_params) = input.parse::<Option<CallParams<_>>>()? {
                expr = Expr::WithoutBlock(ExprWithoutBlock::CallExpr(CallExpr {
                    expr: Box::new(expr),
                    call_params,
                }));

                continue;
            } else if let Some(index) = input.parse::<Option<Bracket<I, Box<Expr<I>>>>>()? {
                expr = Expr::WithoutBlock(ExprWithoutBlock::IndexExpr(IndexExpr {
                    expr: Box::new(expr),
                    index,
                }));

                continue;
            }

            break;
        }

        Ok(expr)
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        match self {
            Expr::WithBlock(expr) => expr.to_span(),
            Expr::WithoutBlock(expr) => expr.to_span(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[parserc(map_err = SyntaxKind::Expr.map())]
enum Prefix<I>
where
    I: CSTInput,
{
    LitExpr(LitExpr<I>),
    PathExpr(PathExpr<I>),
    TupleExpr(#[parserc(semantic = check_tuple_expr)] Paren<I, Punctuated<Expr<I>, Comma<I>>>),
    GroupedExpr(Paren<I, Box<Expr<I>>>),
    ArrayExpr(ArrayExpr<I>),
    UnderscoreExpr(Underscore<I>),
    MarcoInvocation(MacroInvocation<I>),
    BlockExpr(Box<BlockExpr<I>>),
    ContinueExpr {
        keyword: Continue<I>,
        label: Option<LifeTimeOrLabel<I>>,
    },
    BreakExpr {
        keyword: Break<I>,
        label: Option<LifeTimeOrLabel<I>>,
        expr: Option<Box<Expr<I>>>,
    },
}

#[inline]
fn check_tuple_expr<I>(
    _: I,
    ty: Paren<I, Punctuated<Expr<I>, Comma<I>>>,
) -> Result<Paren<I, Punctuated<Expr<I>, Comma<I>>>, CSTError>
where
    I: CSTInput,
{
    if ty.body.pairs.len() == 0 && ty.body.tail.is_some() {
        return Err(CSTError::Syntax(
            SyntaxKind::TupleExpr,
            ControlFlow::Recovable,
            ty.to_span(),
        ));
    }

    Ok(ty)
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions.html#grammar-ExpressionWithoutBlock
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ExprWithoutBlock<I>
where
    I: CSTInput,
{
    ContinueExpr {
        attrs: Vec<OuterAttribute<I>>,
        keyword: Continue<I>,
        label: Option<LifeTimeOrLabel<I>>,
    },
    BreakExpr {
        attrs: Vec<OuterAttribute<I>>,
        keyword: Break<I>,
        label: Option<LifeTimeOrLabel<I>>,
        expr: Option<Box<Expr<I>>>,
    },
    LitExpr(Vec<OuterAttribute<I>>, LitExpr<I>),
    PathExpr(Vec<OuterAttribute<I>>, PathExpr<I>),
    TupleExpr(
        Vec<OuterAttribute<I>>,
        Paren<I, Punctuated<Expr<I>, Comma<I>>>,
    ),
    GroupedExpr(Vec<OuterAttribute<I>>, Paren<I, Box<Expr<I>>>),
    ArrayExpr(Vec<OuterAttribute<I>>, ArrayExpr<I>),
    UnderscoreExpr(Vec<OuterAttribute<I>>, Underscore<I>),
    MarcoInvocation(Vec<OuterAttribute<I>>, MacroInvocation<I>),
    AwaitExpr(AwaitExpr<I>),
    TupleIndexExpr(TupleIndexExpr<I>),
    FieldExpr(FieldExpr<I>),
    CallExpr(CallExpr<I>),
    MethodCallExpr(MethodCallExpr<I>),
    IndexExpr(IndexExpr<I>),
    StructExpr(StructExpr<I>),
}

impl<I> Syntax<I> for ExprWithoutBlock<I>
where
    I: CSTInput,
{
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let expr = Expr::parse(input)?;

        match expr {
            Expr::WithBlock(expr) => Err(CSTError::Syntax(
                SyntaxKind::ExprWithBlock,
                ControlFlow::Recovable,
                expr.to_span(),
            )),
            Expr::WithoutBlock(expr) => {
                return Err(CSTError::Syntax(
                    SyntaxKind::ExprWithoutBlock,
                    ControlFlow::Recovable,
                    expr.to_span(),
                ));
            }
        }
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        match self {
            ExprWithoutBlock::LitExpr(attrs, expr) => attrs.to_span() + expr.to_span(),
            ExprWithoutBlock::PathExpr(attrs, expr) => attrs.to_span() + expr.to_span(),
            ExprWithoutBlock::TupleExpr(attrs, expr) => attrs.to_span() + expr.to_span(),
            ExprWithoutBlock::GroupedExpr(attrs, expr) => attrs.to_span() + expr.to_span(),
            ExprWithoutBlock::ArrayExpr(attrs, expr) => attrs.to_span() + expr.to_span(),
            ExprWithoutBlock::UnderscoreExpr(attrs, expr) => attrs.to_span() + expr.to_span(),
            ExprWithoutBlock::MarcoInvocation(attrs, expr) => attrs.to_span() + expr.to_span(),
            ExprWithoutBlock::AwaitExpr(expr) => expr.to_span(),
            ExprWithoutBlock::TupleIndexExpr(expr) => expr.to_span(),
            ExprWithoutBlock::FieldExpr(expr) => expr.to_span(),
            ExprWithoutBlock::MethodCallExpr(expr) => expr.to_span(),
            ExprWithoutBlock::CallExpr(expr) => expr.to_span(),
            ExprWithoutBlock::ContinueExpr {
                attrs,
                keyword,
                label,
            } => attrs.to_span() + keyword.to_span() + label.to_span(),
            ExprWithoutBlock::BreakExpr {
                attrs,
                keyword,
                label,
                expr,
            } => attrs.to_span() + keyword.to_span() + label.to_span() + expr.to_span(),
            ExprWithoutBlock::IndexExpr(delimiter) => delimiter.to_span(),
            ExprWithoutBlock::StructExpr(expr) => expr.to_span(),
        }
    }
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions.html#grammar-ExpressionWithBlock
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ExprWithBlock<I>
where
    I: CSTInput,
{
    BlockExpr(Vec<OuterAttribute<I>>, Box<BlockExpr<I>>),
}

impl<I> Syntax<I> for ExprWithBlock<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let expr = Expr::parse(input)?;

        match expr {
            Expr::WithBlock(expr) => Ok(expr),
            Expr::WithoutBlock(expr) => {
                return Err(CSTError::Syntax(
                    SyntaxKind::ExprWithBlock,
                    ControlFlow::Recovable,
                    expr.to_span(),
                ));
            }
        }
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        match self {
            ExprWithBlock::BlockExpr(attrs, expr) => attrs.to_span() + expr.to_span(),
        }
    }
}
