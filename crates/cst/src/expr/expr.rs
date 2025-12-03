use parserc::syntax::Syntax;

use crate::{
    expr::{ExprArray, ExprAssgin, ExprLet, ExprLit, ExprPath},
    input::CSTInput,
};

/// A stylang expression.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Expr<I>
where
    I: CSTInput,
{
    Array(ExprArray<I>),
    Assign(ExprAssgin<I>),
    Lit(ExprLit<I>),
    Path(ExprPath<I>),
    Let(ExprLet<I>),
}
