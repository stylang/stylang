use parserc::syntax::Syntax;

use crate::{
    expr::{ExprLit, ExprPath},
    input::CSTInput,
};

/// A stylang expression.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Expr<I>
where
    I: CSTInput,
{
    Lit(ExprLit<I>),
    Path(ExprPath<I>),
}
