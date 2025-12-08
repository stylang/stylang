use parserc::syntax::{Or, Syntax};

use crate::{
    block::Block,
    expr::Expr,
    input::CSTInput,
    keyword::{Else, If},
};
/// An if expression with an optional else block: if expr { ... } else { ... }.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprIf<I>
where
    I: CSTInput,
{
    /// leading keyword `if`
    #[parserc(crucial)]
    pub keyword: If<I>,
    /// if condition sub-expr.
    pub cond: Box<Expr<I>>,
    /// then branch.
    pub then_branch: Block<I>,
    /// Optional else branch.
    pub else_branch: Option<ElseBranch<I>>,
}

/// Else block.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ElseBranch<I>
where
    I: CSTInput,
{
    /// leading keyword `else`
    pub keyword: Else<I>,
    /// `if {}` or `{}`
    pub branch: Or<Box<ExprIf<I>>, Block<I>>,
}
