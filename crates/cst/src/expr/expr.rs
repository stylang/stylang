use parserc::syntax::Syntax;

use crate::{
    expr::{ExprArray, ExprAssgin, ExprBinary, ExprBlock, ExprCall, ExprLet, ExprLit, ExprPath},
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
    Array(ExprArray<I>),
    Block(ExprBlock<I>),
    Assign(ExprAssgin<I>),
    Lit(ExprLit<I>),
    Let(ExprLet<I>),
    Call(ExprCall<I>),
    Path(ExprPath<I>),
}
