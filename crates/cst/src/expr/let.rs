use parserc::syntax::Syntax;

use crate::{expr::Expr, input::CSTInput, keyword::Let, pat::Pat, punct::Equal};

/// An assignment expression: a = compute().
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprLet<I>
where
    I: CSTInput,
{
    /// leading keyword `let`
    pub keyword: Let<I>,
    /// left Operand pattern.
    pub pat: Box<Pat<I>>,
    /// punct `=`
    pub eq: Equal<I>,
    /// right Operand expression.
    pub expr: Box<Expr<I>>,
}
