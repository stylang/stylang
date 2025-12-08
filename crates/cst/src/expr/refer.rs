use parserc::syntax::Syntax;

use crate::{
    expr::{Expr, ExprChain},
    input::CSTInput,
    keyword::Mut,
    punct::And,
};

// A referencing operation: &a or &mut a.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprReference<I>
where
    I: CSTInput,
{
    /// leading token `&`
    pub leading_token: And<I>,
    /// optional keyword `mut`
    pub mutability: Option<Mut<I>>,
    /// target expr.
    #[parserc(parser = ExprChain::into_parser().map(ExprChain::into).boxed())]
    pub expr: Box<Expr<I>>,
}
