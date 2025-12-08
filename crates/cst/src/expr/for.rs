use parserc::syntax::Syntax;

use crate::{
    block::Block,
    expr::Expr,
    input::CSTInput,
    keyword::{For, In},
    misc::{Label, S},
    pat::Pat,
    punct::Colon,
};

/// A for loop: `for pat in expr { ... }`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprForLoop<I>
where
    I: CSTInput,
{
    /// optional label
    pub label: Option<(Label<I>, Colon<I>)>,
    /// keyword `for`
    #[parserc(crucial)]
    pub for_token: For<I>,
    /// match pattern.
    pub pat: (Box<Pat<I>>, Option<S<I>>),
    /// keyword `in`
    pub in_token: In<I>,
    /// target iterator.
    pub expr: Box<Expr<I>>,
    /// for block.
    pub body: Block<I>,
}
