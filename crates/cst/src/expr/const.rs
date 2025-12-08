use parserc::syntax::Syntax;

use crate::{block::Block, input::CSTInput, keyword::Const};

/// A const block: `const { ... }`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprConst<I>
where
    I: CSTInput,
{
    /// A keyword `const`
    pub keyword: Const<I>,
    /// A const block .
    pub block: Block<I>,
}
