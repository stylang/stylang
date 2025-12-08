use parserc::syntax::Syntax;

use crate::{block::Block, input::CSTInput, misc::Label, punct::Colon};

/// A blocked scope: `{ ... }`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprBlock<I>
where
    I: CSTInput,
{
    /// optional block label.
    pub label: Option<(Label<I>, Colon<I>)>,
    /// A braced block containing Rust statements.
    pub block: Block<I>,
}
