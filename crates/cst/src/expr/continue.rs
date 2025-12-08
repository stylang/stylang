use parserc::syntax::Syntax;

use crate::{input::CSTInput, keyword::Continue, misc::Label};

/// A continue, with an optional label.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprContinue<I>
where
    I: CSTInput,
{
    /// leading keyword `continue`.
    pub keyword: Continue<I>,
    /// optional label name.
    pub label: Option<Label<I>>,
}
