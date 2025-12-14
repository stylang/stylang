use parserc::syntax::Syntax;

use crate::input::CSTInput;

/// whitespace characters: `\r,\n,...`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(take_while = |c: char| c.is_whitespace())]
pub struct S<I>(pub I)
where
    I: CSTInput;
