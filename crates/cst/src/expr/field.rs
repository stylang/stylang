use parserc::syntax::Syntax;

use crate::{errors::SyntaxKind, expr::Digits, input::CSTInput, misc::Ident, punct::Dot};

/// A struct or tuple struct field accessed in a struct literal or field expression.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::ExprFieldDot.map())]
pub enum Member<I>
where
    I: CSTInput,
{
    Named(Ident<I>),
    Unamed(Digits<I>),
}

/// Access of a named struct field (`obj.k`) or unnamed tuple struct field (`obj.0`).
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Field<I>
where
    I: CSTInput,
{
    /// punct `.`
    pub dot: Dot<I>,
    /// member of struct.
    pub member: Member<I>,
}
