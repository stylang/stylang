//! Keywords for `stylang`

use crate::input::CSTInput;

define_token!(Fn, "fn");
define_token!(Struct, "struct");
define_token!(Enum, "enum");
define_token!(View, "view");

/// A parser for keywords.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, parserc::syntax::Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Keyword<I>
where
    I: CSTInput,
{
    Fn(Fn<I>),
    Struct(Struct<I>),
    Enum(Enum<I>),
    View(View<I>),
}
