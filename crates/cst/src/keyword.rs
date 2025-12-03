//! Defines keywords.
//!
use parserc::syntax::Syntax;

use crate::{errors::KeywordKind, input::CSTInput};

macro_rules! define_keyword {
    ($ident: ident, $value: literal) => {
        #[doc = "define keyword `"]
        #[doc = stringify!($value)]
        #[doc = "`"]
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        pub struct $ident<I>(pub I, pub Option<crate::misc::S<I>>)
        where
            I: crate::input::CSTInput;

        impl<I> parserc::syntax::Syntax<I> for $ident<I>
        where
            I: crate::input::CSTInput,
        {
            #[inline]
            fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
                use parserc::Parser;
                use parserc::syntax::InputSyntaxExt;
                Ok(Self(
                    parserc::keyword($value)
                        .parse(input)
                        .map_err(crate::errors::KeywordKind::$ident.map())?,
                    input.parse()?,
                ))
            }

            #[inline]
            fn to_span(&self) -> parserc::Span {
                self.0.to_span() + self.1.to_span()
            }
        }
    };
}

/// `self` or `Self` keyword.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Self_<I>
where
    I: CSTInput,
{
    /// keyword `self` for object reference.
    Object(#[parserc(keyword = "self",map_err = KeywordKind::SelfObject.map())] I),
    /// keyword `Self` for class reference.
    Class(#[parserc(keyword = "Self",map_err = KeywordKind::SelfClass.map())] I),
}

define_keyword!(Struct, "struct");
define_keyword!(Enum, "enum");
define_keyword!(Fn_, "fn");
define_keyword!(Mut, "mut");
define_keyword!(Const, "const");
define_keyword!(Where, "where");
define_keyword!(Extern, "Extern");
