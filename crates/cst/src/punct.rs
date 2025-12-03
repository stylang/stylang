//! puncts for stylang parser.

macro_rules! define_punct {
    ($ident: ident, $value: literal) => {
        #[doc = "define punct `"]
        #[doc = stringify!($value)]
        #[doc = "`"]
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        pub struct $ident<I>(
            pub Option<crate::misc::S<I>>,
            pub I,
            pub Option<crate::misc::S<I>>,
        )
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
                    input.parse()?,
                    parserc::keyword($value)
                        .parse(input)
                        .map_err(crate::errors::PunctKind::$ident.map())?,
                    input.parse()?,
                ))
            }

            #[inline]
            fn to_span(&self) -> parserc::Span {
                self.0.to_span() + self.1.to_span() + self.2.to_span()
            }
        }
    };
}

define_punct!(BraceStart, "{");
define_punct!(BraceEnd, "}");
define_punct!(BracketStart, "[");
define_punct!(BracketEnd, "]");
define_punct!(ParenStart, "(");
define_punct!(ParenEnd, ")");
define_punct!(At, "@");
define_punct!(ArrowRight, "->");
define_punct!(Colon, ":");
define_punct!(Comma, ",");
define_punct!(Semi, ";");
define_punct!(Or, "|");
define_punct!(PathSep, "::");
define_punct!(AngleBracketStart, "<");
define_punct!(AngleBracketEnd, ">");
define_punct!(Question, "?");
define_punct!(Equal, "=");
define_punct!(Plus, "+");
define_punct!(And, "&");
