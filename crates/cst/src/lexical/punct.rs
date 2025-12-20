//! punct tokens, more information see [`The Rust Reference`]
//!
//! [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#punctuation

macro_rules! define_punct {
    ($ident: ident, $value: literal) => {
        #[doc = "define punct `"]
        #[doc = stringify!($value)]
        #[doc = "`"]
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        pub struct $ident<I>(pub Option<super::S<I>>, pub I, pub Option<super::S<I>>)
        where
            I: crate::input::CSTInput;

        impl<I> parserc::syntax::Syntax<I> for $ident<I>
        where
            I: crate::input::CSTInput,
        {
            #[inline]
            fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
                use parserc::Parser;
                use parserc::syntax::SyntaxInput;

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
                self.0.to_span() + self.1.to_span()
            }
        }
    };
}

define_punct!(Plus, "+");
define_punct!(Minus, "-");
define_punct!(Star, "*");
define_punct!(Slash, "/");
define_punct!(Percent, "%");
define_punct!(Caret, "^");
define_punct!(Not, "!");
define_punct!(And, "&");
define_punct!(Or, "|");
define_punct!(AndAnd, "&&");
define_punct!(OrOr, "||");
define_punct!(Shl, "<<");
define_punct!(Shr, ">>");
define_punct!(PlusEq, "+=");
define_punct!(MinusEq, "-=");
define_punct!(StarEq, "*=");
define_punct!(SlashEq, "/=");
define_punct!(PercentEq, "%=");
define_punct!(CaretEq, "^=");
define_punct!(AndEq, "&=");
define_punct!(OrEq, "|=");
define_punct!(ShlEq, "<<=");
define_punct!(ShrEq, ">>=");
define_punct!(Eq, "=");
define_punct!(EqEq, "==");
define_punct!(Ne, "!=");
define_punct!(Gt, ">");
define_punct!(Lt, "<");
define_punct!(Ge, ">=");
define_punct!(Le, "<=");
define_punct!(At, "@");
define_punct!(Underscore, "_");
define_punct!(Dot, ".");
define_punct!(DotDot, "..");
define_punct!(DotDotDot, "...");
define_punct!(DotDotEq, "..=");
define_punct!(Comma, ",");
define_punct!(Semi, ";");
define_punct!(Colon, ":");
define_punct!(PathSep, "::");
define_punct!(RArrow, "->");
define_punct!(FatArrow, "=>");
define_punct!(LArrow, "<-");
define_punct!(Pound, "#");
define_punct!(Dollar, "$");
define_punct!(Question, "?");
define_punct!(Tilde, "~");

define_punct!(BraceStart, "{");
define_punct!(BraceEnd, "}");

define_punct!(BracketStart, "[");
define_punct!(BracketEnd, "]");

define_punct!(ParenStart, "(");
define_punct!(ParenEnd, ")");
