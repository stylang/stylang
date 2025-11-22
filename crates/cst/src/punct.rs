macro_rules! punctuated {
    ($ident: ident, $value: literal) => {
        #[doc = "define token `"]
        #[doc = stringify!($value)]
        #[doc = "`"]
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        pub struct $ident<I>(pub Option<crate::S<I>>, pub I, pub Option<crate::S<I>>)
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
                        .map_err(crate::errors::TokenKind::$ident.map())?,
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

punctuated!(BraceStart, "{");
punctuated!(BraceEnd, "}");
punctuated!(BracketStart, "[");
punctuated!(BracketEnd, "]");
punctuated!(ParenStart, "(");
punctuated!(ParenEnd, ")");
punctuated!(At, "@");
punctuated!(ArrowRight, "->");
punctuated!(Colon, ":");
punctuated!(Comma, ",");
punctuated!(Semi, ";");
punctuated!(PathSep, "::");
