//! `CST` tokens for `stylang`.

macro_rules! define_token {
    ($ident: ident, $value: literal) => {
        #[doc = "define token `"]
        #[doc = stringify!($value)]
        #[doc = "`"]
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, parserc::syntax::Syntax)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        #[parserc(keyword = $value, map_err = crate::errors::TokenKind::$ident.map())]
        pub struct $ident<I>(pub I)
        where
            I: crate::input::CSTInput;
    };
}

macro_rules! define_token_c {
    ($ident: ident, $value: literal) => {
        #[doc = "define token `"]
        #[doc = stringify!($value)]
        #[doc = "`"]
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, parserc::syntax::Syntax)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        #[parserc(char = $value, map_err = crate::errors::TokenKind::$ident.map())]
        pub struct $ident<I>(pub I)
        where
            I: crate::input::CSTInput;
    };
}

#[cfg(test)]
macro_rules! assert_token_parse {
    ($ident: ident, $input: literal, $match: literal) => {
        assert_eq!(
            parserc::syntax::InputSyntaxExt::parse(&mut crate::input::TokenStream::from($input)),
            Ok($ident(crate::input::TokenStream::from($match)))
        )
    };
}

pub mod keyword;
pub mod op;
pub mod punct;
