//! `CST` tokens for `stylang`.

use parserc::{Parser, next_if, syntax::Syntax, take_while};

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

/// A sequnce of whitespace chars
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, parserc::syntax::Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(token = |c:u8| c.is_ascii_whitespace())]
pub struct S<I>(pub I)
where
    I: crate::input::CSTInput;

/// Ident of type/variable.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Ident<I>(pub I)
where
    I: crate::input::CSTInput;

impl<I> Syntax<I> for Ident<I>
where
    I: crate::input::CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let mut content = input.clone();

        next_if(|c: u8| c == b'_' || c.is_ascii_alphabetic())
            .parse(input)
            .map_err(crate::errors::TokenKind::Ident.map())?;

        let other = take_while(|c: u8| c == b'_' || c.is_ascii_alphanumeric())
            .parse(input)
            .map_err(crate::errors::TokenKind::Ident.map())?;

        let content = content.split_to(1 + other.len());

        Ok(Self(content))
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

/// Ident of xml tag
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct XmlIdent<I>(pub I)
where
    I: crate::input::CSTInput;

impl<I> Syntax<I> for XmlIdent<I>
where
    I: crate::input::CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let mut content = input.clone();

        next_if(|c: u8| c == b'_' || c == b'-' || c.is_ascii_alphabetic())
            .parse(input)
            .map_err(crate::errors::TokenKind::XmlIdent.map())?;

        let other = take_while(|c: u8| c == b'_' || c == b'-' || c.is_ascii_alphanumeric())
            .parse(input)
            .map_err(crate::errors::TokenKind::XmlIdent.map())?;

        let content = content.split_to(1 + other.len());

        Ok(Self(content))
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Span, syntax::InputSyntaxExt};

    use crate::{
        errors::{CSTError, TokenKind},
        input::TokenStream,
    };

    use super::*;

    #[test]
    fn test_ident() {
        assert_token_parse!(Ident, "_hell9o_10-", "_hell9o_10");
        assert_token_parse!(XmlIdent, "-hell9o-10-", "-hell9o-10-");

        assert_eq!(
            TokenStream::from("-hello").parse::<Ident<_>>(),
            Err(CSTError::Token(
                TokenKind::Ident,
                ControlFlow::Recovable,
                Span::Range(0..1)
            ))
        );

        assert_eq!(
            TokenStream::from("9hello").parse::<XmlIdent<_>>(),
            Err(CSTError::Token(
                TokenKind::XmlIdent,
                ControlFlow::Recovable,
                Span::Range(0..1)
            ))
        );
    }

    #[test]
    fn test_s() {
        assert_token_parse!(S, "\t \r\n_hell9o_10-", "\t \r\n");
    }
}
