use parserc::{ControlFlow, Parser, syntax::Syntax, take_while_range_from};

use crate::{
    errors::{CSTError, TokenKind},
    input::CSTInput,
};

/// literial digits.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Digits<I>
where
    I: CSTInput,
{
    pub input: I,
    pub value: u128,
}

impl<I> Syntax<I> for Digits<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let content = take_while_range_from(1, |c: u8| c.is_ascii_digit())
            .parse(input)
            .map_err(TokenKind::LitDigits.map())?;

        let value = u128::from_str_radix(content.as_str(), 10).map_err(|_| {
            CSTError::Token(TokenKind::LitDigits, ControlFlow::Fatal, content.to_span())
        })?;

        Ok(Self {
            input: content,
            value,
        })
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.input.to_span()
    }
}

/// literial hex-digits.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct HexDigits<I>
where
    I: CSTInput,
{
    pub input: I,
    pub value: u128,
}

impl<I> Syntax<I> for HexDigits<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let content = take_while_range_from(1, |c: u8| c.is_ascii_hexdigit())
            .parse(input)
            .map_err(TokenKind::LitDigits.map())?;

        let value = u128::from_str_radix(content.as_str(), 16).map_err(|_| {
            CSTError::Token(TokenKind::LitDigits, ControlFlow::Fatal, content.to_span())
        })?;

        Ok(Self {
            input: content,
            value,
        })
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.input.to_span()
    }
}
