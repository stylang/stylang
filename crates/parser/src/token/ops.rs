use parserc::{ControlFlow, Parser, Span, keyword, next, syntax::Syntax};

use crate::{StylangError, TokenKind, input::StylangInput};

/// Op `&&`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct AndAnd<I>(I)
where
    I: StylangInput;

impl<I> Syntax<I> for AndAnd<I>
where
    I: StylangInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        keyword("&&")
            .parse(input)
            .map(|input| Self(input))
            .map_err(TokenKind::AndAnd.map())
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

/// Op `&=`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct AndEq<I>(I)
where
    I: StylangInput;

impl<I> Syntax<I> for AndEq<I>
where
    I: StylangInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        keyword("&=")
            .parse(input)
            .map(|input| Self(input))
            .map_err(TokenKind::AndEq.map())
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

/// Op `&&`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct And<I>(I)
where
    I: StylangInput;

impl<I> Syntax<I> for And<I>
where
    I: StylangInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let token = next(b'&')
            .parse(input)
            .map(|input| Self(input))
            .map_err(TokenKind::And.map())?;

        // Check whether the following character is a specific character
        if let Some(c) = input.iter().next() {
            match c {
                // Returns an error according to the longest match principle.
                b'&' | b'=' => {
                    return Err(StylangError::Token(
                        TokenKind::And,
                        ControlFlow::Recovable,
                        Span::Range(token.0.start()..token.0.start() + 1),
                    ));
                }
                _ => {}
            }
        }

        Ok(token)
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

/// Op `||`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct OrOr<I>(I)
where
    I: StylangInput;

impl<I> Syntax<I> for OrOr<I>
where
    I: StylangInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        keyword("||")
            .parse(input)
            .map(|input| Self(input))
            .map_err(TokenKind::OrOr.map())
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

/// Op `&=`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct OrEq<I>(I)
where
    I: StylangInput;

impl<I> Syntax<I> for OrEq<I>
where
    I: StylangInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        keyword("|=")
            .parse(input)
            .map(|input| Self(input))
            .map_err(TokenKind::OrEq.map())
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

/// Op `|`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Or<I>(I)
where
    I: StylangInput;

impl<I> Syntax<I> for Or<I>
where
    I: StylangInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let token = next(b'|')
            .parse(input)
            .map(|input| Self(input))
            .map_err(TokenKind::And.map())?;

        // Check whether the following character is a specific character
        if let Some(c) = input.iter().next() {
            match c {
                // Returns an error according to the longest match principle.
                b'|' | b'=' => {
                    return Err(StylangError::Token(
                        TokenKind::Or,
                        ControlFlow::Recovable,
                        Span::Range(token.0.start()..token.0.start() + 1),
                    ));
                }
                _ => {}
            }
        }

        Ok(token)
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

/// Op `^=`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct CaretEq<I>(I)
where
    I: StylangInput;

impl<I> Syntax<I> for CaretEq<I>
where
    I: StylangInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        keyword("|=")
            .parse(input)
            .map(|input| Self(input))
            .map_err(TokenKind::CaretEq.map())
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

/// Op `^`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Caret<I>(I)
where
    I: StylangInput;

impl<I> Syntax<I> for Caret<I>
where
    I: StylangInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let token = next(b'|')
            .parse(input)
            .map(|input| Self(input))
            .map_err(TokenKind::Caret.map())?;

        // Check whether the following character is a specific character
        if let Some(c) = input.iter().next() {
            match c {
                // Returns an error according to the longest match principle.
                b'|' | b'=' => {
                    return Err(StylangError::Token(
                        TokenKind::Caret,
                        ControlFlow::Recovable,
                        Span::Range(token.0.start()..token.0.start() + 1),
                    ));
                }
                _ => {}
            }
        }

        Ok(token)
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}
