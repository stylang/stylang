/// A sequence of `whitespace` chars.
use parserc::{ControlFlow, Parser, Span, syntax::Syntax, take_while};

use crate::{StylangError, TokenKind, input::StylangInput};

/// Sequence of ascii whitespace characters
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct S<I>(pub I)
where
    I: StylangInput;

impl<I> Syntax<I> for S<I>
where
    I: StylangInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let s = take_while(|c: u8| c.is_ascii_whitespace())
            .parse(input)
            .map_err(TokenKind::S.map())?;

        if s.is_empty() {
            return Err(StylangError::Token(
                TokenKind::S,
                ControlFlow::Recovable,
                Span::Range(input.start()..input.start()),
            ));
        }

        Ok(Self(s))
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

/// Punctuation `{`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(char = b'{', map_err = TokenKind::BraceStart.map())]
pub struct BraceStart<I>(pub I)
where
    I: StylangInput;

/// Punctuation `}`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(char = b'}', map_err = TokenKind::BraceEnd.map())]
pub struct BraceEnd<I>(pub I)
where
    I: StylangInput;

/// Punctuation `[`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(char = b'[', map_err = TokenKind::BracketStart.map())]
pub struct BracketStart<I>(pub I)
where
    I: StylangInput;

/// Punctuation `]`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(char = b']', map_err = TokenKind::BracketEnd.map())]
pub struct BracketEnd<I>(pub I)
where
    I: StylangInput;

/// Punctuation `(`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(char = b'(', map_err = TokenKind::ParenStart.map())]
pub struct ParenStart<I>(pub I)
where
    I: StylangInput;

/// Punctuation `)`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(char = b')', map_err = TokenKind::ParenEnd.map())]
pub struct ParenEnd<I>(pub I)
where
    I: StylangInput;

#[cfg(test)]
mod tests {
    use parserc::syntax::InputSyntaxExt;

    use crate::input::TokenStream;

    use super::*;

    #[test]
    fn s() {
        assert_eq!(
            TokenStream::from(" \t \r\n").parse(),
            Ok(S(TokenStream::from(" \t \r\n")))
        );
    }

    #[test]
    fn punctuation() {
        macro_rules! make_test {
            ($ty: ident, $input: literal, $match: literal) => {
                assert_eq!(
                    TokenStream::from($input).parse(),
                    Ok($ty(TokenStream::from($match)))
                );
            };
        }

        make_test!(BraceStart, "{{", "{");
        make_test!(BraceEnd, "}{", "}");

        make_test!(BracketStart, "[{", "[");
        make_test!(BracketEnd, "]{", "]");

        make_test!(ParenStart, "({", "(");
        make_test!(ParenEnd, "){", ")");
    }
}
