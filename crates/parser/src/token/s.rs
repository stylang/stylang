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

#[cfg(test)]
mod tests {
    use parserc::syntax::InputSyntaxExt;

    use crate::{input::TokenStream, token::S};

    #[test]
    fn s() {
        assert_eq!(
            TokenStream::from(" \t \r\n").parse(),
            Ok(S(TokenStream::from(" \t \r\n")))
        );
    }
}
