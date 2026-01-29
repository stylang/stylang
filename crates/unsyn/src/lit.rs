//! literal values.

use parserc::{ParseError, Parser, keyword, syntax::Syntax, take_while_range};

use crate::{
    errors::{SemanticsKind, SyntaxKind, UnsynError},
    input::UnsynInput,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LitUnicode<I>(pub I)
where
    I: UnsynInput;

impl<I> Syntax<I> for LitUnicode<I>
where
    I: UnsynInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let mut content = input.clone();

        let prefix = keyword("U+")
            .parse(input)
            .map_err(SyntaxKind::Unicode.map())?;

        take_while_range(4..5, |c: char| c.is_ascii_hexdigit())
            .parse(input)
            .map_err(|err| {
                UnsynError::Semantics(SemanticsKind::Unicode, prefix.to_span() + err.to_span())
            })?;

        if let Some(c) = input.iter().next() {
            if c.is_ascii_hexdigit() {
                return Err(UnsynError::Semantics(
                    SemanticsKind::Unicode,
                    prefix.to_span() + input.to_span_at(1),
                ));
            }
        }

        Ok(Self(content.split_to(6)))
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

#[cfg(test)]
mod tests {
    use parserc::{Span, syntax::SyntaxInput};

    use crate::{
        errors::{SemanticsKind, UnsynError},
        input::TokenStream,
        lit::LitUnicode,
    };

    #[test]
    fn test_unicode() {
        assert_eq!(
            TokenStream::from("U+2029").parse::<LitUnicode<_>>(),
            Ok(LitUnicode(TokenStream::from((0, "U+2029"))))
        );

        assert_eq!(
            TokenStream::from("U+1").parse::<LitUnicode<_>>(),
            Err(UnsynError::Semantics(
                SemanticsKind::Unicode,
                Span::Range(0..3)
            ))
        );
        assert_eq!(
            TokenStream::from("U+11234").parse::<LitUnicode<_>>(),
            Err(UnsynError::Semantics(
                SemanticsKind::Unicode,
                Span::Range(0..7)
            ))
        );
    }
}
