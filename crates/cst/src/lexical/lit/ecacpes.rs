use parserc::{Parser, keyword, syntax::Syntax};

use crate::{
    errors::{CSTError, SemanticsKind, SyntaxKind},
    input::CSTInput,
};

/// ASCII escape, more information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#railroad-ASCII_ESCAPE
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ASCIIEscape<I>
where
    I: CSTInput,
{
    /// New line break
    LF(#[parserc(keyword = "\\n")] I),
    /// Carriage return
    CR(#[parserc(keyword = "\\r")] I),
    /// Table
    Tab(#[parserc(keyword = "\\t")] I),
    /// Black slash
    BlackSlash(#[parserc(keyword = "\\\\")] I),
    /// Null
    Null(#[parserc(keyword = "\\0")] I),
    /// 7-bit character code (exactly 2 digits, up to 0x7F)
    Char(#[parserc(parser = parse_7bit_char)] I),
}

#[inline]
fn parse_7bit_char<I>(input: &mut I) -> Result<I, CSTError>
where
    I: CSTInput,
{
    let mut content = input.clone();

    keyword("\\x")
        .parse(input)
        .map_err(SyntaxKind::ASCIIEscape.map())?;

    let buf = input.as_bytes();

    if buf.len() < 2 {
        return Err(CSTError::Semantics(
            SemanticsKind::Char7BitEscapeTooShort,
            input.to_span(),
        ));
    }

    if !buf[0].is_ascii_hexdigit() {
        return Err(CSTError::Semantics(
            SemanticsKind::HexDigit,
            input.to_span_at(1),
        ));
    }

    if !matches!(buf[0], b'0'..=b'7') {
        return Err(CSTError::Semantics(
            SemanticsKind::Char7BitEscapeOutOfRange,
            input.to_span_at(1),
        ));
    }

    if !buf[1].is_ascii_hexdigit() {
        return Err(CSTError::Semantics(
            SemanticsKind::HexDigit,
            input.to_span_at(2),
        ));
    }

    input.split_to(2);

    Ok(content.split_to(4))
}

#[cfg(test)]
mod tests {
    use parserc::{Span, syntax::SyntaxInput};

    use crate::{
        errors::{CSTError, SemanticsKind},
        input::TokenStream,
        lexical::lit::ASCIIEscape,
    };

    #[test]
    fn test_ascii_escape() {
        assert_eq!(
            TokenStream::from("\\n").parse::<ASCIIEscape<_>>(),
            Ok(ASCIIEscape::LF(TokenStream::from("\\n")))
        );

        assert_eq!(
            TokenStream::from("\\r").parse::<ASCIIEscape<_>>(),
            Ok(ASCIIEscape::CR(TokenStream::from("\\r")))
        );

        assert_eq!(
            TokenStream::from("\\t").parse::<ASCIIEscape<_>>(),
            Ok(ASCIIEscape::Tab(TokenStream::from("\\t")))
        );

        assert_eq!(
            TokenStream::from("\\\\").parse::<ASCIIEscape<_>>(),
            Ok(ASCIIEscape::BlackSlash(TokenStream::from("\\\\")))
        );

        assert_eq!(
            TokenStream::from("\\0").parse::<ASCIIEscape<_>>(),
            Ok(ASCIIEscape::Null(TokenStream::from("\\0")))
        );
    }

    #[test]
    fn test_7bit_escape() {
        assert_eq!(
            TokenStream::from("\\x41f").parse::<ASCIIEscape<_>>(),
            Ok(ASCIIEscape::Char(TokenStream::from("\\x41")))
        );

        assert_eq!(
            TokenStream::from("\\x").parse::<ASCIIEscape<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::Char7BitEscapeTooShort,
                Span::Range(2..2)
            ))
        );

        assert_eq!(
            TokenStream::from("\\xgg").parse::<ASCIIEscape<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::HexDigit,
                Span::Range(2..3)
            ))
        );

        assert_eq!(
            TokenStream::from("\\x7g").parse::<ASCIIEscape<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::HexDigit,
                Span::Range(2..4)
            ))
        );

        assert_eq!(
            TokenStream::from("\\x8fg").parse::<ASCIIEscape<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::Char7BitEscapeOutOfRange,
                Span::Range(2..3)
            ))
        );
    }
}
