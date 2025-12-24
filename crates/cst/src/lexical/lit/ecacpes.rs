use parserc::{Parser, keyword, syntax::Syntax, take_while_range};

use crate::{
    errors::{CSTError, PunctKind, SemanticsKind, SyntaxKind},
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

/// ASCII escape, more information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#railroad-ASCII_ESCAPE
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ByteEscape<I>
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
    Char(#[parserc(parser = parse_8bit_char)] I),
}

#[inline]
fn parse_8bit_char<I>(input: &mut I) -> Result<I, CSTError>
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
            SemanticsKind::Byte8BitEscapeTooShort,
            input.to_span(),
        ));
    }

    if !buf[0].is_ascii_hexdigit() {
        return Err(CSTError::Semantics(
            SemanticsKind::HexDigit,
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

/// Unicode escape, more information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#railroad-UNICODE_ESCAPE
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct UnicodeEscape<I>
where
    I: CSTInput,
{
    /// leading chars `\u{`
    #[parserc(keyword = "\\u{", crucial, map_err = SyntaxKind::UnicodeEscape.map())]
    pub delimiter_start: I,
    /// 24-bits hex-digits up to `10ffff`
    #[parserc(parser = parse_unicode_hex_digits)]
    pub digits: I,
    /// tailing punct `}`
    #[parserc(keyword = "}", map_err = PunctKind::BraceEnd.map())]
    pub delimiter_end: I,
}

#[inline]
fn parse_unicode_hex_digits<I>(input: &mut I) -> Result<I, CSTError>
where
    I: CSTInput,
{
    take_while_range(1..7, |c: char| c.is_ascii_hexdigit())
        .parse(input)
        .map_err(SemanticsKind::UnicodeEscapeLength.map())
}

/// Quote char `"` or ``' escapes, more information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#quote-escapes
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::QuoteEscape.map())]
pub enum QuoteEscape<I>
where
    I: CSTInput,
{
    Single(#[parserc(keyword = "\\'")] I),
    Double(#[parserc(keyword = "\\\"")] I),
}

/// String continue token, more information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#grammar-STRING_CONTINUE
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum NewLineEscape<I>
where
    I: CSTInput,
{
    LF(#[parserc(keyword = "\\\n")] I),
    CRLF(#[parserc(keyword = "\\\\r\n")] I),
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Span, syntax::SyntaxInput};

    use crate::{
        errors::{CSTError, PunctKind, SemanticsKind, SyntaxKind},
        input::TokenStream,
        lexical::lit::{ASCIIEscape, ByteEscape, QuoteEscape, UnicodeEscape},
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

    #[test]
    fn test_byte_escape() {
        assert_eq!(
            TokenStream::from("\\n").parse::<ByteEscape<_>>(),
            Ok(ByteEscape::LF(TokenStream::from("\\n")))
        );

        assert_eq!(
            TokenStream::from("\\r").parse::<ByteEscape<_>>(),
            Ok(ByteEscape::CR(TokenStream::from("\\r")))
        );

        assert_eq!(
            TokenStream::from("\\t").parse::<ByteEscape<_>>(),
            Ok(ByteEscape::Tab(TokenStream::from("\\t")))
        );

        assert_eq!(
            TokenStream::from("\\\\").parse::<ByteEscape<_>>(),
            Ok(ByteEscape::BlackSlash(TokenStream::from("\\\\")))
        );

        assert_eq!(
            TokenStream::from("\\0").parse::<ByteEscape<_>>(),
            Ok(ByteEscape::Null(TokenStream::from("\\0")))
        );
    }

    #[test]
    fn test_8bit_escape() {
        assert_eq!(
            TokenStream::from("\\x41f").parse::<ByteEscape<_>>(),
            Ok(ByteEscape::Char(TokenStream::from("\\x41")))
        );

        assert_eq!(
            TokenStream::from("\\x").parse::<ByteEscape<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::Byte8BitEscapeTooShort,
                Span::Range(2..2)
            ))
        );

        assert_eq!(
            TokenStream::from("\\xgg").parse::<ByteEscape<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::HexDigit,
                Span::Range(2..3)
            ))
        );

        assert_eq!(
            TokenStream::from("\\x7g").parse::<ByteEscape<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::HexDigit,
                Span::Range(2..4)
            ))
        );

        assert_eq!(
            TokenStream::from("\\x8fg").parse::<ByteEscape<_>>(),
            Ok(ByteEscape::Char(TokenStream::from("\\x8f")))
        );
    }

    #[test]
    fn test_unicode_escape() {
        assert_eq!(
            TokenStream::from("\\u{af}").parse::<UnicodeEscape<_>>(),
            Ok(UnicodeEscape {
                delimiter_start: TokenStream::from("\\u{"),
                digits: TokenStream::from((3, "af")),
                delimiter_end: TokenStream::from((5, "}"))
            })
        );

        assert_eq!(
            TokenStream::from("\\u{1}").parse::<UnicodeEscape<_>>(),
            Ok(UnicodeEscape {
                delimiter_start: TokenStream::from("\\u{"),
                digits: TokenStream::from((3, "1")),
                delimiter_end: TokenStream::from((4, "}"))
            })
        );
        assert_eq!(
            TokenStream::from("\\u{10ffff}").parse::<UnicodeEscape<_>>(),
            Ok(UnicodeEscape {
                delimiter_start: TokenStream::from("\\u{"),
                digits: TokenStream::from((3, "10ffff")),
                delimiter_end: TokenStream::from((9, "}"))
            })
        );

        assert_eq!(
            TokenStream::from("\\u{g}").parse::<UnicodeEscape<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::UnicodeEscapeLength,
                Span::Range(3..3)
            ))
        );

        assert_eq!(
            TokenStream::from("\\u{ffffffff}").parse::<UnicodeEscape<_>>(),
            Err(CSTError::Punct(
                PunctKind::BraceEnd,
                ControlFlow::Fatal,
                Span::Range(9..10)
            ))
        );

        assert_eq!(
            TokenStream::from("\\u{a").parse::<UnicodeEscape<_>>(),
            Err(CSTError::Punct(
                PunctKind::BraceEnd,
                ControlFlow::Fatal,
                Span::Range(4..4)
            ))
        );
    }

    #[test]
    fn test_quote_escape() {
        assert_eq!(
            TokenStream::from("\\'").parse::<QuoteEscape<_>>(),
            Ok(QuoteEscape::Single(TokenStream::from("\\'")))
        );
        assert_eq!(
            TokenStream::from("\\\"").parse::<QuoteEscape<_>>(),
            Ok(QuoteEscape::Double(TokenStream::from("\\\"")))
        );

        assert_eq!(
            TokenStream::from("'").parse::<QuoteEscape<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::QuoteEscape,
                ControlFlow::Recovable,
                Span::Range(0..1)
            ))
        );
    }
}
