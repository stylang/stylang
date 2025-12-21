use parserc::{ControlFlow, Item, syntax::Syntax};

use crate::{
    errors::{CSTError, PunctKind, SemanticsKind, SyntaxKind},
    input::CSTInput,
    lexical::lit::{ASCIIEscape, NewLineEscape, QuoteEscape, UnicodeEscape},
};

/// A character literal is a single Unicode character enclosed within
/// two U+0027 (single-quote) characters, see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#character-literals
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum CharContent<I>
where
    I: CSTInput,
{
    QuoteEscape(QuoteEscape<I>),
    ASCIIEscape(ASCIIEscape<I>),
    UnicodeEscape(UnicodeEscape<I>),
    CharWithException(#[parserc(parser = parser_char_with_exception)] I),
}

#[inline]
fn parser_char_with_exception<I>(input: &mut I) -> Result<I, CSTError>
where
    I: CSTInput,
{
    match input.iter().next() {
        Some('\\' | '\n' | '\r' | '\t') => Err(CSTError::Semantics(
            SemanticsKind::Character,
            input.to_span_at(1),
        )),
        Some('\'') | None => Err(CSTError::Syntax(
            SyntaxKind::Character,
            ControlFlow::Fatal,
            input.to_span_at(1),
        )),
        Some(c) => Ok(input.split_to(c.len())),
    }
}

/// A character literal is a single Unicode character enclosed within
/// two U+0027 (single-quote) characters, see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#character-literals
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
pub struct LitChar<I>
where
    I: CSTInput,
{
    #[parserc(keyword = "'", crucial)]
    pub delimiter_start: I,
    /// content of character literal.
    pub content: CharContent<I>,
    #[parserc(keyword = "'", map_err = PunctKind::Quote.map())]
    pub delimiter_end: I,
}

/// Content item of [`LitStr`]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
pub enum StrContent<I>
where
    I: CSTInput,
{
    CharWithException(#[parserc(parser = parse_str_item_with_exception)] I),
    QuoteEscape(QuoteEscape<I>),
    ASCIIEscape(ASCIIEscape<I>),
    UnicodeEscape(UnicodeEscape<I>),
    NewLineEscape(NewLineEscape<I>),
}

#[inline]
fn parse_str_item_with_exception<I>(input: &mut I) -> Result<I, CSTError>
where
    I: CSTInput,
{
    if input.is_empty() {
        return Err(CSTError::Syntax(
            SyntaxKind::StrContent,
            ControlFlow::Recovable,
            input.to_span_at(1),
        ));
    }

    let mut iter = input.iter_indices();

    loop {
        match iter.next() {
            Some((offset, '\r')) => {
                input.split_to(offset);
                return Err(CSTError::Semantics(
                    SemanticsKind::StrContent,
                    input.to_span_at(1),
                ));
            }
            Some((offset, '"' | '\\')) => {
                if offset == 0 {
                    return Err(CSTError::Syntax(
                        SyntaxKind::StrContent,
                        ControlFlow::Recovable,
                        input.to_span_at(1),
                    ));
                }

                return Ok(input.split_to(offset));
            }
            Some((_, _)) => {
                continue;
            }
            None => return Ok(input.split_to(input.len())),
        }
    }
}

/// A string literal is a sequence of any Unicode characters enclosed within two U+0022 (double-quote) characters,
/// with the exception of U+0022 itself, which must be escaped by a preceding U+005C character (\).
/// see [`The Rust Reference`]
///
/// [`The Rust Reference`]:https://doc.rust-lang.org/reference/tokens.html#string-literals
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
pub struct LitStr<I>
where
    I: CSTInput,
{
    #[parserc(keyword = "\"", crucial)]
    pub delimiter_start: I,
    /// sequence of content item of literal string.
    pub content: Vec<StrContent<I>>,
    #[parserc(keyword = "\"", map_err = PunctKind::DoubleQuote.map())]
    pub delimiter_end: I,
}

/// A byte literal is a single ASCII character enclosed within
/// two U+0027 (single-quote) characters, see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#byte-literals
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ByteContent<I>
where
    I: CSTInput,
{
    QuoteEscape(QuoteEscape<I>),
    ASCIIEscape(ASCIIEscape<I>),
    UnicodeEscape(UnicodeEscape<I>),
    ASCIIWithException(#[parserc(parser = parser_ascii_with_exception)] I),
}

/// A byte literal is a single ASCII character enclosed within
/// two U+0027 (single-quote) characters, see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#byte-literals
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
pub struct LitByte<I>
where
    I: CSTInput,
{
    #[parserc(keyword = "r'", crucial)]
    pub delimiter_start: I,
    /// content of byte literal.
    pub content: ByteContent<I>,
    #[parserc(keyword = "'", map_err = PunctKind::Quote.map())]
    pub delimiter_end: I,
}

#[inline]
fn parser_ascii_with_exception<I>(input: &mut I) -> Result<I, CSTError>
where
    I: CSTInput,
{
    match input.iter().next() {
        Some('\\' | '\n' | '\r' | '\t') => Err(CSTError::Semantics(
            SemanticsKind::Byte,
            input.to_span_at(1),
        )),

        Some(c) if !c.is_ascii() => Err(CSTError::Semantics(
            SemanticsKind::Byte,
            input.to_span_at(c.len()),
        )),
        Some('\'') | None => Err(CSTError::Syntax(
            SyntaxKind::Byte,
            ControlFlow::Fatal,
            input.to_span_at(1),
        )),
        Some(c) => Ok(input.split_to(c.len())),
    }
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Span, syntax::SyntaxInput};

    use crate::{
        errors::{CSTError, PunctKind, SemanticsKind, SyntaxKind},
        input::TokenStream,
        lexical::lit::{ASCIIEscape, ByteContent, CharContent, LitStr, QuoteEscape, StrContent},
    };

    #[test]
    fn test_exception_char() {
        assert_eq!(
            TokenStream::from("\\").parse::<CharContent<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::Character,
                Span::Range(0..1)
            ))
        );

        assert_eq!(
            TokenStream::from("\r").parse::<CharContent<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::Character,
                Span::Range(0..1)
            ))
        );

        assert_eq!(
            TokenStream::from("\n").parse::<CharContent<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::Character,
                Span::Range(0..1)
            ))
        );

        assert_eq!(
            TokenStream::from("\t").parse::<CharContent<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::Character,
                Span::Range(0..1)
            ))
        );

        assert_eq!(
            TokenStream::from("'").parse::<CharContent<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::Character,
                ControlFlow::Fatal,
                Span::Range(0..1)
            ))
        );
    }

    #[test]
    fn test_char_cjk_content() {
        assert_eq!(
            TokenStream::from("你").parse::<CharContent<_>>(),
            Ok(CharContent::CharWithException(TokenStream::from("你")))
        )
    }

    #[test]
    fn test_exception_byte() {
        assert_eq!(
            TokenStream::from("\\").parse::<ByteContent<_>>(),
            Err(CSTError::Semantics(SemanticsKind::Byte, Span::Range(0..1)))
        );

        assert_eq!(
            TokenStream::from("\r").parse::<ByteContent<_>>(),
            Err(CSTError::Semantics(SemanticsKind::Byte, Span::Range(0..1)))
        );

        assert_eq!(
            TokenStream::from("\n").parse::<ByteContent<_>>(),
            Err(CSTError::Semantics(SemanticsKind::Byte, Span::Range(0..1)))
        );

        assert_eq!(
            TokenStream::from("\t").parse::<ByteContent<_>>(),
            Err(CSTError::Semantics(SemanticsKind::Byte, Span::Range(0..1)))
        );

        assert_eq!(
            TokenStream::from("'").parse::<ByteContent<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::Byte,
                ControlFlow::Fatal,
                Span::Range(0..1)
            ))
        );
    }

    #[test]
    fn test_byte_cjk_content() {
        assert_eq!(
            TokenStream::from("你").parse::<ByteContent<_>>(),
            Err(CSTError::Semantics(SemanticsKind::Byte, Span::Range(0..3)))
        )
    }

    #[test]
    fn test_cjk_str_content() {
        assert_eq!(
            TokenStream::from("hello\\t你好\"").parse::<Vec<StrContent<_>>>(),
            Ok(vec![
                StrContent::CharWithException(TokenStream::from((0, "hello"))),
                StrContent::ASCIIEscape(ASCIIEscape::Tab(TokenStream::from((5, "\\t")))),
                StrContent::CharWithException(TokenStream::from((7, "你好")))
            ])
        )
    }

    #[test]
    fn test_lit_str() {
        assert_eq!(
            TokenStream::from(r#""hello\t你好\"""#).parse::<LitStr<_>>(),
            Ok(LitStr {
                delimiter_start: TokenStream::from((0, "\"")),
                content: vec![
                    StrContent::CharWithException(TokenStream::from((1, "hello"))),
                    StrContent::ASCIIEscape(ASCIIEscape::Tab(TokenStream::from((6, "\\t")))),
                    StrContent::CharWithException(TokenStream::from((8, "你好"))),
                    StrContent::QuoteEscape(QuoteEscape::Double(TokenStream::from((14, "\\\""))))
                ],
                delimiter_end: TokenStream::from((16, "\""))
            })
        );

        assert_eq!(
            TokenStream::from(r#""""#).parse::<LitStr<_>>(),
            Ok(LitStr {
                delimiter_start: TokenStream::from((0, "\"")),
                content: vec![],
                delimiter_end: TokenStream::from((1, "\""))
            })
        );

        assert_eq!(
            TokenStream::from("\"").parse::<LitStr<_>>(),
            Err(CSTError::Punct(
                PunctKind::DoubleQuote,
                ControlFlow::Fatal,
                Span::Range(1..1)
            ))
        );
    }
}
