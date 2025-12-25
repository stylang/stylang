use parserc::{
    ControlFlow, Item, Parser, keyword, next, syntax::Syntax, take_while_range, take_while_range_to,
};

use crate::{
    errors::{CSTError, PunctKind, SemanticsKind, SyntaxKind},
    input::CSTInput,
    lexical::lit::{ASCIIEscape, ByteEscape, NewLineEscape, QuoteEscape, UnicodeEscape},
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
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum StrContent<I>
where
    I: CSTInput,
{
    QuoteEscape(QuoteEscape<I>),
    ASCIIEscape(ASCIIEscape<I>),
    UnicodeEscape(UnicodeEscape<I>),
    NewLineEscape(NewLineEscape<I>),
    CharWithException(#[parserc(parser = parse_str_item_with_exception)] I),
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
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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

/// Raw string literals do not process any escapes. They start with the character U+0072 (r),
/// followed by fewer than 256 of the character U+0023 (#) and a U+0022 (double-quote) character.
///
/// see [`The Rust Reference`]
///
/// [`The Rust Reference`]:https://doc.rust-lang.org/reference/tokens.html#string-literals
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LitRawStr<I>
where
    I: CSTInput,
{
    /// leading char `r`
    pub leading_char: I,
    /// leading pounds fewer than 256,
    pub leading_pounds: I,
    /// delimiter `"`
    pub delimiter_start: I,
    /// sequence of unicode characters.
    pub content: I,
    /// delimiter `"`
    pub delimiter_end: I,
    /// tailing pounds fewer than 256,
    pub tailing_pounds: I,
}

impl<I> Syntax<I> for LitRawStr<I>
where
    I: CSTInput,
{
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let leading_char = next('r').parse(input)?;

        let leading_pounds = take_while_range_to(256, |c| c == '#')
            .parse(input)
            .map_err(SemanticsKind::Pounds.map())?;

        let delimiter_start = next('"')
            .parse(input)
            .map_err(PunctKind::DoubleQuote.map_into_fatal())?;

        let mut iter = input.iter_indices();

        while let Some((offset, c)) = iter.next() {
            if c == '"' {
                if !leading_pounds.is_empty() {
                    if let Some(tailing_pounds) =
                        take_while_range(leading_pounds.len()..leading_pounds.len() + 1, |c| {
                            c == '#'
                        })
                        .ok()
                        .parse(&mut input.clone().split_off(offset + 1))?
                    {
                        return Ok(Self {
                            leading_char,
                            leading_pounds,
                            delimiter_start,
                            content: input.split_to(offset),
                            delimiter_end: input.split_to(1),
                            tailing_pounds: input.split_to(tailing_pounds.len()),
                        });
                    }

                    continue;
                }

                return Ok(Self {
                    leading_char,
                    leading_pounds,
                    delimiter_start,
                    content: input.split_to(offset),
                    delimiter_end: input.split_to(1),
                    tailing_pounds: input.split_to(0),
                });
            }
        }

        Err(CSTError::Punct(
            PunctKind::DoubleQuote,
            ControlFlow::Fatal,
            input.to_span(),
        ))
    }

    fn to_span(&self) -> parserc::Span {
        self.leading_char.to_span() + self.delimiter_end.to_span() + self.tailing_pounds.to_span()
    }
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
    ByteEscape(ByteEscape<I>),
    UnicodeEscape(UnicodeEscape<I>),
    ASCIIWithException(#[parserc(parser = parser_ascii_with_exception)] I),
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

/// A byte literal is a single ASCII character enclosed within
/// two U+0027 (single-quote) characters, see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#byte-literals
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
pub struct LitByte<I>
where
    I: CSTInput,
{
    #[parserc(keyword = "b'", crucial, map_err = PunctKind::BQuote.map())]
    pub delimiter_start: I,
    /// content of byte literal.
    pub content: ByteContent<I>,
    #[parserc(keyword = "'", map_err = PunctKind::Quote.map())]
    pub delimiter_end: I,
}

/// Content item of [`LitByteStr`]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ByteStrContent<I>
where
    I: CSTInput,
{
    QuoteEscape(QuoteEscape<I>),
    ByteEscape(ByteEscape<I>),
    NewLineEscape(NewLineEscape<I>),
    CharWithException(#[parserc(parser = parse_byt_str_item_with_exception)] I),
}

#[inline]
fn parse_byt_str_item_with_exception<I>(input: &mut I) -> Result<I, CSTError>
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
                    SemanticsKind::ByteStrContent,
                    input.to_span_at(1),
                ));
            }
            Some((offset, '"' | '\\')) => {
                if offset == 0 {
                    return Err(CSTError::Syntax(
                        SyntaxKind::ByteStrContent,
                        ControlFlow::Recovable,
                        input.to_span_at(1),
                    ));
                }

                return Ok(input.split_to(offset));
            }
            Some((_, c)) if c.is_ascii() => {
                continue;
            }
            Some((offset, c)) => {
                return Err(CSTError::Semantics(
                    SemanticsKind::ByteStrContent,
                    input.split_off(offset).to_span_at(c.len_utf8()),
                ));
            }
            None => return Ok(input.split_to(input.len())),
        }
    }
}

/// A non-raw byte string literal is a sequence of ASCII characters and escapes,
/// preceded by the characters U+0062 (b) and U+0022 (double-quote), and followed
/// by the character U+0022.
///
/// see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#byte-string-literals
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LitByteStr<I>
where
    I: CSTInput,
{
    #[parserc(keyword = "b\"", crucial)]
    pub delimiter_start: I,
    /// sequence of content item of literal string.
    pub content: Vec<ByteStrContent<I>>,
    #[parserc(keyword = "\"", map_err = PunctKind::DoubleQuote.map())]
    pub delimiter_end: I,
}

/// Raw byte string literals do not process any escapes. They start with the character U+0062 (b),
/// followed by U+0072 (r), followed by fewer than 256 of the character U+0023 (#),
/// and a U+0022 (double-quote) character.
///
/// see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#raw-byte-string-literals
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LitRawByteStr<I>
where
    I: CSTInput,
{
    /// leading char `br`
    pub leading_char: I,
    /// leading pounds fewer than 256,
    pub leading_pounds: I,
    /// delimiter `"`
    pub delimiter_start: I,
    /// sequence of unicode characters.
    pub content: I,
    /// delimiter `"`
    pub delimiter_end: I,
    /// tailing pounds fewer than 256,
    pub tailing_pounds: I,
}

impl<I> Syntax<I> for LitRawByteStr<I>
where
    I: CSTInput,
{
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let leading_char = keyword("br").parse(input)?;

        let leading_pounds = take_while_range_to(256, |c| c == '#')
            .parse(input)
            .map_err(SemanticsKind::Pounds.map())?;

        let delimiter_start = next('"')
            .parse(input)
            .map_err(PunctKind::DoubleQuote.map_into_fatal())?;

        let mut iter = input.iter_indices();

        while let Some((offset, c)) = iter.next() {
            if !c.is_ascii() {
                return Err(CSTError::Semantics(
                    SemanticsKind::RawByteStrContent,
                    input.split_off(offset).to_span_at(c.len_utf8()),
                ));
            }

            if c == '"' {
                if !leading_pounds.is_empty() {
                    if let Some(tailing_pounds) =
                        take_while_range(leading_pounds.len()..leading_pounds.len() + 1, |c| {
                            c == '#'
                        })
                        .ok()
                        .parse(&mut input.clone().split_off(offset + 1))?
                    {
                        return Ok(Self {
                            leading_char,
                            leading_pounds,
                            delimiter_start,
                            content: input.split_to(offset),
                            delimiter_end: input.split_to(1),
                            tailing_pounds: input.split_to(tailing_pounds.len()),
                        });
                    }

                    continue;
                }

                return Ok(Self {
                    leading_char,
                    leading_pounds,
                    delimiter_start,
                    content: input.split_to(offset),
                    delimiter_end: input.split_to(1),
                    tailing_pounds: input.split_to(0),
                });
            }
        }

        Err(CSTError::Punct(
            PunctKind::DoubleQuote,
            ControlFlow::Fatal,
            input.to_span(),
        ))
    }

    fn to_span(&self) -> parserc::Span {
        self.leading_char.to_span() + self.delimiter_end.to_span() + self.tailing_pounds.to_span()
    }
}

/// Content item of [`LitCStr`]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum CStrContent<I>
where
    I: CSTInput,
{
    ByteEscape(#[parserc(semantic = check_byte_escape_except)] ByteEscape<I>),
    UnicodeEscape(#[parserc(semantic = check_unicode_escape_except)] UnicodeEscape<I>),
    NewLineEscape(NewLineEscape<I>),
    CharWithException(#[parserc(parser = parse_c_str_item_with_exception)] I),
}

fn check_byte_escape_except<I>(_: I, escape: ByteEscape<I>) -> Result<ByteEscape<I>, CSTError>
where
    I: CSTInput,
{
    match &escape {
        ByteEscape::Null(input) => Err(CSTError::Semantics(
            SemanticsKind::CStrContent,
            input.to_span(),
        )),
        ByteEscape::Char(input) if input.as_str() == "\x00" => Err(CSTError::Semantics(
            SemanticsKind::CStrContent,
            input.to_span(),
        )),
        _ => Ok(escape),
    }
}

fn check_unicode_escape_except<I>(
    _: I,
    escape: UnicodeEscape<I>,
) -> Result<UnicodeEscape<I>, CSTError>
where
    I: CSTInput,
{
    match escape.digits.as_str() {
        "0" | "00" | "000" | "0000" | "00000" | "000000" => Err(CSTError::Semantics(
            SemanticsKind::CStrContent,
            escape.digits.to_span(),
        )),
        _ => Ok(escape),
    }
}

#[inline]
fn parse_c_str_item_with_exception<I>(input: &mut I) -> Result<I, CSTError>
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
            Some((offset, '\r' | '\0')) => {
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

/// A C string literal is a sequence of Unicode characters and escapes, preceded by the
/// characters U+0063 (c) and U+0022 (double-quote), and followed by the character U+0022.
///
/// see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#c-string-literals
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LitCStr<I>
where
    I: CSTInput,
{
    #[parserc(keyword = "c\"", crucial)]
    pub delimiter_start: I,
    /// sequence of content item of literal string.
    pub content: Vec<CStrContent<I>>,
    #[parserc(keyword = "\"", map_err = PunctKind::DoubleQuote.map())]
    pub delimiter_end: I,
}

/// Raw C string literals do not process any escapes. They start with the character U+0063 (c),
/// followed by U+0072 (r), followed by fewer than 256 of the character U+0023 (#), and a U+0022
/// (double-quote) character.
///
/// see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#raw-byte-string-literals
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LitRawCStr<I>
where
    I: CSTInput,
{
    /// leading char `cr`
    pub leading_char: I,
    /// leading pounds fewer than 256,
    pub leading_pounds: I,
    /// delimiter `"`
    pub delimiter_start: I,
    /// sequence of unicode characters.
    pub content: I,
    /// delimiter `"`
    pub delimiter_end: I,
    /// tailing pounds fewer than 256,
    pub tailing_pounds: I,
}

impl<I> Syntax<I> for LitRawCStr<I>
where
    I: CSTInput,
{
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let leading_char = keyword("cr").parse(input)?;

        let leading_pounds = take_while_range_to(256, |c| c == '#')
            .parse(input)
            .map_err(SemanticsKind::Pounds.map())?;

        let delimiter_start = next('"')
            .parse(input)
            .map_err(PunctKind::DoubleQuote.map_into_fatal())?;

        let mut iter = input.iter_indices();

        while let Some((offset, c)) = iter.next() {
            match c {
                '\0' | '\r' => {
                    return Err(CSTError::Semantics(
                        SemanticsKind::RawCStrContent,
                        input.split_off(offset).to_span_at(c.len_utf8()),
                    ));
                }
                _ => {}
            }

            if c == '"' {
                if !leading_pounds.is_empty() {
                    if let Some(tailing_pounds) =
                        take_while_range(leading_pounds.len()..leading_pounds.len() + 1, |c| {
                            c == '#'
                        })
                        .ok()
                        .parse(&mut input.clone().split_off(offset + 1))?
                    {
                        return Ok(Self {
                            leading_char,
                            leading_pounds,
                            delimiter_start,
                            content: input.split_to(offset),
                            delimiter_end: input.split_to(1),
                            tailing_pounds: input.split_to(tailing_pounds.len()),
                        });
                    }

                    continue;
                }

                return Ok(Self {
                    leading_char,
                    leading_pounds,
                    delimiter_start,
                    content: input.split_to(offset),
                    delimiter_end: input.split_to(1),
                    tailing_pounds: input.split_to(0),
                });
            }
        }

        Err(CSTError::Punct(
            PunctKind::DoubleQuote,
            ControlFlow::Fatal,
            input.to_span(),
        ))
    }

    fn to_span(&self) -> parserc::Span {
        self.leading_char.to_span() + self.delimiter_end.to_span() + self.tailing_pounds.to_span()
    }
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Span, syntax::SyntaxInput};

    use crate::{
        errors::{CSTError, PunctKind, SemanticsKind, SyntaxKind},
        input::TokenStream,
        lexical::lit::{
            ASCIIEscape, ByteContent, ByteEscape, ByteStrContent, CStrContent, CharContent,
            LitByte, LitByteStr, LitCStr, LitChar, LitRawByteStr, LitRawCStr, LitRawStr, LitStr,
            NewLineEscape, QuoteEscape, StrContent,
        },
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
    fn test_char_escape() {
        assert_eq!(
            TokenStream::from("'\\x0a'").parse::<LitChar<_>>(),
            Ok(LitChar {
                delimiter_start: TokenStream::from((0, "'")),
                content: CharContent::ASCIIEscape(ASCIIEscape::Char(TokenStream::from((
                    1, "\\x0a"
                )))),
                delimiter_end: TokenStream::from((5, "'"))
            })
        );

        assert_eq!(
            TokenStream::from("'\\x8a'").parse::<LitChar<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::Char7BitEscapeOutOfRange,
                Span::Range(3..4)
            ))
        );
    }

    #[test]
    fn test_byte_escape() {
        assert_eq!(
            TokenStream::from("b'\\xfa'").parse::<LitByte<_>>(),
            Ok(LitByte {
                delimiter_start: TokenStream::from((0, "b'")),
                content: ByteContent::ByteEscape(ByteEscape::Char(TokenStream::from((2, "\\xfa")))),
                delimiter_end: TokenStream::from((6, "'"))
            })
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

        assert_eq!(
            TokenStream::from(r##""\""##).parse::<LitStr<_>>(),
            Err(CSTError::Punct(
                PunctKind::DoubleQuote,
                ControlFlow::Fatal,
                Span::Range(3..3)
            ))
        );
    }

    #[test]
    fn test_raw_str() {
        assert_eq!(
            TokenStream::from(include_str!("tests/rstr0.txt")).parse::<LitRawStr<_>>(),
            Ok(LitRawStr {
                leading_char: TokenStream::from((0, "r")),
                leading_pounds: TokenStream::from((1, "####")),
                delimiter_start: TokenStream::from((5, "\"")),
                content: TokenStream::from((6, "hello\"\"\" \\nworld")),
                delimiter_end: TokenStream::from((22, "\"")),
                tailing_pounds: TokenStream::from((23, "####"))
            })
        );

        assert_eq!(
            TokenStream::from(include_str!("tests/rstr1.txt")).parse::<LitRawStr<_>>(),
            Ok(LitRawStr {
                leading_char: TokenStream::from((0, "r")),
                leading_pounds: TokenStream::from((1, "")),
                delimiter_start: TokenStream::from((1, "\"")),
                content: TokenStream::from((2, "hello")),
                delimiter_end: TokenStream::from((7, "\"")),
                tailing_pounds: TokenStream::from((8, ""))
            })
        );
    }

    #[test]
    fn test_byte_str() {
        assert_eq!(
            TokenStream::from(r#"b"你好""#).parse::<LitByteStr<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::ByteStrContent,
                Span::Range(2..5)
            ))
        );

        assert_eq!(
            TokenStream::from(r#"b"hello\tworld\"""#).parse::<LitByteStr<_>>(),
            Ok(LitByteStr {
                delimiter_start: TokenStream::from((0, "b\"")),
                content: vec![
                    ByteStrContent::CharWithException(TokenStream::from((2, "hello"))),
                    ByteStrContent::ByteEscape(ByteEscape::Tab(TokenStream::from((7, "\\t")))),
                    ByteStrContent::CharWithException(TokenStream::from((9, "world"))),
                    ByteStrContent::QuoteEscape(QuoteEscape::Double(TokenStream::from((
                        14, "\\\""
                    ))))
                ],
                delimiter_end: TokenStream::from((16, "\""))
            })
        );

        assert_eq!(
            TokenStream::from(r#"b"""#).parse::<LitByteStr<_>>(),
            Ok(LitByteStr {
                delimiter_start: TokenStream::from((0, "b\"")),
                content: vec![],
                delimiter_end: TokenStream::from((2, "\""))
            })
        );

        assert_eq!(
            TokenStream::from("b\"").parse::<LitByteStr<_>>(),
            Err(CSTError::Punct(
                PunctKind::DoubleQuote,
                ControlFlow::Fatal,
                Span::Range(2..2)
            ))
        );

        assert_eq!(
            TokenStream::from(r##"b"\""##).parse::<LitByteStr<_>>(),
            Err(CSTError::Punct(
                PunctKind::DoubleQuote,
                ControlFlow::Fatal,
                Span::Range(4..4)
            ))
        );
    }

    #[test]
    fn test_raw_byte_str() {
        assert_eq!(
            TokenStream::from(include_str!("tests/brstr0.txt")).parse::<LitRawByteStr<_>>(),
            Ok(LitRawByteStr {
                leading_char: TokenStream::from((0, "br")),
                leading_pounds: TokenStream::from((2, "####")),
                delimiter_start: TokenStream::from((6, "\"")),
                content: TokenStream::from((7, "hello\"\"\" \\nworld")),
                delimiter_end: TokenStream::from((23, "\"")),
                tailing_pounds: TokenStream::from((24, "####"))
            })
        );

        assert_eq!(
            TokenStream::from(include_str!("tests/brstr1.txt")).parse::<LitRawByteStr<_>>(),
            Ok(LitRawByteStr {
                leading_char: TokenStream::from((0, "br")),
                leading_pounds: TokenStream::from((2, "")),
                delimiter_start: TokenStream::from((2, "\"")),
                content: TokenStream::from((3, "hello")),
                delimiter_end: TokenStream::from((8, "\"")),
                tailing_pounds: TokenStream::from((9, ""))
            })
        );
    }

    #[test]
    fn test_c_str() {
        assert_eq!(
            TokenStream::from(r#"c"\0""#).parse::<LitCStr<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::CStrContent,
                Span::Range(2..4)
            ))
        );

        assert_eq!(
            TokenStream::from(
                r#"c"你好\
            ""#
            )
            .parse::<LitCStr<_>>(),
            Ok(LitCStr {
                delimiter_start: TokenStream::from((0, "c\"")),
                content: vec![
                    CStrContent::CharWithException(TokenStream::from((2, "你好"))),
                    CStrContent::NewLineEscape(NewLineEscape::LF(TokenStream::from((8, "\\\n")))),
                    CStrContent::CharWithException(TokenStream::from((10, "            ")))
                ],
                delimiter_end: TokenStream::from((22, "\""))
            })
        );
    }

    #[test]
    fn test_raw_c_str() {
        assert_eq!(
            TokenStream::from(include_str!("tests/crstr0.txt")).parse::<LitRawCStr<_>>(),
            Ok(LitRawCStr {
                leading_char: TokenStream::from((0, "cr")),
                leading_pounds: TokenStream::from((2, "####")),
                delimiter_start: TokenStream::from((6, "\"")),
                content: TokenStream::from((7, "hello\"\"\" \\nworld")),
                delimiter_end: TokenStream::from((23, "\"")),
                tailing_pounds: TokenStream::from((24, "####"))
            })
        );

        assert_eq!(
            TokenStream::from(include_str!("tests/crstr1.txt")).parse::<LitRawCStr<_>>(),
            Ok(LitRawCStr {
                leading_char: TokenStream::from((0, "cr")),
                leading_pounds: TokenStream::from((2, "")),
                delimiter_start: TokenStream::from((2, "\"")),
                content: TokenStream::from((3, "hello")),
                delimiter_end: TokenStream::from((8, "\"")),
                tailing_pounds: TokenStream::from((9, ""))
            })
        );

        assert_eq!(
            TokenStream::from("cr\"\0\"").parse::<LitRawCStr<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::RawCStrContent,
                Span::Range(3..4)
            ))
        );

        assert_eq!(
            TokenStream::from("cr\"\r\"").parse::<LitRawCStr<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::RawCStrContent,
                Span::Range(3..4)
            ))
        );
    }
}
