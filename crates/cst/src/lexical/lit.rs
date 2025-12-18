//! literal tokens, more information see [`The Rust Reference`]
//!
//! [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#literals

use parserc::{
    ControlFlow, Parser, keyword, next, syntax::Syntax, take_while_range, take_while_range_from,
    take_while_range_to,
};

use crate::{
    errors::{CSTError, PunctKind, SemanticsKind, SyntaxKind},
    input::CSTInput,
};

/// quote escapes, more information see [`The Rust Reference`]
///
/// [`The rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#railroad-QUOTE_ESCAPE
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum QuoteEscape<I>
where
    I: CSTInput,
{
    Single(#[parserc(keyword = "\\'")] I),
    Double(#[parserc(keyword = "\\\"")] I),
}

/// ascii escapes, more information see [`The Rust Reference`]
///
/// [`The rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#railroad-ASCII_ESCAPE
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::ASCIIEscape.map())]
pub enum ASCIIEscape<I>
where
    I: CSTInput,
{
    LF(#[parserc(keyword = "\\n")] I),
    CR(#[parserc(keyword = "\\r")] I),
    TF(#[parserc(keyword = "\\t")] I),
    BackSlash(#[parserc(keyword = "\\\\")] I),
    Zero(#[parserc(keyword = "\\0")] I),
    Hex(#[parserc(parser = parse_ascii_hex_escape)] I),
}

#[inline]
fn parse_ascii_hex_escape<I>(input: &mut I) -> Result<I, CSTError>
where
    I: CSTInput,
{
    let mut content = input.clone();

    keyword("\\x").parse(input)?;

    let mut too_short = true;

    for (index, c) in input.iter().enumerate() {
        if index == 0 {
            if !c.is_ascii_hexdigit() {
                return Err(CSTError::Semantics(
                    SemanticsKind::ASCIIHexEscape,
                    input.to_span_at(3),
                ));
            }

            if matches!(c, '0'..='7') {
                continue;
            }

            return Err(CSTError::Semantics(
                SemanticsKind::ASCIIHexEscapeOutOfRange,
                input.to_span_at(3),
            ));
        }

        if index == 1 {
            if c.is_ascii_hexdigit() {
                too_short = false;
                break;
            }

            return Err(CSTError::Semantics(
                SemanticsKind::ASCIIHexEscape,
                input.to_span_at(4),
            ));
        }
    }

    if too_short {
        return Err(CSTError::Semantics(
            SemanticsKind::ASCIIHexEscapeTooShort,
            input.to_span_at(4),
        ));
    }

    input.split_to(2);

    Ok(content.split_to(4))
}

/// unicode escape, more information see [`The Rust Reference`]
///
/// [`The rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#railroad-UNICODE_ESCAPE
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct UnicodeEscape<I>
where
    I: CSTInput,
{
    /// delimiter start `\u{`
    #[parserc(keyword = "\\u{", crucial)]
    pub delimiter_start: I,

    /// (hex-digit){1..6}
    #[parserc(parser = parse_hexdigits)]
    pub digits: I,

    /// delimiter end `}`
    #[parserc(keyword = "}")]
    pub delimiter_end: I,
}

#[inline]
fn parse_hexdigits<I>(input: &mut I) -> Result<I, CSTError>
where
    I: CSTInput,
{
    let mut iter = input.iter();

    let mut len = 0usize;
    let mut underscores = 0usize;

    while let Some(c) = iter.next() {
        if c.is_ascii_hexdigit() {
            len += 1;
            continue;
        }

        if c == '_' {
            len += 1;
            underscores += 1;
            continue;
        }
    }

    if len - underscores == 0 || len - underscores > 5 {
        return Err(CSTError::Semantics(
            SemanticsKind::UnicodeEscapeDigits,
            input.to_span_at(len),
        ));
    }

    Ok(input.split_to(len))
}

/// Content of [`CharLiteral`]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum CharContent<I>
where
    I: CSTInput,
{
    /// unicode escape: `\u{..}`
    UnicodeEscape(UnicodeEscape<I>),
    /// ascii escape: `\t`,`\n`,..
    ASCIIEscape(ASCIIEscape<I>),
    /// quote escape.
    QuoteEscape(QuoteEscape<I>),
    /// unescaped character with the exception of `'`,`\`,`LF`,`CR`,`TAB`
    Char(#[parserc(parser = parse_literal_char)] I),
}

#[inline]
fn parse_literal_char<I>(input: &mut I) -> Result<I, CSTError>
where
    I: CSTInput,
{
    match input.iter().next() {
        Some('\\' | '\n' | '\r' | '\t') => Err(CSTError::Semantics(
            SemanticsKind::CharLiteral,
            input.to_span_at(1),
        )),
        Some('\'') | None => Err(CSTError::Semantics(
            SemanticsKind::EmptyCharLiteral,
            input.to_span_at(1),
        )),
        Some(c) => Ok(input.split_to(c.len_utf8())),
    }
}

/// A character literal token, more information see [`The Rust Reference`]
///
/// [`The rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#character-literals
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct CharLiteral<I>
where
    I: CSTInput,
{
    /// delimiter start `'`
    #[parserc(keyword = "'", crucial, map_err = PunctKind::Quote.map())]
    pub delimiter_start: I,
    /// content of char literal token.
    pub content: CharContent<I>,
    /// delimiter end `'`
    #[parserc(keyword = "'", map_err = PunctKind::Quote.map())]
    pub delimiter_end: I,
}

/// item of content of [`StrLiteral`]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum StrItem<I>
where
    I: CSTInput,
{
    /// unicode escape: `\u{..}`
    UnicodeEscape(UnicodeEscape<I>),
    /// ascii escape: `\t`,`\n`,..
    ASCIIEscape(ASCIIEscape<I>),
    /// quote escape.
    QuoteEscape(QuoteEscape<I>),
    /// string continue token `\CRLF` or `\LF`
    LineBreakEscape(#[parserc(parser = keyword("\\\r\n").or(keyword("\\\n")))] I),
    /// unescaped char sequence.
    Chars(#[parserc(parser = parse_literal_char_of_str)] I),
}

#[inline]
fn parse_literal_char_of_str<I>(input: &mut I) -> Result<I, CSTError>
where
    I: CSTInput,
{
    take_while_range_from(1, |c| c != '"' && c != '\\')
        .parse(input)
        .map_err(SyntaxKind::StrChar.map())
}

/// A string literal is a sequence of any Unicode characters enclosed within two `U+0022` (double-quote) characters,
/// more information see [`The Rust Reference`]
///
/// [`The rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#character-literals
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct StrLiteral<I>
where
    I: CSTInput,
{
    /// delimiter start `"`
    #[parserc(keyword = "\"", crucial, map_err = PunctKind::DoubleQuote.map())]
    pub delimiter_start: I,
    /// escaped / unescaped character seqencuce.
    pub chars: Vec<StrItem<I>>,
    /// delimiter end `"`
    #[parserc(keyword = "\"", map_err = PunctKind::DoubleQuote.map())]
    pub delimiter_end: I,
}

/// Raw string literals do not process any escapes, more information see [`The Rust Reference`]
///
/// [`The rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#raw-string-literals
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct RawStrLiteral<I>
where
    I: CSTInput,
{
    /// leading character `r`.
    pub leading_char: I,
    /// followed by fewer than 256 of the character `#`
    pub leading_pounds: I,
    /// delimiter start character `"`
    pub delimiter_start: I,
    /// The raw string body can contain any sequence of Unicode characters other than `U+000D` (CR).
    pub content: I,
    /// delimiter end character `"`
    pub delimiter_end: I,
    /// that preceded the opening `U+0022` (double-quote) character.
    pub tailing_pounds: I,
}

impl<I> Syntax<I> for RawStrLiteral<I>
where
    I: CSTInput,
{
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let leading_char = next('r').parse(input)?;
        let leading_pounds = take_while_range_to(256, |c| c == '#')
            .parse(input)
            .map_err(SemanticsKind::RawStringLeadingPounds.map())?;
        let delimiter_start = next('"')
            .parse(input)
            .map_err(PunctKind::DoubleQuote.map_into_fatal())?;

        let mut iter = input.iter_indices();

        while let Some((index, c)) = iter.next() {
            if c == '\r' {
                return Err(CSTError::Semantics(
                    SemanticsKind::StrChar,
                    input.split_to(index).to_span_at(c.len_utf8()),
                ));
            }

            if c == '"' {
                if leading_pounds.len() > 0 {
                    let mut tailing = input.clone();

                    let content = tailing.split_to(index);
                    let delimiter_end = tailing.split_to(1);
                    if let Some(tailing_pounds) =
                        take_while_range(leading_pounds.len()..leading_pounds.len() + 1, |c| {
                            c == '#'
                        })
                        .ok()
                        .parse(&mut tailing)?
                    {
                        *input = tailing;

                        return Ok(Self {
                            leading_char,
                            leading_pounds,
                            delimiter_start,
                            content,
                            delimiter_end,
                            tailing_pounds,
                        });
                    }

                    continue;
                }

                let content = input.split_to(index);
                let delimiter_end = input.split_to(1);
                let tailing_pounds = input.split_to(0);

                return Ok(Self {
                    leading_char,
                    leading_pounds,
                    delimiter_start,
                    content,
                    delimiter_end,
                    tailing_pounds,
                });
            }
        }

        Err(CSTError::Syntax(
            SyntaxKind::RawStringDelimiterEnd,
            ControlFlow::Fatal,
            input.to_span(),
        ))
    }

    fn to_span(&self) -> parserc::Span {
        self.leading_char.to_span() + self.delimiter_end.to_span() + self.tailing_pounds.to_span()
    }
}

/// Content of [`ByteLiteral`]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ByteContent<I>
where
    I: CSTInput,
{
    /// byte escape: `\t`,`\n`,..
    ASCIIEscape(ASCIIEscape<I>),
    /// quote escape.
    QuoteEscape(QuoteEscape<I>),
    /// unescaped character with the exception of `'`,`\`,`LF`,`CR`,`TAB`
    Char(#[parserc(parser = parse_literal_byte)] I),
}

#[inline]
fn parse_literal_byte<I>(input: &mut I) -> Result<I, CSTError>
where
    I: CSTInput,
{
    match input.iter().next() {
        Some('\\' | '\n' | '\r' | '\t') => Err(CSTError::Semantics(
            SemanticsKind::ByteLiteral,
            input.to_span_at(1),
        )),
        Some('\'') | None => Err(CSTError::Semantics(
            SemanticsKind::EmptyByteLiteral,
            input.to_span_at(1),
        )),
        Some(c) if c.is_ascii() => Ok(input.split_to(c.len_utf8())),
        Some(_) => Err(CSTError::Semantics(
            SemanticsKind::ByteLiteral,
            input.to_span_at(1),
        )),
    }
}

/// A byte literal is a single ASCII character (in the U+0000 to U+007F range) or a single escape,
/// more information see [`The Rust Reference`]
///
/// [`The rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#byte-literals
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ByteLiteral<I>
where
    I: CSTInput,
{
    #[parserc(keyword = "b", crucial)]
    pub leading_char: I,
    /// delimiter start `'`
    #[parserc(keyword = "'", map_err = PunctKind::Quote.map())]
    pub delimiter_start: I,
    /// content of char literal token.
    pub content: ByteContent<I>,
    /// delimiter end `'`
    #[parserc(keyword = "'", map_err = PunctKind::Quote.map())]
    pub delimiter_end: I,
}

/// item of content of [`ByteStrLiteral`]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ByteStrItem<I>
where
    I: CSTInput,
{
    /// ascii escape: `\t`,`\n`,..
    ASCIIEscape(ASCIIEscape<I>),
    /// quote escape.
    QuoteEscape(QuoteEscape<I>),
    /// string continue token `\CRLF` or `\LF`
    LineBreakEscape(#[parserc(parser = keyword("\\\r\n").or(keyword("\\\n")))] I),
    /// unescaped char sequence.
    Chars(#[parserc(parser = parse_literal_char_of_byte_str)] I),
}

#[inline]
fn parse_literal_char_of_byte_str<I>(input: &mut I) -> Result<I, CSTError>
where
    I: CSTInput,
{
    take_while_range_from(1, |c: char| {
        c.is_ascii() && c != '"' && c != '\\' && c != '\r'
    })
    .parse(input)
    .map_err(SyntaxKind::ByteStrChar.map())
}

/// A non-raw byte string literal is a sequence of ASCII characters and escapes,
/// more information see [`The Rust Reference`]
///
/// [`The rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#railroad-BYTE_STRING_LITERAL
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ByteStrLiteral<I>
where
    I: CSTInput,
{
    /// leading character `b`,
    #[parserc(keyword = "b", crucial)]
    pub leading_char: I,
    /// delimiter start `"`
    #[parserc(keyword = "\"", crucial, map_err = PunctKind::DoubleQuote.map())]
    pub delimiter_start: I,
    /// escaped / unescaped character seqencuce.
    pub chars: Vec<ByteStrItem<I>>,
    /// delimiter end `"`
    #[parserc(keyword = "\"", map_err = PunctKind::DoubleQuote.map())]
    pub delimiter_end: I,
}

/// Raw byte string literals do not process any escapes. , more information see [`The Rust Reference`]
///
/// [`The rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#raw-byte-string-literals
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct RawByteStrLiteral<I>
where
    I: CSTInput,
{
    /// leading character `br`.
    pub leading_char: I,
    /// followed by fewer than 256 of the character `#`
    pub leading_pounds: I,
    /// delimiter start character `"`
    pub delimiter_start: I,
    /// The raw string body can contain any sequence of Unicode characters other than `U+000D` (CR).
    pub content: I,
    /// delimiter end character `"`
    pub delimiter_end: I,
    /// that preceded the opening `U+0022` (double-quote) character.
    pub tailing_pounds: I,
}

impl<I> Syntax<I> for RawByteStrLiteral<I>
where
    I: CSTInput,
{
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let leading_char = keyword("br").parse(input)?;
        let leading_pounds = take_while_range_to(256, |c| c == '#')
            .parse(input)
            .map_err(SemanticsKind::RawStringLeadingPounds.map())?;
        let delimiter_start = next('"')
            .parse(input)
            .map_err(PunctKind::DoubleQuote.map_into_fatal())?;

        let mut iter = input.iter_indices();

        while let Some((index, c)) = iter.next() {
            if c == '\r' || !c.is_ascii() {
                return Err(CSTError::Semantics(
                    SemanticsKind::ByteStrChar,
                    input.split_to(index).to_span_at(c.len_utf8()),
                ));
            }

            if c == '"' {
                if leading_pounds.len() > 0 {
                    let mut tailing = input.clone();

                    let content = tailing.split_to(index);
                    let delimiter_end = tailing.split_to(1);
                    if let Some(tailing_pounds) =
                        take_while_range(leading_pounds.len()..leading_pounds.len() + 1, |c| {
                            c == '#'
                        })
                        .ok()
                        .parse(&mut tailing)?
                    {
                        *input = tailing;

                        return Ok(Self {
                            leading_char,
                            leading_pounds,
                            delimiter_start,
                            content,
                            delimiter_end,
                            tailing_pounds,
                        });
                    }

                    continue;
                }

                let content = input.split_to(index);
                let delimiter_end = input.split_to(1);
                let tailing_pounds = input.split_to(0);

                return Ok(Self {
                    leading_char,
                    leading_pounds,
                    delimiter_start,
                    content,
                    delimiter_end,
                    tailing_pounds,
                });
            }
        }

        Err(CSTError::Syntax(
            SyntaxKind::RawStringDelimiterEnd,
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
    use parserc::{Span, syntax::SyntaxInput};

    use crate::{input::TokenStream, lexical::lit::CharLiteral};

    use super::*;

    #[test]
    fn test_ascii_escapes() {
        macro_rules! make_ascii_escape_test {
            ($ident: ident, $value: literal) => {
                assert_eq!(
                    TokenStream::from(format!("'{}'", $value).as_str()).parse(),
                    Ok(CharLiteral {
                        delimiter_start: TokenStream::from((0, "'")),
                        content: CharContent::ASCIIEscape(ASCIIEscape::$ident(TokenStream::from(
                            (1, $value)
                        ))),
                        delimiter_end: TokenStream::from((3, "'")),
                    })
                )
            };
        }

        make_ascii_escape_test!(TF, "\\t");
        make_ascii_escape_test!(CR, "\\r");
        make_ascii_escape_test!(LF, "\\n");
        make_ascii_escape_test!(BackSlash, "\\\\");
        make_ascii_escape_test!(Zero, "\\0");

        assert_eq!(
            TokenStream::from("\\ab").parse::<ASCIIEscape<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::ASCIIEscape,
                ControlFlow::Recovable,
                Span::Range(0..1)
            ))
        );
    }

    #[test]
    fn test_empty_char_literal() {
        assert_eq!(
            TokenStream::from("''").parse::<CharLiteral<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::EmptyCharLiteral,
                Span::Range(1..2)
            ))
        );
    }

    #[test]
    fn test_invalid_char_literal() {
        macro_rules! make_invalid_char_literal {
            ($value: literal) => {
                assert_eq!(
                    TokenStream::from($value).parse::<CharLiteral<_>>(),
                    Err(CSTError::Semantics(
                        SemanticsKind::CharLiteral,
                        Span::Range(1..2)
                    ))
                );
            };
        }

        // make_invalid_char_literal!("'\\'");
        make_invalid_char_literal!("'\n'");
        make_invalid_char_literal!("'\r'");
        make_invalid_char_literal!("'\t'");

        assert_eq!(
            TokenStream::from("'\\'").parse::<CharLiteral<_>>(),
            Err(CSTError::Punct(
                PunctKind::Quote,
                ControlFlow::Fatal,
                Span::Range(3..3)
            ))
        );
    }

    #[test]
    fn test_quote_escapes() {
        assert_eq!(
            TokenStream::from("\\'").parse::<QuoteEscape<_>>(),
            Ok(QuoteEscape::Single(TokenStream::from("\\'")))
        );
        assert_eq!(
            TokenStream::from("\\\"").parse::<QuoteEscape<_>>(),
            Ok(QuoteEscape::Double(TokenStream::from("\\\"")))
        );
    }

    #[test]
    fn test_hex_escape() {
        macro_rules! make_hex_escape {
            ($value: literal) => {
                assert_eq!(
                    TokenStream::from(format!("'\\u{{{}}}'", $value).as_str())
                        .parse::<CharLiteral<_>>(),
                    Ok(CharLiteral {
                        delimiter_start: TokenStream::from((0, "'")),
                        content: CharContent::UnicodeEscape(UnicodeEscape {
                            delimiter_start: TokenStream::from((1, "\\u{")),
                            digits: TokenStream::from((4, $value)),
                            delimiter_end: TokenStream::from((4 + $value.len(), "}")),
                        }),
                        delimiter_end: TokenStream::from((5 + $value.len(), "'")),
                    })
                );
            };
        }

        make_hex_escape!("a");
        make_hex_escape!("af");
        make_hex_escape!("af10");
        make_hex_escape!("af100");
    }

    #[test]
    fn char_literal_utf8() {
        assert_eq!(
            TokenStream::from("'中'").parse::<CharLiteral<_>>(),
            Ok(CharLiteral {
                delimiter_start: TokenStream::from((0, "'")),
                content: CharContent::Char(TokenStream::from((1, "中"))),
                delimiter_end: TokenStream::from((4, "'"))
            })
        );
    }

    #[test]
    fn hex_escape_out_of_range() {
        assert_eq!(
            TokenStream::from("").parse::<CharLiteral<_>>(),
            Err(CSTError::Punct(
                PunctKind::Quote,
                ControlFlow::Recovable,
                Span::Range(0..0)
            ))
        );

        assert_eq!(
            TokenStream::from("'a").parse::<CharLiteral<_>>(),
            Err(CSTError::Punct(
                PunctKind::Quote,
                ControlFlow::Fatal,
                Span::Range(2..2)
            ))
        );

        assert_eq!(
            TokenStream::from("'\\u{}'").parse::<CharLiteral<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::UnicodeEscapeDigits,
                Span::Range(4..4)
            ))
        );

        assert_eq!(
            TokenStream::from("'\\u{000000}'").parse::<CharLiteral<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::UnicodeEscapeDigits,
                Span::Range(4..10)
            ))
        );
    }

    #[test]
    fn char_literal_quote_escape() {
        assert_eq!(
            TokenStream::from("'\\''").parse::<CharLiteral<_>>(),
            Ok(CharLiteral {
                delimiter_start: TokenStream::from("'"),
                content: CharContent::QuoteEscape(QuoteEscape::Single(TokenStream::from((
                    1, "\\'"
                )))),
                delimiter_end: TokenStream::from((3, "'"))
            })
        );
        assert_eq!(
            TokenStream::from("'\\\"'").parse::<CharLiteral<_>>(),
            Ok(CharLiteral {
                delimiter_start: TokenStream::from("'"),
                content: CharContent::QuoteEscape(QuoteEscape::Double(TokenStream::from((
                    1, "\\\""
                )))),
                delimiter_end: TokenStream::from((3, "'"))
            })
        );
    }

    #[test]
    fn test_string_literal() {
        assert_eq!(
            TokenStream::from(r##""""##).parse::<StrLiteral<_>>(),
            Ok(StrLiteral {
                delimiter_start: TokenStream::from((0, "\"")),
                chars: vec![],
                delimiter_end: TokenStream::from((1, "\""))
            })
        );

        assert_eq!(
            TokenStream::from("\"\\\n\"").parse::<StrLiteral<_>>(),
            Ok(StrLiteral {
                delimiter_start: TokenStream::from((0, "\"")),
                chars: vec![StrItem::LineBreakEscape(TokenStream::from((1, "\\\n")))],
                delimiter_end: TokenStream::from((3, "\""))
            })
        );

        assert_eq!(
            TokenStream::from("\"你好\\\n世界\"").parse::<StrLiteral<_>>(),
            Ok(StrLiteral {
                delimiter_start: TokenStream::from((0, "\"")),
                chars: vec![
                    StrItem::Chars(TokenStream::from((1, "你好"))),
                    StrItem::LineBreakEscape(TokenStream::from((7, "\\\n"))),
                    StrItem::Chars(TokenStream::from((9, "世界")))
                ],
                delimiter_end: TokenStream::from((15, "\""))
            })
        );
    }

    #[test]
    fn test_raw_string() {
        assert_eq!(
            TokenStream::from(include_str!("rstr0.txt")).parse::<RawStrLiteral<_>>(),
            Ok(RawStrLiteral {
                leading_char: TokenStream::from((0, "r")),
                leading_pounds: TokenStream::from((1, "####")),
                delimiter_start: TokenStream::from((5, "\"")),
                content: TokenStream::from((6, "hello\"\"\" \\nworld")),
                delimiter_end: TokenStream::from((22, "\"")),
                tailing_pounds: TokenStream::from((23, "####"))
            })
        );

        assert_eq!(
            TokenStream::from(include_str!("rstr1.txt")).parse::<RawStrLiteral<_>>(),
            Ok(RawStrLiteral {
                leading_char: TokenStream::from((0, "r")),
                leading_pounds: TokenStream::from((1, "")),
                delimiter_start: TokenStream::from((1, "\"")),
                content: TokenStream::from((2, "hello")),
                delimiter_end: TokenStream::from((7, "\"")),
                tailing_pounds: TokenStream::from((8, ""))
            })
        );

        let raw_string_out_of_range = format!("r{}\"\"{}", "#".repeat(256), "#".repeat(256));

        assert_eq!(
            TokenStream::from(raw_string_out_of_range.as_str()).parse::<RawStrLiteral<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::RawStringLeadingPounds,
                Span::Range(1..257)
            ))
        );
    }

    #[test]
    fn test_byte_literal() {
        assert_eq!(
            TokenStream::from("b'a'").parse::<ByteLiteral<_>>(),
            Ok(ByteLiteral {
                leading_char: TokenStream::from((0, "b")),
                delimiter_start: TokenStream::from((1, "'")),
                content: ByteContent::Char(TokenStream::from((2, "a"))),
                delimiter_end: TokenStream::from((3, "'"))
            })
        );

        assert_eq!(
            TokenStream::from("b'\\x0f'").parse::<ByteLiteral<_>>(),
            Ok(ByteLiteral {
                leading_char: TokenStream::from((0, "b")),
                delimiter_start: TokenStream::from((1, "'")),
                content: ByteContent::ASCIIEscape(ASCIIEscape::Hex(TokenStream::from((
                    2, "\\x0f"
                )))),
                delimiter_end: TokenStream::from((6, "'"))
            })
        );

        assert_eq!(
            TokenStream::from("b'\\xaf'").parse::<ByteLiteral<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::ASCIIHexEscapeOutOfRange,
                Span::Range(4..7)
            ))
        );

        assert_eq!(
            TokenStream::from("b'你'").parse::<ByteLiteral<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::ByteLiteral,
                Span::Range(2..3)
            ))
        );

        assert_eq!(
            TokenStream::from("b ").parse::<ByteLiteral<_>>(),
            Err(CSTError::Punct(
                PunctKind::Quote,
                ControlFlow::Fatal,
                Span::Range(1..2)
            ))
        );

        assert_eq!(
            TokenStream::from("b''").parse::<ByteLiteral<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::EmptyByteLiteral,
                Span::Range(2..3)
            ))
        );
    }

    #[test]
    fn test_byte_string_literal() {
        assert_eq!(
            TokenStream::from(
                r##"b"a\
                ""##
            )
            .parse::<ByteStrLiteral<_>>(),
            Ok(ByteStrLiteral {
                leading_char: TokenStream::from((0, "b")),
                delimiter_start: TokenStream::from((1, "\"")),
                chars: vec![
                    ByteStrItem::Chars(TokenStream::from((2, "a"))),
                    ByteStrItem::LineBreakEscape(TokenStream::from((3, "\\\n"))),
                    ByteStrItem::Chars(TokenStream::from((5, "                ")))
                ],
                delimiter_end: TokenStream::from((21, "\""))
            })
        );
    }
}
