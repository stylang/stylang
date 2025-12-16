//! literal tokens, more information see [`The Rust Reference`]
//!
//! [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#literals

use parserc::{ControlFlow, Parser, keyword, syntax::Syntax};

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
            if matches!(c, '0'..='7') {
                continue;
            }

            return Err(CSTError::Syntax(
                SyntaxKind::ASCIIHexEscape,
                ControlFlow::Fatal,
                input.to_span_at(3),
            ));
        }

        if index == 1 {
            if c.is_ascii_hexdigit() {
                too_short = false;
                break;
            }

            return Err(CSTError::Syntax(
                SyntaxKind::ASCIIHexEscape,
                ControlFlow::Fatal,
                input.to_span_at(4),
            ));
        }
    }

    if too_short {
        return Err(CSTError::Syntax(
            SyntaxKind::ASCIIHexEscapeTooShort,
            ControlFlow::Fatal,
            input.to_span_at(4),
        ));
    }

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

/// Body of [`CharLiteral`]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Char<I>
where
    I: CSTInput,
{
    /// unicode escape: `\u{..}`
    UnicodeEscape(UnicodeEscape<I>),
    /// ascii escape: `\t`,`\n`,..
    ASCIIEscape(ASCIIEscape<I>),
    /// char literal with the exception of `'`,`\`,`LF`,`CR`,`TAB`
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
        Some(_) => Ok(input.split_to(1)),
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
    /// literal char value.
    pub body: Char<I>,
    /// delimiter end `'`
    #[parserc(keyword = "'", map_err = PunctKind::Quote.map())]
    pub delimiter_end: I,
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
                        body: Char::ASCIIEscape(ASCIIEscape::$ident(TokenStream::from((
                            1, $value
                        )))),
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

        make_invalid_char_literal!("'\\'");
        make_invalid_char_literal!("'\n'");
        make_invalid_char_literal!("'\r'");
        make_invalid_char_literal!("'\t'");
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
                        body: Char::UnicodeEscape(UnicodeEscape {
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
}
