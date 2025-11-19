use parserc::{
    ControlFlow, Parser,
    syntax::{Byte, InputSyntaxExt, Or, Syntax},
    take_while, take_while_range, take_while_range_from,
};

use crate::{
    errors::{CSTError, OverflowKind, SyntaxKind},
    input::CSTInput,
    token::{
        S,
        keyword::Rgb,
        lit::Digits,
        punct::{Comma, DoubleQuote, ParenEnd, ParenStart, Pound, SingleQuote},
    },
};

/// Parser for rgb component value.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct RgbComponent<I>
where
    I: CSTInput,
{
    /// input of this component.
    pub input: I,
    /// parsed value.
    pub value: u8,
}

impl<I> Syntax<I> for RgbComponent<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let digits = Digits::parse(input).map_err(SyntaxKind::RgbComponent.map())?;

        if digits.value > u8::MAX as u128 {
            return Err(crate::errors::CSTError::Overflow(
                OverflowKind::RgbComponent,
                digits.to_span(),
            ));
        }

        Ok(Self {
            input: digits.input,
            value: digits.value as u8,
        })
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.input.to_span()
    }
}

/// Literial rgb value.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LitRgb<I>
where
    I: CSTInput,
{
    /// leading keyword `rgb`
    #[parserc(crucial)]
    pub leading_keyword: (Rgb<I>, Option<S<I>>),
    /// leading delimiter `(`
    pub paren_start: (ParenStart<I>, Option<S<I>>),
    /// literial value of red component.
    pub red: (RgbComponent<I>, Option<S<I>>, Comma<I>, Option<S<I>>),
    /// literial value of green component.
    pub green: (RgbComponent<I>, Option<S<I>>, Comma<I>, Option<S<I>>),
    /// literial value of blue component.
    pub blue: (
        RgbComponent<I>,
        Option<S<I>>,
        Option<Comma<I>>,
        Option<S<I>>,
    ),
    /// tailing delimiter `)`
    pub paren_end: ParenEnd<I>,
}

/// Literal hex rgb value, `#fff, #00ff00,...`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LitHexRgb<I>
where
    I: CSTInput,
{
    /// leading punct `#`
    pub leading_pound: Pound<I>,
    /// red component,
    pub red: (I, u8),
    /// green component,
    pub green: (I, u8),
    /// blue component,
    pub blue: (I, u8),
}

impl<I> Syntax<I> for LitHexRgb<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let leading_pound = Pound::parse(input).map_err(SyntaxKind::LitHexRgb.map())?;

        let mut hexdigits = take_while_range_from(3, |c: u8| c.is_ascii_hexdigit())
            .parse(input)
            .map_err(SyntaxKind::LitHexRgb.map_fatal())?;

        match hexdigits.len() {
            3 => {
                let bytes = hexdigits.as_bytes();

                let r = (bytes[0] as char).to_digit(16).unwrap();
                let g = (bytes[1] as char).to_digit(16).unwrap();
                let b = (bytes[2] as char).to_digit(16).unwrap();

                return Ok(Self {
                    leading_pound,
                    red: (hexdigits.split_to(1), (r + r * 16) as u8),
                    green: (hexdigits.split_to(1), (g + g * 16) as u8),
                    blue: (hexdigits.split_to(1), (b + b * 16) as u8),
                });
            }
            6 => {
                let bytes = hexdigits.as_bytes();

                let r = (bytes[0] as char).to_digit(16).unwrap() * 16
                    + (bytes[1] as char).to_digit(16).unwrap();
                let g = (bytes[2] as char).to_digit(16).unwrap() * 16
                    + (bytes[3] as char).to_digit(16).unwrap();
                let b = (bytes[4] as char).to_digit(16).unwrap() * 16
                    + (bytes[5] as char).to_digit(16).unwrap();

                return Ok(Self {
                    leading_pound,
                    red: (hexdigits.split_to(2), r as u8),
                    green: (hexdigits.split_to(2), g as u8),
                    blue: (hexdigits.split_to(2), b as u8),
                });
            }
            _ => {
                return Err(CSTError::Syntax(
                    SyntaxKind::LitHexRgb,
                    ControlFlow::Fatal,
                    hexdigits.to_span(),
                ));
            }
        }
    }

    fn to_span(&self) -> parserc::Span {
        self.leading_pound.to_span() + self.blue.0.to_span()
    }
}

/// A literal string value
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LitStr<I>
where
    I: CSTInput,
{
    /// leading raw string flag.
    pub leading_raw_flag: Option<Byte<I, b'r'>>,
    /// leading pounds chars.
    pub leading_pounds: I,
    /// leading quote char `"` or `'`
    pub leading_quote: Or<SingleQuote<I>, DoubleQuote<I>>,
    /// escape or raw literal string content.
    pub content: I,
    /// tailing quote char `"` or `'`
    pub tailing_quote: Or<SingleQuote<I>, DoubleQuote<I>>,
    /// tailing pounds chars.
    pub tailing_pounds: I,
}

impl<I> Syntax<I> for LitStr<I>
where
    I: CSTInput,
{
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let leading_raw_flag: Option<Byte<_, b'r'>> =
            input.parse().map_err(SyntaxKind::LitStr.map())?;

        let (leading_pounds, escape) = if leading_raw_flag.is_some() {
            (take_while(|c| c == b'#').parse(input)?, false)
        } else {
            (input.split_to(0), true)
        };

        let leading_quote: Or<SingleQuote<_>, DoubleQuote<_>> =
            input.parse().map_err(SyntaxKind::LitStr.map())?;

        let leading_quote_c = match &leading_quote {
            Or::First(_) => b'\'',
            Or::Second(_) => b'"',
        };

        let mut iter = input.iter_indices();

        let mut escape_next_c = false;

        while let Some((offset, c)) = iter.next() {
            if escape && escape_next_c {
                escape_next_c = false;
                continue;
            }

            if escape && c == b'\\' {
                escape_next_c = true;
                continue;
            }

            if c == leading_quote_c {
                if leading_pounds.is_empty() {
                    return Ok(Self {
                        leading_raw_flag,
                        leading_pounds,
                        leading_quote,
                        content: input.split_to(offset),
                        tailing_quote: input.parse().unwrap(),
                        tailing_pounds: input.split_to(0),
                    });
                }

                let mut tailing = input.clone().split_off(offset + 1);

                if let Some(tailing_pounds) =
                    take_while_range(leading_pounds.len()..leading_pounds.len(), |c| c == b'#')
                        .ok()
                        .parse(&mut tailing)?
                {
                    let content = input.split_to(offset);
                    let tailing_quote = input.parse().unwrap();
                    input.split_to(tailing_pounds.len());

                    return Ok(Self {
                        leading_raw_flag,
                        leading_pounds,
                        leading_quote,
                        content,
                        tailing_quote,
                        tailing_pounds: tailing_pounds,
                    });
                }
            }
        }

        Err(CSTError::Syntax(
            SyntaxKind::TailingQuote,
            ControlFlow::Fatal,
            input.to_span(),
        ))
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.leading_raw_flag.to_span()
            + self.leading_quote.to_span()
            + self.tailing_quote.to_span()
            + self.tailing_pounds.to_span()
    }
}

#[cfg(test)]
mod tests {
    use parserc::{Span, syntax::InputSyntaxExt};

    use super::*;
    use crate::{errors::CSTError, input::TokenStream};

    #[test]
    fn test_rgb() {
        assert_eq!(
            TokenStream::from("rgb(1,255,1)").parse(),
            Ok(LitRgb {
                leading_keyword: (Rgb(TokenStream::from("rgb")), None),
                paren_start: (ParenStart(TokenStream::from((3, "("))), None),
                red: (
                    RgbComponent {
                        input: TokenStream::from((4, "1")),
                        value: 1
                    },
                    None,
                    Comma(TokenStream::from((5, ","))),
                    None
                ),
                green: (
                    RgbComponent {
                        input: TokenStream::from((6, "255")),
                        value: 255
                    },
                    None,
                    Comma(TokenStream::from((9, ","))),
                    None
                ),
                blue: (
                    RgbComponent {
                        input: TokenStream::from((10, "1")),
                        value: 1
                    },
                    None,
                    None,
                    None
                ),
                paren_end: ParenEnd(TokenStream::from((11, ")")))
            })
        );

        assert_eq!(
            TokenStream::from("rgb(1,255,1,)").parse(),
            Ok(LitRgb {
                leading_keyword: (Rgb(TokenStream::from("rgb")), None),
                paren_start: (ParenStart(TokenStream::from((3, "("))), None),
                red: (
                    RgbComponent {
                        input: TokenStream::from((4, "1")),
                        value: 1
                    },
                    None,
                    Comma(TokenStream::from((5, ","))),
                    None
                ),
                green: (
                    RgbComponent {
                        input: TokenStream::from((6, "255")),
                        value: 255
                    },
                    None,
                    Comma(TokenStream::from((9, ","))),
                    None
                ),
                blue: (
                    RgbComponent {
                        input: TokenStream::from((10, "1")),
                        value: 1
                    },
                    None,
                    Some(Comma(TokenStream::from((11, ",")))),
                    None
                ),
                paren_end: ParenEnd(TokenStream::from((12, ")")))
            })
        );

        assert_eq!(
            TokenStream::from("rgb(256,255,1)").parse::<LitRgb<_>>(),
            Err(CSTError::Overflow(
                OverflowKind::RgbComponent,
                Span::Range(4..7)
            ))
        );
    }

    #[test]
    fn test_hex_rgb() {
        assert_eq!(
            TokenStream::from("#fff").parse(),
            Ok(LitHexRgb {
                leading_pound: Pound(TokenStream::from("#")),
                red: (TokenStream::from((1, "f")), 0xff),
                green: (TokenStream::from((2, "f")), 0xff),
                blue: (TokenStream::from((3, "f")), 0xff)
            })
        );

        assert_eq!(
            TokenStream::from("#f00aa0").parse(),
            Ok(LitHexRgb {
                leading_pound: Pound(TokenStream::from("#")),
                red: (TokenStream::from((1, "f0")), 0xf0),
                green: (TokenStream::from((3, "0a")), 0x0a),
                blue: (TokenStream::from((5, "a0")), 0xa0)
            })
        );

        assert_eq!(
            TokenStream::from("#f034").parse::<LitHexRgb<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::LitHexRgb,
                ControlFlow::Fatal,
                Span::Range(1..5)
            ))
        );

        assert_eq!(
            TokenStream::from("#f03411111").parse::<LitHexRgb<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::LitHexRgb,
                ControlFlow::Fatal,
                Span::Range(1..10)
            ))
        );
    }

    #[test]
    fn test_lit_str() {
        assert_eq!(
            TokenStream::from(r#""I'm writing \" \x52\x75\x73\x74!""#).parse(),
            Ok(LitStr {
                leading_raw_flag: None,
                leading_pounds: TokenStream::from(""),
                leading_quote: Or::Second(DoubleQuote(TokenStream::from("\""))),
                content: TokenStream::from((1, r#"I'm writing \" \x52\x75\x73\x74!"#)),
                tailing_quote: Or::Second(DoubleQuote(TokenStream::from((33, "\"")))),
                tailing_pounds: TokenStream::from((34, ""))
            })
        );

        assert_eq!(
            TokenStream::from(r#"'I\'m writing \" \x52\x75\x73\x74!'"#).parse(),
            Ok(LitStr {
                leading_raw_flag: None,
                leading_pounds: TokenStream::from(""),
                leading_quote: Or::First(SingleQuote(TokenStream::from("'"))),
                content: TokenStream::from((1, r#"I\'m writing \" \x52\x75\x73\x74!"#)),
                tailing_quote: Or::First(SingleQuote(TokenStream::from((34, "'")))),
                tailing_pounds: TokenStream::from((35, ""))
            })
        );

        assert_eq!(
            TokenStream::from(r#"r"Escapes don't work here: \x3F \u{211D}""#).parse(),
            Ok(LitStr {
                leading_raw_flag: Some(Byte(TokenStream::from("r"))),
                leading_pounds: TokenStream::from((1, "")),
                leading_quote: Or::Second(DoubleQuote(TokenStream::from((1, "\"")))),
                content: TokenStream::from((2, r#"Escapes don't work here: \x3F \u{211D}"#)),
                tailing_quote: Or::Second(DoubleQuote(TokenStream::from((40, "\"")))),
                tailing_pounds: TokenStream::from((41, ""))
            })
        );

        assert_eq!(
            TokenStream::from("r###\"A string with \"# in it. And even \"##!\"###").parse(),
            Ok(LitStr {
                leading_raw_flag: Some(Byte(TokenStream::from("r"))),
                leading_pounds: TokenStream::from((1, "###")),
                leading_quote: Or::Second(DoubleQuote(TokenStream::from((4, "\"")))),
                content: TokenStream::from((5, "A string with \"# in it. And even \"##!")),
                tailing_quote: Or::Second(DoubleQuote(TokenStream::from((42, "\"")))),
                tailing_pounds: TokenStream::from((43, "###"))
            })
        );

        assert_eq!(
            TokenStream::from(r"'hello").parse::<LitStr<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::TailingQuote,
                ControlFlow::Fatal,
                Span::Range(1..6)
            ))
        );

        assert_eq!(
            TokenStream::from(r#"'hello""#).parse::<LitStr<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::TailingQuote,
                ControlFlow::Fatal,
                Span::Range(1..7)
            ))
        );
    }
}
