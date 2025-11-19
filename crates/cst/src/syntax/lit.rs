use parserc::{ControlFlow, Parser, syntax::Syntax, take_while_range_from};

use crate::{
    errors::{CSTError, OverflowKind, SyntaxKind},
    input::CSTInput,
    token::{
        S,
        keyword::Rgb,
        lit::Digits,
        punct::{Comma, ParenEnd, ParenStart, Pound},
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
#[parserc(map_err = SyntaxKind::Rgb.map())]
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

/// Literial hex rgb value, `#fff, #00ff00,...`
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
        let leading_pound = Pound::parse(input).map_err(SyntaxKind::HexRgb.map())?;

        let mut hexdigits = take_while_range_from(3, |c: u8| c.is_ascii_hexdigit())
            .parse(input)
            .map_err(SyntaxKind::HexRgb.map_fatal())?;

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
                    SyntaxKind::HexRgb,
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
                SyntaxKind::HexRgb,
                ControlFlow::Fatal,
                Span::Range(1..5)
            ))
        );

        assert_eq!(
            TokenStream::from("#f03411111").parse::<LitHexRgb<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::HexRgb,
                ControlFlow::Fatal,
                Span::Range(1..10)
            ))
        );
    }
}
