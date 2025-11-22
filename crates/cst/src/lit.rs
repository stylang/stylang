use parserc::{
    ControlFlow, Parser, Span, next, syntax::Syntax, take_while, take_while_range,
    take_while_range_from,
};

use crate::{CSTError, CSTInput, SemanticsKind, SyntaxKind, TokenKind};

/// A literal string value.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LitStr<I>
where
    I: CSTInput,
{
    /// raw string flag `r`
    pub leading_flag: Option<I>,
    /// leading `#` characters.
    pub leading_pounds: I,
    /// char `"`
    pub delimiter_start: I,
    /// string content.
    pub content: I,
    /// char `"`
    pub delimiter_end: I,
    /// tailing `#` characters.
    pub tailing_pounds: I,
}

impl<I> Syntax<I> for LitStr<I>
where
    I: CSTInput,
{
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let leading_flag = next(b'r')
            .ok()
            .parse(input)
            .map_err(SyntaxKind::LitStr.map())?;

        let (leading_pounds, escape) = if leading_flag.is_some() {
            (take_while(|c| c == b'#').parse(input)?, false)
        } else {
            (input.split_to(0), true)
        };

        let delimiter_start = next(b'"').parse(input).map_err(SyntaxKind::LitStr.map())?;

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

            if c == b'"' {
                if leading_pounds.is_empty() {
                    return Ok(Self {
                        leading_flag,
                        leading_pounds,
                        delimiter_start,
                        content: input.split_to(offset),
                        delimiter_end: next(b'"').parse(input).unwrap(),
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
                    let delimiter_end = next(b'"').parse(input).unwrap();
                    input.split_to(tailing_pounds.len());

                    return Ok(Self {
                        leading_flag,
                        leading_pounds,
                        delimiter_start,
                        content,
                        delimiter_end,
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
        self.leading_flag
            .as_ref()
            .map(|input| input.to_span())
            .unwrap_or(Span::None)
            + self.delimiter_start.to_span()
            + self.delimiter_end.to_span()
            + self.tailing_pounds.to_span()
    }
}

/// Unit of literal numbers.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Unit<I>
where
    I: CSTInput,
{
    I8(#[parserc(keyword = "i8")] I),
    I16(#[parserc(keyword = "i16")] I),
    I32(#[parserc(keyword = "i32")] I),
    I64(#[parserc(keyword = "i64")] I),
    I128(#[parserc(keyword = "i128")] I),
    I256(#[parserc(keyword = "i256")] I),
    U8(#[parserc(keyword = "u8")] I),
    U16(#[parserc(keyword = "u16")] I),
    U32(#[parserc(keyword = "u32")] I),
    U64(#[parserc(keyword = "u64")] I),
    U128(#[parserc(keyword = "u128")] I),
    U256(#[parserc(keyword = "u256")] I),
    F16(#[parserc(keyword = "f16")] I),
    F32(#[parserc(keyword = "f32")] I),
    F64(#[parserc(keyword = "f64")] I),
    Nanosecond(#[parserc(keyword = "ns")] I),
    Microsecond(#[parserc(keyword = "us")] I),
    Millisecond(#[parserc(keyword = "ms")] I),
    Minute(#[parserc(keyword = "mins")] I),
    Deg(#[parserc(keyword = "deg")] I),
    Em(#[parserc(keyword = "em")] I),
    Ex(#[parserc(keyword = "ex")] I),
    Px(#[parserc(keyword = "px")] I),
    In(#[parserc(keyword = "in")] I),
    Cm(#[parserc(keyword = "cm")] I),
    Mm(#[parserc(keyword = "mm")] I),
    Pt(#[parserc(keyword = "pt")] I),
    Pc(#[parserc(keyword = "pc")] I),
    Percent(#[parserc(keyword = "%")] I),
    Grad(#[parserc(keyword = "grad")] I),
    Rad(#[parserc(keyword = "rad")] I),
    Khz(#[parserc(keyword = "khz")] I),
    Hz(#[parserc(keyword = "hz")] I),
    Day(#[parserc(keyword = "d")] I),
    Second(#[parserc(keyword = "s")] I),
    Hour(#[parserc(keyword = "h")] I),
}

/// literial digits.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Digits<I>
where
    I: CSTInput,
{
    pub input: I,
    pub value: u128,
}

impl<I> Syntax<I> for Digits<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let content = take_while_range_from(1, |c: u8| c.is_ascii_digit())
            .parse(input)
            .map_err(TokenKind::LitDigits.map())?;

        let value = u128::from_str_radix(content.as_str(), 10).map_err(|_| {
            CSTError::Token(TokenKind::LitDigits, ControlFlow::Fatal, content.to_span())
        })?;

        Ok(Self {
            input: content,
            value,
        })
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.input.to_span()
    }
}

/// literial hex-digits.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct HexDigits<I>
where
    I: CSTInput,
{
    pub input: I,
    pub value: u128,
}

impl<I> Syntax<I> for HexDigits<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let content = take_while_range_from(1, |c: u8| c.is_ascii_hexdigit())
            .parse(input)
            .map_err(TokenKind::LitDigits.map())?;

        let value = u128::from_str_radix(content.as_str(), 16).map_err(|_| {
            CSTError::Token(TokenKind::LitDigits, ControlFlow::Fatal, content.to_span())
        })?;

        Ok(Self {
            input: content,
            value,
        })
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.input.to_span()
    }
}

/// keyword `+` or `-`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Sign<I>(#[parserc(parser = next(b'+').or(next(b'-')))] pub I)
where
    I: CSTInput;

/// character `.`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(char = b'.')]
pub struct DecimalPoint<I>(pub I)
where
    I: CSTInput;

/// keyword `E` or `e`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct KeywordExp<I>(#[parserc(parser = next(b'E').or(next(b'e')))] pub I)
where
    I: CSTInput;

/// Literal number
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(semantic = number_semantic_check)]
pub struct LitNumber<I>
where
    I: CSTInput,
{
    /// Signature char `+` or `-`
    pub sign: Option<Sign<I>>,
    /// integer part of this number.
    pub trunc: Option<Digits<I>>,
    /// fractional part of number.
    pub fract: Option<(DecimalPoint<I>, Digits<I>)>,
    /// exponential part of number.
    pub exp: Option<(KeywordExp<I>, Option<Sign<I>>, Digits<I>)>,
    /// unit part of number.
    pub unit: Option<Unit<I>>,
}

/// Perform semantic checks on the parsed number
fn number_semantic_check<I>(number: LitNumber<I>) -> Result<LitNumber<I>, CSTError>
where
    I: CSTInput,
{
    if number.trunc.is_none() && number.fract.is_none() {
        return Err(CSTError::Syntax(
            SyntaxKind::LitNumber,
            ControlFlow::Fatal,
            number.to_span(),
        ));
    }

    if number.fract.is_some() || number.exp.is_some() {
        match &number.unit {
            Some(Unit::I8(_)) | Some(Unit::I16(_)) | Some(Unit::I32(_)) | Some(Unit::I64(_))
            | Some(Unit::I128(_)) | Some(Unit::I256(_)) | Some(Unit::U8(_))
            | Some(Unit::U16(_)) | Some(Unit::U32(_)) | Some(Unit::U64(_))
            | Some(Unit::U128(_)) | Some(Unit::U256(_)) => {
                return Err(CSTError::Semantics(
                    SemanticsKind::Unit,
                    number.unit.to_span(),
                ));
            }
            _ => {}
        }
    }

    if number.sign.as_ref().map(|input| input.0.as_str()) == Some("-") {
        match &number.unit {
            Some(Unit::U8(_)) | Some(Unit::U16(_)) | Some(Unit::U32(_)) | Some(Unit::U64(_))
            | Some(Unit::U128(_)) | Some(Unit::U256(_)) => {
                return Err(CSTError::Semantics(
                    SemanticsKind::Unit,
                    number.unit.to_span(),
                ));
            }
            _ => {}
        }
    }

    Ok(number)
}

/// A literal hex-number value.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(semantic = hex_number_semantic_check)]
pub struct LitHexNumber<I>
where
    I: CSTInput,
{
    /// signature part of number.
    pub sign: Option<Sign<I>>,
    /// leading `0x`
    #[parserc(crucial, keyword = "0x")]
    pub leading_0x: I,
    /// hex-digits sequence.
    #[parserc(map_err = SyntaxKind::LitHexNumber.map())]
    pub digits: HexDigits<I>,
    /// optional unit part.
    pub unit: Option<Unit<I>>,
}

/// Perform semantic checks on the parsed number
fn hex_number_semantic_check<I>(number: LitHexNumber<I>) -> Result<LitHexNumber<I>, CSTError>
where
    I: CSTInput,
{
    if number.sign.as_ref().map(|input| input.0.as_str()) == Some("-") {
        match &number.unit {
            Some(Unit::U8(_)) | Some(Unit::U16(_)) | Some(Unit::U32(_)) | Some(Unit::U64(_))
            | Some(Unit::U128(_)) | Some(Unit::U256(_)) => {
                return Err(CSTError::Semantics(
                    SemanticsKind::Unit,
                    number.unit.to_span(),
                ));
            }
            _ => {}
        }
    }

    Ok(number)
}

/// literal bool value.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::LitBool.map_unhandle())]
pub enum LitBool<I>
where
    I: CSTInput,
{
    True(#[parserc(keyword = "true")] I),
    False(#[parserc(keyword = "false")] I),
}

/// Literal hex rgb value, `#fff, #00ff00,...`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LitRgb<I>
where
    I: CSTInput,
{
    /// leading punct `#`
    pub leading_pound: I,
    /// red component,
    pub red: (I, u8),
    /// green component,
    pub green: (I, u8),
    /// blue component,
    pub blue: (I, u8),
}

impl<I> Syntax<I> for LitRgb<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let leading_pound = next(b'#')
            .parse(input)
            .map_err(SyntaxKind::LitHexRgb.map())?;

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

/// A literal value expr.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::Lit.map_unhandle())]
pub enum Lit<I>
where
    I: CSTInput,
{
    Rgb(LitRgb<I>),
    Str(LitStr<I>),
    Number(LitNumber<I>),
    HexNumber(LitHexNumber<I>),
    Bool(LitBool<I>),
}

#[cfg(test)]
mod tests {
    use parserc::syntax::InputSyntaxExt;

    use crate::{SemanticsKind, TokenStream};

    use super::*;

    #[test]
    fn test_unit() {
        macro_rules! check_unit {
            ($ident: ident, $input: literal, $match: literal) => {
                assert_eq!(
                    TokenStream::from($input).parse(),
                    Ok(Unit::$ident(TokenStream::from($match))),
                );
            };
        }

        check_unit!(I8, "i8~", "i8");
        check_unit!(I16, "i16~", "i16");
        check_unit!(I32, "i32~", "i32");
        check_unit!(I64, "i64~", "i64");
        check_unit!(I128, "i128~", "i128");
        check_unit!(I256, "i256~", "i256");

        check_unit!(U8, "u8~", "u8");
        check_unit!(U16, "u16~", "u16");
        check_unit!(U32, "u32~", "u32");
        check_unit!(U64, "u64~", "u64");
        check_unit!(U128, "u128~", "u128");
        check_unit!(U256, "u256~", "u256");

        check_unit!(F16, "f16~", "f16");
        check_unit!(F32, "f32~", "f32");
        check_unit!(F64, "f64~", "f64");

        check_unit!(Nanosecond, "ns~", "ns");
        check_unit!(Microsecond, "us~", "us");
        check_unit!(Millisecond, "ms~", "ms");
        check_unit!(Second, "s~", "s");
        check_unit!(Minute, "minss", "mins");
        check_unit!(Hour, "hrs", "h");
        check_unit!(Day, "day", "d");
        check_unit!(Em, "em~", "em");
        check_unit!(Ex, "ex", "ex");
        check_unit!(Px, "px", "px");
        check_unit!(In, "in~", "in");
        check_unit!(Cm, "cm", "cm");
        check_unit!(Mm, "mm~", "mm");
        check_unit!(Pt, "pt", "pt");
        check_unit!(Pc, "pc", "pc");
        check_unit!(Percent, "%~", "%");

        check_unit!(Deg, "deg~", "deg");
        check_unit!(Grad, "grad~", "grad");
        check_unit!(Rad, "rad~", "rad");

        check_unit!(Hz, "hz~", "hz");
        check_unit!(Khz, "khz~", "khz");
    }

    #[test]
    fn test_hex_rgb() {
        assert_eq!(
            TokenStream::from("#fff").parse(),
            Ok(LitRgb {
                leading_pound: TokenStream::from("#"),
                red: (TokenStream::from((1, "f")), 0xff),
                green: (TokenStream::from((2, "f")), 0xff),
                blue: (TokenStream::from((3, "f")), 0xff)
            })
        );

        assert_eq!(
            TokenStream::from("#f00aa0").parse(),
            Ok(LitRgb {
                leading_pound: TokenStream::from("#"),
                red: (TokenStream::from((1, "f0")), 0xf0),
                green: (TokenStream::from((3, "0a")), 0x0a),
                blue: (TokenStream::from((5, "a0")), 0xa0)
            })
        );

        assert_eq!(
            TokenStream::from("#f034").parse::<LitRgb<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::LitHexRgb,
                ControlFlow::Fatal,
                Span::Range(1..5)
            ))
        );

        assert_eq!(
            TokenStream::from("#f03411111").parse::<LitRgb<_>>(),
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
                leading_flag: None,
                leading_pounds: TokenStream::from(""),
                delimiter_start: TokenStream::from("\""),
                content: TokenStream::from((1, r#"I'm writing \" \x52\x75\x73\x74!"#)),
                delimiter_end: TokenStream::from((33, "\"")),
                tailing_pounds: TokenStream::from((34, ""))
            })
        );

        assert_eq!(
            TokenStream::from(r#""I\'m writing \" \x52\x75\x73\x74!""#).parse(),
            Ok(LitStr {
                leading_flag: None,
                leading_pounds: TokenStream::from(""),
                delimiter_start: TokenStream::from("\""),
                content: TokenStream::from((1, r#"I\'m writing \" \x52\x75\x73\x74!"#)),
                delimiter_end: TokenStream::from((34, "\"")),
                tailing_pounds: TokenStream::from((35, ""))
            })
        );

        assert_eq!(
            TokenStream::from(r#"r"Escapes don't work here: \x3F \u{211D}""#).parse(),
            Ok(LitStr {
                leading_flag: Some(TokenStream::from("r")),
                leading_pounds: TokenStream::from((1, "")),
                delimiter_start: TokenStream::from((1, "\"")),
                content: TokenStream::from((2, r#"Escapes don't work here: \x3F \u{211D}"#)),
                delimiter_end: TokenStream::from((40, "\"")),
                tailing_pounds: TokenStream::from((41, ""))
            })
        );

        assert_eq!(
            TokenStream::from("r###\"A string with \"# in it. And even \"##!\"###").parse(),
            Ok(LitStr {
                leading_flag: Some(TokenStream::from("r")),
                leading_pounds: TokenStream::from((1, "###")),
                delimiter_start: TokenStream::from((4, "\"")),
                content: TokenStream::from((5, "A string with \"# in it. And even \"##!")),
                delimiter_end: TokenStream::from((42, "\"")),
                tailing_pounds: TokenStream::from((43, "###"))
            })
        );

        assert_eq!(
            TokenStream::from(r#""hello"#).parse::<LitStr<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::TailingQuote,
                ControlFlow::Fatal,
                Span::Range(1..6)
            ))
        );
    }

    #[test]
    fn test_number() {
        assert_eq!(
            TokenStream::from("123").parse(),
            Ok(LitNumber {
                sign: None,
                trunc: Some(Digits {
                    input: TokenStream::from("123"),
                    value: 123
                }),
                fract: None,
                exp: None,
                unit: None
            })
        );

        assert_eq!(
            TokenStream::from("123i3212").parse(),
            Ok(LitNumber {
                sign: None,
                trunc: Some(Digits {
                    input: TokenStream::from("123"),
                    value: 123
                }),
                fract: None,
                exp: None,
                unit: Some(Unit::I32(TokenStream::from((3, "i32"))))
            })
        );

        assert_eq!(
            TokenStream::from("-.123f64").parse(),
            Ok(LitNumber {
                sign: Some(Sign(TokenStream::from("-"))),
                trunc: None,
                fract: Some((
                    DecimalPoint(TokenStream::from((1, "."))),
                    Digits {
                        input: TokenStream::from((2, "123")),
                        value: 123
                    }
                )),
                exp: None,
                unit: Some(Unit::F64(TokenStream::from((5, "f64"))))
            })
        );

        assert_eq!(
            TokenStream::from("-0.123e-10f64").parse(),
            Ok(LitNumber {
                sign: Some(Sign(TokenStream::from("-"))),
                trunc: Some(Digits {
                    input: TokenStream::from((1, "0")),
                    value: 0
                }),
                fract: Some((
                    DecimalPoint(TokenStream::from((2, "."))),
                    Digits {
                        input: TokenStream::from((3, "123")),
                        value: 123
                    }
                )),
                exp: Some((
                    KeywordExp(TokenStream::from((6, "e"))),
                    Some(Sign(TokenStream::from((7, "-")))),
                    Digits {
                        input: TokenStream::from((8, "10")),
                        value: 10
                    }
                )),
                unit: Some(Unit::F64(TokenStream::from((10, "f64"))))
            })
        );

        assert_eq!(
            TokenStream::from("-e-10f64").parse::<LitNumber<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::LitNumber,
                ControlFlow::Fatal,
                Span::Range(0..8)
            ))
        );

        assert_eq!(
            TokenStream::from(".10i32").parse::<LitNumber<_>>(),
            Err(CSTError::Semantics(SemanticsKind::Unit, Span::Range(3..6)))
        );

        assert_eq!(
            TokenStream::from("-10u32").parse::<LitNumber<_>>(),
            Err(CSTError::Semantics(SemanticsKind::Unit, Span::Range(3..6)))
        );
    }

    #[test]
    fn test_hex_number() {
        assert_eq!(
            TokenStream::from("-0x10.0").parse(),
            Ok(LitHexNumber {
                sign: Some(Sign(TokenStream::from("-"))),
                leading_0x: TokenStream::from((1, "0x")),
                digits: HexDigits {
                    input: TokenStream::from((3, "10")),
                    value: 0x10
                },
                unit: None
            })
        );

        assert_eq!(
            TokenStream::from("-0x10i64").parse(),
            Ok(LitHexNumber {
                sign: Some(Sign(TokenStream::from("-"))),
                leading_0x: TokenStream::from((1, "0x")),
                digits: HexDigits {
                    input: TokenStream::from((3, "10")),
                    value: 0x10
                },
                unit: Some(Unit::I64(TokenStream::from((5, "i64"))))
            })
        );

        assert_eq!(
            TokenStream::from("0x").parse::<LitHexNumber<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::LitHexNumber,
                ControlFlow::Fatal,
                Span::Range(2..2)
            ))
        );

        assert_eq!(
            TokenStream::from("-0xffu32").parse::<LitHexNumber<_>>(),
            Err(CSTError::Semantics(SemanticsKind::Unit, Span::Range(5..8)))
        );
    }

    #[test]
    fn test_bool() {
        assert_eq!(
            TokenStream::from("true").parse(),
            Ok(LitBool::True(TokenStream::from("true")))
        );

        assert_eq!(
            TokenStream::from("false").parse(),
            Ok(LitBool::False(TokenStream::from("false")))
        );
    }
}
