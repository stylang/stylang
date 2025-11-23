use parserc::{ControlFlow, Parser, Span, next, syntax::Syntax, take_while, take_while_range};

use crate::{CSTError, CSTInput, Digits, HexDigits, SyntaxKind};

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

    Ok(number)
}

/// A literal hex-number value.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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

/// A literal value expr.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::Lit.map_unhandle())]
pub enum Lit<I>
where
    I: CSTInput,
{
    Str(LitStr<I>),
    Number(LitNumber<I>),
    HexNumber(LitHexNumber<I>),
    Bool(LitBool<I>),
}

#[cfg(test)]
mod tests {
    use parserc::syntax::InputSyntaxExt;

    use crate::TokenStream;

    use super::*;

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
            })
        );

        assert_eq!(
            TokenStream::from("-e-10f64").parse::<LitNumber<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::LitNumber,
                ControlFlow::Fatal,
                Span::Range(0..5)
            ))
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
