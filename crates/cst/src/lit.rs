use parserc::{ControlFlow, Parser, Span, next, syntax::Syntax, take_while, take_while_range};

use crate::{CSTError, CSTInput, SyntaxKind};

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
}
