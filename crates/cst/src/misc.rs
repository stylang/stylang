use parserc::{
    ControlFlow, Parser, keyword, next_if, syntax::Syntax, take_till, take_while,
    take_while_range_from,
};

use crate::{CSTError, CSTInput, SyntaxKind, TokenKind};

/// Ascii whitespace character sequence.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct S<I>(
    #[parserc(parser = take_while_range_from(1, |c:u8| c.is_ascii_whitespace()))] pub I,
)
where
    I: CSTInput;

macro_rules! def_punct {
    ($ident: ident, $value: literal) => {
        #[doc = "define token `"]
        #[doc = stringify!($value)]
        #[doc = "`"]
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        pub struct $ident<I>(pub Option<crate::S<I>>, pub I, pub Option<crate::S<I>>)
        where
            I: crate::input::CSTInput;

        impl<I> parserc::syntax::Syntax<I> for $ident<I>
        where
            I: crate::input::CSTInput,
        {
            #[inline]
            fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
                use parserc::Parser;
                use parserc::syntax::InputSyntaxExt;
                Ok(Self(
                    input.parse()?,
                    parserc::keyword($value)
                        .parse(input)
                        .map_err(crate::errors::TokenKind::$ident.map())?,
                    input.parse()?,
                ))
            }

            #[inline]
            fn to_span(&self) -> parserc::Span {
                self.0.to_span() + self.1.to_span() + self.2.to_span()
            }
        }
    };
}

def_punct!(BraceStart, "{");
def_punct!(BraceEnd, "}");
def_punct!(BracketStart, "[");
def_punct!(BracketEnd, "]");
def_punct!(ParenStart, "(");
def_punct!(ParenEnd, ")");
def_punct!(At, "@");
def_punct!(ArrowRight, "->");
def_punct!(Colon, ":");
def_punct!(Comma, ",");
def_punct!(Semi, ";");
def_punct!(PathSep, "::");
def_punct!(Or, "|");
def_punct!(AngleBracketStart, "<");
def_punct!(AngleBracketEnd, ">");
def_punct!(Eequal, "=");
def_punct!(Question, "?");
def_punct!(Plus, "+");

/// line comment.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LineComment<I>
where
    I: CSTInput,
{
    /// start token `//`
    pub start: I,
    /// line content.
    pub line: I,
}

impl<I> Syntax<I> for LineComment<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let start = keyword("//")
            .parse(input)
            .map_err(SyntaxKind::LineComment.map())?;

        let line = take_till(|c: u8| c == b'\r' || c == b'\n').parse(input)?;

        Ok(Self { start, line })
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.start.to_span() + self.line.to_span()
    }
}

/// outer line document
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct OuterLineDoc<I>
where
    I: CSTInput,
{
    /// start token `///`
    #[parserc(keyword = "///")]
    pub start: I,
    /// line content.
    #[parserc(take_while = |c: u8| c != b'\r' && c != b'\n')]
    pub line: I,
}

/// outer line document
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct InnerLineDoc<I>
where
    I: CSTInput,
{
    /// start token `//!`
    #[parserc(keyword = "//!")]
    pub start: I,
    /// line content.
    #[parserc(take_while = |c: u8| c != b'\r' && c != b'\n')]
    pub line: I,
}

/// block comment.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct BlockComment<I>
where
    I: CSTInput,
{
    /// start token `/*`
    pub start: I,
    /// lines content.
    pub lines: I,
    /// end token `*/`
    pub end: I,
}

impl<I> Syntax<I> for BlockComment<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let start = keyword("/*")
            .parse(input)
            .map_err(SyntaxKind::BlockComment.map())?;

        let mut iter = input.iter_indices().peekable();

        let mut start_count = 1usize;
        while let Some((_, c)) = iter.next() {
            if c == b'*' {
                if let Some((_, b'/')) = iter.peek() {
                    let (offset, _) = iter.next().unwrap();
                    start_count -= 1;

                    if start_count == 0 {
                        return Ok(Self {
                            start,
                            lines: input.split_to(offset - 1),
                            end: input.split_to(2),
                        });
                    }

                    continue;
                }
            } else if c == b'/' {
                if let Some((_, b'*')) = iter.peek() {
                    iter.next();
                    start_count += 1;
                    continue;
                }
            }
        }

        Err(CSTError::Syntax(
            SyntaxKind::BlockComment,
            ControlFlow::Fatal,
            input.to_span(),
        ))
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.start.to_span() + self.end.to_span()
    }
}

/// block comment.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct OuterBlockDoc<I>
where
    I: CSTInput,
{
    /// start token `/**`
    pub start: I,
    /// lines content.
    pub lines: I,
    /// end token `*/`
    pub end: I,
}

/**
 *  /** /* **/ */
 */
impl<I> Syntax<I> for OuterBlockDoc<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let start = keyword("/**")
            .parse(input)
            .map_err(SyntaxKind::OuterBlockDoc.map())?;

        let mut iter = input.iter_indices().peekable();

        let mut start_count = 1usize;
        while let Some((_, c)) = iter.next() {
            if c == b'*' {
                if let Some((_, b'/')) = iter.peek() {
                    let (offset, _) = iter.next().unwrap();
                    start_count -= 1;

                    if start_count == 0 {
                        return Ok(Self {
                            start,
                            lines: input.split_to(offset - 1),
                            end: input.split_to(2),
                        });
                    }

                    continue;
                }
            } else if c == b'/' {
                if let Some((_, b'*')) = iter.peek() {
                    iter.next();
                    start_count += 1;
                    continue;
                }
            }
        }

        Err(CSTError::Syntax(
            SyntaxKind::OuterBlockDoc,
            ControlFlow::Fatal,
            input.to_span(),
        ))
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.start.to_span() + self.end.to_span()
    }
}

/// block comment.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct InnerBlockDoc<I>
where
    I: CSTInput,
{
    /// start token `/*!`
    pub start: I,
    /// lines content.
    pub lines: I,
    /// end token `*/`
    pub end: I,
}

/**
 *  /** /* **/ */
 */
impl<I> Syntax<I> for InnerBlockDoc<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let start = keyword("/*!")
            .parse(input)
            .map_err(SyntaxKind::InnerBlockDoc.map())?;

        let mut iter = input.iter_indices().peekable();

        let mut start_count = 1usize;
        while let Some((_, c)) = iter.next() {
            if c == b'*' {
                if let Some((_, b'/')) = iter.peek() {
                    let (offset, _) = iter.next().unwrap();
                    start_count -= 1;

                    if start_count == 0 {
                        return Ok(Self {
                            start,
                            lines: input.split_to(offset - 1),
                            end: input.split_to(2),
                        });
                    }

                    continue;
                }
            } else if c == b'/' {
                if let Some((_, b'*')) = iter.peek() {
                    iter.next();
                    start_count += 1;
                    continue;
                }
            }
        }

        Err(CSTError::Syntax(
            SyntaxKind::InnerBlockDoc,
            ControlFlow::Fatal,
            input.to_span(),
        ))
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.start.to_span() + self.end.to_span()
    }
}

/// Ident of type/variable.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Ident<I>(pub I)
where
    I: crate::input::CSTInput;

impl<I> Syntax<I> for Ident<I>
where
    I: crate::input::CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let mut content = input.clone();

        next_if(|c: u8| c == b'_' || c.is_ascii_alphabetic())
            .parse(input)
            .map_err(crate::errors::TokenKind::Ident.map())?;

        let other = take_while(|c: u8| c == b'_' || c.is_ascii_alphanumeric())
            .parse(input)
            .map_err(crate::errors::TokenKind::Ident.map())?;

        let content = content.split_to(1 + other.len());

        Ok(Self(content))
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

/// Ident of xml tag
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct XmlIdent<I>(pub I)
where
    I: crate::input::CSTInput;

impl<I> Syntax<I> for XmlIdent<I>
where
    I: crate::input::CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let mut content = input.clone();

        next_if(|c: u8| c == b'_' || c == b'-' || c.is_ascii_alphabetic())
            .parse(input)
            .map_err(crate::errors::TokenKind::XmlIdent.map())?;

        let other = take_while(|c: u8| c == b'_' || c == b'-' || c.is_ascii_alphanumeric())
            .parse(input)
            .map_err(crate::errors::TokenKind::XmlIdent.map())?;

        let content = content.split_to(1 + other.len());

        Ok(Self(content))
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
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
            .map_err(TokenKind::LitHexDigits.map())?;

        let value = u128::from_str_radix(content.as_str(), 16).map_err(|_| {
            CSTError::Token(
                TokenKind::LitHexDigits,
                ControlFlow::Fatal,
                content.to_span(),
            )
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

#[cfg(test)]
mod tests {
    use parserc::{Span, syntax::InputSyntaxExt};

    use super::*;
    use crate::{TokenKind, TokenStream};

    #[test]
    fn test_ident() {
        assert_eq!(
            TokenStream::from("-hello").parse::<Ident<_>>(),
            Err(CSTError::Token(
                TokenKind::Ident,
                ControlFlow::Recovable,
                Span::Range(0..1)
            ))
        );

        assert_eq!(
            TokenStream::from("9hello").parse::<XmlIdent<_>>(),
            Err(CSTError::Token(
                TokenKind::XmlIdent,
                ControlFlow::Recovable,
                Span::Range(0..1)
            ))
        );
    }

    #[test]
    fn s() {
        assert_eq!(
            TokenStream::from("\t \r\n_hell9o_10-").parse(),
            Ok(S(TokenStream::from("\t \r\n")))
        );
    }

    #[test]
    fn test_line_comment() {
        assert_eq!(
            TokenStream::from("// hello world").parse(),
            Ok(LineComment {
                start: TokenStream::from("//"),
                line: TokenStream::from((2, " hello world"))
            })
        );
    }

    #[test]
    fn test_outer_line_doc() {
        assert_eq!(
            TokenStream::from("/// hello world").parse(),
            Ok(OuterLineDoc {
                start: TokenStream::from("///"),
                line: TokenStream::from((3, " hello world"))
            })
        );
    }

    #[test]
    fn test_inner_line_doc() {
        assert_eq!(
            TokenStream::from("//! hello world").parse(),
            Ok(InnerLineDoc {
                start: TokenStream::from("//!"),
                line: TokenStream::from((3, " hello world"))
            })
        );
    }

    #[test]
    fn test_block_comment() {
        assert_eq!(
            TokenStream::from("/* hello world /* */ */").parse(),
            Ok(BlockComment {
                start: TokenStream::from("/*"),
                lines: TokenStream::from((2, " hello world /* */ ")),
                end: TokenStream::from((21, "*/"))
            })
        );

        assert_eq!(
            TokenStream::from("/* hello world /* */").parse::<BlockComment<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::BlockComment,
                ControlFlow::Fatal,
                Span::Range(2..20)
            ))
        );
    }

    #[test]
    fn test_outer_block_doc() {
        assert_eq!(
            TokenStream::from("/** hello world /* */ */").parse(),
            Ok(OuterBlockDoc {
                start: TokenStream::from("/**"),
                lines: TokenStream::from((3, " hello world /* */ ")),
                end: TokenStream::from((22, "*/"))
            })
        );

        assert_eq!(
            TokenStream::from("/** hello world /* */").parse::<OuterBlockDoc<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::OuterBlockDoc,
                ControlFlow::Fatal,
                Span::Range(3..21)
            ))
        );
    }

    #[test]
    fn test_inner_block_doc() {
        assert_eq!(
            TokenStream::from("/*! hello world /* */ */").parse(),
            Ok(InnerBlockDoc {
                start: TokenStream::from("/*!"),
                lines: TokenStream::from((3, " hello world /* */ ")),
                end: TokenStream::from((22, "*/"))
            })
        );

        assert_eq!(
            TokenStream::from("/*! hello world /* */").parse::<InnerBlockDoc<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::InnerBlockDoc,
                ControlFlow::Fatal,
                Span::Range(3..21)
            ))
        );
    }
}
