use parserc::{ControlFlow, Parser, keyword, syntax::Syntax, take_till};

use crate::{CSTError, CSTInput, SyntaxKind};

/// A whitespace character sequence.

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Syntax)]
#[parserc(take_while = |c:u8| c.is_ascii_whitespace())]
pub struct S<I>(pub I)
where
    I: CSTInput;

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

#[cfg(test)]
mod tests {
    use parserc::{Span, syntax::InputSyntaxExt};

    use super::*;
    use crate::TokenStream;

    #[test]
    fn s() {
        assert_eq!(
            TokenStream::from(" \t\r\n").parse(),
            Ok(S(TokenStream::from(" \t\r\n")))
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
