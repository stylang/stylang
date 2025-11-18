use parserc::{ControlFlow, Parser, syntax::Syntax, take_till};

use crate::{
    errors::{CSTError, SyntaxKind},
    input::CSTInput,
    token::comment::{
        CommentStart, SlashSlash, SlashSlashNot, SlashSlashSlash, SlashStar, SlashStarNot,
        SlashStarStar, StarSlash,
    },
};

/// line comment.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LineComment<I>
where
    I: CSTInput,
{
    /// start token `//`
    pub start: SlashSlash<I>,
    /// line content.
    pub line: I,
}

impl<I> Syntax<I> for LineComment<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let comment_start = CommentStart::parse(input)?;

        let CommentStart::SlashSlash(start) = comment_start else {
            return Err(CSTError::Syntax(
                SyntaxKind::LineComment,
                ControlFlow::Recovable,
                comment_start.to_span(),
            ));
        };

        let line = take_till(|c: u8| c == b'\r' || c == b'\n').parse(input)?;

        Ok(Self { start, line })
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.start.to_span() + self.line.to_span()
    }
}

/// outer line document
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct OuterLineDoc<I>
where
    I: CSTInput,
{
    /// start token `///`
    pub start: SlashSlashSlash<I>,
    /// line content.
    pub line: I,
}

impl<I> Syntax<I> for OuterLineDoc<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let comment_start = CommentStart::parse(input)?;

        let CommentStart::SlashSlashSlash(start) = comment_start else {
            return Err(CSTError::Syntax(
                SyntaxKind::OuterLineDoc,
                ControlFlow::Recovable,
                comment_start.to_span(),
            ));
        };

        let line = take_till(|c: u8| c == b'\r' || c == b'\n').parse(input)?;

        Ok(Self { start, line })
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.start.to_span() + self.line.to_span()
    }
}

/// outer line document
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct InnerLineDoc<I>
where
    I: CSTInput,
{
    /// start token `//!`
    pub start: SlashSlashNot<I>,
    /// line content.
    pub line: I,
}

impl<I> Syntax<I> for InnerLineDoc<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let comment_start = CommentStart::parse(input)?;

        let CommentStart::SlashSlashNot(start) = comment_start else {
            return Err(CSTError::Syntax(
                SyntaxKind::InnerLineDoc,
                ControlFlow::Recovable,
                comment_start.to_span(),
            ));
        };

        let line = take_till(|c: u8| c == b'\r' || c == b'\n').parse(input)?;

        Ok(Self { start, line })
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.start.to_span() + self.line.to_span()
    }
}

/// block comment.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct BlockComment<I>
where
    I: CSTInput,
{
    /// start token `/*`
    pub start: SlashStar<I>,
    /// lines content.
    pub lines: I,
    /// end token `*/`
    pub end: StarSlash<I>,
}

impl<I> Syntax<I> for BlockComment<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let comment_start = CommentStart::parse(input)?;

        let CommentStart::SlashStar(start) = comment_start else {
            return Err(CSTError::Syntax(
                SyntaxKind::LineComment,
                ControlFlow::Recovable,
                comment_start.to_span(),
            ));
        };

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
                            end: StarSlash(input.split_to(2)),
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
    pub start: SlashStarStar<I>,
    /// lines content.
    pub lines: I,
    /// end token `*/`
    pub end: StarSlash<I>,
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
        let comment_start = CommentStart::parse(input)?;

        let CommentStart::SlashStarStar(start) = comment_start else {
            return Err(CSTError::Syntax(
                SyntaxKind::LineComment,
                ControlFlow::Recovable,
                comment_start.to_span(),
            ));
        };

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
                            end: StarSlash(input.split_to(2)),
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
    pub start: SlashStarNot<I>,
    /// lines content.
    pub lines: I,
    /// end token `*/`
    pub end: StarSlash<I>,
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
        let comment_start = CommentStart::parse(input)?;

        let CommentStart::SlashStarNot(start) = comment_start else {
            return Err(CSTError::Syntax(
                SyntaxKind::LineComment,
                ControlFlow::Recovable,
                comment_start.to_span(),
            ));
        };

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
                            end: StarSlash(input.split_to(2)),
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
    use parserc::{ControlFlow, Span, syntax::InputSyntaxExt};

    use crate::{
        errors::{CSTError, SyntaxKind},
        input::TokenStream,
        syntax::{
            BlockComment, InnerBlockDoc, InnerLineDoc, LineComment, OuterBlockDoc, OuterLineDoc,
        },
        token::comment::{
            SlashSlash, SlashSlashNot, SlashSlashSlash, SlashStar, SlashStarNot, SlashStarStar,
            StarSlash,
        },
    };

    #[test]
    fn test_line_comment() {
        assert_eq!(
            TokenStream::from("// hello world").parse(),
            Ok(LineComment {
                start: SlashSlash(TokenStream::from("//")),
                line: TokenStream::from((2, " hello world"))
            })
        );
    }

    #[test]
    fn test_outer_line_doc() {
        assert_eq!(
            TokenStream::from("/// hello world").parse(),
            Ok(OuterLineDoc {
                start: SlashSlashSlash(TokenStream::from("///")),
                line: TokenStream::from((3, " hello world"))
            })
        );
    }

    #[test]
    fn test_inner_line_doc() {
        assert_eq!(
            TokenStream::from("//! hello world").parse(),
            Ok(InnerLineDoc {
                start: SlashSlashNot(TokenStream::from("//!")),
                line: TokenStream::from((3, " hello world"))
            })
        );
    }

    #[test]
    fn test_block_comment() {
        assert_eq!(
            TokenStream::from("/* hello world /* */ */").parse(),
            Ok(BlockComment {
                start: SlashStar(TokenStream::from("/*")),
                lines: TokenStream::from((2, " hello world /* */ ")),
                end: StarSlash(TokenStream::from((21, "*/")))
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
                start: SlashStarStar(TokenStream::from("/**")),
                lines: TokenStream::from((3, " hello world /* */ ")),
                end: StarSlash(TokenStream::from((22, "*/")))
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
                start: SlashStarNot(TokenStream::from("/*!")),
                lines: TokenStream::from((3, " hello world /* */ ")),
                end: StarSlash(TokenStream::from((22, "*/")))
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
