use parserc::{
    ControlFlow, ParseError,
    syntax::{InputSyntaxExt, Punctuated, Syntax},
};

use crate::{
    errors::{CSTError, SyntaxKind},
    generics::GenericArgument,
    input::CSTInput,
    misc::Ident,
    punct::{AngleBracketEnd, AngleBracketStart, Comma, PathSep},
};

/// Angle bracketed arguments of a path segment.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PathArguments<I>
where
    I: CSTInput,
{
    /// optional leading `::`
    pub leading_pathsep: Option<PathSep<I>>,
    /// delimiter start `<`
    pub delimiter_start: AngleBracketStart<I>,
    /// path arguments.
    pub args: Punctuated<GenericArgument<I>, Comma<I>>,
    /// delimiter end `>`
    pub delimiter_end: AngleBracketEnd<I>,
}

impl<I> Syntax<I> for PathArguments<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let leading_pathsep: Option<PathSep<_>> = input.parse()?;
        let delimiter_start: AngleBracketStart<_> = input.parse()?;
        let args = input.parse()?;
        let delimiter_end: AngleBracketEnd<_> = input.parse().map_err(|err| {
            if leading_pathsep.is_some() {
                err.into_fatal()
            } else {
                err
            }
        })?;

        Ok(Self {
            leading_pathsep,
            delimiter_start,
            args,
            delimiter_end,
        })
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.leading_pathsep.to_span() + self.delimiter_end.to_span()
    }
}

/// A segment of a path together with any path arguments on that segment.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PathSegment<I>
where
    I: CSTInput,
{
    /// segment name.
    pub ident: Ident<I>,
    /// path arguments.
    pub arguments: Option<PathArguments<I>>,
}

/// A path at which a named item is exported (e.g. std::collections::HashMap).
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(semantic = check_path)]
pub struct Path<I>
where
    I: CSTInput,
{
    /// optional leading `::`
    pub leading_pathsep: Option<PathSep<I>>,
    /// segments of path.
    pub segments: Punctuated<PathSegment<I>, PathSep<I>>,
}

fn check_path<I>(input: &mut I, path: Path<I>) -> Result<Path<I>, CSTError>
where
    I: CSTInput,
{
    if path.leading_pathsep.is_none() && path.segments.is_empty() {
        Err(CSTError::Syntax(
            SyntaxKind::Path,
            ControlFlow::Recovable,
            input.to_span_at(1),
        ))
    } else {
        Ok(path)
    }
}
