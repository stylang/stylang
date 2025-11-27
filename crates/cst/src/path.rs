use parserc::{
    ControlFlow,
    syntax::{Delimiter, Punctuated, Syntax},
};

use crate::{
    AngleBracketEnd, AngleBracketStart, CSTError, CSTInput, Comma, GenericArgument, Ident, PathSep,
    SyntaxKind,
};

/// Angle bracketed arguments of a path segment.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PathArguments<I>
where
    I: CSTInput,
{
    /// optional leading `::`
    pub leading_pathsep: Option<PathSep<I>>,
    /// angle bracketd arguments.
    pub args: Delimiter<
        AngleBracketStart<I>,
        AngleBracketEnd<I>,
        Punctuated<GenericArgument<I>, Comma<I>>,
    >,
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
    pub arguments: PathArguments<I>,
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

fn check_path<I>(path: Path<I>) -> Result<Path<I>, CSTError>
where
    I: CSTInput,
{
    if path.leading_pathsep.is_none() && path.segments.is_empty() {
        Err(CSTError::Syntax(
            SyntaxKind::Path,
            ControlFlow::Recovable,
            path.to_span(),
        ))
    } else {
        Ok(path)
    }
}
