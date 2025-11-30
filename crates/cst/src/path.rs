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

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, InputSyntaxExt, Or, Punctuated};

    use crate::{
        AngleBracketEnd, AngleBracketStart, Eequal, GenericArgument, Ident, Path, PathArguments,
        PathSegment, PathSep, S, TokenStream, Type,
    };

    #[test]
    fn test_path() {
        assert_eq!(
            TokenStream::from("std::iter::Iteartor<Item = u8>").parse(),
            Ok(Path {
                leading_pathsep: None,
                segments: Punctuated {
                    pairs: vec![
                        (
                            PathSegment {
                                ident: Ident(TokenStream::from((0, "std"))),
                                arguments: None
                            },
                            PathSep(None, TokenStream::from((3, "::")), None)
                        ),
                        (
                            PathSegment {
                                ident: Ident(TokenStream::from((5, "iter"))),
                                arguments: None
                            },
                            PathSep(None, TokenStream::from((9, "::")), None)
                        )
                    ],
                    tail: Some(Box::new(PathSegment {
                        ident: Ident(TokenStream::from((11, "Iteartor"))),
                        arguments: Some(PathArguments {
                            leading_pathsep: None,
                            args: Delimiter {
                                start: AngleBracketStart(None, TokenStream::from((19, "<")), None),
                                end: AngleBracketEnd(None, TokenStream::from((29, ">")), None),
                                body: Punctuated {
                                    pairs: vec![],
                                    tail: Some(Box::new(GenericArgument::Associated {
                                        ident: Ident(TokenStream::from((20, "Item"))),
                                        eq: Eequal(
                                            Some(S(TokenStream::from((24, " ")))),
                                            TokenStream::from((25, "=")),
                                            Some(S(TokenStream::from((26, " "))))
                                        ),
                                        ty: Or::First(Type::U8(TokenStream::from((27, "u8"))))
                                    }))
                                }
                            }
                        })
                    }))
                }
            })
        );
    }
}
