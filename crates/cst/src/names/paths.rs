//! [`Paths`] are used to refer to an entity, possibly in another module or type.
//!
//! [`Paths`]: https://doc.rust-lang.org/reference/paths.html

use parserc::syntax::Syntax;

use crate::{
    errors::SyntaxKind,
    input::CSTInput,
    lexical::{
        ident::Ident,
        keywords::{Crate, SelfLower, Super},
        punct::PathSep,
    },
};

/// Segment of `SimplePath`
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/paths.html#railroad-SimplePathSegment
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::SimplePathSegment.map_unhandle())]
pub enum SimplePathSegment<I>
where
    I: CSTInput,
{
    /// keyword `super`
    Super(Super<I>),
    /// keyword `self`
    SelfLower(SelfLower<I>),
    /// keyword `crate`
    Crate(Crate<I>),
    /// segment `$crate`
    VariableCrate(#[parserc(keyword = "$")] I, Crate<I>),
    /// idenfitier
    Ident(Ident<I>),
}

/// Simple paths are used in `visibility markers`, `attributes`, `macros`, and `use` items.
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/paths.html#railroad-SimplePath
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::SimplePath.map_unhandle())]
pub struct SimplePath<I>
where
    I: CSTInput,
{
    /// optional leading path seperate `::`
    pub leading_sep: Option<PathSep<I>>,
    /// first segment
    pub first: SimplePathSegment<I>,
    /// rest segments
    pub rest: Vec<(PathSep<I>, SimplePathSegment<I>)>,
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Span, syntax::SyntaxInput};

    use crate::{
        errors::{CSTError, SyntaxKind},
        input::TokenStream,
        lexical::{
            ident::{Ident, NonKeywordIdent},
            keywords::Crate,
            punct::PathSep,
        },
        names::paths::{SimplePath, SimplePathSegment},
    };

    #[test]
    fn test_simple_path() {
        let non_simple_paths = [(
            "",
            CSTError::Syntax(
                SyntaxKind::SimplePathSegment,
                ControlFlow::Recovable,
                Span::Range(0..0),
            ),
        )];

        for (input, err) in non_simple_paths {
            assert_eq!(TokenStream::from(input).parse::<SimplePath<_>>(), Err(err));
        }

        let simple_paths = [
            (
                "$crate",
                SimplePath::<TokenStream<'_>> {
                    leading_sep: None,
                    first: SimplePathSegment::VariableCrate(
                        TokenStream::from("$"),
                        Crate(TokenStream::from((1, "crate")), None),
                    ),
                    rest: vec![],
                },
            ),
            (
                "crate",
                SimplePath::<TokenStream<'_>> {
                    leading_sep: None,
                    first: SimplePathSegment::Crate(Crate(TokenStream::from("crate"), None)),
                    rest: vec![],
                },
            ),
            (
                "::a",
                SimplePath::<TokenStream<'_>> {
                    leading_sep: Some(PathSep(None, TokenStream::from("::"), None)),
                    first: SimplePathSegment::Ident(Ident::NonKeywordIdent(NonKeywordIdent(
                        TokenStream::from((2, "a")),
                    ))),
                    rest: vec![],
                },
            ),
            (
                "::a::",
                SimplePath::<TokenStream<'_>> {
                    leading_sep: Some(PathSep(None, TokenStream::from("::"), None)),
                    first: SimplePathSegment::Ident(Ident::NonKeywordIdent(NonKeywordIdent(
                        TokenStream::from((2, "a")),
                    ))),
                    rest: vec![],
                },
            ),
        ];

        for (input, expect) in simple_paths {
            assert_eq!(
                TokenStream::from(input).parse::<SimplePath<_>>(),
                Ok(expect)
            );
        }
    }
}
