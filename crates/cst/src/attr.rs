//! An attribute is a general, free-form metadatum that is interpreted according to name,
//! convention, language, and compiler version. Attributes are modeled on Attributes in
//! ECMA-335, with the syntax coming from ECMA-334 (C#).
//!
//! More information see [`The Rust Reference`]
//!
//! [`The Rust Reference`]: https://doc.rust-lang.org/nightly/reference/attributes.html
//!
use parserc::syntax::Syntax;

use crate::{
    input::CSTInput,
    lexical::{
        delimiter::{Bracket, Paren},
        keywords::strict::Unsafe,
        punct::{Not, Pound},
    },
    macros::invocation::DelimTokenTree,
    names::paths::SimplePath,
};

/// The attribute consists of a path to the attribute, followed by an optional delimited token
/// tree whose interpretation is defined by the attribute. Attributes other than macro attributes
/// also allow the input to be an equals sign (=) followed by an expression. See the meta item
/// syntax below for more details.
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/nightly/reference/attributes.html#railroad-Attr
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Attr<I>
where
    I: CSTInput,
{
    Unsafe {
        keyword: Unsafe<I>,
        delimiter: Paren<I, (SimplePath<I>, Option<AttrInput<I>>)>,
    },
    Safe(
        /// Path of this attribute.
        SimplePath<I>,
        /// Input of this attribute.
        Option<AttrInput<I>>,
    ),
}

/// Input of one [`Attr`]
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/nightly/reference/attributes.html#railroad-Attr
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum AttrInput<I>
where
    I: CSTInput,
{
    DelimTokenTree(DelimTokenTree<I>),
}

/// Outer attributes, written without the bang after the hash, apply to the thing that follows the attribute.
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/attributes.html#r-attributes.inner
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct OuterAttribute<I>
where
    I: CSTInput,
{
    /// punct `#`
    pub pound: Pound<I>,
    /// `[` + [`Attr`] + `]`
    pub delimiter: Bracket<I, Attr<I>>,
}

/// Outer attributes, written without the bang after the hash, apply to the thing that follows the attribute.
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/attributes.html#r-attributes.inner
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct InnerAttribute<I>
where
    I: CSTInput,
{
    /// punct `#`
    pub pound: Pound<I>,
    /// punct `!`
    pub bang: Not<I>,
    /// `[` + [`Attr`] + `]`
    pub delimiter: Bracket<I, Attr<I>>,
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, SyntaxInput};

    use crate::{
        attr::{InnerAttribute, OuterAttribute},
        input::TokenStream,
        lexical::{
            Token,
            ident::{Ident, IdentOrKeyword, NonKeywordIdent},
            punct::{BracketEnd, BracketStart, ParenEnd, ParenStart},
        },
        macros::invocation::TokenTree,
        names::paths::SimplePathSegment,
    };

    use super::*;

    #[test]
    fn test_attr() {
        assert_eq!(
            TokenStream::from("#[test]").parse::<OuterAttribute<_>>(),
            Ok(OuterAttribute {
                pound: Pound(None, TokenStream::from((0, "#")), None),
                delimiter: Delimiter {
                    start: BracketStart(None, TokenStream::from((1, "[")), None),
                    end: BracketEnd(None, TokenStream::from((6, "]")), None),
                    body: Attr::Safe(
                        SimplePath {
                            leading_sep: None,
                            first: SimplePathSegment::Ident(Ident::NonKeywordIdent(
                                NonKeywordIdent(TokenStream::from((2, "test")))
                            )),
                            rest: vec![]
                        },
                        None
                    )
                }
            })
        );
        assert_eq!(
            TokenStream::from("#[allow(non_camel_case_types)]").parse::<OuterAttribute<_>>(),
            Ok(OuterAttribute {
                pound: Pound(None, TokenStream::from((0, "#")), None),
                delimiter: Delimiter {
                    start: BracketStart(None, TokenStream::from((1, "[")), None),
                    end: BracketEnd(None, TokenStream::from((29, "]")), None),
                    body: Attr::Safe(
                        SimplePath {
                            leading_sep: None,
                            first: SimplePathSegment::Ident(Ident::NonKeywordIdent(
                                NonKeywordIdent(TokenStream::from((2, "allow")))
                            )),
                            rest: vec![]
                        },
                        Some(AttrInput::DelimTokenTree(DelimTokenTree::Paren(
                            Delimiter {
                                start: ParenStart(None, TokenStream::from((7, "(")), None),
                                end: ParenEnd(None, TokenStream::from((28, ")")), None),
                                body: vec![TokenTree::Token(Token::IdentOrKeyWord(
                                    IdentOrKeyword(TokenStream::from((8, "non_camel_case_types")))
                                ))]
                            }
                        )))
                    )
                }
            })
        );

        assert_eq!(
            TokenStream::from("#![allow(non_camel_case_types)]").parse::<InnerAttribute<_>>(),
            Ok(InnerAttribute {
                pound: Pound(None, TokenStream::from((0, "#")), None),
                bang: Not(None, TokenStream::from((1, "!")), None),
                delimiter: Delimiter {
                    start: BracketStart(None, TokenStream::from((2, "[")), None),
                    end: BracketEnd(None, TokenStream::from((30, "]")), None),
                    body: Attr::Safe(
                        SimplePath {
                            leading_sep: None,
                            first: SimplePathSegment::Ident(Ident::NonKeywordIdent(
                                NonKeywordIdent(TokenStream::from((3, "allow")))
                            )),
                            rest: vec![]
                        },
                        Some(AttrInput::DelimTokenTree(DelimTokenTree::Paren(
                            Delimiter {
                                start: ParenStart(None, TokenStream::from((8, "(")), None),
                                end: ParenEnd(None, TokenStream::from((29, ")")), None),
                                body: vec![TokenTree::Token(Token::IdentOrKeyWord(
                                    IdentOrKeyword(TokenStream::from((9, "non_camel_case_types")))
                                ))]
                            }
                        )))
                    )
                }
            })
        );

        assert_eq!(
            TokenStream::from("#[unsafe(no_mangle)]").parse::<OuterAttribute<_>>(),
            Ok(OuterAttribute {
                pound: Pound(None, TokenStream::from((0, "#")), None),
                delimiter: Delimiter {
                    start: BracketStart(None, TokenStream::from((1, "[")), None),
                    end: BracketEnd(None, TokenStream::from((19, "]")), None),
                    body: Attr::Unsafe {
                        keyword: Unsafe(TokenStream::from((2, "unsafe")), None),
                        delimiter: Delimiter {
                            start: ParenStart(None, TokenStream::from((8, "(")), None),
                            end: ParenEnd(None, TokenStream::from((18, ")")), None),
                            body: (
                                SimplePath {
                                    leading_sep: None,
                                    first: SimplePathSegment::Ident(Ident::NonKeywordIdent(
                                        NonKeywordIdent(TokenStream::from((9, "no_mangle")))
                                    )),
                                    rest: vec![]
                                },
                                None
                            )
                        }
                    }
                }
            })
        );
    }
}
