//! Source code can be made conditionally compiled using the cfg
//! and cfg_attr attributes and the built-in cfg macro.
//!
//! More information see [`The Rust Reference`]
//!
//! [`The Rust Reference`]: https://doc.rust-lang.org/reference/conditional-compilation.html

use parserc::syntax::{Or, Punctuated, Syntax};

use crate::{
    input::CSTInput,
    lexical::{
        delimiter::Paren,
        ident::Ident,
        keywords::strict::{False, True},
        lit::{LitRawStr, LitStr},
        punct::{self, Comma},
    },
};

/// Each form of conditional compilation takes a configuration predicate
/// that evaluates to true or false
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/conditional-compilation.html#r-cfg.predicate
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum CfgPredicate<I>
where
    I: CSTInput,
{
    All {
        #[parserc(keyword = "all")]
        leading_keyword: I,
        predicates: Paren<I, Punctuated<CfgPredicate<I>, Comma<I>>>,
    },
    Any {
        #[parserc(keyword = "any")]
        leading_keyword: I,
        predicates: Paren<I, Punctuated<CfgPredicate<I>, Comma<I>>>,
    },
    Not {
        #[parserc(keyword = "not")]
        leading_keyword: I,
        predicate: Paren<I, Box<CfgPredicate<I>>>,
    },
    True(True<I>),
    Flase(False<I>),
    Option {
        ident: Ident<I>,
        assign: Option<(punct::Eq<I>, Or<LitStr<I>, LitRawStr<I>>)>,
    },
}

/// The cfg attribute may be used anywhere attributes are allowed.
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/conditional-compilation.html#railroad-CfgAttribute
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Cfg<I>
where
    I: CSTInput,
{
    #[parserc(keyword = "cfg")]
    pub leading_keyword: I,
    /// predicate of this attribute.
    pub predicate: Paren<I, CfgPredicate<I>>,
}

/// The `cfg_attr` attribute conditionally includes attributes based on a configuration predicate.
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/conditional-compilation.html#the-cfg_attr-attribute
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct CfgAttr<I>
where
    I: CSTInput,
{
    #[parserc(keyword = "cfg_attr")]
    pub leading_keyword: I,
    /// predicate of this attribute.
    pub predicate: Paren<I, CfgPredicate<I>>,
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, SyntaxInput};

    use crate::{
        cfg::CfgPredicate,
        input::TokenStream,
        lexical::{
            S,
            ident::NonKeywordIdent,
            lit::StrContent,
            punct::{ParenEnd, ParenStart},
        },
    };

    use super::*;

    #[test]
    fn test_predicates() {
        assert_eq!(
            TokenStream::from("all(a,b,c)").parse::<CfgPredicate<_>>(),
            Ok(CfgPredicate::All {
                leading_keyword: TokenStream::from((0, "all")),
                predicates: Delimiter {
                    start: ParenStart(None, TokenStream::from((3, "(")), None),
                    end: ParenEnd(None, TokenStream::from((9, ")")), None),
                    body: Punctuated {
                        pairs: vec![
                            (
                                CfgPredicate::Option {
                                    ident: Ident::NonKeywordIdent(NonKeywordIdent(
                                        TokenStream::from((4, "a"))
                                    )),
                                    assign: None
                                },
                                Comma(None, TokenStream::from((5, ",")), None)
                            ),
                            (
                                CfgPredicate::Option {
                                    ident: Ident::NonKeywordIdent(NonKeywordIdent(
                                        TokenStream::from((6, "b"))
                                    )),
                                    assign: None
                                },
                                Comma(None, TokenStream::from((7, ",")), None)
                            )
                        ],
                        tail: Some(Box::new(CfgPredicate::Option {
                            ident: Ident::NonKeywordIdent(NonKeywordIdent(TokenStream::from((
                                8, "c"
                            )))),
                            assign: None
                        }))
                    }
                }
            })
        );
    }

    #[test]
    fn test_cfg() {
        assert_eq!(
            TokenStream::from(r#"cfg(all(target_os = "unix", x86))"#).parse::<Cfg<_>>(),
            Ok(Cfg {
                leading_keyword: TokenStream::from((0, "cfg")),
                predicate: Delimiter {
                    start: ParenStart(None, TokenStream::from((3, "(")), None),
                    end: ParenEnd(None, TokenStream::from((32, ")")), None),
                    body: CfgPredicate::All {
                        leading_keyword: TokenStream::from((4, "all")),
                        predicates: Delimiter {
                            start: ParenStart(None, TokenStream::from((7, "(")), None),
                            end: ParenEnd(None, TokenStream::from((31, ")")), None),
                            body: Punctuated {
                                pairs: vec![(
                                    CfgPredicate::Option {
                                        ident: Ident::NonKeywordIdent(NonKeywordIdent(
                                            TokenStream::from((8, "target_os"))
                                        )),
                                        assign: Some((
                                            punct::Eq(
                                                Some(S(TokenStream::from((17, " ")))),
                                                TokenStream::from((18, "=")),
                                                Some(S(TokenStream::from((19, " "))))
                                            ),
                                            Or::First(LitStr {
                                                delimiter_start: TokenStream::from((20, "\"")),
                                                content: vec![StrContent::CharWithException(
                                                    TokenStream::from((21, "unix"))
                                                )],
                                                delimiter_end: TokenStream::from((25, "\""))
                                            })
                                        ))
                                    },
                                    Comma(
                                        None,
                                        TokenStream::from((26, ",")),
                                        Some(S(TokenStream::from((27, " "))))
                                    )
                                )],
                                tail: Some(Box::new(CfgPredicate::Option {
                                    ident: Ident::NonKeywordIdent(NonKeywordIdent(
                                        TokenStream::from((28, "x86"))
                                    )),
                                    assign: None
                                }))
                            }
                        }
                    }
                }
            })
        );
    }
}
