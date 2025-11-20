use parserc::syntax::{Delimiter, Punctuated, Syntax};

use crate::{
    input::CSTInput,
    token::{
        keyword::{All, Any, KeywordNot},
        punct::{Comma, ParenEnd, ParenStart},
    },
};

/// `cfg` attribute parser.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct AttrCfg<I>
where
    I: CSTInput,
{
    /// ident of attribute `cfg`
    #[parserc(keyword = "cfg")]
    pub ident: I,
    /// predicate of this expr.
    pub predicate: Delimiter<ParenStart<I>, ParenEnd<I>, CfgPredicate<I>>,
}

/// Predicate of `cfg` attribute.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum CfgPredicate<I>
where
    I: CSTInput,
{
    /// --cfg web3
    Web3(#[parserc(keyword = "web3")] I),
    /// --cfg test
    Test(#[parserc(keyword = "test")] I),
    /// cfg is not defined.
    Not {
        /// keyword `not`.
        #[parserc(crucial)]
        keyword: KeywordNot<I>,
        /// predicate of this expr.
        predicate: Delimiter<ParenStart<I>, ParenEnd<I>, Box<CfgPredicate<I>>>,
    },
    /// with a comma-separated list of configuration predicates.
    /// It is true if at least one of the given predicates is true.
    /// If there are no predicates, it is false.
    Any {
        /// keyword `any`.
        #[parserc(crucial)]
        keyword: Any<I>,
        /// predicate of this expr.
        predicate: Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<CfgPredicate<I>, Comma<I>>>,
    },
    All {
        /// keyword `all`.
        #[parserc(crucial)]
        keyword: All<I>,
        /// predicate of this expr.
        predicate: Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<CfgPredicate<I>, Comma<I>>>,
    },
}

#[cfg(test)]
mod tests {
    use parserc::syntax::InputSyntaxExt;

    use super::*;
    use crate::{input::TokenStream, token::S};

    #[test]
    fn test_attribute_cfg() {
        assert_eq!(
            TokenStream::from("cfg(test)").parse(),
            Ok(AttrCfg {
                ident: TokenStream::from("cfg"),
                predicate: Delimiter {
                    start: ParenStart(None, TokenStream::from((3, "(")), None),
                    end: ParenEnd(None, TokenStream::from((8, ")")), None),
                    body: CfgPredicate::Test(TokenStream::from((4, "test")))
                }
            })
        );

        assert_eq!(
            TokenStream::from("cfg(not(test))").parse(),
            Ok(AttrCfg {
                ident: TokenStream::from((0, "cfg")),
                predicate: Delimiter {
                    start: ParenStart(None, TokenStream::from((3, "(")), None),
                    end: ParenEnd(None, TokenStream::from((13, ")")), None),
                    body: CfgPredicate::Not {
                        keyword: KeywordNot(TokenStream::from((4, "not"))),
                        predicate: Delimiter {
                            start: ParenStart(None, TokenStream::from((7, "(")), None),
                            end: ParenEnd(None, TokenStream::from((12, ")")), None),
                            body: Box::new(CfgPredicate::Test(TokenStream::from((8, "test"))))
                        }
                    }
                }
            })
        );

        assert_eq!(
            TokenStream::from("cfg ( all ( test , web3 ) )").parse(),
            Ok(AttrCfg {
                ident: TokenStream::from("cfg"),
                predicate: Delimiter {
                    start: ParenStart(
                        Some(S(TokenStream::from((3, " ")))),
                        TokenStream::from((4, "(")),
                        Some(S(TokenStream::from((5, " "))))
                    ),
                    end: ParenEnd(None, TokenStream::from((26, ")")), None),
                    body: CfgPredicate::All {
                        keyword: All(TokenStream::from((6, "all"))),
                        predicate: Delimiter {
                            start: ParenStart(
                                Some(S(TokenStream::from((9, " ")))),
                                TokenStream::from((10, "(")),
                                Some(S(TokenStream::from((11, " "))))
                            ),
                            end: ParenEnd(
                                Some(S(TokenStream::from((23, " ")))),
                                TokenStream::from((24, ")")),
                                Some(S(TokenStream::from((25, " "))))
                            ),
                            body: Punctuated {
                                pairs: vec![(
                                    CfgPredicate::Test(TokenStream::from((12, "test"))),
                                    Comma(
                                        Some(S(TokenStream::from((16, " ")))),
                                        TokenStream::from((17, ",")),
                                        Some(S(TokenStream::from((18, " "))))
                                    )
                                )],
                                tail: Some(Box::new(CfgPredicate::Web3(TokenStream::from((
                                    19, "web3"
                                )))))
                            }
                        }
                    }
                }
            })
        );

        assert_eq!(
            TokenStream::from("cfg ( any ( test , web3 ) )").parse(),
            Ok(AttrCfg {
                ident: TokenStream::from("cfg"),
                predicate: Delimiter {
                    start: ParenStart(
                        Some(S(TokenStream::from((3, " ")))),
                        TokenStream::from((4, "(")),
                        Some(S(TokenStream::from((5, " "))))
                    ),
                    end: ParenEnd(None, TokenStream::from((26, ")")), None),
                    body: CfgPredicate::Any {
                        keyword: Any(TokenStream::from((6, "any"))),
                        predicate: Delimiter {
                            start: ParenStart(
                                Some(S(TokenStream::from((9, " ")))),
                                TokenStream::from((10, "(")),
                                Some(S(TokenStream::from((11, " "))))
                            ),
                            end: ParenEnd(
                                Some(S(TokenStream::from((23, " ")))),
                                TokenStream::from((24, ")")),
                                Some(S(TokenStream::from((25, " "))))
                            ),
                            body: Punctuated {
                                pairs: vec![(
                                    CfgPredicate::Test(TokenStream::from((12, "test"))),
                                    Comma(
                                        Some(S(TokenStream::from((16, " ")))),
                                        TokenStream::from((17, ",")),
                                        Some(S(TokenStream::from((18, " "))))
                                    )
                                )],
                                tail: Some(Box::new(CfgPredicate::Web3(TokenStream::from((
                                    19, "web3"
                                )))))
                            }
                        }
                    }
                }
            })
        );
    }
}
