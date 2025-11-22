use parserc::syntax::{Delimiter, Or, Punctuated, Syntax};

use crate::{At, Comma, Ident, LitStr, ParenEnd, ParenStart, errors::SyntaxKind, input::CSTInput};

/// `cfg` attribute.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::Cfg.map_unhandle())]
pub struct Cfg<I>
where
    I: CSTInput,
{
    /// ident of attribute `cfg`
    #[parserc(crucial, keyword = "cfg")]
    pub ident: I,
    /// predicate of this expr.
    pub predicate: Delimiter<ParenStart<I>, ParenEnd<I>, CfgPredicate<I>>,
}

/// `cfg_attr` attribute.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::Cfg.map_unhandle())]
pub struct CfgAttr<I>
where
    I: CSTInput,
{
    /// ident of attribute `cfg`
    #[parserc(crucial, keyword = "cfg_attr")]
    pub ident: I,
    /// predicate of this attribute.
    pub predicate:
        Delimiter<ParenStart<I>, ParenEnd<I>, (CfgPredicate<I>, Comma<I>, CfgAttrMeta<I>)>,
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
        #[parserc(crucial, keyword = "not")]
        keyword: I,
        /// predicate of this expr.
        predicate: Delimiter<ParenStart<I>, ParenEnd<I>, Box<CfgPredicate<I>>>,
    },
    /// with a comma-separated list of configuration predicates.
    /// It is true if at least one of the given predicates is true.
    /// If there are no predicates, it is false.
    Any {
        /// keyword `any`.
        #[parserc(crucial, keyword = "any")]
        keyword: I,
        /// predicate of this expr.
        predicate: Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<CfgPredicate<I>, Comma<I>>>,
    },
    All {
        /// keyword `all`.
        #[parserc(crucial, keyword = "all")]
        keyword: I,
        /// predicate of this expr.
        predicate: Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<CfgPredicate<I>, Comma<I>>>,
    },
}

/// attribute `contract`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::Contract.map_unhandle())]
pub struct Contract<I>
where
    I: CSTInput,
{
    /// ident of attribute `cfg`
    #[parserc(crucial, keyword = "contract")]
    pub ident: I,
    /// predicate of the `contract` attribute
    predicate: Delimiter<ParenStart<I>, ParenEnd<I>, Or<Ident<I>, LitStr<I>>>,
}

/// attribute `runtime`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::Contract.map_unhandle())]
pub struct Runtime<I>(
    /// ident of attribute `cfg`
    #[parserc(keyword = "runtime")]
    pub I,
)
where
    I: CSTInput;

/// content of `cfg_attr`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum CfgAttrMeta<I>
where
    I: CSTInput,
{
    Contract(Contract<I>),
    Runtime(Runtime<I>),
}

/// content of attribute.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum AttrMeta<I>
where
    I: CSTInput,
{
    CfgAttr(CfgAttr<I>),
    Cfg(Cfg<I>),
    Runtime(Runtime<I>),
    Contract(Contract<I>),
}

/// Compile-time structure attribute.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct OuterAttribute<I>
where
    I: CSTInput,
{
    /// leading `@` char.
    pub leading_at: At<I>,
    /// content of this attribute.
    pub meta: AttrMeta<I>,
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Span, syntax::InputSyntaxExt};

    use super::*;
    use crate::{
        S,
        errors::{CSTError, TokenKind},
        input::TokenStream,
    };

    #[test]
    fn test_attribute_cfg() {
        assert_eq!(
            TokenStream::from("cfg(test)").parse(),
            Ok(Cfg {
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
            Ok(Cfg {
                ident: TokenStream::from((0, "cfg")),
                predicate: Delimiter {
                    start: ParenStart(None, TokenStream::from((3, "(")), None),
                    end: ParenEnd(None, TokenStream::from((13, ")")), None),
                    body: CfgPredicate::Not {
                        keyword: TokenStream::from((4, "not")),
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
            Ok(Cfg {
                ident: TokenStream::from("cfg"),
                predicate: Delimiter {
                    start: ParenStart(
                        Some(S(TokenStream::from((3, " ")))),
                        TokenStream::from((4, "(")),
                        Some(S(TokenStream::from((5, " "))))
                    ),
                    end: ParenEnd(None, TokenStream::from((26, ")")), None),
                    body: CfgPredicate::All {
                        keyword: TokenStream::from((6, "all")),
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
            Ok(Cfg {
                ident: TokenStream::from("cfg"),
                predicate: Delimiter {
                    start: ParenStart(
                        Some(S(TokenStream::from((3, " ")))),
                        TokenStream::from((4, "(")),
                        Some(S(TokenStream::from((5, " "))))
                    ),
                    end: ParenEnd(None, TokenStream::from((26, ")")), None),
                    body: CfgPredicate::Any {
                        keyword: TokenStream::from((6, "any")),
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

    #[test]
    fn test_contract() {
        assert_eq!(
            TokenStream::from("contract(hello)").parse(),
            Ok(Contract {
                ident: TokenStream::from("contract"),
                predicate: Delimiter {
                    start: ParenStart(None, TokenStream::from((8, "(")), None),
                    end: ParenEnd(None, TokenStream::from((14, ")")), None),
                    body: Or::First(Ident(TokenStream::from(TokenStream::from((9, "hello")))))
                }
            })
        );

        assert_eq!(
            TokenStream::from("contract(\"0x...\")").parse(),
            Ok(Contract {
                ident: TokenStream::from("contract"),
                predicate: Delimiter {
                    start: ParenStart(None, TokenStream::from((8, "(")), None),
                    end: ParenEnd(None, TokenStream::from((16, ")")), None),
                    body: Or::Second(LitStr {
                        leading_flag: None,
                        leading_pounds: TokenStream::from((9, "")),
                        delimiter_start: TokenStream::from((9, "\"")),
                        content: TokenStream::from((10, "0x...")),
                        delimiter_end: TokenStream::from((15, "\"")),
                        tailing_pounds: TokenStream::from((16, ""))
                    }),
                }
            })
        );

        assert_eq!(
            TokenStream::from("contract").parse::<Contract<_>>(),
            Err(CSTError::Token(
                TokenKind::ParenStart,
                ControlFlow::Fatal,
                Span::Range(8..8)
            ))
        );
    }

    #[test]
    fn test_cfg_attr() {
        assert_eq!(
            TokenStream::from("@cfg_attr(all(test,web3),contract(\"0x...\"))")
                .parse::<OuterAttribute<_>>(),
            Ok(OuterAttribute {
                leading_at: At(None, TokenStream::from((0, "@")), None),
                meta: AttrMeta::CfgAttr(CfgAttr {
                    ident: TokenStream::from((1, "cfg_attr")),
                    predicate: Delimiter {
                        start: ParenStart(None, TokenStream::from((9, "(")), None),
                        end: ParenEnd(None, TokenStream::from((42, ")")), None),
                        body: (
                            CfgPredicate::All {
                                keyword: TokenStream::from((10, "all")),
                                predicate: Delimiter {
                                    start: ParenStart(None, TokenStream::from((13, "(")), None),
                                    end: ParenEnd(None, TokenStream::from((23, ")")), None),
                                    body: Punctuated {
                                        pairs: vec![(
                                            CfgPredicate::Test(TokenStream::from((14, "test"))),
                                            Comma(None, TokenStream::from((18, ",")), None)
                                        )],
                                        tail: Some(Box::new(CfgPredicate::Web3(
                                            TokenStream::from((19, "web3"))
                                        )))
                                    }
                                }
                            },
                            Comma(None, TokenStream::from((24, ",")), None),
                            CfgAttrMeta::Contract(Contract {
                                ident: TokenStream::from((25, "contract")),
                                predicate: Delimiter {
                                    start: ParenStart(None, TokenStream::from((33, "(")), None),
                                    end: ParenEnd(None, TokenStream::from((41, ")")), None),
                                    body: Or::Second(LitStr {
                                        leading_flag: None,
                                        leading_pounds: TokenStream::from((34, "")),
                                        delimiter_start: TokenStream::from((34, "\"")),
                                        content: TokenStream::from((35, "0x...")),
                                        delimiter_end: TokenStream::from((40, "\"")),
                                        tailing_pounds: TokenStream::from((41, ""))
                                    })
                                }
                            })
                        )
                    }
                })
            })
        );
    }
}
