//! `macro_rules` allows users to define syntax extension in a declarative way.
//! We call such extensions `macros by example` or simply `macros`.

use parserc::{
    ControlFlow,
    syntax::{Punctuated, Syntax},
};

use crate::{
    errors::{CSTError, SyntaxKind},
    input::CSTInput,
    lexical::{
        Token,
        delimiter::{Brace, Bracket, Paren},
        ident::{Ident, IdentOrKeyword, RawIdent},
        punct::{
            Colon, Dollar, FatArrow, Not, ParenEnd, ParenStart, Plus, Punct, Question, Semi, Star,
        },
    },
    macros::invocation::DelimTokenTree,
};

/// Each macro by example has a name, and one or more rules. Each rule has two parts: a matcher,
/// describing the syntax that it matches, and a transcriber, describing the syntax that will
/// replace a successfully matched invocation. Both the matcher and the transcriber must be
/// surrounded by delimiters. Macros can expand to expressions, statements,
/// items (including traits, impls, and foreign items), types, or patterns.
///
/// see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#grammar-PUNCTUATION
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct MacroRulesDefinition<I>
where
    I: CSTInput,
{
    /// keyword `macro_rules`
    #[parserc(keyword = "macro_rules")]
    pub keyword: I,
    /// punct `!`
    pub not: Not<I>,
    /// example name.
    pub name: Ident<I>,
    /// body of this examples.
    pub body: MacroRulesDef<I>,
}

/// Body of [`MacroRulesDefinition`]
///
/// see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/macros-by-example.html#railroad-MacroRulesDef
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum MacroRulesDef<I>
where
    I: CSTInput,
{
    Paren(Paren<I, Punctuated<MacroRule<I>, Semi<I>>>, Semi<I>),
    Bracket(Bracket<I, Punctuated<MacroRule<I>, Semi<I>>>, Semi<I>),
    Brace(Brace<I, Punctuated<MacroRule<I>, Semi<I>>>),
}

/// One arm of macro_rules.
///
/// see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/macros-by-example.html#railroad-MacroRule
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct MacroRule<I>
where
    I: CSTInput,
{
    /// pattern of this arm.
    pub matcher: MacroMatcher<I>,
    /// punct `=>`
    pub arrow: FatArrow<I>,
    /// transcriber of this arm.
    pub trans: DelimTokenTree<I>,
}

/// Pattern of one macro rule.
///
/// see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/macros-by-example.html#railroad-MacroRule
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum MacroMatcher<I>
where
    I: CSTInput,
{
    Paren(Paren<I, Vec<MacroMatch<I>>>),
    Bracket(Bracket<I, Vec<MacroMatch<I>>>),
    Brace(Brace<I, Vec<MacroMatch<I>>>),
}

/// macro matcher parameter.
///
/// see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/macros-by-example.html#railroad-MacroMatch
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum MacroMatch<I>
where
    I: CSTInput,
{
    Matcher(Box<MacroMatcher<I>>),
    Token(#[parserc(semantic = check_macro_match_token)] Token<I>),
    Fragement {
        /// leading punct `$`
        leading_char: Dollar<I>,
        /// name of this fragment.
        ident: MacroFragIdent<I>,
        /// punct `:`
        colon: Colon<I>,
        /// spec of one macro fragement.
        sepc: MacroFragSpec<I>,
    },
    Repeat {
        /// leading punct `$`
        leading_char: Dollar<I>,
        /// delimter punct `(`
        delimiter_start: ParenStart<I>,
        /// at least one sub-macro-match.
        first_match: Box<MacroMatch<I>>,
        /// rest macro matches.
        rest_matches: Vec<MacroMatch<I>>,
        /// delimter punct `)`
        delimiter_end: ParenEnd<I>,
        /// optional seperator token.
        sep: Option<MacroRepSep<I>>,
        /// repeat operator.
        op: MacroRepOp<I>,
    },
}

#[inline]
fn check_macro_match_token<I>(_: I, token: Token<I>) -> Result<Token<I>, CSTError>
where
    I: CSTInput,
{
    match &token {
        Token::Punct(Punct::BraceStart(_))
        | Token::Punct(Punct::BraceEnd(_))
        | Token::Punct(Punct::BracketStart(_))
        | Token::Punct(Punct::BracketEnd(_))
        | Token::Punct(Punct::ParenStart(_))
        | Token::Punct(Punct::ParenEnd(_))
        | Token::Punct(Punct::Dollar(_)) => {
            return Err(CSTError::Syntax(
                SyntaxKind::MacroMatchToken,
                ControlFlow::Recovable,
                token.to_span(),
            ));
        }
        _ => {}
    }

    Ok(token)
}

/// Name of fragment.
///
/// see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/macros-by-example.html#railroad-MacroMatch
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::MacroFragIdent.map())]
pub enum MacroFragIdent<I>
where
    I: CSTInput,
{
    IdentOrKeyword(#[parserc(semantic = fragment_ident_with_except)] IdentOrKeyword<I>),
    RawIdent(RawIdent<I>),
    Underscore(#[parserc(keyword = "_")] I),
}

#[inline]
fn fragment_ident_with_except<I>(
    _: I,
    ident: IdentOrKeyword<I>,
) -> Result<IdentOrKeyword<I>, CSTError>
where
    I: CSTInput,
{
    if ident.0.as_str() == "crate" {
        return Err(CSTError::Syntax(
            SyntaxKind::MacroFragIdent,
            ControlFlow::Recovable,
            ident.to_span(),
        ));
    }

    Ok(ident)
}

/// fragment type.
///
/// see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/macros-by-example.html#railroad-MacroFragSpec
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err =  SyntaxKind::MacroFragSpec.map())]
pub enum MacroFragSpec<I>
where
    I: CSTInput,
{
    Block(#[parserc(keyword = "block")] I),
    Expr(#[parserc(keyword = "expr")] I),
    Expr2021(#[parserc(keyword = "expr_2021")] I),
    Ident(#[parserc(keyword = "ident")] I),
    Item(#[parserc(keyword = "item")] I),
    Lifetime(#[parserc(keyword = "lifetime")] I),
    Literal(#[parserc(keyword = "literal")] I),
    Meta(#[parserc(keyword = "meta")] I),
    Pat(#[parserc(keyword = "pat")] I),
    PatParam(#[parserc(keyword = "pat_param")] I),
    Path(#[parserc(keyword = "path")] I),
    Stmt(#[parserc(keyword = "stmt")] I),
    TT(#[parserc(keyword = "tt")] I),
    TY(#[parserc(keyword = "ty")] I),
    VIS(#[parserc(keyword = "vis")] I),
}

/// Seperator of macro repeat match
///
/// see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/macros-by-example.html#railroad-MacroMatch
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::MacroRepSep.map())]
pub struct MacroRepSep<I>(#[parserc(semantic = macro_rep_sep_token_with_except)] pub Token<I>)
where
    I: CSTInput;

#[inline]
fn macro_rep_sep_token_with_except<I>(_: I, token: Token<I>) -> Result<Token<I>, CSTError>
where
    I: CSTInput,
{
    match &token {
        Token::Punct(Punct::BraceStart(_))
        | Token::Punct(Punct::BraceEnd(_))
        | Token::Punct(Punct::BracketStart(_))
        | Token::Punct(Punct::BracketEnd(_))
        | Token::Punct(Punct::ParenStart(_))
        | Token::Punct(Punct::ParenEnd(_))
        | Token::Punct(Punct::Star(_))
        | Token::Punct(Punct::Plus(_))
        | Token::Punct(Punct::Question(_)) => {
            return Err(CSTError::Syntax(
                SyntaxKind::MacroMatchToken,
                ControlFlow::Recovable,
                token.to_span(),
            ));
        }
        _ => {}
    }

    Ok(token)
}

/// Macro repeat match op.
///
/// see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/macros-by-example.html#railroad-MacroMatch
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err =  SyntaxKind::MacroRepOp.map())]
pub enum MacroRepOp<I>
where
    I: CSTInput,
{
    Plus(Plus<I>),
    Star(Star<I>),
    Question(Question<I>),
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, SyntaxInput};

    use crate::{
        input::TokenStream,
        lexical::{
            S,
            comments::{CommentOrDoc, LineComment},
            ident::NonKeywordIdent,
            punct::{BraceEnd, BraceStart, BracketEnd, BracketStart, Pound},
        },
        macros::{invocation::TokenTree, rule::MacroRulesDefinition},
    };

    use super::*;

    #[test]
    fn test_macro_rule_definition() {
        let code = r###"macro_rules! foo {
            ($l:expr) => { bar!($l); }
        }
        "###;

        assert_eq!(
            TokenStream::from(code).parse::<MacroRulesDefinition<_>>(),
            Ok(MacroRulesDefinition {
                keyword: TokenStream::from((0, "macro_rules")),
                not: Not(
                    None,
                    TokenStream::from((11, "!")),
                    Some(S(TokenStream::from((12, " "))))
                ),
                name: Ident::NonKeywordIdent(NonKeywordIdent(TokenStream::from((13, "foo")))),
                body: MacroRulesDef::Brace(Delimiter {
                    start: BraceStart(
                        Some(S(TokenStream::from((16, " ")))),
                        TokenStream::from((17, "{")),
                        Some(S(TokenStream::from((18, "\n            "))))
                    ),
                    end: BraceEnd(
                        None,
                        TokenStream::from((66, "}")),
                        Some(S(TokenStream::from((67, "\n        "))))
                    ),
                    body: Punctuated {
                        pairs: vec![],
                        tail: Some(Box::new(MacroRule {
                            matcher: MacroMatcher::Paren(Delimiter {
                                start: ParenStart(None, TokenStream::from((31, "(")), None),
                                end: ParenEnd(
                                    None,
                                    TokenStream::from((39, ")")),
                                    Some(S(TokenStream::from((40, " "))))
                                ),
                                body: vec![MacroMatch::Fragement {
                                    leading_char: Dollar(None, TokenStream::from((32, "$")), None),
                                    ident: MacroFragIdent::IdentOrKeyword(IdentOrKeyword(
                                        TokenStream::from((33, "l"))
                                    )),
                                    colon: Colon(None, TokenStream::from((34, ":")), None),
                                    sepc: MacroFragSpec::Expr(TokenStream::from((35, "expr")))
                                }]
                            }),
                            arrow: FatArrow(
                                None,
                                TokenStream::from((41, "=>")),
                                Some(S(TokenStream::from((43, " "))))
                            ),
                            trans: DelimTokenTree::Brace(Delimiter {
                                start: BraceStart(
                                    None,
                                    TokenStream::from((44, "{")),
                                    Some(S(TokenStream::from((45, " "))))
                                ),
                                end: BraceEnd(
                                    None,
                                    TokenStream::from((56, "}")),
                                    Some(S(TokenStream::from((57, "\n        "))))
                                ),
                                body: vec![
                                    TokenTree::Token(Token::IdentOrKeyWord(IdentOrKeyword(
                                        TokenStream::from((46, "bar"))
                                    ))),
                                    TokenTree::Token(Token::Punct(Punct::Not(Not(
                                        None,
                                        TokenStream::from((49, "!")),
                                        None
                                    )))),
                                    TokenTree::Delim(Box::new(DelimTokenTree::Paren(Delimiter {
                                        start: ParenStart(None, TokenStream::from((50, "(")), None),
                                        end: ParenEnd(None, TokenStream::from((53, ")")), None),
                                        body: vec![
                                            TokenTree::Token(Token::Punct(Punct::Dollar(Dollar(
                                                None,
                                                TokenStream::from((51, "$")),
                                                None
                                            )))),
                                            TokenTree::Token(Token::IdentOrKeyWord(
                                                IdentOrKeyword(TokenStream::from((52, "l")))
                                            ))
                                        ]
                                    }))),
                                    TokenTree::Token(Token::Punct(Punct::Semi(Semi(
                                        None,
                                        TokenStream::from((54, ";")),
                                        Some(S(TokenStream::from((55, " "))))
                                    ))))
                                ]
                            })
                        }))
                    }
                })
            })
        );
    }

    #[test]
    fn test_macro_rules_with_comments() {
        let code = r#"macro_rules! hello {
            () => {
                // helloworld
                #[make]
            };
            }"#;

        assert_eq!(
            TokenStream::from(code).parse::<MacroRulesDefinition<_>>(),
            Ok(MacroRulesDefinition {
                keyword: TokenStream::from((0, "macro_rules")),
                not: Not(
                    None,
                    TokenStream::from((11, "!")),
                    Some(S(TokenStream::from((12, " "))))
                ),
                name: Ident::NonKeywordIdent(NonKeywordIdent(TokenStream::from((13, "hello")))),
                body: MacroRulesDef::Brace(Delimiter {
                    start: BraceStart(
                        Some(S(TokenStream::from((18, " ")))),
                        TokenStream::from((19, "{")),
                        Some(S(TokenStream::from((20, "\n            "))))
                    ),
                    end: BraceEnd(None, TokenStream::from((122, "}")), None),
                    body: Punctuated {
                        pairs: vec![(
                            MacroRule {
                                matcher: MacroMatcher::Paren(Delimiter {
                                    start: ParenStart(None, TokenStream::from((33, "(")), None),
                                    end: ParenEnd(
                                        None,
                                        TokenStream::from((34, ")")),
                                        Some(S(TokenStream::from((35, " "))))
                                    ),
                                    body: vec![]
                                }),
                                arrow: FatArrow(
                                    None,
                                    TokenStream::from((36, "=>")),
                                    Some(S(TokenStream::from((38, " "))))
                                ),
                                trans: DelimTokenTree::Brace(Delimiter {
                                    start: BraceStart(
                                        None,
                                        TokenStream::from((39, "{")),
                                        Some(S(TokenStream::from((40, "\n                "))))
                                    ),
                                    end: BraceEnd(None, TokenStream::from((107, "}")), None),
                                    body: vec![
                                        TokenTree::Token(Token::CommentOrDoc(
                                            CommentOrDoc::LineComment(LineComment(
                                                TokenStream::from((57, "// helloworld"))
                                            ))
                                        )),
                                        TokenTree::Token(Token::Punct(Punct::Pound(Pound(
                                            Some(S(TokenStream::from((70, "\n                ")))),
                                            TokenStream::from((87, "#")),
                                            None
                                        )))),
                                        TokenTree::Delim(Box::new(DelimTokenTree::Bracket(
                                            Delimiter {
                                                start: BracketStart(
                                                    None,
                                                    TokenStream::from((88, "[")),
                                                    None
                                                ),
                                                end: BracketEnd(
                                                    None,
                                                    TokenStream::from((93, "]")),
                                                    Some(S(TokenStream::from((
                                                        94,
                                                        "\n            "
                                                    ))))
                                                ),
                                                body: vec![TokenTree::Token(
                                                    Token::IdentOrKeyWord(IdentOrKeyword(
                                                        TokenStream::from((89, "make"))
                                                    ))
                                                )]
                                            }
                                        )))
                                    ]
                                })
                            },
                            Semi(
                                None,
                                TokenStream::from((108, ";")),
                                Some(S(TokenStream::from((109, "\n            "))))
                            )
                        )],
                        tail: None
                    }
                })
            })
        );
    }
}
