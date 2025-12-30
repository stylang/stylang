//! A macro invocation expands a macro at compile time and replaces the invocation with the result of the macro

use parserc::syntax::Syntax;

use crate::{
    input::CSTInput,
    lexical::{
        delimiter::{Brace, Bracket, Paren},
        ident::{IdentOrKeyword, RawIdent},
        label::LifeTime,
        lit::{
            LitByte, LitByteStr, LitCStr, LitChar, LitFloat, LitInt, LitRawByteStr, LitRawCStr,
            LitRawStr, LitStr,
        },
        punct::*,
    },
    names::paths::SimplePath,
};

/// A punct token except delimiter
///
///  see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#grammar-PUNCTUATION
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Punct<I>
where
    I: CSTInput,
{
    AndAnd(AndAnd<I>),
    OrOr(OrOr<I>),

    PlusEq(PlusEq<I>),
    MinusEq(MinusEq<I>),
    StarEq(StarEq<I>),
    SlashEq(SlashEq<I>),
    PercentEq(PercentEq<I>),
    CaretEq(CaretEq<I>),
    AndEq(AndEq<I>),
    OrEq(OrEq<I>),
    ShlEq(ShlEq<I>),
    ShrEq(ShrEq<I>),
    Eq(Eq<I>),
    EqEq(EqEq<I>),
    Ne(Ne<I>),
    Ge(Ge<I>),
    Le(Le<I>),
    FatArrow(FatArrow<I>),
    RArrow(RArrow<I>),
    LArrow(LArrow<I>),

    Plus(Plus<I>),
    Minus(Minus<I>),
    Star(Star<I>),
    Slash(Slash<I>),
    Percent(Percent<I>),
    Caret(Caret<I>),
    Not(Not<I>),

    And(And<I>),
    Or(Or<I>),

    Shl(Shl<I>),
    Shr(Shr<I>),

    Gt(Gt<I>),
    Lt(Lt<I>),

    At(At<I>),
    Underscore(Underscore<I>),

    DotDotEq(DotDotEq<I>),
    DotDotDot(DotDotDot<I>),

    DotDot(DotDot<I>),
    Dot(Dot<I>),

    Comma(Comma<I>),
    Semi(Semi<I>),
    Colon(Colon<I>),
    PathSep(PathSep<I>),
    Pound(Pound<I>),
    Dollar(Dollar<I>),
    Question(Question<I>),
    Tilde(Tilde<I>),
}

/// Token except delimiters
///
/// see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/macros.html#railroad-TokenTree
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Token<I>
where
    I: CSTInput,
{
    IdentOrKeyWord(IdentOrKeyword<I>),
    RawIdent(RawIdent<I>),
    LitChar(LitChar<I>),
    LitStr(LitStr<I>),
    LitRawStr(LitRawStr<I>),
    LitByte(LitByte<I>),
    LitByteStr(LitByteStr<I>),
    LitRawByteStr(LitRawByteStr<I>),
    LitCStr(LitCStr<I>),
    LitRawCStr(LitRawCStr<I>),
    LitInt(LitInt<I>),
    LitFloat(LitFloat<I>),
    LifeTime(LifeTime<I>),
    Punct(Punct<I>),
}

/// Token tree with delimter.
///
/// see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/macros.html#railroad-DelimTokenTree
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum DelimTokenTree<I>
where
    I: CSTInput,
{
    Paren(Paren<I, Vec<TokenTree<I>>>),

    Bracket(Bracket<I, Vec<TokenTree<I>>>),

    Brace(Brace<I, Vec<TokenTree<I>>>),
}

/// Token tree used by [`MacroInvocationSemi`]
///
/// see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/macros.html#grammar-MacroInvocationSemi
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum DelimTokenTreeSemi<I>
where
    I: CSTInput,
{
    Paren(Paren<I, Vec<TokenTree<I>>>, Semi<I>),

    Bracket(Bracket<I, Vec<TokenTree<I>>>, Semi<I>),

    Brace(Brace<I, Vec<TokenTree<I>>>),
}

/// Token tree
///
/// see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/macros.html#railroad-TokenTree
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum TokenTree<I>
where
    I: CSTInput,
{
    Token(Token<I>),
    Delim(Box<DelimTokenTree<I>>),
}

/// A macro invocation expands a macro at compile time and replaces the invocation
/// with the result of the macro.
///
/// see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/macros.html#railroad-MacroInvocation
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct MacroInvocation<I>
where
    I: CSTInput,
{
    /// macro path
    pub simple_path: SimplePath<I>,
    /// token `!`
    pub not: Not<I>,
    /// delimiter token tree.
    pub delim_token_tree: DelimTokenTree<I>,
}

/// When used as an item or a statement, the MacroInvocationSemi form is used
/// where a semicolon is required at the end when not using curly braces.
///
/// see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/macros.html#grammar-MacroInvocationSemi
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct MacroInvocationSemi<I>
where
    I: CSTInput,
{
    /// macro path
    pub simple_path: SimplePath<I>,
    /// token `!`
    pub not: Not<I>,
    /// delimiter token tree.
    pub delim_token_tree: DelimTokenTreeSemi<I>,
}

#[cfg(test)]
mod tests {

    use parserc::syntax::{Delimiter, SyntaxInput};

    use crate::{
        input::TokenStream,
        lexical::{
            S,
            ident::{Ident, NonKeywordIdent},
            lit::LitDec,
        },
        names::paths::SimplePathSegment,
    };

    use super::*;

    #[test]
    fn test_macro_invocation() {
        assert_eq!(
            TokenStream::from("pat!(x)").parse::<MacroInvocation<_>>(),
            Ok(MacroInvocation {
                simple_path: SimplePath {
                    leading_sep: None,
                    first: SimplePathSegment::Ident(Ident::NonKeywordIdent(NonKeywordIdent(
                        TokenStream::from((0, "pat"))
                    ))),
                    rest: vec![]
                },
                not: Not(None, TokenStream::from((3, "!")), None),
                delim_token_tree: DelimTokenTree::Paren(Delimiter {
                    start: ParenStart(None, TokenStream::from((4, "(")), None),
                    end: ParenEnd(None, TokenStream::from((6, ")")), None),
                    body: vec![TokenTree::Token(Token::IdentOrKeyWord(IdentOrKeyword(
                        TokenStream::from((5, "x"))
                    )))]
                })
            })
        );
    }

    #[test]
    fn test_macro_invocation_stmt() {
        assert_eq!(
            TokenStream::from("vec![1,2,3] ;").parse::<MacroInvocationSemi<_>>(),
            Ok(MacroInvocationSemi {
                simple_path: SimplePath {
                    leading_sep: None,
                    first: SimplePathSegment::Ident(Ident::NonKeywordIdent(NonKeywordIdent(
                        TokenStream::from((0, "vec"))
                    ))),
                    rest: vec![]
                },
                not: Not(None, TokenStream::from((3, "!")), None),
                delim_token_tree: DelimTokenTreeSemi::Bracket(
                    Delimiter {
                        start: BracketStart(None, TokenStream::from((4, "[")), None),
                        end: BracketEnd(
                            None,
                            TokenStream::from((10, "]")),
                            Some(S(TokenStream::from((11, " "))))
                        ),
                        body: vec![
                            TokenTree::Token(Token::LitInt(LitInt::Dec(LitDec(
                                TokenStream::from((5, "1"))
                            )))),
                            TokenTree::Token(Token::Punct(Punct::Comma(Comma(
                                None,
                                TokenStream::from((6, ",")),
                                None
                            )))),
                            TokenTree::Token(Token::LitInt(LitInt::Dec(LitDec(
                                TokenStream::from((7, "2"))
                            )))),
                            TokenTree::Token(Token::Punct(Punct::Comma(Comma(
                                None,
                                TokenStream::from((8, ",")),
                                None
                            )))),
                            TokenTree::Token(Token::LitInt(LitInt::Dec(LitDec(
                                TokenStream::from((9, "3"))
                            ))))
                        ]
                    },
                    Semi(None, TokenStream::from((12, ";")), None)
                )
            })
        );
    }
}
