use parserc::syntax::{Punctuated, Syntax};

use crate::{
    input::UnsynInput,
    lexical::{
        S,
        delimiter::{Brace, Bracket, Paren},
        ident::Ident,
        keyword::{Except, Followed, Lexer},
        lit::{LitStr, LitUnicode},
        punct::{ArrowRight, Comma, DotDot, Minus, Or, Plus, Question, Semi, Star, Tilde},
    },
    syntax::Path,
};

/// A stmt define a node of syntax tree.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Stmt<I>
where
    I: UnsynInput,
{
    Lexer {
        /// keyword `lexer`
        keyword: Lexer<I>,
        /// node name.
        ident: Ident<I>,
        /// separator punct `->`
        arrow_right: ArrowRight<I>,
        /// node definition expression
        expr: Expr<I>,
        /// Termination punct `;`
        semi: Semi<I>,
    },

    Syntax {
        /// keyword `syntax`
        keyword: crate::lexical::keyword::Syntax<I>,
        /// node name.
        ident: Ident<I>,
        /// separator punct `->`
        arrow_right: ArrowRight<I>,
        /// node definition expression
        expr: Expr<I>,
        /// Termination punct `;`
        semi: Semi<I>,
    },
}

/// Node definition expression.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Expr<I>
where
    I: UnsynInput,
{
    pub first: ExprNoTopAlt<I>,
    pub rest: Vec<(Or<I>, ExprNoTopAlt<I>)>,
}

/// No top alt expression.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ExprNoTopAlt<I>
where
    I: UnsynInput,
{
    WithSuffix(ExprWithSuffix<I>),
    WithoutSuffix(ExprWithoutSuffix<I>),
}

/// Expr with suffix.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ExprWithSuffix<I>
where
    I: UnsynInput,
{
    /// star expr.
    Star(
        /// target expr.
        ExprWithoutSuffix<I>,
        /// start punct `*`
        Star<I>,
    ),
    Question(
        /// target expr.
        ExprWithoutSuffix<I>,
        /// question punct `?`
        Question<I>,
    ),
    Plus(
        /// target expr.
        ExprWithoutSuffix<I>,
        /// plus punct `+`
        Plus<I>,
    ),
    Repeat {
        /// target expr.
        target: ExprWithoutSuffix<I>,
        suffix: Brace<I, Repeat<I>>,
    },
    /// a followed expression
    Followed {
        /// target expr.
        target: ExprWithoutSuffix<I>,
        /// prefix whitespace.
        s: S<I>,
        /// keyword `followed`,
        #[parserc(crucial)]
        keyword: Followed<I>,
        /// suffix expr.
        suffix: Box<ExprNoTopAlt<I>>,
    },
    /// A except expression.
    Except {
        /// target expr.
        target: ExprWithoutSuffix<I>,
        /// prefix whitespace.
        s: S<I>,
        /// keyword `except`,
        #[parserc(crucial)]
        keyword: Except<I>,
        /// expect tokens.
        tokens: ExprWithoutSuffix<I>,
    },
}

/// Expr without suffix.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ExprWithoutSuffix<I>
where
    I: UnsynInput,
{
    /// tilde expr.
    Tilde(
        /// tilde punct `~`
        Tilde<I>,
        /// target expr.
        Box<ExprWithoutSuffix<I>>,
    ),
    /// paren expr `(T)`
    Paren(Paren<I, Box<Expr<I>>>),
    /// a set expression,
    Set(Bracket<I, Punctuated<SetItem<I>, Comma<I>>>),
    /// A literal string expr.
    Str(LitStr<I>),
    /// A literal unicode expr.
    Unicode(LitUnicode<I>),
    /// A path expression.
    Path(Path<I>),
}

/// expr for set item.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum SetItem<I>
where
    I: UnsynInput,
{
    /// a range expr.
    Range(Range<I>),
    /// A literal string expr.
    Str(LitStr<I>),
    /// A literal unicode expr.
    Unicode(LitUnicode<I>),
    /// A path expression.
    Path(Path<I>),
}

/// A literal range expression
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Range<I>
where
    I: UnsynInput,
{
    Unicode(LitUnicode<I>, #[parserc(crucial)] Minus<I>, LitUnicode<I>),
    Str(LitStr<I>, #[parserc(crucial)] Minus<I>, LitStr<I>),
}

/// The suffix of repeat expresison.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Repeat<I>
where
    I: UnsynInput,
{
    To(DotDot<I>, Box<Expr<I>>),
    From(Box<Expr<I>>, DotDot<I>, Option<Box<Expr<I>>>),
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, SyntaxInput};

    use crate::{
        input::TokenStream,
        lexical::{
            lit::{QuoteEscape, StrSegment},
            punct::{BracketEnd, BracketStart, ParenEnd, ParenStart},
        },
        syntax::{Expr, ExprNoTopAlt, ExprWithoutSuffix, PathSegment},
    };

    use super::*;

    #[test]
    fn test_expr() {
        assert_eq!(
            TokenStream::from("~[U+0020-U+0030] | a except '\\'' | b | c followed (a|b)")
                .parse::<Expr<_>>(),
            Ok(Expr {
                first: ExprNoTopAlt::WithoutSuffix(ExprWithoutSuffix::Tilde(
                    Tilde(None, TokenStream::from((0, "~")), None),
                    Box::new(ExprWithoutSuffix::Set(Delimiter {
                        start: BracketStart(None, TokenStream::from((1, "[")), None),
                        end: BracketEnd(
                            None,
                            TokenStream::from((15, "]")),
                            Some(S(TokenStream::from((16, " "))))
                        ),
                        body: Punctuated {
                            pairs: vec![],
                            tail: Some(Box::new(SetItem::Range(Range::Unicode(
                                LitUnicode(TokenStream::from((2, "U+0020"))),
                                Minus(None, TokenStream::from((8, "-")), None),
                                LitUnicode(TokenStream::from((9, "U+0030")))
                            ))))
                        }
                    }))
                )),
                rest: vec![
                    (
                        Or(
                            None,
                            TokenStream::from((17, "|")),
                            Some(S(TokenStream::from((18, " "))))
                        ),
                        ExprNoTopAlt::WithSuffix(ExprWithSuffix::Except {
                            target: ExprWithoutSuffix::Path(Path {
                                leading_sep: None,
                                first: PathSegment::Ident(Ident(TokenStream::from((19, "a")))),
                                rest: vec![]
                            }),
                            s: S(TokenStream::from((20, " "))),
                            keyword: Except(
                                TokenStream::from((21, "except")),
                                Some(S(TokenStream::from((27, " "))))
                            ),
                            tokens: ExprWithoutSuffix::Str(LitStr {
                                delimiter_start: TokenStream::from((28, "'")),
                                content: vec![StrSegment::QuoteEscape(QuoteEscape(
                                    TokenStream::from((29, "\\'"))
                                ))],
                                delimiter_end: TokenStream::from((31, "'"))
                            })
                        })
                    ),
                    (
                        Or(
                            Some(S(TokenStream::from((32, " ")))),
                            TokenStream::from((33, "|")),
                            Some(S(TokenStream::from((34, " "))))
                        ),
                        ExprNoTopAlt::WithoutSuffix(ExprWithoutSuffix::Path(Path {
                            leading_sep: None,
                            first: PathSegment::Ident(Ident(TokenStream::from((35, "b")))),
                            rest: vec![]
                        }))
                    ),
                    (
                        Or(
                            Some(S(TokenStream::from((36, " ")))),
                            TokenStream::from((37, "|")),
                            Some(S(TokenStream::from((38, " "))))
                        ),
                        ExprNoTopAlt::WithSuffix(ExprWithSuffix::Followed {
                            target: ExprWithoutSuffix::Path(Path {
                                leading_sep: None,
                                first: PathSegment::Ident(Ident(TokenStream::from((39, "c")))),
                                rest: vec![]
                            }),
                            s: S(TokenStream::from((40, " "))),
                            keyword: Followed(
                                TokenStream::from((41, "followed")),
                                Some(S(TokenStream::from((49, " "))))
                            ),
                            suffix: Box::new(ExprNoTopAlt::WithoutSuffix(
                                ExprWithoutSuffix::Paren(Delimiter {
                                    start: ParenStart(None, TokenStream::from((50, "(")), None),
                                    end: ParenEnd(None, TokenStream::from((54, ")")), None),
                                    body: Box::new(Expr {
                                        first: ExprNoTopAlt::WithoutSuffix(
                                            ExprWithoutSuffix::Path(Path {
                                                leading_sep: None,
                                                first: PathSegment::Ident(Ident(
                                                    TokenStream::from((51, "a"))
                                                )),
                                                rest: vec![]
                                            })
                                        ),
                                        rest: vec![(
                                            Or(None, TokenStream::from((52, "|")), None),
                                            ExprNoTopAlt::WithoutSuffix(ExprWithoutSuffix::Path(
                                                Path {
                                                    leading_sep: None,
                                                    first: PathSegment::Ident(Ident(
                                                        TokenStream::from((53, "b"))
                                                    )),
                                                    rest: vec![]
                                                }
                                            ))
                                        )]
                                    })
                                })
                            ))
                        })
                    )
                ]
            })
        );
    }
}
