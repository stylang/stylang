//! A braced block containing Rust statements.

use parserc::syntax::{Delimiter, Syntax};

use crate::{
    expr::{Expr, ExprBlock},
    input::CSTInput,
    keyword::{Else, Let},
    pat::Pat,
    punct::{BraceEnd, BraceStart, Equal, Semi},
};

/// The expression assigned in a local let binding, including optional diverging else block.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LocalInit<I>
where
    I: CSTInput,
{
    /// leading equal punct `=`
    pub eq: Equal<I>,
    /// init expr.
    pub expr: Box<Expr<I>>,
    /// diverge `else` block.
    pub diverge: Option<(Else<I>, Box<ExprBlock<I>>)>,
}

/// A statement, usually ending in a semicolon.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Stmt<I>
where
    I: CSTInput,
{
    Local {
        keyword: Let<I>,
        pat: Pat<I>,
        init: Option<LocalInit<I>>,
        semi: Semi<I>,
    },
    Expr(Expr<I>, Option<Semi<I>>),
}

/// A braced block containing Rust statements.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Block<I>(pub Delimiter<BraceStart<I>, BraceEnd<I>, Vec<Stmt<I>>>)
where
    I: CSTInput;

#[cfg(test)]
mod tests {
    use parserc::syntax::InputSyntaxExt;

    use crate::{
        block::{LocalInit, Stmt},
        expr::{Digits, Expr, ExprLit, LitNumber},
        input::TokenStream,
        keyword::Let,
        misc::{Ident, S},
        pat::Pat,
        punct::{Equal, Semi},
    };

    #[test]
    fn test_local_stmt() {
        assert_eq!(
            TokenStream::from("let a;").parse::<Stmt<_>>(),
            Ok(Stmt::Local {
                keyword: Let(
                    TokenStream::from((0, "let")),
                    Some(S(TokenStream::from((3, " "))))
                ),
                pat: Pat::Ident(Ident(TokenStream::from((4, "a")))),
                init: None,
                semi: Semi(None, TokenStream::from((5, ";")), None)
            })
        );
    }

    #[test]
    fn test_local_init() {
        assert_eq!(
            TokenStream::from("let a = 1;").parse::<Stmt<_>>(),
            Ok(Stmt::Local {
                keyword: Let(
                    TokenStream::from((0, "let")),
                    Some(S(TokenStream::from((3, " "))))
                ),
                pat: Pat::Ident(Ident(TokenStream::from((4, "a")))),
                init: Some(LocalInit {
                    eq: Equal(
                        Some(S(TokenStream::from((5, " ")))),
                        TokenStream::from((6, "=")),
                        Some(S(TokenStream::from((7, " "))))
                    ),
                    expr: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                        sign: None,
                        trunc: Some(Digits {
                            input: TokenStream::from((8, "1")),
                            value: 1
                        }),
                        fract: None,
                        exp: None
                    }))),
                    diverge: None
                }),
                semi: Semi(None, TokenStream::from((9, ";")), None)
            })
        );
    }
}
