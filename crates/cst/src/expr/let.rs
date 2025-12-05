use parserc::syntax::Syntax;

use crate::{
    expr::{Expr, group::Composable},
    input::CSTInput,
    keyword::Let,
    pat::Pat,
    punct::Equal,
};

/// An assignment expression: a = compute().
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprLet<I>
where
    I: CSTInput,
{
    /// leading keyword `let`
    pub keyword: Let<I>,
    /// left Operand pattern.
    pub pat: Box<Pat<I>>,
    /// punct `=`
    pub eq: Equal<I>,
    /// right Operand expression.
    pub expr: Box<Expr<I>>,
}

impl<I> Composable<I> for ExprLet<I>
where
    I: CSTInput,
{
    fn priority(&self) -> usize {
        1
    }

    fn compose<F>(self, _: usize, _f: F) -> super::Expr<I>
    where
        F: FnOnce(super::Expr<I>) -> super::Expr<I>,
    {
        unreachable!("`ExprLet` cannot be used as an operand on the left-hand side.")
    }
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{InputSyntaxExt, Punctuated};

    use crate::{
        expr::{Expr, ExprLet, ExprPath},
        input::TokenStream,
        keyword::Let,
        misc::{Ident, S},
        pat::Pat,
        path::{Path, PathSegment},
        punct::{Equal, PathSep},
    };

    #[test]
    fn test_let() {
        assert_eq!(
            TokenStream::from("let a = Target::Field").parse::<Expr<_>>(),
            Ok(Expr::Let(ExprLet {
                keyword: Let(
                    TokenStream::from((0, "let")),
                    Some(S(TokenStream::from((3, " "))))
                ),
                pat: Box::new(Pat::Ident(Ident(TokenStream::from((4, "a"))))),
                eq: Equal(
                    Some(S(TokenStream::from((5, " ")))),
                    TokenStream::from((6, "=")),
                    Some(S(TokenStream::from((7, " "))))
                ),
                expr: Box::new(Expr::Path(ExprPath {
                    qself: None,
                    path: Path {
                        leading_pathsep: None,
                        segments: Punctuated {
                            pairs: vec![(
                                PathSegment {
                                    ident: Ident(TokenStream::from((8, "Target"))),
                                    arguments: None
                                },
                                PathSep(None, TokenStream::from((14, "::")), None)
                            )],
                            tail: Some(Box::new(PathSegment {
                                ident: Ident(TokenStream::from((16, "Field"))),
                                arguments: None
                            }))
                        }
                    }
                }))
            }))
        );
    }
}
