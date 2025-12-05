use parserc::syntax::{Delimiter, Punctuated, Syntax};

use crate::{
    expr::{Expr, group::Composable},
    input::CSTInput,
    keyword::Const,
    pat::Pat,
    punct::{ArrowRight, Comma, Or},
    ty::Type,
};

/// A closure expression: |a, b| a + b.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprClosure<I>
where
    I: CSTInput,
{
    /// optional `const` keyword.
    pub constness: Option<Const<I>>,
    /// input arguments of closure.
    pub inputs: Delimiter<Or<I>, Or<I>, Punctuated<Pat<I>, Comma<I>>>,
    /// Return type of a function signature.
    pub output: Option<(ArrowRight<I>, Box<Type<I>>)>,
}

impl<I> Composable<I> for ExprClosure<I>
where
    I: CSTInput,
{
    #[inline]
    fn priority(&self) -> usize {
        1
    }

    #[inline]
    fn compose<F>(self, _priority: usize, f: F) -> super::Expr<I>
    where
        F: FnOnce(super::Expr<I>) -> super::Expr<I>,
    {
        f(Expr::Closure(self))
    }
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, InputSyntaxExt, Punctuated};

    use crate::{
        expr::{Expr, ExprClosure},
        input::TokenStream,
        misc::{Ident, S},
        pat::Pat,
        punct::{ArrowRight, Comma, Or},
        ty::Type,
    };

    #[test]
    fn test_closure() {
        assert_eq!(
            TokenStream::from("|| 1 ").parse::<Expr<_>>(),
            Ok(Expr::Closure(ExprClosure {
                constness: None,
                inputs: Delimiter {
                    start: Or(None, TokenStream::from((0, "|")), None),
                    end: Or(
                        None,
                        TokenStream::from((1, "|")),
                        Some(S(TokenStream::from((2, " "))))
                    ),
                    body: Punctuated {
                        pairs: vec![],
                        tail: None
                    }
                },
                output: None
            }))
        );
        assert_eq!(
            TokenStream::from("|a,b| -> u32 { a + b } ").parse::<Expr<_>>(),
            Ok(Expr::Closure(ExprClosure {
                constness: None,
                inputs: Delimiter {
                    start: Or(None, TokenStream::from((0, "|")), None),
                    end: Or(
                        None,
                        TokenStream::from((4, "|")),
                        Some(S(TokenStream::from((5, " "))))
                    ),
                    body: Punctuated {
                        pairs: vec![(
                            Pat::Ident(Ident(TokenStream::from((1, "a")))),
                            Comma(None, TokenStream::from((2, ",")), None)
                        )],
                        tail: Some(Box::new(Pat::Ident(Ident(TokenStream::from((3, "b"))))))
                    }
                },
                output: Some((
                    ArrowRight(
                        None,
                        TokenStream::from((6, "->")),
                        Some(S(TokenStream::from((8, " "))))
                    ),
                    Box::new(Type::U32(TokenStream::from((9, "u32"))))
                ))
            }))
        );
    }
}
