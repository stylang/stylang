use parserc::syntax::Syntax;

use crate::{
    errors::SyntaxKind,
    expr::{Digits, Expr, group::Composable},
    input::CSTInput,
    misc::Ident,
    punct::Dot,
};

/// A struct or tuple struct field accessed in a struct literal or field expression.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::ExprFieldDot.map())]
pub enum Member<I>
where
    I: CSTInput,
{
    Named(Ident<I>),
    Unamed(Digits<I>),
}

/// Access of a named struct field (`obj.k`) or unnamed tuple struct field (`obj.0`).
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprFiled<I>
where
    I: CSTInput,
{
    /// Reference to a object.
    pub base: Box<Expr<I>>,
    /// punct `.`
    pub dot: Dot<I>,
    /// member of struct.
    pub member: Member<I>,
}

impl<I> Composable<I> for ExprFiled<I>
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
        f(Expr::Field(self))
    }
}

#[cfg(test)]
mod test {
    use parserc::syntax::{Delimiter, InputSyntaxExt, Punctuated};

    use crate::{
        expr::{Digits, Expr, ExprCall, ExprFiled, ExprLit, ExprPath, LitNumber, Member},
        input::TokenStream,
        misc::Ident,
        path::{Path, PathSegment},
        punct::{Dot, ParenEnd, ParenStart},
    };

    #[test]
    fn test_field() {
        assert_eq!(
            TokenStream::from("1.name()").parse::<Expr<_>>(),
            Ok(Expr::Call(ExprCall {
                func: Box::new(Expr::Field(ExprFiled {
                    base: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                        sign: None,
                        trunc: Some(Digits {
                            input: TokenStream::from((0, "1")),
                            value: 1
                        }),
                        fract: None,
                        exp: None
                    }))),
                    dot: Dot(None, TokenStream::from((1, ".")), None),
                    member: Member::Named(Ident(TokenStream::from((2, "name"))))
                })),
                args: Delimiter {
                    start: ParenStart(None, TokenStream::from((6, "(")), None),
                    end: ParenEnd(None, TokenStream::from((7, ")")), None),
                    body: Punctuated {
                        pairs: vec![],
                        tail: None
                    }
                }
            }))
        );

        assert_eq!(
            TokenStream::from("a.c().b").parse::<Expr<_>>(),
            Ok(Expr::Field(ExprFiled {
                base: Box::new(Expr::Call(ExprCall {
                    func: Box::new(Expr::Field(ExprFiled {
                        base: Box::new(Expr::Path(ExprPath {
                            qself: None,
                            path: Path {
                                leading_pathsep: None,
                                segments: Punctuated {
                                    pairs: vec![],
                                    tail: Some(Box::new(PathSegment {
                                        ident: Ident(TokenStream::from((0, "a"))),
                                        arguments: None
                                    }))
                                }
                            }
                        })),
                        dot: Dot(None, TokenStream::from((1, ".")), None),
                        member: Member::Named(Ident(TokenStream::from((2, "c"))))
                    })),
                    args: Delimiter {
                        start: ParenStart(None, TokenStream::from((3, "(")), None),
                        end: ParenEnd(None, TokenStream::from((4, ")")), None),
                        body: Punctuated {
                            pairs: vec![],
                            tail: None
                        }
                    }
                })),
                dot: Dot(None, TokenStream::from((5, ".")), None),
                member: Member::Named(Ident(TokenStream::from((6, "b"))))
            }))
        );
    }
}
