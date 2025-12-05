use parserc::{ControlFlow, syntax::Syntax};

use crate::{errors::CSTError, expr::group::Composable, input::CSTInput, punct::Underscore};

/// The inferred value of a const generic argument, denoted _.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprInfer<I>(pub Underscore<I>)
where
    I: CSTInput;

impl<I> Syntax<I> for ExprInfer<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let underscore = Underscore::parse(input)?;

        if underscore.2.is_none() {
            if let Some(c) = input.iter().next() {
                if c.is_ascii_alphanumeric() {
                    return Err(CSTError::Syntax(
                        crate::errors::SyntaxKind::ExprInfer,
                        ControlFlow::Recovable,
                        underscore.to_span(),
                    ));
                }
            }
        }

        Ok(Self(underscore))
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

impl<I> Composable<I> for ExprInfer<I>
where
    I: CSTInput,
{
    fn priority(&self) -> usize {
        1
    }

    fn compose<F>(self, _: usize, _: F) -> super::Expr<I>
    where
        F: FnOnce(super::Expr<I>) -> super::Expr<I>,
    {
        unreachable!("`ExprInfer` cannot be used as an operand on the left-hand side.")
    }
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, InputSyntaxExt, Punctuated};

    use crate::{
        expr::{Expr, ExprMethodCall, ExprPath},
        generics::GenericArgument,
        input::TokenStream,
        misc::Ident,
        path::{Path, PathArguments, PathSegment},
        punct::{AngleBracketEnd, AngleBracketStart, Dot, ParenEnd, ParenStart, PathSep},
        ty::Type,
    };

    #[test]
    fn test_infer() {
        assert_eq!(
            TokenStream::from("a.b::<_>()").parse::<Expr<_>>(),
            Ok(Expr::MethodCall(ExprMethodCall {
                receiver: Box::new(Expr::Path(ExprPath {
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
                ident: Ident(TokenStream::from((2, "b"))),
                turbofish: Some(PathArguments {
                    leading_pathsep: Some(PathSep(None, TokenStream::from((3, "::")), None)),
                    args: Delimiter {
                        start: AngleBracketStart(None, TokenStream::from((5, "<")), None),
                        end: AngleBracketEnd(None, TokenStream::from((7, ">")), None),
                        body: Punctuated {
                            pairs: vec![],
                            tail: Some(Box::new(GenericArgument::Type(Type::Path(Path {
                                leading_pathsep: None,
                                segments: Punctuated {
                                    pairs: vec![],
                                    tail: Some(Box::new(PathSegment {
                                        ident: Ident(TokenStream::from((6, "_"))),
                                        arguments: None
                                    }))
                                }
                            }))))
                        }
                    }
                }),
                args: Delimiter {
                    start: ParenStart(None, TokenStream::from((8, "(")), None),
                    end: ParenEnd(None, TokenStream::from((9, ")")), None),
                    body: Punctuated {
                        pairs: vec![],
                        tail: None
                    }
                }
            }))
        );
    }
}
