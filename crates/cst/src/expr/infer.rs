use parserc::{ControlFlow, syntax::Syntax};

use crate::{errors::CSTError, input::CSTInput, punct::Underscore};

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

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, Punctuated, SyntaxInput};

    use crate::{
        expr::{Call, Expr, ExprInfer, ExprPath, MethodCall},
        generics::GenericArgument,
        input::TokenStream,
        misc::Ident,
        path::{Path, PathArguments, PathSegment},
        punct::{
            AngleBracketEnd, AngleBracketStart, Dot, ParenEnd, ParenStart, PathSep, Underscore,
        },
    };

    #[test]
    fn test_infer() {
        assert_eq!(
            TokenStream::from("a.f::<_>()").parse::<Expr<_>>(),
            Ok(Expr::MethodCall(
                Box::new(Expr::Path(ExprPath {
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
                vec![],
                MethodCall {
                    dot: Dot(None, TokenStream::from((1, ".")), None),
                    ident: Ident(TokenStream::from((2, "f"))),
                    turbofish: Some(PathArguments {
                        leading_pathsep: Some(PathSep(None, TokenStream::from((3, "::")), None)),
                        delimiter_start: AngleBracketStart(None, TokenStream::from((5, "<")), None),
                        args: Punctuated {
                            pairs: vec![],
                            tail: Some(Box::new(GenericArgument::Const(Expr::Infer(ExprInfer(
                                Underscore(None, TokenStream::from((6, "_")), None)
                            )))))
                        },
                        delimiter_end: AngleBracketEnd(None, TokenStream::from((7, ">")), None)
                    }),
                    args: Call(Delimiter {
                        start: ParenStart(None, TokenStream::from((8, "(")), None),
                        end: ParenEnd(None, TokenStream::from((9, ")")), None),
                        body: Punctuated {
                            pairs: vec![],
                            tail: None
                        }
                    })
                }
            ))
        );
    }
}
