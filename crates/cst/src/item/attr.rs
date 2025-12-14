use parserc::syntax::{Delimiter, Punctuated, Syntax};

use crate::{
    errors::SyntaxKind,
    expr::Expr,
    input::CSTInput,
    misc::Ident,
    punct::{At, Comma, Equal, ParenEnd, ParenStart},
};

/// Outer attribute.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct OuterAttr<I>
where
    I: CSTInput,
{
    /// leading `@` character.
    #[parserc(crucial)]
    pub leading_at: At<I>,
    /// name of attribute.
    pub name: Ident<I>,
    /// argument list of this attribute.
    pub arguments:
        Option<Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<AttrArgument<I>, Comma<I>>>>,
}

/// Argument of attribute.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::AttrArgument.map_unhandle())]
pub enum AttrArgument<I>
where
    I: CSTInput,
{
    Named {
        /// parameter name.
        name: Ident<I>,
        /// punct `=`
        eq: Equal<I>,
        /// argument value.
        expr: Expr<I>,
    },
    /// unamed argument.
    Unamed(Expr<I>),
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, Punctuated, SyntaxInput};

    use crate::{
        expr::ExprPath,
        input::TokenStream,
        misc::S,
        path::{Path, PathSegment},
        punct::PathSep,
    };

    use super::*;

    #[test]
    fn test_attr() {
        assert_eq!(
            TokenStream::from("@ runtime()").parse::<OuterAttr<_>>(),
            Ok(OuterAttr {
                leading_at: At(
                    None,
                    TokenStream::from((0, "@")),
                    Some(S(TokenStream::from((1, " "))))
                ),
                name: Ident(TokenStream::from((2, "runtime"))),
                arguments: Some(Delimiter {
                    start: ParenStart(None, TokenStream::from((9, "(")), None),
                    end: ParenEnd(None, TokenStream::from((10, ")")), None),
                    body: Punctuated {
                        pairs: vec![],
                        tail: None
                    }
                })
            })
        );

        assert_eq!(
            TokenStream::from("@attribute(Target::All, ty = Derive)").parse::<OuterAttr<_>>(),
            Ok(OuterAttr {
                leading_at: At(None, TokenStream::from((0, "@")), None),
                name: Ident(TokenStream::from((1, "attribute"))),
                arguments: Some(Delimiter {
                    start: ParenStart(None, TokenStream::from((10, "(")), None),
                    end: ParenEnd(None, TokenStream::from((35, ")")), None),
                    body: Punctuated {
                        pairs: vec![(
                            AttrArgument::Unamed(Expr::Path(ExprPath {
                                qself: None,
                                path: Path {
                                    leading_pathsep: None,
                                    segments: Punctuated {
                                        pairs: vec![(
                                            PathSegment {
                                                ident: Ident(TokenStream::from((11, "Target"))),
                                                arguments: None
                                            },
                                            PathSep(None, TokenStream::from((17, "::")), None)
                                        )],
                                        tail: Some(Box::new(PathSegment {
                                            ident: Ident(TokenStream::from((19, "All"))),
                                            arguments: None
                                        }))
                                    }
                                }
                            })),
                            Comma(
                                None,
                                TokenStream::from((22, ",")),
                                Some(S(TokenStream::from((23, " "))))
                            )
                        )],
                        tail: Some(Box::new(AttrArgument::Named {
                            name: Ident(TokenStream::from((24, "ty"))),
                            eq: Equal(
                                Some(S(TokenStream::from((26, " ")))),
                                TokenStream::from((27, "=")),
                                Some(S(TokenStream::from((28, " "))))
                            ),
                            expr: Expr::Path(ExprPath {
                                qself: None,
                                path: Path {
                                    leading_pathsep: None,
                                    segments: Punctuated {
                                        pairs: vec![],
                                        tail: Some(Box::new(PathSegment {
                                            ident: Ident(TokenStream::from((29, "Derive"))),
                                            arguments: None
                                        }))
                                    }
                                }
                            })
                        }))
                    }
                })
            })
        );
    }
}
