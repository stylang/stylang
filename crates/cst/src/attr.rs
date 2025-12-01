use parserc::syntax::{Delimiter, Or, Punctuated, Syntax};

use crate::{At, CSTInput, Comma, Eequal, Ident, Lit, ParenEnd, ParenStart, Path, SyntaxKind};

/// Outer attribute.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Attribute<I>
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
        Option<Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<AttributeArgument<I>, Comma<I>>>>,
}

/// Argument of attribute.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::AttrArgument.map_unhandle())]
pub enum AttributeArgument<I>
where
    I: CSTInput,
{
    Named {
        /// parameter name.
        name: Ident<I>,
        /// punct `=`
        eq: Eequal<I>,
        /// argument value.
        expr: Or<Lit<I>, Path<I>>,
    },
    /// unamed argument.
    Unamed(Or<Lit<I>, Path<I>>),
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, InputSyntaxExt, Or, Punctuated};

    use crate::{
        At, Attribute, AttributeArgument, Comma, Eequal, Ident, ParenEnd, ParenStart, Path,
        PathSegment, PathSep, S, TokenStream,
    };

    #[test]
    fn test_attr() {
        assert_eq!(
            TokenStream::from("@ runtime()").parse::<Attribute<_>>(),
            Ok(Attribute {
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
            TokenStream::from("@attribute(Target::All, ty = Derive)").parse::<Attribute<_>>(),
            Ok(Attribute {
                leading_at: At(None, TokenStream::from((0, "@")), None),
                name: Ident(TokenStream::from((1, "attribute"))),
                arguments: Some(Delimiter {
                    start: ParenStart(None, TokenStream::from((10, "(")), None),
                    end: ParenEnd(None, TokenStream::from((35, ")")), None),
                    body: Punctuated {
                        pairs: vec![(
                            AttributeArgument::Unamed(Or::Second(Path {
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
                            })),
                            Comma(
                                None,
                                TokenStream::from((22, ",")),
                                Some(S(TokenStream::from((23, " "))))
                            )
                        )],
                        tail: Some(Box::new(AttributeArgument::Named {
                            name: Ident(TokenStream::from((24, "ty"))),
                            eq: Eequal(
                                Some(S(TokenStream::from((26, " ")))),
                                TokenStream::from((27, "=")),
                                Some(S(TokenStream::from((28, " "))))
                            ),
                            expr: Or::Second(Path {
                                leading_pathsep: None,
                                segments: Punctuated {
                                    pairs: vec![],
                                    tail: Some(Box::new(PathSegment {
                                        ident: Ident(TokenStream::from((29, "Derive"))),
                                        arguments: None
                                    }))
                                }
                            })
                        }))
                    }
                })
            })
        )
    }
}
