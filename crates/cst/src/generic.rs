use parserc::syntax::{Delimiter, Or, Punctuated, Syntax};

use crate::{
    AngleBracketEnd, AngleBracketStart, CSTInput, Colon, Comma, Const, Eequal, Ident, Lit, Path,
    Plus, Question, Type, Where,
};

/// A trait used as a bound on a type parameter.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]

pub struct TraitBound<I>
where
    I: CSTInput,
{
    /// A modifier on a trait bound, currently only used for the ? in ?Sized.
    pub modifier: Option<Question<I>>,
    /// The Foo<&'a T> in for<'a> Foo<&'a T>
    pub path: Path<I>,
}

/// An individual generic argument, like  T, or Item = T.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum GenericArgument<I>
where
    I: CSTInput,
{
    /// A binding (equality constraint) on an associated type: the Item = u8 in Iterator<Item = u8>.
    Associated {
        ident: Ident<I>,
        eq: Eequal<I>,
        ty: Or<Type<I>, Lit<I>>,
    },
    /// An associated type bound: Iterator<Item: Display>.
    Constraint {
        ident: Ident<I>,
        colon: Colon<I>,
        bounds: Punctuated<TraitBound<I>, Plus<I>>,
    },
    /// A type argument.
    Type(Type<I>),
}

/// A generic type parameter, or const generic: T: Into<String>, 'a: 'b, const LEN: usize.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum GenericParam<I>
where
    I: CSTInput,
{
    Const {
        #[parserc(crucial)]
        keyword: Const<I>,
        ident: Ident<I>,
        colon: Colon<I>,
        ty: Type<I>,
        default: Option<(Eequal<I>, Lit<I>)>,
    },
    Type {
        ident: Ident<I>,
        bounds: Option<(Colon<I>, Punctuated<TraitBound<I>, Plus<I>>)>,
    },
}

/// A single predicate in a where clause: T: Deserialize<'de>.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct WherePredicate<I>
where
    I: CSTInput,
{
    pub ident: Ident<I>,
    pub colon: Colon<I>,
    pub bounds: Punctuated<TraitBound<I>, Plus<I>>,
}

/// A where clause in a definition: where T: Deserialize<'de>, D: 'static.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct WhereClause<I>
where
    I: CSTInput,
{
    /// leading keyword `where`
    pub keyword: Where<I>,
    /// predicate statments of where clause.
    pub predicates: Punctuated<WherePredicate<I>, Comma<I>>,
}

/// Type parameters attached to a declaration of a function, enum, trait, etc.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Generics<I>(
    pub Delimiter<AngleBracketStart<I>, AngleBracketEnd<I>, Punctuated<GenericParam<I>, Comma<I>>>,
)
where
    I: CSTInput;

#[cfg(test)]
mod test {

    use parserc::syntax::{Delimiter, InputSyntaxExt, Or, Punctuated};

    use crate::{
        AngleBracketEnd, AngleBracketStart, Colon, Comma, Const, Digits, Eequal, GenericArgument,
        GenericParam, Generics, Ident, Lit, LitNumber, Path, PathSegment, Plus, Question, S,
        TokenStream, TraitBound, Type, Where, WhereClause, WherePredicate,
    };

    #[test]
    fn test_trait_bound() {
        assert_eq!(
            TokenStream::from("?Sizied").parse::<TraitBound<_>>(),
            Ok(TraitBound {
                modifier: Some(Question(None, TokenStream::from((0, "?")), None)),
                path: Path {
                    leading_pathsep: None,
                    segments: Punctuated {
                        pairs: vec![],
                        tail: Some(Box::new(PathSegment {
                            ident: Ident(TokenStream::from((1, "Sizied"))),
                            arguments: None
                        }))
                    }
                }
            })
        );
    }

    #[test]
    fn test_generic_argument() {
        assert_eq!(
            TokenStream::from("T = u8").parse::<GenericArgument<_>>(),
            Ok(GenericArgument::Associated {
                ident: Ident(TokenStream::from((0, "T"))),
                eq: Eequal(
                    Some(S(TokenStream::from((1, " ")))),
                    TokenStream::from((2, "=")),
                    Some(S(TokenStream::from((3, " "))))
                ),
                ty: Or::First(Type::U8(TokenStream::from((4, "u8"))))
            })
        );

        assert_eq!(
            TokenStream::from("N = 19").parse::<GenericArgument<_>>(),
            Ok(GenericArgument::Associated {
                ident: Ident(TokenStream::from((0, "N"))),
                eq: Eequal(
                    Some(S(TokenStream::from((1, " ")))),
                    TokenStream::from((2, "=")),
                    Some(S(TokenStream::from((3, " "))))
                ),
                ty: Or::Second(Lit::Number(LitNumber {
                    sign: None,
                    trunc: Some(Digits {
                        input: TokenStream::from((4, "19")),
                        value: 19
                    }),
                    fract: None,
                    exp: None
                }))
            })
        );

        assert_eq!(
            TokenStream::from("T: Display + Eq + ").parse::<GenericArgument<_>>(),
            Ok(GenericArgument::Constraint {
                ident: Ident(TokenStream::from((0, "T"))),
                colon: Colon(
                    None,
                    TokenStream::from((1, ":")),
                    Some(S(TokenStream::from((2, " "))))
                ),
                bounds: Punctuated {
                    pairs: vec![
                        (
                            TraitBound {
                                modifier: None,
                                path: Path {
                                    leading_pathsep: None,
                                    segments: Punctuated {
                                        pairs: vec![],
                                        tail: Some(Box::new(PathSegment {
                                            ident: Ident(TokenStream::from((3, "Display"))),
                                            arguments: None
                                        }))
                                    }
                                }
                            },
                            Plus(
                                Some(S(TokenStream::from((10, " ")))),
                                TokenStream::from((11, "+")),
                                Some(S(TokenStream::from((12, " "))))
                            )
                        ),
                        (
                            TraitBound {
                                modifier: None,
                                path: Path {
                                    leading_pathsep: None,
                                    segments: Punctuated {
                                        pairs: vec![],
                                        tail: Some(Box::new(PathSegment {
                                            ident: Ident(TokenStream::from((13, "Eq"))),
                                            arguments: None
                                        }))
                                    }
                                }
                            },
                            Plus(
                                Some(S(TokenStream::from((15, " ")))),
                                TokenStream::from((16, "+")),
                                Some(S(TokenStream::from((17, " "))))
                            )
                        )
                    ],
                    tail: None
                }
            })
        );
    }

    #[test]
    fn test_generics() {
        assert_eq!(
            TokenStream::from("<const N: u32 = 10, T: Display + Debug + >").parse(),
            Ok(Generics(Delimiter {
                start: AngleBracketStart(None, TokenStream::from((0, "<")), None),
                end: AngleBracketEnd(None, TokenStream::from((41, ">")), None),
                body: Punctuated {
                    pairs: vec![(
                        GenericParam::Const {
                            keyword: Const(
                                TokenStream::from((1, "const")),
                                Some(S(TokenStream::from((6, " "))))
                            ),
                            ident: Ident(TokenStream::from((7, "N"))),
                            colon: Colon(
                                None,
                                TokenStream::from((8, ":")),
                                Some(S(TokenStream::from((9, " "))))
                            ),
                            ty: Type::U32(TokenStream::from((10, "u32"))),
                            default: Some((
                                Eequal(
                                    Some(S(TokenStream::from((13, " ")))),
                                    TokenStream::from((14, "=")),
                                    Some(S(TokenStream::from((15, " "))))
                                ),
                                Lit::Number(LitNumber {
                                    sign: None,
                                    trunc: Some(Digits {
                                        input: TokenStream::from((16, "10")),
                                        value: 10
                                    }),
                                    fract: None,
                                    exp: None
                                })
                            ))
                        },
                        Comma(
                            None,
                            TokenStream::from((18, ",")),
                            Some(S(TokenStream::from((19, " "))))
                        )
                    )],
                    tail: Some(Box::new(GenericParam::Type {
                        ident: Ident(TokenStream::from((20, "T"))),
                        bounds: Some((
                            Colon(
                                None,
                                TokenStream::from((21, ":")),
                                Some(S(TokenStream::from((22, " "))))
                            ),
                            Punctuated {
                                pairs: vec![
                                    (
                                        TraitBound {
                                            modifier: None,
                                            path: Path {
                                                leading_pathsep: None,
                                                segments: Punctuated {
                                                    pairs: vec![],
                                                    tail: Some(Box::new(PathSegment {
                                                        ident: Ident(TokenStream::from((
                                                            23, "Display"
                                                        ))),
                                                        arguments: None
                                                    }))
                                                }
                                            }
                                        },
                                        Plus(
                                            Some(S(TokenStream::from((30, " ")))),
                                            TokenStream::from((31, "+")),
                                            Some(S(TokenStream::from((32, " "))))
                                        )
                                    ),
                                    (
                                        TraitBound {
                                            modifier: None,
                                            path: Path {
                                                leading_pathsep: None,
                                                segments: Punctuated {
                                                    pairs: vec![],
                                                    tail: Some(Box::new(PathSegment {
                                                        ident: Ident(TokenStream::from((
                                                            33, "Debug"
                                                        ))),
                                                        arguments: None
                                                    }))
                                                }
                                            }
                                        },
                                        Plus(
                                            Some(S(TokenStream::from((38, " ")))),
                                            TokenStream::from((39, "+")),
                                            Some(S(TokenStream::from((40, " "))))
                                        )
                                    )
                                ],
                                tail: None
                            }
                        ))
                    }))
                }
            }))
        );
    }

    #[test]
    fn test_where_clause() {
        assert_eq!(
            TokenStream::from("where\t\nT: Display + Debug +,\n B: Eq,").parse::<WhereClause<_>>(),
            Ok(WhereClause {
                keyword: Where(
                    TokenStream::from((0, "where")),
                    Some(S(TokenStream::from((5, "\t\n"))))
                ),
                predicates: Punctuated {
                    pairs: vec![
                        (
                            WherePredicate {
                                ident: Ident(TokenStream::from((7, "T"))),
                                colon: Colon(
                                    None,
                                    TokenStream::from((8, ":")),
                                    Some(S(TokenStream::from((9, " "))))
                                ),
                                bounds: Punctuated {
                                    pairs: vec![
                                        (
                                            TraitBound {
                                                modifier: None,
                                                path: Path {
                                                    leading_pathsep: None,
                                                    segments: Punctuated {
                                                        pairs: vec![],
                                                        tail: Some(Box::new(PathSegment {
                                                            ident: Ident(TokenStream::from((
                                                                10, "Display"
                                                            ))),
                                                            arguments: None
                                                        }))
                                                    }
                                                }
                                            },
                                            Plus(
                                                Some(S(TokenStream::from((17, " ")))),
                                                TokenStream::from((18, "+")),
                                                Some(S(TokenStream::from((19, " "))))
                                            )
                                        ),
                                        (
                                            TraitBound {
                                                modifier: None,
                                                path: Path {
                                                    leading_pathsep: None,
                                                    segments: Punctuated {
                                                        pairs: vec![],
                                                        tail: Some(Box::new(PathSegment {
                                                            ident: Ident(TokenStream::from((
                                                                20, "Debug"
                                                            ))),
                                                            arguments: None
                                                        }))
                                                    }
                                                }
                                            },
                                            Plus(
                                                Some(S(TokenStream::from((25, " ")))),
                                                TokenStream::from((26, "+")),
                                                None
                                            )
                                        )
                                    ],
                                    tail: None
                                }
                            },
                            Comma(
                                None,
                                TokenStream::from((27, ",")),
                                Some(S(TokenStream::from((28, "\n "))))
                            )
                        ),
                        (
                            WherePredicate {
                                ident: Ident(TokenStream::from((30, "B"))),
                                colon: Colon(
                                    None,
                                    TokenStream::from((31, ":")),
                                    Some(S(TokenStream::from((32, " "))))
                                ),
                                bounds: Punctuated {
                                    pairs: vec![],
                                    tail: Some(Box::new(TraitBound {
                                        modifier: None,
                                        path: Path {
                                            leading_pathsep: None,
                                            segments: Punctuated {
                                                pairs: vec![],
                                                tail: Some(Box::new(PathSegment {
                                                    ident: Ident(TokenStream::from((33, "Eq"))),
                                                    arguments: None
                                                }))
                                            }
                                        }
                                    }))
                                }
                            },
                            Comma(None, TokenStream::from((35, ",")), None)
                        )
                    ],
                    tail: None
                }
            })
        );
    }
}
