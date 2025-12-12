use parserc::syntax::{Delimiter, Punctuated, Syntax};

use crate::{
    expr::{ExprPath, Member},
    input::CSTInput,
    keyword::{Mut, Ref},
    misc::Ident,
    pat::Pat,
    punct::{BraceEnd, BraceStart, Colon, Comma, DotDot, ParenEnd, ParenStart},
};

/// A tuple struct or tuple variant pattern: Variant(x, y, .., z).
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PatTupleStruct<I>
where
    I: CSTInput,
{
    /// type path.
    pub path: ExprPath<I>,
    /// struct elements.
    pub elms: Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<Pat<I>, Comma<I>>>,
}

/// A single field in a struct pattern.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum FieldPat<I>
where
    I: CSTInput,
{
    Member {
        /// type path.
        member: Member<I>,
        /// punct `:`
        colon: Colon<I>,
        /// field sub-parttern.
        subpat: Box<Pat<I>>,
    },
    Ident {
        /// an optional keyword `ref`.
        by_ref: Option<Ref<I>>,
        /// an optional keyword `mut`
        mutability: Option<Mut<I>>,
        /// unique identifier within the pattern.
        ident: Ident<I>,
    },
}

/// A struct or struct variant pattern: Variant { x, y, .. }.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PatStruct<I>
where
    I: CSTInput,
{
    /// type path.
    pub path: ExprPath<I>,
    /// delimiter start punct `{`
    pub delimiter_start: BraceStart<I>,
    /// field pats.
    pub elems: Punctuated<FieldPat<I>, Comma<I>>,
    /// optional rest pattern.
    pub rest: Option<DotDot<I>>,
    /// delimiter start punct `}`
    pub delimiter_end: BraceEnd<I>,
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, Punctuated, SyntaxInput};

    use crate::{
        expr::{Digits, ExprLit, ExprPath, LitNumber, Member},
        input::TokenStream,
        keyword::{Mut, Ref},
        misc::{Ident, S},
        pat::{FieldPat, Pat, PatIdent, PatStruct, PatTupleStruct},
        path::{Path, PathSegment},
        punct::{At, BraceEnd, BraceStart, Colon, Comma, DotDot, ParenEnd, ParenStart, PathSep},
    };

    #[test]
    fn test_tuple_struct() {
        assert_eq!(
            TokenStream::from("a::b(x,y,n @..,z)").parse::<Pat<_>>(),
            Ok(Pat::TupleStruct(PatTupleStruct {
                path: ExprPath {
                    qself: None,
                    path: Path {
                        leading_pathsep: None,
                        segments: Punctuated {
                            pairs: vec![(
                                PathSegment {
                                    ident: Ident(TokenStream::from((0, "a"))),
                                    arguments: None
                                },
                                PathSep(None, TokenStream::from((1, "::")), None)
                            )],
                            tail: Some(Box::new(PathSegment {
                                ident: Ident(TokenStream::from((3, "b"))),
                                arguments: None
                            }))
                        }
                    }
                },
                elms: Delimiter {
                    start: ParenStart(None, TokenStream::from((4, "(")), None),
                    end: ParenEnd(None, TokenStream::from((16, ")")), None),
                    body: Punctuated {
                        pairs: vec![
                            (
                                Pat::Ident(PatIdent {
                                    by_ref: None,
                                    mutability: None,
                                    ident: Ident(TokenStream::from((5, "x"))),
                                    subpat: None
                                }),
                                Comma(None, TokenStream::from((6, ",")), None)
                            ),
                            (
                                Pat::Ident(PatIdent {
                                    by_ref: None,
                                    mutability: None,
                                    ident: Ident(TokenStream::from((7, "y"))),
                                    subpat: None
                                }),
                                Comma(None, TokenStream::from((8, ",")), None)
                            ),
                            (
                                Pat::Ident(PatIdent {
                                    by_ref: None,
                                    mutability: None,
                                    ident: Ident(TokenStream::from((9, "n"))),
                                    subpat: Some((
                                        At(
                                            Some(S(TokenStream::from((10, " ")))),
                                            TokenStream::from((11, "@")),
                                            None
                                        ),
                                        Box::new(Pat::Rest(DotDot(
                                            None,
                                            TokenStream::from((12, "..")),
                                            None
                                        )))
                                    ))
                                }),
                                Comma(None, TokenStream::from((14, ",")), None)
                            )
                        ],
                        tail: Some(Box::new(Pat::Ident(PatIdent {
                            by_ref: None,
                            mutability: None,
                            ident: Ident(TokenStream::from((15, "z"))),
                            subpat: None
                        })))
                    }
                }
            }))
        );
    }

    #[test]
    fn test_ident_pattner() {
        assert_eq!(
            TokenStream::from("Point { x:10, y:20, .. }").parse::<Pat<_>>(),
            Ok(Pat::Struct(PatStruct {
                path: ExprPath {
                    qself: None,
                    path: Path {
                        leading_pathsep: None,
                        segments: Punctuated {
                            pairs: vec![],
                            tail: Some(Box::new(PathSegment {
                                ident: Ident(TokenStream::from((0, "Point"))),
                                arguments: None
                            }))
                        }
                    }
                },
                delimiter_start: BraceStart(
                    Some(S(TokenStream::from((5, " ")))),
                    TokenStream::from((6, "{")),
                    Some(S(TokenStream::from((7, " "))))
                ),
                elems: Punctuated {
                    pairs: vec![
                        (
                            FieldPat::Member {
                                member: Member::Named(Ident(TokenStream::from((8, "x")))),
                                colon: Colon(None, TokenStream::from((9, ":")), None),
                                subpat: Box::new(Pat::Lit(ExprLit::Number(LitNumber {
                                    sign: None,
                                    trunc: Some(Digits {
                                        input: TokenStream::from((10, "10")),
                                        value: 10
                                    }),
                                    fract: None,
                                    exp: None
                                })))
                            },
                            Comma(
                                None,
                                TokenStream::from((12, ",")),
                                Some(S(TokenStream::from((13, " "))))
                            )
                        ),
                        (
                            FieldPat::Member {
                                member: Member::Named(Ident(TokenStream::from((14, "y")))),
                                colon: Colon(None, TokenStream::from((15, ":")), None),
                                subpat: Box::new(Pat::Lit(ExprLit::Number(LitNumber {
                                    sign: None,
                                    trunc: Some(Digits {
                                        input: TokenStream::from((16, "20")),
                                        value: 20
                                    }),
                                    fract: None,
                                    exp: None
                                })))
                            },
                            Comma(
                                None,
                                TokenStream::from((18, ",")),
                                Some(S(TokenStream::from((19, " "))))
                            )
                        )
                    ],
                    tail: None
                },
                rest: Some(DotDot(
                    None,
                    TokenStream::from((20, "..")),
                    Some(S(TokenStream::from((22, " "))))
                )),
                delimiter_end: BraceEnd(None, TokenStream::from((23, "}")), None)
            }))
        );
    }

    #[test]
    fn test_struct_tuple_index_pattern() {
        assert_eq!(
            TokenStream::from("T { 0: 10}").parse::<Pat<_>>(),
            Ok(Pat::Struct(PatStruct {
                path: ExprPath {
                    qself: None,
                    path: Path {
                        leading_pathsep: None,
                        segments: Punctuated {
                            pairs: vec![],
                            tail: Some(Box::new(PathSegment {
                                ident: Ident(TokenStream::from((0, "T"))),
                                arguments: None
                            }))
                        }
                    }
                },
                delimiter_start: BraceStart(
                    Some(S(TokenStream::from((1, " ")))),
                    TokenStream::from((2, "{")),
                    Some(S(TokenStream::from((3, " "))))
                ),
                elems: Punctuated {
                    pairs: vec![],
                    tail: Some(Box::new(FieldPat::Member {
                        member: Member::Unamed(Digits {
                            input: TokenStream::from((4, "0")),
                            value: 0
                        }),
                        colon: Colon(
                            None,
                            TokenStream::from((5, ":")),
                            Some(S(TokenStream::from((6, " "))))
                        ),
                        subpat: Box::new(Pat::Lit(ExprLit::Number(LitNumber {
                            sign: None,
                            trunc: Some(Digits {
                                input: TokenStream::from((7, "10")),
                                value: 10
                            }),
                            fract: None,
                            exp: None
                        })))
                    }))
                },
                rest: None,
                delimiter_end: BraceEnd(None, TokenStream::from((9, "}")), None)
            }))
        );
    }

    #[test]
    fn test_struct_ident() {
        assert_eq!(
            TokenStream::from("A { ref mut c }").parse::<Pat<_>>(),
            Ok(Pat::Struct(PatStruct {
                path: ExprPath {
                    qself: None,
                    path: Path {
                        leading_pathsep: None,
                        segments: Punctuated {
                            pairs: vec![],
                            tail: Some(Box::new(PathSegment {
                                ident: Ident(TokenStream::from((0, "A"))),
                                arguments: None
                            }))
                        }
                    }
                },
                delimiter_start: BraceStart(
                    Some(S(TokenStream::from((1, " ")))),
                    TokenStream::from((2, "{")),
                    Some(S(TokenStream::from((3, " "))))
                ),
                elems: Punctuated {
                    pairs: vec![],
                    tail: Some(Box::new(FieldPat::Ident {
                        by_ref: Some(Ref(
                            TokenStream::from((4, "ref")),
                            Some(S(TokenStream::from((7, " "))))
                        )),
                        mutability: Some(Mut(
                            TokenStream::from((8, "mut")),
                            Some(S(TokenStream::from((11, " "))))
                        )),
                        ident: Ident(TokenStream::from((12, "c")))
                    }))
                },
                rest: None,
                delimiter_end: BraceEnd(
                    Some(S(TokenStream::from((13, " ")))),
                    TokenStream::from((14, "}")),
                    None
                )
            }))
        );
    }

    #[test]
    fn test_rest() {
        assert_eq!(
            TokenStream::from("T { .. } ").parse::<Pat<_>>(),
            Ok(Pat::Struct(PatStruct {
                path: ExprPath {
                    qself: None,
                    path: Path {
                        leading_pathsep: None,
                        segments: Punctuated {
                            pairs: vec![],
                            tail: Some(Box::new(PathSegment {
                                ident: Ident(TokenStream::from((0, "T"))),
                                arguments: None
                            }))
                        }
                    }
                },
                delimiter_start: BraceStart(
                    Some(S(TokenStream::from((1, " ")))),
                    TokenStream::from((2, "{")),
                    Some(S(TokenStream::from((3, " "))))
                ),
                elems: Punctuated {
                    pairs: vec![],
                    tail: None
                },
                rest: Some(DotDot(
                    None,
                    TokenStream::from((4, "..")),
                    Some(S(TokenStream::from((6, " "))))
                )),
                delimiter_end: BraceEnd(
                    None,
                    TokenStream::from((7, "}")),
                    Some(S(TokenStream::from((8, " "))))
                )
            }))
        );
    }
}
