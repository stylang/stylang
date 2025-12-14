use parserc::syntax::{Delimiter, Or, Punctuated, Syntax};

use crate::{
    block::Block,
    errors::{CSTError, SemanticsKind},
    generics::{Generics, WhereClause},
    input::CSTInput,
    keyword::{Extern, Fn_},
    misc::{Ident, S},
    pat::PatType,
    punct::{ArrowRight, Comma, ParenEnd, ParenStart, Semi},
    ty::Type,
    vs::Visibility,
};

/// A free-standing extern function: `extern fn process(n: usize) -> Result<()>;`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(semantic = item_fn_semantic_check)]
pub struct ItemFn<I>
where
    I: CSTInput,
{
    /// optional vs
    pub vs: Option<Visibility<I>>,
    /// optional keyword `extern`
    pub keyword_extern: Option<Extern<I>>,
    /// keyword `fn`
    #[parserc(crucial)]
    pub keyword_fn: Fn_<I>,
    /// name of this function.
    pub ident: Ident<I>,
    /// generic parameters.
    pub generics: Option<Generics<I>>,
    /// input parameters.
    pub inputs: Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<PatType<I>, Comma<I>>>,
    /// Return type of a function signature.
    pub output: Option<(ArrowRight<I>, Box<Type<I>>)>,
    /// optional generic where clause.
    pub where_clause: Option<(Option<S<I>>, WhereClause<I>)>,
    /// function body `;` or `block`
    pub body: Or<Semi<I>, Block<I>>,
}

#[inline]
fn item_fn_semantic_check<I>(_input: &mut I, item: ItemFn<I>) -> Result<ItemFn<I>, CSTError>
where
    I: CSTInput,
{
    if item.keyword_extern.is_some() {
        if let Some(generic) = &item.generics {
            return Err(CSTError::Semantics(
                SemanticsKind::Generics,
                generic.to_span(),
            ));
        }

        if let Some(where_clause) = &item.where_clause {
            return Err(CSTError::Semantics(
                SemanticsKind::WhereClause,
                where_clause.to_span(),
            ));
        }

        if let Or::Second(block) = &item.body {
            return Err(CSTError::Semantics(SemanticsKind::Block, block.to_span()));
        }
    } else {
        if let Or::First(semi) = &item.body {
            return Err(CSTError::Semantics(SemanticsKind::Semi, semi.to_span()));
        }
    }

    Ok(item)
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, Or, Punctuated, SyntaxInput};

    use crate::{
        block::{Block, Stmt},
        expr::{Expr, ExprPath},
        generics::{GenericParam, Generics, TraitBound, WhereClause, WherePredicate},
        input::TokenStream,
        item::ItemFn,
        keyword::{Extern, Fn_, Where},
        misc::{Ident, S},
        pat::{Pat, PatIdent, PatType},
        path::{Path, PathSegment},
        punct::{
            AngleBracketEnd, AngleBracketStart, ArrowRight, BraceEnd, BraceStart, Colon, ParenEnd,
            ParenStart, Semi,
        },
        ty::Type,
        vs::{Visibility, VisibilityPredicate},
    };

    #[test]
    fn test_extern_fn() {
        assert_eq!(
            TokenStream::from("extern fn f();").parse::<ItemFn<_>>(),
            Ok(ItemFn {
                vs: None,
                keyword_extern: Some(Extern(
                    TokenStream::from((0, "extern")),
                    Some(S(TokenStream::from((6, " "))))
                )),
                keyword_fn: Fn_(
                    TokenStream::from((7, "fn")),
                    Some(S(TokenStream::from((9, " "))))
                ),
                ident: Ident(TokenStream::from((10, "f"))),
                generics: None,
                inputs: Delimiter {
                    start: ParenStart(None, TokenStream::from((11, "(")), None),
                    end: ParenEnd(None, TokenStream::from((12, ")")), None),
                    body: Punctuated {
                        pairs: vec![],
                        tail: None
                    }
                },
                output: None,
                where_clause: None,
                body: Or::First(Semi(None, TokenStream::from((13, ";")), None))
            })
        );
    }

    #[test]
    fn test_fn() {
        assert_eq!(
            TokenStream::from("pub(super) fn A<T>(input: T) -> T where T: Debug { input }")
                .parse::<ItemFn<_>>(),
            Ok(ItemFn {
                vs: Some(Visibility {
                    keyword: TokenStream::from((0, "pub")),
                    predicate: Or::First(Delimiter {
                        start: ParenStart(None, TokenStream::from((3, "(")), None),
                        end: ParenEnd(
                            None,
                            TokenStream::from((9, ")")),
                            Some(S(TokenStream::from((10, " "))))
                        ),
                        body: VisibilityPredicate::Super(TokenStream::from((4, "super")))
                    })
                }),
                keyword_extern: None,
                keyword_fn: Fn_(
                    TokenStream::from((11, "fn")),
                    Some(S(TokenStream::from((13, " "))))
                ),
                ident: Ident(TokenStream::from((14, "A"))),
                generics: Some(Generics(Delimiter {
                    start: AngleBracketStart(None, TokenStream::from((15, "<")), None),
                    end: AngleBracketEnd(None, TokenStream::from((17, ">")), None),
                    body: Punctuated {
                        pairs: vec![],
                        tail: Some(Box::new(GenericParam::Type {
                            ident: Ident(TokenStream::from((16, "T"))),
                            bounds: None
                        }))
                    }
                })),
                inputs: Delimiter {
                    start: ParenStart(None, TokenStream::from((18, "(")), None),
                    end: ParenEnd(
                        None,
                        TokenStream::from((27, ")")),
                        Some(S(TokenStream::from((28, " "))))
                    ),
                    body: Punctuated {
                        pairs: vec![],
                        tail: Some(Box::new(PatType {
                            pat: Box::new(Pat::Ident(PatIdent {
                                by_ref: None,
                                mutability: None,
                                ident: Ident(TokenStream::from((19, "input"))),
                                subpat: None
                            })),
                            colon: Colon(
                                None,
                                TokenStream::from((24, ":")),
                                Some(S(TokenStream::from((25, " "))))
                            ),
                            ty: Box::new(Type::Path(Path {
                                leading_pathsep: None,
                                segments: Punctuated {
                                    pairs: vec![],
                                    tail: Some(Box::new(PathSegment {
                                        ident: Ident(TokenStream::from((26, "T"))),
                                        arguments: None
                                    }))
                                }
                            }))
                        }))
                    }
                },
                output: Some((
                    ArrowRight(
                        None,
                        TokenStream::from((29, "->")),
                        Some(S(TokenStream::from((31, " "))))
                    ),
                    Box::new(Type::Path(Path {
                        leading_pathsep: None,
                        segments: Punctuated {
                            pairs: vec![],
                            tail: Some(Box::new(PathSegment {
                                ident: Ident(TokenStream::from((32, "T"))),
                                arguments: None
                            }))
                        }
                    }))
                )),
                where_clause: Some((
                    Some(S(TokenStream::from((33, " ")))),
                    WhereClause {
                        keyword: Where(
                            TokenStream::from((34, "where")),
                            Some(S(TokenStream::from((39, " "))))
                        ),
                        predicates: Punctuated {
                            pairs: vec![],
                            tail: Some(Box::new(WherePredicate {
                                ident: Ident(TokenStream::from((40, "T"))),
                                colon: Colon(
                                    None,
                                    TokenStream::from((41, ":")),
                                    Some(S(TokenStream::from((42, " "))))
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
                                                    ident: Ident(TokenStream::from((43, "Debug"))),
                                                    arguments: None
                                                }))
                                            }
                                        }
                                    }))
                                }
                            }))
                        }
                    }
                )),
                body: Or::Second(Block(Delimiter {
                    start: BraceStart(
                        Some(S(TokenStream::from((48, " ")))),
                        TokenStream::from((49, "{")),
                        Some(S(TokenStream::from((50, " "))))
                    ),
                    end: BraceEnd(
                        Some(S(TokenStream::from((56, " ")))),
                        TokenStream::from((57, "}")),
                        None
                    ),
                    body: vec![Stmt::Expr(
                        Expr::Path(ExprPath {
                            qself: None,
                            path: Path {
                                leading_pathsep: None,
                                segments: Punctuated {
                                    pairs: vec![],
                                    tail: Some(Box::new(PathSegment {
                                        ident: Ident(TokenStream::from((51, "input"))),
                                        arguments: None
                                    }))
                                }
                            }
                        }),
                        None
                    )]
                }))
            })
        );
    }
}
