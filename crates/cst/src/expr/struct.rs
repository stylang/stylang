use parserc::{
    ControlFlow,
    syntax::{Or, Punctuated, Syntax},
};

use crate::{
    attr::OuterAttribute,
    errors::{CSTError, PunctKind, SemanticsKind},
    expr::Expr,
    input::CSTInput,
    lexical::{
        delimiter::Brace,
        ident::Ident,
        lit::TupleIndex,
        punct::{Colon, Comma, DotDot},
    },
    names::paths::PathInExpr,
};

/// A struct expression creates a struct, enum, or union value.
/// It consists of a path to a struct, enum variant, or union
/// item followed by the values for the fields of the item.
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions/struct-expr.html#grammar-StructExpression
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct StructExpr<I>
where
    I: CSTInput,
{
    /// struct name.
    pub path: PathInExpr<I>,
    pub fields: Brace<I, StructExprFields<I>>,
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions/struct-expr.html#railroad-StructExprFields
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(semantic = check_struct_expr_fields)]
pub struct StructExprFields<I>
where
    I: CSTInput,
{
    pub fields: Punctuated<StructExprField<I>, Comma<I>>,
    pub base: Option<(Option<Comma<I>>, DotDot<I>, Box<Expr<I>>)>,
}

#[inline]
fn check_struct_expr_fields<I>(
    _: I,
    fields: StructExprFields<I>,
) -> Result<StructExprFields<I>, CSTError>
where
    I: CSTInput,
{
    if let Some((comma, dotdot, _)) = &fields.base {
        if (fields.fields.is_empty() || fields.fields.tail.is_some()) && comma.is_none() {
            return Err(CSTError::Punct(
                PunctKind::Comma,
                ControlFlow::Fatal,
                dotdot.0.to_span() + dotdot.1.to_span_at(0),
            ));
        }

        if fields.fields.tail.is_none() && comma.is_some() && !fields.fields.pairs.is_empty() {
            return Err(CSTError::Semantics(SemanticsKind::Comma, comma.to_span()));
        }
    }

    Ok(fields)
}

/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions/struct-expr.html#railroad-StructExprFields
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(semantic = check_struct_expr_field)]
pub struct StructExprField<I>
where
    I: CSTInput,
{
    pub attrs: Vec<OuterAttribute<I>>,
    pub name: Or<Ident<I>, TupleIndex<I>>,
    pub expr: Option<(Colon<I>, Box<Expr<I>>)>,
}

#[inline]
fn check_struct_expr_field<I>(
    _: I,
    field: StructExprField<I>,
) -> Result<StructExprField<I>, CSTError>
where
    I: CSTInput,
{
    if field.expr.is_none() {
        if let Or::Second(_) = field.name {
            return Err(CSTError::Syntax(
                crate::errors::SyntaxKind::StructExprFieldPredicate,
                ControlFlow::Fatal,
                field.to_span(),
            ));
        }
    }

    Ok(field)
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, SyntaxInput};

    use crate::{
        expr::{Expr, ExprWithoutBlock, LitExpr, PathExpr},
        input::TokenStream,
        lexical::{
            S,
            ident::NonKeywordIdent,
            lit::{LitDec, LitFloat, LitInt},
            punct::{BraceEnd, BraceStart},
        },
        names::paths::{PathExprSegment, PathIdentSegment},
    };

    use super::*;

    #[test]
    fn test_struct_expr() {
        assert_eq!(
            TokenStream::from("Point {x: 10.0, y: 20.0}").parse::<Expr<_>>(),
            Ok(Expr::WithoutBlock(ExprWithoutBlock::StructExpr(
                StructExpr {
                    path: PathInExpr {
                        leading_sep: None,
                        first: PathExprSegment {
                            ident: PathIdentSegment::Ident(Ident::NonKeywordIdent(
                                NonKeywordIdent(TokenStream::from((0, "Point")))
                            )),
                            generic_args: None
                        },
                        rest: vec![]
                    },
                    fields: Delimiter {
                        start: BraceStart(
                            Some(S(TokenStream::from((5, " ")))),
                            TokenStream::from((6, "{")),
                            None
                        ),
                        end: BraceEnd(None, TokenStream::from((23, "}")), None),
                        body: StructExprFields {
                            fields: Punctuated {
                                pairs: vec![(
                                    StructExprField {
                                        attrs: vec![],
                                        name: Or::First(Ident::NonKeywordIdent(NonKeywordIdent(
                                            TokenStream::from((7, "x"))
                                        ))),
                                        expr: Some((
                                            Colon(
                                                None,
                                                TokenStream::from((8, ":")),
                                                Some(S(TokenStream::from((9, " "))))
                                            ),
                                            Box::new(Expr::WithoutBlock(
                                                ExprWithoutBlock::LitExpr(
                                                    vec![],
                                                    LitExpr::Float(
                                                        LitFloat {
                                                            trunc: LitDec(TokenStream::from((
                                                                10, "10"
                                                            ))),
                                                            period: Some(TokenStream::from((
                                                                12, "."
                                                            ))),
                                                            fract: Some(LitDec(TokenStream::from(
                                                                (13, "0")
                                                            ))),
                                                            exp: None
                                                        },
                                                        None
                                                    )
                                                )
                                            ))
                                        ))
                                    },
                                    Comma(
                                        None,
                                        TokenStream::from((14, ",")),
                                        Some(S(TokenStream::from((15, " "))))
                                    )
                                )],
                                tail: Some(Box::new(StructExprField {
                                    attrs: vec![],
                                    name: Or::First(Ident::NonKeywordIdent(NonKeywordIdent(
                                        TokenStream::from((16, "y"))
                                    ))),
                                    expr: Some((
                                        Colon(
                                            None,
                                            TokenStream::from((17, ":")),
                                            Some(S(TokenStream::from((18, " "))))
                                        ),
                                        Box::new(Expr::WithoutBlock(ExprWithoutBlock::LitExpr(
                                            vec![],
                                            LitExpr::Float(
                                                LitFloat {
                                                    trunc: LitDec(TokenStream::from((19, "20"))),
                                                    period: Some(TokenStream::from((21, "."))),
                                                    fract: Some(LitDec(TokenStream::from((
                                                        22, "0"
                                                    )))),
                                                    exp: None
                                                },
                                                None
                                            )
                                        )))
                                    ))
                                }))
                            },
                            base: None
                        }
                    }
                }
            )))
        );

        assert_eq!(
            TokenStream::from("Point3d {y: 0, z: 10, .. base}").parse::<Expr<_>>(),
            Ok(Expr::WithoutBlock(ExprWithoutBlock::StructExpr(
                StructExpr {
                    path: PathInExpr {
                        leading_sep: None,
                        first: PathExprSegment {
                            ident: PathIdentSegment::Ident(Ident::NonKeywordIdent(
                                NonKeywordIdent(TokenStream::from((0, "Point3d")))
                            )),
                            generic_args: None
                        },
                        rest: vec![]
                    },
                    fields: Delimiter {
                        start: BraceStart(
                            Some(S(TokenStream::from((7, " ")))),
                            TokenStream::from((8, "{")),
                            None
                        ),
                        end: BraceEnd(None, TokenStream::from((29, "}")), None),
                        body: StructExprFields {
                            fields: Punctuated {
                                pairs: vec![
                                    (
                                        StructExprField {
                                            attrs: vec![],
                                            name: Or::First(Ident::NonKeywordIdent(
                                                NonKeywordIdent(TokenStream::from((9, "y")))
                                            )),
                                            expr: Some((
                                                Colon(
                                                    None,
                                                    TokenStream::from((10, ":")),
                                                    Some(S(TokenStream::from((11, " "))))
                                                ),
                                                Box::new(Expr::WithoutBlock(
                                                    ExprWithoutBlock::LitExpr(
                                                        vec![],
                                                        LitExpr::Int(
                                                            LitInt::Dec(LitDec(TokenStream::from(
                                                                (12, "0")
                                                            ))),
                                                            None
                                                        )
                                                    )
                                                ))
                                            ))
                                        },
                                        Comma(
                                            None,
                                            TokenStream::from((13, ",")),
                                            Some(S(TokenStream::from((14, " "))))
                                        )
                                    ),
                                    (
                                        StructExprField {
                                            attrs: vec![],
                                            name: Or::First(Ident::NonKeywordIdent(
                                                NonKeywordIdent(TokenStream::from((15, "z")))
                                            )),
                                            expr: Some((
                                                Colon(
                                                    None,
                                                    TokenStream::from((16, ":")),
                                                    Some(S(TokenStream::from((17, " "))))
                                                ),
                                                Box::new(Expr::WithoutBlock(
                                                    ExprWithoutBlock::LitExpr(
                                                        vec![],
                                                        LitExpr::Int(
                                                            LitInt::Dec(LitDec(TokenStream::from(
                                                                (18, "10")
                                                            ))),
                                                            None
                                                        )
                                                    )
                                                ))
                                            ))
                                        },
                                        Comma(
                                            None,
                                            TokenStream::from((20, ",")),
                                            Some(S(TokenStream::from((21, " "))))
                                        )
                                    )
                                ],
                                tail: None
                            },
                            base: Some((
                                None,
                                DotDot(
                                    None,
                                    TokenStream::from((22, "..")),
                                    Some(S(TokenStream::from((24, " "))))
                                ),
                                Box::new(Expr::WithoutBlock(ExprWithoutBlock::PathExpr(
                                    vec![],
                                    PathExpr::Path(PathInExpr {
                                        leading_sep: None,
                                        first: PathExprSegment {
                                            ident: PathIdentSegment::Ident(Ident::NonKeywordIdent(
                                                NonKeywordIdent(TokenStream::from((25, "base")))
                                            )),
                                            generic_args: None
                                        },
                                        rest: vec![]
                                    })
                                )))
                            ))
                        }
                    }
                }
            )))
        );

        assert_eq!(
            TokenStream::from("Color{1: 0, ..c2}").parse::<Expr<_>>(),
            Ok(Expr::WithoutBlock(ExprWithoutBlock::StructExpr(
                StructExpr {
                    path: PathInExpr {
                        leading_sep: None,
                        first: PathExprSegment {
                            ident: PathIdentSegment::Ident(Ident::NonKeywordIdent(
                                NonKeywordIdent(TokenStream::from((0, "Color")))
                            )),
                            generic_args: None
                        },
                        rest: vec![]
                    },
                    fields: Delimiter {
                        start: BraceStart(None, TokenStream::from((5, "{")), None),
                        end: BraceEnd(None, TokenStream::from((16, "}")), None),
                        body: StructExprFields {
                            fields: Punctuated {
                                pairs: vec![(
                                    StructExprField {
                                        attrs: vec![],
                                        name: Or::Second(TupleIndex::Dec(LitDec(
                                            TokenStream::from((6, "1"))
                                        ))),
                                        expr: Some((
                                            Colon(
                                                None,
                                                TokenStream::from((7, ":")),
                                                Some(S(TokenStream::from((8, " "))))
                                            ),
                                            Box::new(Expr::WithoutBlock(
                                                ExprWithoutBlock::LitExpr(
                                                    vec![],
                                                    LitExpr::Int(
                                                        LitInt::Dec(LitDec(TokenStream::from((
                                                            9, "0"
                                                        )))),
                                                        None
                                                    )
                                                )
                                            ))
                                        ))
                                    },
                                    Comma(
                                        None,
                                        TokenStream::from((10, ",")),
                                        Some(S(TokenStream::from((11, " "))))
                                    )
                                )],
                                tail: None
                            },
                            base: Some((
                                None,
                                DotDot(None, TokenStream::from((12, "..")), None),
                                Box::new(Expr::WithoutBlock(ExprWithoutBlock::PathExpr(
                                    vec![],
                                    PathExpr::Path(PathInExpr {
                                        leading_sep: None,
                                        first: PathExprSegment {
                                            ident: PathIdentSegment::Ident(Ident::NonKeywordIdent(
                                                NonKeywordIdent(TokenStream::from((14, "c2")))
                                            )),
                                            generic_args: None
                                        },
                                        rest: vec![]
                                    })
                                )))
                            ))
                        }
                    }
                }
            )))
        );
    }
}
