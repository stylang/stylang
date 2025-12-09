use parserc::syntax::Syntax;

use crate::{
    errors::SyntaxKind,
    expr::{Expr, ExprChain},
    input::CSTInput,
    punct::{Minus, Not, Star},
};

/// Unary operator.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::ExprBinaryOp.map())]
pub enum UnaryOp<I>
where
    I: CSTInput,
{
    Deref(Star<I>),
    Not(Not<I>),
    Neg(Minus<I>),
}

/// A unary operator: `*`, `!`, `-`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprUnary<I>
where
    I: CSTInput,
{
    /// leading unary operator char.
    #[parserc(crucial)]
    pub op: UnaryOp<I>,
    /// right operand.
    #[parserc(
        parser = ExprChain::into_parser().map(ExprChain::into).boxed(),
        map_err = SyntaxKind::ExprUnaryRightOprand.map_unhandle()
    )]
    pub right: Box<Expr<I>>,
}

#[cfg(test)]
mod tests {
    use parserc::{
        ControlFlow, Span,
        syntax::{Delimiter, Punctuated, SyntaxInput},
    };

    use crate::{
        errors::{CSTError, SyntaxKind},
        expr::{
            BinOp, Call, ChainOp, Digits, Expr, ExprBinary, ExprLit, ExprPath, ExprRange,
            ExprReference, LitNumber, LitStr, MethodCall, RangeLimits,
        },
        generics::GenericArgument,
        input::TokenStream,
        misc::{Ident, S},
        path::{Path, PathArguments, PathSegment},
        punct::{
            And, AngleBracketEnd, AngleBracketStart, Comma, Dot, DotDot, OrOr, ParenEnd,
            ParenStart, PathSep,
        },
    };

    #[test]
    fn test_unary() {
        assert_eq!(
            TokenStream::from("").parse::<Expr<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::Expr,
                ControlFlow::Recovable,
                Span::Range(0..0)
            ))
        );

        assert_eq!(
            TokenStream::from(r##"&a::b.c().b::<i32>(1,"world")"##).parse::<Expr<_>>(),
            Ok(Expr::Ref(ExprReference {
                leading_token: And(None, TokenStream::from((0, "&")), None),
                mutability: None,
                expr: Box::new(Expr::MethodCall(
                    Box::new(Expr::Path(ExprPath {
                        qself: None,
                        path: Path {
                            leading_pathsep: None,
                            segments: Punctuated {
                                pairs: vec![(
                                    PathSegment {
                                        ident: Ident(TokenStream::from((1, "a"))),
                                        arguments: None
                                    },
                                    PathSep(None, TokenStream::from((2, "::")), None)
                                )],
                                tail: Some(Box::new(PathSegment {
                                    ident: Ident(TokenStream::from((4, "b"))),
                                    arguments: None
                                }))
                            }
                        }
                    })),
                    vec![ChainOp::MethodCall(MethodCall {
                        dot: Dot(None, TokenStream::from((5, ".")), None),
                        ident: Ident(TokenStream::from((6, "c"))),
                        turbofish: None,
                        args: Call(Delimiter {
                            start: ParenStart(None, TokenStream::from((7, "(")), None),
                            end: ParenEnd(None, TokenStream::from((8, ")")), None),
                            body: Punctuated {
                                pairs: vec![],
                                tail: None
                            }
                        })
                    })],
                    MethodCall {
                        dot: Dot(None, TokenStream::from((9, ".")), None),
                        ident: Ident(TokenStream::from((10, "b"))),
                        turbofish: Some(PathArguments {
                            leading_pathsep: Some(PathSep(
                                None,
                                TokenStream::from((11, "::")),
                                None
                            )),

                            delimiter_start: AngleBracketStart(
                                None,
                                TokenStream::from((13, "<")),
                                None
                            ),
                            delimiter_end: AngleBracketEnd(
                                None,
                                TokenStream::from((17, ">")),
                                None
                            ),
                            args: Punctuated {
                                pairs: vec![],
                                tail: Some(Box::new(GenericArgument::Const(Expr::Path(
                                    ExprPath {
                                        qself: None,
                                        path: Path {
                                            leading_pathsep: None,
                                            segments: Punctuated {
                                                pairs: vec![],
                                                tail: Some(Box::new(PathSegment {
                                                    ident: Ident(TokenStream::from((14, "i32"))),
                                                    arguments: None
                                                }))
                                            }
                                        }
                                    }
                                ))))
                            }
                        }),
                        args: Call(Delimiter {
                            start: ParenStart(None, TokenStream::from((18, "(")), None),
                            end: ParenEnd(None, TokenStream::from((28, ")")), None),
                            body: Punctuated {
                                pairs: vec![(
                                    Expr::Lit(ExprLit::Number(LitNumber {
                                        sign: None,
                                        trunc: Some(Digits {
                                            input: TokenStream::from((19, "1")),
                                            value: 1
                                        }),
                                        fract: None,
                                        exp: None
                                    })),
                                    Comma(None, TokenStream::from((20, ",")), None)
                                )],
                                tail: Some(Box::new(Expr::Lit(ExprLit::String(LitStr {
                                    leading_flag: None,
                                    leading_pounds: TokenStream::from((21, "")),
                                    delimiter_start: TokenStream::from((21, "\"")),
                                    content: TokenStream::from((22, "world")),
                                    delimiter_end: TokenStream::from((27, "\"")),
                                    tailing_pounds: TokenStream::from((28, ""))
                                }))))
                            }
                        })
                    }
                ))
            }))
        );
    }

    #[test]
    fn test_unary_priority() {
        assert_eq!(
            TokenStream::from("&a || b ..").parse::<Expr<_>>(),
            Ok(Expr::Range(ExprRange {
                start: Some(Box::new(Expr::Binary(ExprBinary {
                    left: Box::new(Expr::Ref(ExprReference {
                        leading_token: And(None, TokenStream::from((0, "&")), None),
                        mutability: None,
                        expr: Box::new(Expr::Path(ExprPath {
                            qself: None,
                            path: Path {
                                leading_pathsep: None,
                                segments: Punctuated {
                                    pairs: vec![],
                                    tail: Some(Box::new(PathSegment {
                                        ident: Ident(TokenStream::from((1, "a"))),
                                        arguments: None
                                    }))
                                }
                            }
                        }))
                    })),
                    op: BinOp::Or(OrOr(
                        Some(S(TokenStream::from((2, " ")))),
                        TokenStream::from((3, "||")),
                        Some(S(TokenStream::from((5, " "))))
                    )),
                    right: Box::new(Expr::Path(ExprPath {
                        qself: None,
                        path: Path {
                            leading_pathsep: None,
                            segments: Punctuated {
                                pairs: vec![],
                                tail: Some(Box::new(PathSegment {
                                    ident: Ident(TokenStream::from((6, "b"))),
                                    arguments: None
                                }))
                            }
                        }
                    }))
                }))),
                limits: RangeLimits::HalfOpen(DotDot(
                    Some(S(TokenStream::from((7, " ")))),
                    TokenStream::from((8, "..")),
                    None
                )),
                end: None
            }))
        );
    }
}
