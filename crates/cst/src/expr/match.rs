use parserc::syntax::{Delimiter, Syntax};

use crate::{
    expr::Expr,
    input::CSTInput,
    keyword::{If, Match},
    pat::Pat,
    punct::{BraceEnd, BraceStart, Comma, FatArrowRight},
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Arm<I>
where
    I: CSTInput,
{
    pub pat: Pat<I>,
    pub guard: Option<(If<I>, Box<Expr<I>>)>,
    pub arrow: FatArrowRight<I>,
    pub body: Box<Expr<I>>,
    pub comma: Option<Comma<I>>,
}

/// A match expression: match n { Some(n) => {}, None => {} }.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprMatch<I>
where
    I: CSTInput,
{
    /// Leading keyword `match`
    #[parserc(crucial)]
    pub keyword: Match<I>,
    /// match expr
    pub expr: Box<Expr<I>>,
    /// match arms.
    pub arms: Delimiter<BraceStart<I>, BraceEnd<I>, Vec<Arm<I>>>,
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Punctuated, SyntaxInput};

    use crate::{
        block::Block,
        expr::{Expr, ExprBlock, ExprPath},
        input::TokenStream,
        misc::{Ident, S},
        pat::PatIdent,
        path::{Path, PathSegment},
    };

    use super::*;

    #[test]
    fn test_match() {
        assert_eq!(
            TokenStream::from("match n { Some => {}, None => {} }").parse::<Expr<_>>(),
            Ok(Expr::Match(ExprMatch {
                keyword: Match(
                    TokenStream::from((0, "match")),
                    Some(S(TokenStream::from((5, " "))))
                ),
                expr: Box::new(Expr::Path(ExprPath {
                    qself: None,
                    path: Path {
                        leading_pathsep: None,
                        segments: Punctuated {
                            pairs: vec![],
                            tail: Some(Box::new(PathSegment {
                                ident: Ident(TokenStream::from((6, "n"))),
                                arguments: None
                            }))
                        }
                    }
                })),
                arms: Delimiter {
                    start: BraceStart(
                        Some(S(TokenStream::from((7, " ")))),
                        TokenStream::from((8, "{")),
                        Some(S(TokenStream::from((9, " "))))
                    ),
                    end: BraceEnd(None, TokenStream::from((33, "}")), None),
                    body: vec![
                        Arm {
                            pat: Pat::Ident(PatIdent {
                                by_ref: None,
                                mutability: None,
                                ident: Ident(TokenStream::from((10, "Some"))),
                                subpat: None
                            }),
                            guard: None,
                            arrow: FatArrowRight(
                                Some(S(TokenStream::from((14, " ")))),
                                TokenStream::from((15, "=>")),
                                Some(S(TokenStream::from((17, " "))))
                            ),
                            body: Box::new(Expr::Block(ExprBlock {
                                label: None,
                                block: Block(Delimiter {
                                    start: BraceStart(None, TokenStream::from((18, "{")), None),
                                    end: BraceEnd(None, TokenStream::from((19, "}")), None),
                                    body: vec![]
                                })
                            })),
                            comma: Some(Comma(
                                None,
                                TokenStream::from((20, ",")),
                                Some(S(TokenStream::from((21, " "))))
                            ))
                        },
                        Arm {
                            pat: Pat::Ident(PatIdent {
                                by_ref: None,
                                mutability: None,
                                ident: Ident(TokenStream::from((22, "None"))),
                                subpat: None,
                            }),
                            guard: None,
                            arrow: FatArrowRight(
                                Some(S(TokenStream::from((26, " ")))),
                                TokenStream::from((27, "=>")),
                                Some(S(TokenStream::from((29, " "))))
                            ),
                            body: Box::new(Expr::Block(ExprBlock {
                                label: None,
                                block: Block(Delimiter {
                                    start: BraceStart(None, TokenStream::from((30, "{")), None),
                                    end: BraceEnd(
                                        None,
                                        TokenStream::from((31, "}")),
                                        Some(S(TokenStream::from((32, " "))))
                                    ),
                                    body: vec![]
                                })
                            })),
                            comma: None
                        }
                    ]
                }
            }))
        );
    }
}
