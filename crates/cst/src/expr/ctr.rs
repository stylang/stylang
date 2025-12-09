use parserc::syntax::Syntax;

use crate::{
    expr::Expr,
    input::CSTInput,
    keyword::{Break, Continue, Return},
    misc::{Label, S},
};

/// A continue, with an optional label.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprContinue<I>
where
    I: CSTInput,
{
    /// leading keyword `continue`.
    #[parserc(crucial)]
    pub keyword: Continue<I>,
    /// optional label name.
    pub label: Option<(Label<I>, Option<S<I>>)>,
}

/// A break, with an optional label to break and an optional expression.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprBreak<I>
where
    I: CSTInput,
{
    /// leading keyword `break`.
    #[parserc(crucial)]
    pub keyword: Break<I>,
    /// optional label name.
    pub label: Option<(Label<I>, Option<S<I>>)>,
    /// optional returns value.
    pub expr: Option<Box<Expr<I>>>,
}

/// A break, with an optional label to break and an optional expression.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprReturn<I>
where
    I: CSTInput,
{
    /// leading keyword `break`.
    #[parserc(crucial)]
    pub keyword: Return<I>,
    /// optional returns value.
    pub expr: Option<Box<Expr<I>>>,
}

#[cfg(test)]
mod tests {
    use parserc::{
        ControlFlow, Span,
        syntax::{SyntaxInput, Punctuated},
    };

    use crate::{
        errors::{CSTError, SyntaxKind},
        expr::{Digits, Expr, ExprLit, ExprPath, LitNumber},
        input::TokenStream,
        misc::Ident,
        path::{Path, PathSegment},
    };

    use super::*;

    #[test]
    fn test_break() {
        assert_eq!(
            TokenStream::from("break 'a 1").parse::<Expr<_>>(),
            Ok(Expr::Break(ExprBreak {
                keyword: Break(
                    TokenStream::from((0, "break")),
                    Some(S(TokenStream::from((5, " "))))
                ),
                label: Some((
                    Label(
                        TokenStream::from((6, "'")),
                        Ident(TokenStream::from((7, "a")))
                    ),
                    Some(S(TokenStream::from((8, " "))))
                )),
                expr: Some(Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                    sign: None,
                    trunc: Some(Digits {
                        input: TokenStream::from((9, "1")),
                        value: 1
                    }),
                    fract: None,
                    exp: None
                }))))
            }))
        );

        assert_eq!(
            TokenStream::from("break 1").parse::<Expr<_>>(),
            Ok(Expr::Break(ExprBreak {
                keyword: Break(
                    TokenStream::from((0, "break")),
                    Some(S(TokenStream::from((5, " "))))
                ),
                label: None,
                expr: Some(Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                    sign: None,
                    trunc: Some(Digits {
                        input: TokenStream::from((6, "1")),
                        value: 1
                    }),
                    fract: None,
                    exp: None
                }))))
            }))
        );
    }

    #[test]
    fn detect_label_error() {
        assert_eq!(
            TokenStream::from("break '1").parse::<Expr<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::Ident,
                ControlFlow::Fatal,
                Span::Range(7..8)
            ))
        );
    }

    #[test]
    fn test_return_expr() {
        assert_eq!(
            TokenStream::from("return a").parse::<Expr<_>>(),
            Ok(Expr::Return(ExprReturn {
                keyword: Return(
                    TokenStream::from((0, "return")),
                    Some(S(TokenStream::from((6, " "))))
                ),
                expr: Some(Box::new(Expr::Path(ExprPath {
                    qself: None,
                    path: Path {
                        leading_pathsep: None,
                        segments: Punctuated {
                            pairs: vec![],
                            tail: Some(Box::new(PathSegment {
                                ident: Ident(TokenStream::from((7, "a"))),
                                arguments: None
                            }))
                        }
                    }
                })))
            }))
        );
    }
}
