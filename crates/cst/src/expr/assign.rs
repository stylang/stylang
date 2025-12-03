use parserc::{Parser, syntax::Syntax};

use crate::{
    errors::{CSTError, SyntaxKind},
    expr::{Expr, ExprArray, ExprLit, ExprPath},
    input::CSTInput,
    punct::Equal,
};

/// An assignment expression: a = compute().
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprAssgin<I>
where
    I: CSTInput,
{
    /// left Operand expression.
    #[parserc(parser = parse_assgin_operand)]
    pub left: Box<Expr<I>>,
    /// punct `=`
    #[parserc(crucial)]
    pub eq: Equal<I>,
    /// right Operand expression.
    #[parserc(parser = parse_assgin_operand)]
    pub right: Box<Expr<I>>,
}

#[inline]
fn parse_assgin_operand<I>(input: &mut I) -> Result<Box<Expr<I>>, CSTError>
where
    I: CSTInput,
{
    ExprArray::into_parser()
        .map(|expr| Expr::Array(expr))
        .or(ExprLit::into_parser().map(|expr| Expr::Lit(expr)))
        .or(ExprPath::into_parser().map(|expr| Expr::Path(expr)))
        .boxed()
        .parse(input)
        .map_err(SyntaxKind::AssignOperand.map_unhandle())
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{InputSyntaxExt, Punctuated};

    use crate::{
        expr::{Digits, Expr, LitNumber},
        input::TokenStream,
        misc::{Ident, S},
        path::{Path, PathSegment},
    };

    use super::*;

    #[test]
    fn test_assign() {
        assert_eq!(
            TokenStream::from("a = 10").parse::<Expr<_>>(),
            Ok(Expr::Assign(ExprAssgin {
                left: Box::new(Expr::Path(ExprPath {
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
                eq: Equal(
                    Some(S(TokenStream::from((1, " ")))),
                    TokenStream::from((2, "=")),
                    Some(S(TokenStream::from((3, " "))))
                ),
                right: Box::new(Expr::Lit(ExprLit::Number(LitNumber {
                    sign: None,
                    trunc: Some(Digits {
                        input: TokenStream::from((4, "10")),
                        value: 10
                    }),
                    fract: None,
                    exp: None
                })))
            }))
        );
    }
}
