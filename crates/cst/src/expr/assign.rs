use parserc::syntax::Syntax;

use crate::{
    errors::SyntaxKind,
    expr::{Expr, parse_left_hand_operand},
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
    #[parserc(parser = parse_left_hand_operand)]
    pub left: Box<Expr<I>>,
    /// punct `=`
    #[parserc(crucial)]
    pub eq: Equal<I>,
    /// right Operand expression.
    #[parserc(map_err = SyntaxKind::AssignRightOperand.map_unhandle())]
    pub right: Box<Expr<I>>,
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{InputSyntaxExt, Punctuated};

    use crate::{
        expr::{Digits, Expr, ExprLit, ExprPath, LitNumber},
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
