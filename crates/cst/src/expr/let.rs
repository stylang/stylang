use parserc::{Parser, syntax::Syntax};

use crate::{
    errors::{CSTError, SyntaxKind},
    expr::{Expr, ExprArray, ExprLit, ExprPath},
    input::CSTInput,
    keyword::Let,
    pat::Pat,
    punct::Equal,
};

/// An assignment expression: a = compute().
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprLet<I>
where
    I: CSTInput,
{
    /// leading keyword `let`
    pub keyword: Let<I>,
    /// left Operand pattern.
    pub pat: Box<Pat<I>>,
    /// punct `=`
    pub eq: Equal<I>,
    /// right Operand expression.
    #[parserc(parser = parse_let_init_expr)]
    pub expr: Box<Expr<I>>,
}

#[inline]
fn parse_let_init_expr<I>(input: &mut I) -> Result<Box<Expr<I>>, CSTError>
where
    I: CSTInput,
{
    ExprArray::into_parser()
        .map(|expr| Expr::Array(expr))
        .or(ExprLit::into_parser().map(|expr| Expr::Lit(expr)))
        .or(ExprPath::into_parser().map(|expr| Expr::Path(expr)))
        .boxed()
        .parse(input)
        .map_err(SyntaxKind::LetInitExpr.map_unhandle())
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{InputSyntaxExt, Punctuated};

    use crate::{
        expr::{Expr, ExprPath},
        input::TokenStream,
        misc::Ident,
        path::{Path, PathSegment},
    };

    #[test]
    fn test_let() {
        assert_eq!(
            TokenStream::from("let a = Target::Field").parse::<Expr<_>>(),
            Ok(Expr::Path(ExprPath {
                qself: None,
                path: Path {
                    leading_pathsep: None,
                    segments: Punctuated {
                        pairs: vec![],
                        tail: Some(Box::new(PathSegment {
                            ident: Ident(TokenStream::from((0, "let"))),
                            arguments: None
                        }))
                    }
                }
            }))
        );
    }
}
