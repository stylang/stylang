use parserc::syntax::Syntax;

use crate::{block::Block, input::CSTInput, misc::Label};

/// A blocked scope: `{ ... }`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprBlock<I>
where
    I: CSTInput,
{
    /// optional block label.
    pub label: Option<Label<I>>,
    /// A braced block containing Rust statements.
    pub block: Block<I>,
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, InputSyntaxExt};

    use crate::{
        block::{Block, Stmt},
        expr::{Digits, Expr, ExprBlock, ExprLet, ExprLit, LitNumber},
        input::TokenStream,
        keyword::Let,
        misc::{Ident, Label, S},
        pat::Pat,
        punct::{BraceEnd, BraceStart, Colon, Equal},
    };

    #[test]
    fn test_block() {
        assert_eq!(
            TokenStream::from("let a = { 1 };").parse::<Expr<_>>(),
            Ok(Expr::Let(ExprLet {
                keyword: Let(
                    TokenStream::from((0, "let")),
                    Some(S(TokenStream::from((3, " "))))
                ),
                pat: Box::new(Pat::Ident(Ident(TokenStream::from((4, "a"))))),
                eq: Equal(
                    Some(S(TokenStream::from((5, " ")))),
                    TokenStream::from((6, "=")),
                    Some(S(TokenStream::from((7, " "))))
                ),
                expr: Box::new(Expr::Block(ExprBlock {
                    label: None,
                    block: Block(Delimiter {
                        start: BraceStart(
                            None,
                            TokenStream::from((8, "{")),
                            Some(S(TokenStream::from((9, " "))))
                        ),
                        end: BraceEnd(
                            Some(S(TokenStream::from((11, " ")))),
                            TokenStream::from((12, "}")),
                            None
                        ),
                        body: vec![Stmt::Expr(
                            Expr::Lit(ExprLit::Number(LitNumber {
                                sign: None,
                                trunc: Some(Digits {
                                    input: TokenStream::from((10, "1")),
                                    value: 1
                                }),
                                fract: None,
                                exp: None
                            })),
                            None
                        )]
                    })
                }))
            }))
        );
    }

    #[test]
    fn test_block_with_label() {
        assert_eq!(
            TokenStream::from("'test: {}").parse::<Expr<_>>(),
            Ok(Expr::Block(ExprBlock {
                label: Some(Label(
                    TokenStream::from((0, "'")),
                    Ident(TokenStream::from((1, "test"))),
                    Colon(
                        None,
                        TokenStream::from((5, ":")),
                        Some(S(TokenStream::from((6, " "))))
                    )
                )),
                block: Block(Delimiter {
                    start: BraceStart(None, TokenStream::from((7, "{")), None),
                    end: BraceEnd(None, TokenStream::from((8, "}")), None),
                    body: vec![]
                })
            }))
        );
    }
}
