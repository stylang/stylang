use parserc::syntax::Syntax;

use crate::{
    expr::{Expr, group::Composable},
    input::CSTInput,
    keyword::Continue,
    misc::Label,
};

/// A continue, with an optional label.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprContinue<I>
where
    I: CSTInput,
{
    /// leading keyword `continue`.
    pub keyword: Continue<I>,
    /// optional label name.
    pub label: Option<Label<I>>,
}

impl<I> Composable<I> for ExprContinue<I>
where
    I: CSTInput,
{
    fn priority(&self) -> usize {
        1
    }

    fn compose<F>(self, _: usize, f: F) -> super::Expr<I>
    where
        F: FnOnce(super::Expr<I>) -> super::Expr<I>,
    {
        f(Expr::Continue(self))
    }
}

#[cfg(test)]
mod tests {
    use parserc::syntax::InputSyntaxExt;

    use crate::{
        expr::{Expr, ExprContinue},
        input::TokenStream,
        keyword::Continue,
        misc::{Ident, Label, S},
    };

    #[test]
    fn test_continue() {
        assert_eq!(
            TokenStream::from("continue 'a").parse::<Expr<_>>(),
            Ok(Expr::Continue(ExprContinue {
                keyword: Continue(
                    TokenStream::from((0, "continue")),
                    Some(S(TokenStream::from((8, " "))))
                ),
                label: Some(Label(
                    TokenStream::from((9, "'")),
                    Ident(TokenStream::from((10, "a")))
                ))
            }))
        );
    }
}
