use parserc::syntax::Syntax;

use crate::{
    expr::Expr,
    input::CSTInput,
    punct::{BracketEnd, BracketStart, Semi},
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprRepeat<I>
where
    I: CSTInput,
{
    pub delimiter_start: BracketStart<I>,
    pub expr: Box<Expr<I>>,
    #[parserc(crucial)]
    pub semi: Semi<I>,
    pub len: Box<Expr<I>>,
    pub delimiter_end: BracketEnd<I>,
}

#[cfg(test)]
mod tests {
    use parserc::syntax::InputSyntaxExt;

    use crate::{expr::Expr, input::TokenStream};

    #[test]
    fn test_repeat() {
        println!("{:?}", TokenStream::from("[c();10]").parse::<Expr<_>>());
    }
}
