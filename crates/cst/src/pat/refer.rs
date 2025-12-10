use parserc::syntax::Syntax;

use crate::{input::CSTInput, keyword::Mut, pat::Pat, punct::And};

// A referencing operation: &a or &mut a.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PatReference<I>
where
    I: CSTInput,
{
    /// leading token `&`
    pub leading_token: And<I>,
    /// optional keyword `mut`
    pub mutability: Option<Mut<I>>,
    /// target pat.
    pub pat: Box<Pat<I>>,
}

#[cfg(test)]
mod tests {
    use parserc::syntax::SyntaxInput;

    use crate::{
        expr::{Digits, ExprLit, LitNumber},
        input::TokenStream,
        keyword::Mut,
        misc::S,
        pat::{Pat, PatReference},
        punct::And,
    };

    #[test]
    fn test_ref() {
        assert_eq!(
            TokenStream::from("&mut 1").parse::<Pat<_>>(),
            Ok(Pat::Ref(PatReference {
                leading_token: And(None, TokenStream::from((0, "&")), None),
                mutability: Some(Mut(
                    TokenStream::from((1, "mut")),
                    Some(S(TokenStream::from((4, " "))))
                )),
                pat: Box::new(Pat::Lit(ExprLit::Number(LitNumber {
                    sign: None,
                    trunc: Some(Digits {
                        input: TokenStream::from((5, "1")),
                        value: 1
                    }),
                    fract: None,
                    exp: None
                })))
            }))
        );
    }
}
