use parserc::syntax::Syntax;

use crate::{
    input::CSTInput,
    keyword::{Mut, Ref},
    misc::Ident,
    pat::Pat,
    punct::At,
};

/// Identifier patterns bind the value they match to a variable in the value namespace.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PatIdent<I>
where
    I: CSTInput,
{
    /// an optional keyword `ref`.
    pub by_ref: Option<Ref<I>>,
    /// an optional keyword `mut`
    pub mutability: Option<Mut<I>>,
    /// unique identifier within the pattern.
    pub ident: Ident<I>,
    /// Pattern no top alt.
    pub subpat: Option<(At<I>, Box<Pat<I>>)>,
}

#[cfg(test)]
mod tests {
    use parserc::syntax::SyntaxInput;

    use crate::{
        input::TokenStream,
        misc::{Ident, S},
        pat::{Pat, PatIdent},
        punct::{At, DotDot},
    };

    #[test]
    fn test_subpat() {
        assert_eq!(
            TokenStream::from("i @ ..").parse::<Pat<_>>(),
            Ok(Pat::Ident(PatIdent {
                by_ref: None,
                mutability: None,
                ident: Ident(TokenStream::from((0, "i"))),
                subpat: Some((
                    At(
                        Some(S(TokenStream::from((1, " ")))),
                        TokenStream::from((2, "@")),
                        Some(S(TokenStream::from((3, " "))))
                    ),
                    Box::new(Pat::Rest(DotDot(None, TokenStream::from((4, "..")), None)))
                ))
            }))
        );
    }
}
