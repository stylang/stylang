use parserc::syntax::{Delimiter, Punctuated, Syntax};

use crate::{
    input::CSTInput,
    pat::Pat,
    punct::{Comma, ParenEnd, ParenStart},
};

/// A tuple pattern: (a, b).
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PatTuple<I>(pub Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<Pat<I>, Comma<I>>>)
where
    I: CSTInput;

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, Punctuated, SyntaxInput};

    use crate::{
        input::TokenStream,
        misc::Ident,
        pat::{Pat, PatIdent, PatTuple},
        punct::{Comma, DotDot, ParenEnd, ParenStart},
    };

    #[test]
    fn test_tuple() {
        assert_eq!(
            TokenStream::from("(a,..,b)").parse::<Pat<_>>(),
            Ok(Pat::Tuple(PatTuple(Delimiter {
                start: ParenStart(None, TokenStream::from((0, "(")), None),
                end: ParenEnd(None, TokenStream::from((7, ")")), None),
                body: Punctuated {
                    pairs: vec![
                        (
                            Pat::Ident(PatIdent {
                                by_ref: None,
                                mutability: None,
                                ident: Ident(TokenStream::from((1, "a"))),
                                subpat: None
                            }),
                            Comma(None, TokenStream::from((2, ",")), None)
                        ),
                        (
                            Pat::Rest(DotDot(None, TokenStream::from((3, "..")), None)),
                            Comma(None, TokenStream::from((5, ",")), None)
                        )
                    ],
                    tail: Some(Box::new(Pat::Ident(PatIdent {
                        by_ref: None,
                        mutability: None,
                        ident: Ident(TokenStream::from((6, "b"))),
                        subpat: None
                    })))
                }
            })))
        );
    }
}
