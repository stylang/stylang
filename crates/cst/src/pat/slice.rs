use parserc::syntax::{Delimiter, Punctuated, Syntax};

use crate::{
    input::CSTInput,
    pat::Pat,
    punct::{BracketEnd, BracketStart, Comma},
};

/// A dynamically sized slice pattern: `[a, b, ref i @ .., y, z]`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PatSlice<I>(pub Delimiter<BracketStart<I>, BracketEnd<I>, Punctuated<Pat<I>, Comma<I>>>)
where
    I: CSTInput;

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, Punctuated, SyntaxInput};

    use crate::{
        input::TokenStream,
        keyword::Ref,
        misc::{Ident, S},
        pat::{Pat, PatIdent, PatSlice},
        punct::{At, BracketEnd, BracketStart, Comma, DotDot},
    };

    #[test]
    fn test_slice() {
        assert_eq!(
            TokenStream::from("[a, b, ref i @ .., y, z]").parse::<Pat<_>>(),
            Ok(Pat::Slice(PatSlice(Delimiter {
                start: BracketStart(None, TokenStream::from((0, "[")), None),
                end: BracketEnd(None, TokenStream::from((23, "]")), None),
                body: Punctuated {
                    pairs: vec![
                        (
                            Pat::Ident(PatIdent {
                                by_ref: None,
                                mutability: None,
                                ident: Ident(TokenStream::from((1, "a"))),
                                subpat: None
                            }),
                            Comma(
                                None,
                                TokenStream::from((2, ",")),
                                Some(S(TokenStream::from((3, " "))))
                            )
                        ),
                        (
                            Pat::Ident(PatIdent {
                                by_ref: None,
                                mutability: None,
                                ident: Ident(TokenStream::from((4, "b"))),
                                subpat: None
                            }),
                            Comma(
                                None,
                                TokenStream::from((5, ",")),
                                Some(S(TokenStream::from((6, " "))))
                            )
                        ),
                        (
                            Pat::Ident(PatIdent {
                                by_ref: Some(Ref(
                                    TokenStream::from((7, "ref")),
                                    Some(S(TokenStream::from((10, " "))))
                                )),
                                mutability: None,
                                ident: Ident(TokenStream::from((11, "i"))),
                                subpat: Some((
                                    At(
                                        Some(S(TokenStream::from((12, " ")))),
                                        TokenStream::from((13, "@")),
                                        Some(S(TokenStream::from((14, " "))))
                                    ),
                                    Box::new(Pat::Rest(DotDot(
                                        None,
                                        TokenStream::from((15, "..")),
                                        None
                                    )))
                                ))
                            }),
                            Comma(
                                None,
                                TokenStream::from((17, ",")),
                                Some(S(TokenStream::from((18, " "))))
                            )
                        ),
                        (
                            Pat::Ident(PatIdent {
                                by_ref: None,
                                mutability: None,
                                ident: Ident(TokenStream::from((19, "y"))),
                                subpat: None
                            }),
                            Comma(
                                None,
                                TokenStream::from((20, ",")),
                                Some(S(TokenStream::from((21, " "))))
                            )
                        )
                    ],
                    tail: Some(Box::new(Pat::Ident(PatIdent {
                        by_ref: None,
                        mutability: None,
                        ident: Ident(TokenStream::from((22, "z"))),
                        subpat: None
                    })))
                }
            })))
        );
    }
}
