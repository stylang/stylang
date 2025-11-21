use parserc::syntax::{Delimiter, Syntax};

use crate::{
    errors::SyntaxKind,
    input::CSTInput,
    token::punct::{ParenEnd, ParenStart},
};

/// The predicate of `Visibility` expr.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::VisibilityPredicate.map_fatal())]
pub enum VisibilityPredicate<I>
where
    I: CSTInput,
{
    Crate(#[parserc(keyword = "crate")] I),
    Super(#[parserc(keyword = "super")] I),
}

/// visible marker fo items.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Visibility<I>
where
    I: CSTInput,
{
    /// keyword `pub`
    #[parserc(keyword = "pub")]
    pub keyword: I,
    /// optional predicate.
    pub predicate: Option<Delimiter<ParenStart<I>, ParenEnd<I>, VisibilityPredicate<I>>>,
}

#[cfg(test)]
mod tests {
    use parserc::{
        ControlFlow, Span,
        syntax::{Delimiter, InputSyntaxExt},
    };

    use crate::{
        errors::{CSTError, SyntaxKind},
        input::TokenStream,
        syntax::{Visibility, VisibilityPredicate},
        token::{
            S,
            punct::{ParenEnd, ParenStart},
        },
    };

    #[test]
    fn test_vs() {
        assert_eq!(
            TokenStream::from("pub ").parse(),
            Ok(Visibility {
                keyword: TokenStream::from("pub"),
                predicate: None
            })
        );

        assert_eq!(
            TokenStream::from("pub (super)").parse(),
            Ok(Visibility {
                keyword: TokenStream::from((0, "pub")),
                predicate: Some(Delimiter {
                    start: ParenStart(
                        Some(S(TokenStream::from((3, " ")))),
                        TokenStream::from((4, "(")),
                        None
                    ),
                    end: ParenEnd(None, TokenStream::from((10, ")")), None),
                    body: VisibilityPredicate::Super(TokenStream::from((5, "super")))
                })
            })
        );

        assert_eq!(
            TokenStream::from("pub (crate)").parse(),
            Ok(Visibility {
                keyword: TokenStream::from((0, "pub")),
                predicate: Some(Delimiter {
                    start: ParenStart(
                        Some(S(TokenStream::from((3, " ")))),
                        TokenStream::from((4, "(")),
                        None
                    ),
                    end: ParenEnd(None, TokenStream::from((10, ")")), None),
                    body: VisibilityPredicate::Crate(TokenStream::from((5, "crate")))
                })
            })
        );

        assert_eq!(
            TokenStream::from("pub(abc)").parse::<Visibility<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::VisibilityPredicate,
                ControlFlow::Fatal,
                Span::Range(4..5)
            ))
        );
    }
}
