use parserc::{ControlFlow, Parser, keyword, next_if, syntax::Syntax, take_while};
use unicode_ident::{is_xid_continue, is_xid_start};

use crate::{
    errors::{CSTError, SemanticsKind, SyntaxKind},
    input::CSTInput,
};

/// A word of Rust code, which may be a keyword or legal variable name.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct IdentOrKeyword<I>(pub I)
where
    I: CSTInput;

impl<I> Syntax<I> for IdentOrKeyword<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let mut content = input.clone();

        let start = next_if(|c| c == '_' || is_xid_start(c))
            .parse(input)
            .map_err(SyntaxKind::IdentOrKeyword.map())?;

        let continues = take_while(|c| is_xid_continue(c)).parse(input)?;

        if start.as_str() == "_" && continues.is_empty() {
            return Err(CSTError::Syntax(
                SyntaxKind::IdentOrKeyword,
                ControlFlow::Recovable,
                start.to_span(),
            ));
        }

        Ok(Self(content.split_to(1 + continues.len())))
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

/// `r#` IDENTIFIER_OR_KEYWORD except `crate`, `self`, `super`, `Self`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct RawIdent<I>(pub I)
where
    I: CSTInput;

impl<I> Syntax<I> for RawIdent<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let mut content = input.clone();

        keyword("r#")
            .parse(input)
            .map_err(SyntaxKind::RawIdent.map())?;

        let ident_or_keyword = IdentOrKeyword::into_parser()
            .parse(input)
            .map_err(SyntaxKind::RawIdent.map())?;

        match ident_or_keyword.0.as_str() {
            "crate" | "self" | "super" | "Self" => {
                return Err(CSTError::Semantics(
                    SemanticsKind::RawIdent,
                    ident_or_keyword.to_span(),
                ));
            }
            _ => {}
        }

        Ok(Self(content.split_to(2 + ident_or_keyword.0.len())))
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Span, syntax::SyntaxInput};

    use crate::{
        errors::{CSTError, SyntaxKind},
        input::TokenStream,
        lexical::IdentOrKeyword,
    };

    #[test]
    fn test_identifier_or_keyword() {
        assert_eq!(
            TokenStream::from("_hello").parse::<IdentOrKeyword<_>>(),
            Ok(IdentOrKeyword(TokenStream::from("_hello")))
        );

        assert_eq!(
            TokenStream::from("h").parse::<IdentOrKeyword<_>>(),
            Ok(IdentOrKeyword(TokenStream::from("h")))
        );

        assert_eq!(
            TokenStream::from("_").parse::<IdentOrKeyword<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::IdentOrKeyword,
                ControlFlow::Recovable,
                Span::Range(0..1)
            ))
        );
    }
}
