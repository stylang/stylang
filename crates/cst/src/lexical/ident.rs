//! lexical `identifiers`, more information see [`The Rust Reference`]
//!
//! [`The Rust Reference`]: https://doc.rust-lang.org/reference/identifiers.html

use parserc::{ControlFlow, Parser, keyword, next_if, syntax::Syntax, take_while};
use unicode_ident::{is_xid_continue, is_xid_start};

use crate::{
    errors::{CSTError, SemanticsKind, SyntaxKind},
    input::CSTInput,
};

/// A identifier or keyword, see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/identifiers.html#railroad-IDENTIFIER_OR_KEYWORD
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

        let leading = next_if(|c| c == '_' || is_xid_start(c))
            .parse(input)
            .map_err(SyntaxKind::IdentOrKeyword.map())?;

        let rest = take_while(|c| is_xid_continue(c)).parse(input)?;

        if leading.as_str() == "_" && rest.is_empty() {
            return Err(CSTError::Syntax(
                SyntaxKind::IdentOrKeyword,
                ControlFlow::Recovable,
                leading.to_span(),
            ));
        }

        Ok(Self(content.split_to(1 + rest.len())))
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

/// `r#`[`IdentOrKeyword`] except `crate`, `self`, `super`, `Self`, see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/identifiers.html#railroad-RAW_IDENTIFIER
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

/// [`IdentOrKeyword`] except a `strict` or `reserved` keyword, see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/identifiers.html#railroad-NON_KEYWORD_IDENTIFIER
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct NonKeywordIdent<I>(pub I)
where
    I: CSTInput;

impl<I> Syntax<I> for NonKeywordIdent<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let ident_or_keyword = IdentOrKeyword::into_parser()
            .parse(input)
            .map_err(SyntaxKind::NonKeywordIdentifer.map())?;

        match ident_or_keyword.0.as_str() {
            "as" | "async" | "await" | "break" | "const" | "continue" | "crate" | "dyn"
            | "else" | "enum" | "extern" | "false" | "fn" | "for" | "if" | "impl" | "in"
            | "let" | "loop" | "match" | "mod" | "move" | "mut" | "pub" | "ref" | "return"
            | "self" | "Self" | "static" | "struct" | "super" | "trait" | "true" | "type"
            | "unsafe" | "use" | "where" | "while" | "abstract" | "become" | "box" | "do"
            | "final" | "gen" | "macro" | "override" | "priv" | "try" | "typeof" | "unsized"
            | "virtual" | "yied" => {
                return Err(CSTError::Semantics(
                    SemanticsKind::NonKeywordIdent,
                    ident_or_keyword.to_span(),
                ));
            }
            _ => {}
        }

        Ok(Self(ident_or_keyword.0))
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

/// `r#_`, see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/identifiers.html#railroad-RESERVED_RAW_IDENTIFIER
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ReservedRawIdent<I>(pub I)
where
    I: CSTInput;

impl<I> Syntax<I> for ReservedRawIdent<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let keyword = keyword("r#_")
            .parse(input)
            .map_err(SyntaxKind::ReservedRawIdent.map())?;

        if let Some(c) = input.iter().next() {
            if is_xid_continue(c) {
                return Err(CSTError::Syntax(
                    SyntaxKind::ReservedRawIdent,
                    ControlFlow::Recovable,
                    keyword.to_span(),
                ));
            }
        }

        Ok(Self(keyword))
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

/// [`NON_KEYWORD_IDENTIFIER`](NonKeywordIdent) | [`RAW_IDENTIFIER`](RawIdent), see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/identifiers.html#railroad-IDENTIFIER
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Ident<I>
where
    I: CSTInput,
{
    RawIdent(RawIdent<I>),
    NonKeywordIdent(NonKeywordIdent<I>),
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Span, syntax::SyntaxInput};

    use crate::{
        errors::{CSTError, SyntaxKind},
        input::TokenStream,
    };

    use super::*;

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

    #[test]
    fn test_raw_ident() {
        assert_eq!(
            TokenStream::from("r#true").parse::<RawIdent<_>>(),
            Ok(RawIdent(TokenStream::from("r#true")))
        );

        assert_eq!(
            TokenStream::from("r#self").parse::<RawIdent<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::RawIdent,
                Span::Range(2..6)
            ))
        );

        assert_eq!(
            TokenStream::from("r#crate").parse::<RawIdent<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::RawIdent,
                Span::Range(2..7)
            ))
        );

        assert_eq!(
            TokenStream::from("r#Self").parse::<RawIdent<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::RawIdent,
                Span::Range(2..6)
            ))
        );

        assert_eq!(
            TokenStream::from("r#super").parse::<RawIdent<_>>(),
            Err(CSTError::Semantics(
                SemanticsKind::RawIdent,
                Span::Range(2..7)
            ))
        );

        let keywords = [
            "as", "async", "await", "break", "const", "continue", "dyn", "else", "enum", "extern",
            "false", "fn", "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut",
            "pub", "ref", "return", "static", "struct", "trait", "true", "type", "unsafe", "use",
            "where", "while", "abstract", "become", "box", "do", "final", "gen", "macro",
            "override", "priv", "try", "typeof", "unsized", "virtual", "yied",
        ];

        for keyword in keywords {
            let keyword = format!("r#{}", keyword);
            assert_eq!(
                TokenStream::from(keyword.as_str()).parse::<RawIdent<_>>(),
                Ok(RawIdent(TokenStream::from(keyword.as_str())))
            );
        }
    }

    #[test]
    fn test_non_keyword_ident() {
        assert_eq!(
            TokenStream::from("_a").parse::<NonKeywordIdent<_>>(),
            Ok(NonKeywordIdent(TokenStream::from("_a")))
        );
    }

    #[test]
    fn test_non_keyword_ident_with_keywords() {
        let keywords = [
            "as", "async", "await", "break", "const", "continue", "crate", "dyn", "else", "enum",
            "extern", "false", "fn", "for", "if", "impl", "in", "let", "loop", "match", "mod",
            "move", "mut", "pub", "ref", "return", "self", "Self", "static", "struct", "super",
            "trait", "true", "type", "unsafe", "use", "where", "while", "abstract", "become",
            "box", "do", "final", "gen", "macro", "override", "priv", "try", "typeof", "unsized",
            "virtual", "yied",
        ];

        for keyword in keywords {
            assert_eq!(
                TokenStream::from(keyword).parse::<NonKeywordIdent<_>>(),
                Err(CSTError::Semantics(
                    SemanticsKind::NonKeywordIdent,
                    Span::Range(0..keyword.len())
                ))
            );
        }
    }

    #[test]
    fn test_reserved_raw_ident() {
        assert_eq!(
            TokenStream::from("r#_").parse::<ReservedRawIdent<_>>(),
            Ok(ReservedRawIdent(TokenStream::from("r#_")))
        );

        assert_eq!(
            TokenStream::from("r#_a").parse::<ReservedRawIdent<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::ReservedRawIdent,
                ControlFlow::Recovable,
                Span::Range(0..3)
            ))
        );
    }
}
