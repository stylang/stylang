use parserc::{ControlFlow, Parser, keyword, next, syntax::Syntax};
use unicode_ident::is_xid_continue;

use crate::{
    errors::{CSTError, SyntaxKind},
    input::CSTInput,
    lexical::ident::{IdentOrKeyword, NonKeywordIdent, RawIdent},
};

/// Lifetime parameters and loop labels use LIFETIME_OR_LABEL tokens.
/// Any LIFETIME_TOKEN will be accepted by the lexer, and for example,
/// can be used in macros.
///
///  see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#railroad-LIFETIME_TOKEN
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LifeTime<I>(pub I)
where
    I: CSTInput;

#[inline]
fn parse_lifetime_token<I>(input: &mut I) -> Result<I, CSTError>
where
    I: CSTInput,
{
    let mut content = input.clone();

    let start = next('\'')
        .parse(input)
        .map_err(SyntaxKind::LifeTime.map())?;

    let body = IdentOrKeyword::into_parser()
        .map(|v| v.0)
        .parse(input)
        .map_err(SyntaxKind::LifeTime.map())?;

    match input.iter().next() {
        Some('\'') => {
            return Err(CSTError::Syntax(
                SyntaxKind::LifeTime,
                ControlFlow::Recovable,
                start.to_span() + body.to_span(),
            ));
        }
        _ => {}
    }

    Ok(content.split_to(body.len() + 1))
}

impl<I> Syntax<I> for LifeTime<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        PlaceHolderLifeTime::into_parser()
            .map(|raw| raw.0)
            .or(RawLifeTime::into_parser().map(|raw| raw.0))
            .or(parse_lifetime_token)
            .map(Self)
            .parse(input)
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

/// Lifetime parameters and loop labels use LIFETIME_OR_LABEL tokens.
/// Any LIFETIME_TOKEN will be accepted by the lexer, and for example,
/// can be used in macros.
///
///  see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#railroad-LIFETIME_OR_LABEL
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LifeTimeOrLabel<I>(pub I)
where
    I: CSTInput;

#[inline]
fn parse_lifetime_or_label_token<I>(input: &mut I) -> Result<I, CSTError>
where
    I: CSTInput,
{
    let mut content = input.clone();

    let start = next('\'')
        .parse(input)
        .map_err(SyntaxKind::LifeTimeOrLabel.map())?;

    let body = NonKeywordIdent::into_parser()
        .map(|v| v.0)
        .parse(input)
        .map_err(SyntaxKind::LifeTimeOrLabel.map())?;

    match input.iter().next() {
        Some('\'') => {
            return Err(CSTError::Syntax(
                SyntaxKind::LifeTimeOrLabel,
                ControlFlow::Recovable,
                start.to_span() + body.to_span(),
            ));
        }
        _ => {}
    }

    Ok(content.split_to(body.len() + 1))
}

impl<I> Syntax<I> for LifeTimeOrLabel<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        RawLifeTime::into_parser()
            .map(|raw| raw.0)
            .or(parse_lifetime_or_label_token)
            .map(Self)
            .parse(input)
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

/// A raw lifetime is like a normal lifetime, but its identifier is prefixed by r#.
/// (Note that the r# prefix is not included as part of the actual lifetime.)
///
///  see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#railroad-RAW_LIFETIME
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct RawLifeTime<I>(pub I)
where
    I: CSTInput;

impl<I> Syntax<I> for RawLifeTime<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let mut content = input.clone();

        let start = next('\'')
            .parse(input)
            .map_err(SyntaxKind::RawLifeTime.map())?;

        let body = RawIdent::into_parser()
            .map(|v| v.0)
            .parse(input)
            .map_err(SyntaxKind::RawLifeTime.map())?;

        match input.iter().next() {
            Some('\'') => {
                return Err(CSTError::Syntax(
                    SyntaxKind::RawLifeTime,
                    ControlFlow::Recovable,
                    start.to_span() + body.to_span(),
                ));
            }
            _ => {}
        }

        Ok(Self(content.split_to(body.len() + 1)))
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

/// It is an error to use the RESERVED_RAW_LIFETIME token 'r#_ in order to
/// avoid confusion with the placeholder lifetime.
///
///  see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#railroad-RESERVED_RAW_LIFETIME
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ReservedRawLifeTime<I>(pub I);

impl<I> Syntax<I> for ReservedRawLifeTime<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let content = keyword("'r#_")
            .parse(input)
            .map_err(SyntaxKind::ReservedLifeTime.map())?;

        match input.iter().next() {
            Some('\'') => {
                return Err(CSTError::Syntax(
                    SyntaxKind::ReservedLifeTime,
                    ControlFlow::Recovable,
                    content.to_span(),
                ));
            }
            Some(c) if is_xid_continue(c) => {
                return Err(CSTError::Syntax(
                    SyntaxKind::ReservedLifeTime,
                    ControlFlow::Recovable,
                    content.to_span(),
                ));
            }
            _ => {}
        }

        Ok(Self(content))
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

/// Rust has rules that allow lifetimes to be elided in various places where the compiler
/// can infer a sensible default choice.
///
///  see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/lifetime-elision.html#lifetime-elision
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PlaceHolderLifeTime<I>(pub I);

impl<I> Syntax<I> for PlaceHolderLifeTime<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let content = keyword("'_")
            .parse(input)
            .map_err(SyntaxKind::PlaceHolderLifeTime.map())?;

        match input.iter().next() {
            Some('\'') => {
                return Err(CSTError::Syntax(
                    SyntaxKind::PlaceHolderLifeTime,
                    ControlFlow::Recovable,
                    content.to_span(),
                ));
            }
            Some(c) if is_xid_continue(c) => {
                return Err(CSTError::Syntax(
                    SyntaxKind::PlaceHolderLifeTime,
                    ControlFlow::Recovable,
                    content.to_span(),
                ));
            }
            _ => {}
        }

        Ok(Self(content))
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
        lexical::label::{LifeTime, LifeTimeOrLabel},
    };

    #[test]
    fn test_lifetime_or_label() {
        assert_eq!(
            TokenStream::from("'_").parse::<LifeTimeOrLabel<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::LifeTimeOrLabel,
                ControlFlow::Recovable,
                Span::Range(1..2)
            ))
        );

        assert_eq!(
            TokenStream::from("'r#if").parse::<LifeTimeOrLabel<_>>(),
            Ok(LifeTimeOrLabel(TokenStream::from((0, "'r#if"))))
        );
    }

    #[test]
    fn test_liftime_token() {
        assert_eq!(
            TokenStream::from("'r#if").parse::<LifeTime<_>>(),
            Ok(LifeTime(TokenStream::from((0, "'r#if"))))
        );
        assert_eq!(
            TokenStream::from("'_").parse::<LifeTime<_>>(),
            Ok(LifeTime(TokenStream::from((0, "'_"))))
        );

        assert_eq!(
            TokenStream::from("'hello").parse::<LifeTime<_>>(),
            Ok(LifeTime(TokenStream::from((0, "'hello"))))
        );
    }
}
