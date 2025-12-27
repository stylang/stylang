use parserc::{Parser, keyword, next_if, syntax::Syntax, take_while};

use crate::{
    errors::{CSTError, SemanticsKind, SyntaxKind},
    input::CSTInput,
};

/// A decimal literal starts with a decimal digit and continues with any mixture of decimal digits and underscores.
///
///  see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#railroad-DEC_LITERAL
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LitDec<I>(pub I)
where
    I: CSTInput;

impl<I> Syntax<I> for LitDec<I>
where
    I: CSTInput,
{
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let mut content = input.clone();

        next_if(|c: char| c.is_ascii_digit())
            .parse(input)
            .map_err(SyntaxKind::Dec.map())?;

        let rest = take_while(|c: char| c.is_ascii_digit() || c == '_').parse(input)?;

        Ok(Self(content.split_to(rest.len() + 1)))
    }

    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

/// A binary literal starts with the character sequence U+0030 U+0062 (0b) and
/// continues as any mixture (with at least one digit) of binary digits and
/// underscores.
///
///  see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#railroad-BIN_LITERAL
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LitBin<I>(pub I)
where
    I: CSTInput;

impl<I> Syntax<I> for LitBin<I>
where
    I: CSTInput,
{
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let mut content = input.clone();

        keyword("0b").parse(input).map_err(SyntaxKind::Bin.map())?;

        let body = take_while(|c: char| matches!(c, '0'..='1') || c == '_').parse(input)?;

        if body
            .as_str()
            .chars()
            .find(|c| matches!(*c, '0'..='1'))
            .is_none()
        {
            return Err(CSTError::Semantics(SemanticsKind::Bin, body.to_span()));
        }

        Ok(Self(content.split_to(body.len() + 2)))
    }

    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

/// An octal literal starts with the character sequence U+0030 U+006F (0o)
/// and continues as any mixture (with at least one digit) of octal digits
/// and underscores.
///
///  see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#railroad-OCT_LITERAL
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LitOct<I>(pub I)
where
    I: CSTInput;

impl<I> Syntax<I> for LitOct<I>
where
    I: CSTInput,
{
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let mut content = input.clone();

        keyword("0o").parse(input).map_err(SyntaxKind::Oct.map())?;

        let body = take_while(|c: char| matches!(c, '0'..='7') || c == '_').parse(input)?;

        if body
            .as_str()
            .chars()
            .find(|c| matches!(*c, '0'..='7'))
            .is_none()
        {
            return Err(CSTError::Semantics(SemanticsKind::Oct, body.to_span()));
        }

        Ok(Self(content.split_to(body.len() + 2)))
    }

    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

/// A hex literal starts with the character sequence U+0030 U+0078 (0x)
/// and continues as any mixture (with at least one digit) of hex digits
/// and underscores.
///
///  see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#railroad-HEX_LITERAL
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LitHex<I>(pub I)
where
    I: CSTInput;

impl<I> Syntax<I> for LitHex<I>
where
    I: CSTInput,
{
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let mut content = input.clone();

        keyword("0x").parse(input).map_err(SyntaxKind::Hex.map())?;

        let body = take_while(|c: char| c.is_ascii_hexdigit() || c == '_').parse(input)?;

        if body
            .as_str()
            .chars()
            .find(|c| c.is_ascii_hexdigit())
            .is_none()
        {
            return Err(CSTError::Semantics(SemanticsKind::Hex, body.to_span()));
        }

        Ok(Self(content.split_to(body.len() + 2)))
    }

    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Span, syntax::SyntaxInput};

    use crate::{
        errors::{CSTError, SemanticsKind, SyntaxKind},
        input::TokenStream,
        lexical::lit::{LitBin, LitHex, LitOct, num::LitDec},
    };

    #[test]
    fn test_dec() {
        assert_eq!(
            TokenStream::from("9__").parse::<LitDec<_>>(),
            Ok(LitDec(TokenStream::from((0, "9__"))))
        );

        assert_eq!(
            TokenStream::from("_9").parse::<LitDec<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::Dec,
                ControlFlow::Recovable,
                Span::Range(0..1)
            ))
        );
    }

    #[test]
    fn test_bin() {
        assert_eq!(
            TokenStream::from("0b___").parse::<LitBin<_>>(),
            Err(CSTError::Semantics(SemanticsKind::Bin, Span::Range(2..5)))
        );

        assert_eq!(
            TokenStream::from("0b102").parse::<LitBin<_>>(),
            Ok(LitBin(TokenStream::from("0b10")))
        );
    }

    #[test]
    fn test_oct() {
        assert_eq!(
            TokenStream::from("0o___").parse::<LitOct<_>>(),
            Err(CSTError::Semantics(SemanticsKind::Oct, Span::Range(2..5)))
        );

        assert_eq!(
            TokenStream::from("0o0178").parse::<LitOct<_>>(),
            Ok(LitOct(TokenStream::from("0o017")))
        );
    }

    #[test]
    fn test_hex() {
        assert_eq!(
            TokenStream::from("0x___").parse::<LitHex<_>>(),
            Err(CSTError::Semantics(SemanticsKind::Hex, Span::Range(2..5)))
        );

        assert_eq!(
            TokenStream::from("0x0aF12G").parse::<LitHex<_>>(),
            Ok(LitHex(TokenStream::from("0x0aF12")))
        );
    }
}
