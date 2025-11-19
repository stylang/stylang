//! Keywords for `stylang`

use parserc::{Parser, keyword, next, syntax::Syntax};

use crate::{errors::TokenKind, input::CSTInput};

define_token!(Fn, "fn");
define_token!(Struct, "struct");
define_token!(Enum, "enum");
define_token!(View, "view");
define_token!(String, "string");
define_token!(Map, "map");
define_token!(Set, "set");
define_token!(Color, "color");
define_token!(Rgb, "rgb");

/// Parser for keyword `i*`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Int<I>
where
    I: crate::input::CSTInput,
{
    /// input of `Int`
    pub input: I,
    /// int size in bits.
    pub len: usize,
}

impl<I> Syntax<I> for Int<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let mut content = input.clone();

        next(b'i').parse(input).map_err(TokenKind::Int.map())?;

        let len: usize = keyword("8")
            .map(|_| 8)
            .or(keyword("16").map(|_| 16))
            .or(keyword("32").map(|_| 32))
            .or(keyword("64").map(|_| 64))
            .or(keyword("128").map(|_| 128))
            .parse(input)
            .map_err(TokenKind::Int.map())?;

        let content = match len {
            8 => content.split_to(2),
            128 => content.split_to(4),
            _ => content.split_to(3),
        };

        Ok(Self {
            input: content,
            len,
        })
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.input.to_span()
    }
}

/// Parser for keyword `u*`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Uint<I>
where
    I: crate::input::CSTInput,
{
    /// input of `Int`
    pub input: I,
    /// int size in bits.
    pub len: usize,
}

impl<I> Syntax<I> for Uint<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let mut content = input.clone();

        next(b'u').parse(input).map_err(TokenKind::Int.map())?;

        let len: usize = keyword("8")
            .map(|_| 8)
            .or(keyword("16").map(|_| 16))
            .or(keyword("32").map(|_| 32))
            .or(keyword("64").map(|_| 64))
            .or(keyword("128").map(|_| 128))
            .parse(input)
            .map_err(TokenKind::Int.map())?;

        let content = match len {
            8 => content.split_to(2),
            128 => content.split_to(4),
            _ => content.split_to(3),
        };

        Ok(Self {
            input: content,
            len,
        })
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.input.to_span()
    }
}

/// Parser for keyword `f*`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Float<I>
where
    I: crate::input::CSTInput,
{
    /// input of this token.
    pub input: I,
    /// float size in bits.
    pub len: usize,
}

impl<I> Syntax<I> for Float<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let mut content = input.clone();

        next(b'f').parse(input).map_err(TokenKind::Int.map())?;

        let len: usize = keyword("16")
            .map(|_| 16)
            .or(keyword("32").map(|_| 32))
            .or(keyword("64").map(|_| 64))
            .parse(input)
            .map_err(TokenKind::Float.map())?;

        let content = content.split_to(3);

        Ok(Self {
            input: content,
            len,
        })
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.input.to_span()
    }
}

/// A parser for keywords.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, parserc::syntax::Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Keyword<I>
where
    I: CSTInput,
{
    Fn(Fn<I>),
    Struct(Struct<I>),
    Enum(Enum<I>),
    View(View<I>),
    Int(Int<I>),
    Uint(Uint<I>),
    Map(Map<I>),
    Set(Set<I>),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_int() {
        macro_rules! assert_int {
            ($ident: ident, $len: literal, $input: literal, $match: literal) => {
                assert_eq!(
                    parserc::syntax::InputSyntaxExt::parse(&mut crate::input::TokenStream::from(
                        $input
                    )),
                    Ok($ident {
                        input: crate::input::TokenStream::from($match),
                        len: $len
                    })
                )
            };
        }
        assert_int!(Int, 32, "i321", "i32");
        assert_int!(Uint, 32, "u321", "u32");

        assert_int!(Int, 8, "i82", "i8");
        assert_int!(Uint, 8, "u82", "u8");

        assert_int!(Float, 16, "f162", "f16");
        assert_int!(Float, 32, "f322", "f32");
        assert_int!(Float, 64, "f642", "f64");
    }
}
