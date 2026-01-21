use parserc::{ControlFlow, Parser, ToSpan, keyword, next, next_if, syntax::Syntax, take_while};
use unicode_ident::is_xid_start;

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

        let mut iter = input.iter();

        if let Some(c) = iter.next() {
            if matches!(c, '2'..='9') {
                return Err(CSTError::Semantics(
                    SemanticsKind::ReservedNum,
                    content.to_span_at(body.len() + 3),
                ));
            }

            if c == 'e' || c == 'E' {
                return Err(CSTError::Semantics(
                    SemanticsKind::ReservedNum,
                    content.to_span_at(body.len() + 3),
                ));
            }

            if c == '.' {
                match iter.next() {
                    Some('.') => {}
                    Some(c) if is_xid_start(c) => {}
                    _ => {
                        return Err(CSTError::Semantics(
                            SemanticsKind::ReservedNum,
                            content.to_span_at(body.len() + 4),
                        ));
                    }
                }
            }
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

        let mut iter = input.iter();

        if let Some(c) = iter.next() {
            if matches!(c, '8'..='9') {
                return Err(CSTError::Semantics(
                    SemanticsKind::ReservedNum,
                    content.to_span_at(body.len() + 3),
                ));
            }

            if c == 'e' || c == 'E' {
                return Err(CSTError::Semantics(
                    SemanticsKind::ReservedNum,
                    content.to_span_at(body.len() + 3),
                ));
            }

            if c == '.' {
                match iter.next() {
                    Some('.') => {}
                    Some(c) if is_xid_start(c) => {}
                    _ => {
                        return Err(CSTError::Semantics(
                            SemanticsKind::ReservedNum,
                            content.to_span_at(body.len() + 4),
                        ));
                    }
                }
            }
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

        let mut iter = input.iter();

        if let Some(c) = iter.next() {
            if c == '.' {
                match iter.next() {
                    Some('.') => {}
                    Some(c) if is_xid_start(c) => {}
                    _ => {
                        return Err(CSTError::Semantics(
                            SemanticsKind::ReservedNum,
                            content.to_span_at(body.len() + 1),
                        ));
                    }
                }
            }
        }

        Ok(Self(content.split_to(body.len() + 2)))
    }

    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}

/// An integer literal
///
///  see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#railroad-INTEGER_LITERAL
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum LitInt<I>
where
    I: CSTInput,
{
    Dec(LitDec<I>),
    Bin(LitBin<I>),
    Oct(LitOct<I>),
    Hex(LitHex<I>),
}

/// A tuple index is used to refer to the fields of tuples,
/// tuple structs, and tuple enum variants.
///
///  see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#railroad-TUPLE_INDEX
pub type TupleIndex<I> = LitDec<I>;

/// A floating-point literal
///
///  see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#railroad-FLOAT_LITERAL
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(semantic = check_lit_float)]
pub struct LitFloat<I>
where
    I: CSTInput,
{
    /// integer part of float.
    pub trunc: LitDec<I>,
    /// period character `.`
    #[parserc(parser = next('.').ok())]
    pub period: Option<I>,
    /// fract part of float.
    pub fract: Option<LitDec<I>>,
    /// optional exponent part.
    pub exp: Option<Exp<I>>,
}

#[inline]
fn check_lit_float<I>(input: I, lit: LitFloat<I>) -> Result<LitFloat<I>, CSTError>
where
    I: CSTInput,
{
    if lit.period.is_none() {
        assert!(lit.fract.is_none());

        if lit.exp.is_none() {
            return Err(CSTError::Syntax(
                SyntaxKind::Float,
                ControlFlow::Recovable,
                lit.to_span(),
            ));
        }
    } else {
        if lit.fract.is_none() && lit.exp.is_none() {
            let mut lookahead = input.clone().split_off(lit.trunc.0.len() + 1);

            if let Some(_) = next_if(|c| /* for range .. */ c == '.' || c == '_' || is_xid_start(c))
                .ok()
                .parse(&mut lookahead)?
            {
                return Err(CSTError::Syntax(
                    SyntaxKind::Float,
                    ControlFlow::Recovable,
                    lit.trunc.to_span() + lit.period.to_span(),
                ));
            }
        }
    }

    Ok(lit)
}

/// an optional exponent of [`LitFloat`]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Exp<I>
where
    I: CSTInput,
{
    /// leading char `E` or `e`
    #[parserc(parser = next_if(|c:char| c == 'E' || c == 'e'), crucial)]
    pub leading_char: I,
    /// sign character `+` or `-`
    #[parserc(parser = next_if(|c:char| c == '+' || c == '-').ok())]
    pub sign: Option<I>,
    /// decimal literal part of exponent.
    #[parserc(parser = parse_exp_body)]
    pub dec: I,
}

fn parse_exp_body<I>(input: &mut I) -> Result<I, CSTError>
where
    I: CSTInput,
{
    let body = take_while(|c: char| c.is_ascii_digit() || c == '_').parse(input)?;

    if body.as_str().chars().find(|c| c.is_ascii_digit()).is_none() {
        return Err(CSTError::Semantics(SemanticsKind::Exponent, body.to_span()));
    }

    Ok(body)
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Span, syntax::SyntaxInput};

    use crate::{
        errors::{CSTError, SemanticsKind, SyntaxKind},
        input::TokenStream,
        lexical::lit::{Exp, LitBin, LitFloat, LitHex, LitOct, num::LitDec},
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
            Err(CSTError::Semantics(
                SemanticsKind::ReservedNum,
                Span::Range(0..5)
            ))
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
            Err(CSTError::Semantics(
                SemanticsKind::ReservedNum,
                Span::Range(0..6)
            ))
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

    #[test]
    fn test_float_literal() {
        // this is a valid rust float literal token.
        assert_eq!(
            TokenStream::from("12E-__10").parse::<LitFloat<_>>(),
            Ok(LitFloat {
                trunc: LitDec(TokenStream::from((0, "12"))),
                period: None,
                fract: None,
                exp: Some(Exp {
                    leading_char: TokenStream::from((2, "E")),
                    sign: Some(TokenStream::from((3, "-"))),
                    dec: TokenStream::from((4, "__10"))
                })
            })
        );

        // this may be a field index expr.
        assert_eq!(
            TokenStream::from("12._a").parse::<LitFloat<_>>(),
            Err(CSTError::Syntax(
                SyntaxKind::Float,
                ControlFlow::Recovable,
                Span::Range(0..3)
            ))
        );

        assert_eq!(
            TokenStream::from("12.").parse::<LitFloat<_>>(),
            Ok(LitFloat {
                trunc: LitDec(TokenStream::from((0, "12"))),
                period: Some(TokenStream::from((2, "."))),
                fract: None,
                exp: None
            })
        );
        assert_eq!(
            TokenStream::from("12.01").parse::<LitFloat<_>>(),
            Ok(LitFloat {
                trunc: LitDec(TokenStream::from((0, "12"))),
                period: Some(TokenStream::from((2, "."))),
                fract: Some(LitDec(TokenStream::from((3, "01")))),
                exp: None
            })
        );
        assert_eq!(
            TokenStream::from("12.01e+10").parse::<LitFloat<_>>(),
            Ok(LitFloat {
                trunc: LitDec(TokenStream::from((0, "12"))),
                period: Some(TokenStream::from((2, "."))),
                fract: Some(LitDec(TokenStream::from((3, "01")))),
                exp: Some(Exp {
                    leading_char: TokenStream::from((5, "e")),
                    sign: Some(TokenStream::from((6, "+"))),
                    dec: TokenStream::from((7, "10"))
                })
            })
        );
    }
}
