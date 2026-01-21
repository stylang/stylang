use parserc::syntax::Syntax;

use crate::{
    input::CSTInput,
    lexical::{
        ident::SuffixNoE,
        keywords::strict::{False, True},
        lit::{
            LitByte, LitByteStr, LitCStr, LitChar, LitFloat, LitInt, LitRawByteStr, LitRawCStr,
            LitRawStr, LitStr,
        },
    },
};

/// A literal expression is an expression consisting of a single token, rather than a sequence of tokens,
/// that immediately and directly denotes the value it evaluates to, rather than referring to it by name
/// or some other evaluation rule.
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/expressions/literal-expr.html#grammar-LiteralExpression
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum LitExpr<I>
where
    I: CSTInput,
{
    True(True<I>),
    False(False<I>),
    Char(LitChar<I>),
    Str(LitStr<I>),
    RawStr(LitRawStr<I>),
    Byte(LitByte<I>),
    ByteStr(LitByteStr<I>),
    RawByteStr(LitRawByteStr<I>),
    CStr(LitCStr<I>),
    RawCStr(LitRawCStr<I>),
    Float(LitFloat<I>, Option<SuffixNoE<I>>),
    Int(LitInt<I>, Option<SuffixNoE<I>>),
}

#[cfg(test)]
mod tests {
    use parserc::syntax::SyntaxInput;

    use crate::{
        expr::LitExpr,
        input::TokenStream,
        lexical::{
            ident::SuffixNoE,
            lit::{Exp, LitDec, LitFloat},
        },
    };

    #[test]
    fn test_float() {
        assert_eq!(
            TokenStream::from("1.e10").parse::<LitExpr<_>>(),
            Ok(LitExpr::Float(
                LitFloat {
                    trunc: LitDec(TokenStream::from((0, "1"))),
                    period: Some(TokenStream::from((1, "."))),
                    fract: None,
                    exp: Some(Exp {
                        leading_char: TokenStream::from((2, "e")),
                        sign: None,
                        dec: TokenStream::from((3, "10"))
                    })
                },
                None
            ))
        );

        assert_eq!(
            TokenStream::from("1.e10f64").parse::<LitExpr<_>>(),
            Ok(LitExpr::Float(
                LitFloat {
                    trunc: LitDec(TokenStream::from((0, "1"))),
                    period: Some(TokenStream::from((1, "."))),
                    fract: None,
                    exp: Some(Exp {
                        leading_char: TokenStream::from((2, "e")),
                        sign: None,
                        dec: TokenStream::from((3, "10"))
                    })
                },
                Some(SuffixNoE(TokenStream::from((5, "f64"))))
            ))
        );
    }
}
