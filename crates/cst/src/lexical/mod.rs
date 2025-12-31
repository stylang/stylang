//! lexical types.

mod s;
pub use s::*;

pub mod comments;
pub mod delimiter;
pub mod ident;
pub mod keywords;
pub mod label;
pub mod lit;
pub mod punct;

mod token {
    use parserc::syntax::Syntax;

    use crate::{
        input::CSTInput,
        lexical::{
            ident::{IdentOrKeyword, RawIdent},
            label::LifeTime,
            lit::{
                LitByte, LitByteStr, LitCStr, LitChar, LitFloat, LitInt, LitRawByteStr, LitRawCStr,
                LitRawStr, LitStr,
            },
            punct::Punct,
        },
    };

    /// Token except delimiters
    ///
    /// see [`The Rust Reference`]
    ///
    /// [`The Rust Reference`]: https://doc.rust-lang.org/reference/macros.html#railroad-TokenTree
    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
    pub enum Token<I>
    where
        I: CSTInput,
    {
        IdentOrKeyWord(IdentOrKeyword<I>),
        RawIdent(RawIdent<I>),
        LitChar(LitChar<I>),
        LitStr(LitStr<I>),
        LitRawStr(LitRawStr<I>),
        LitByte(LitByte<I>),
        LitByteStr(LitByteStr<I>),
        LitRawByteStr(LitRawByteStr<I>),
        LitCStr(LitCStr<I>),
        LitRawCStr(LitRawCStr<I>),
        LitInt(LitInt<I>),
        LitFloat(LitFloat<I>),
        LifeTime(LifeTime<I>),
        Punct(Punct<I>),
    }
}

pub use token::Token;
