use parserc::syntax::{Delimiter, Punctuated, Syntax};

use crate::{
    input::CSTInput,
    token::{
        keyword::{Float, Int, Uint},
        lit::Digits,
        punct::{ArrowRight, BracketEnd, BracketStart, Comma, ParenEnd, ParenStart, Semi},
    },
};

/// The type of function.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TypeFn<I>
where
    I: CSTInput,
{
    /// leading `fn`
    #[parserc(crucial, keyword = "fn")]
    pub keyword: I,
    /// function body.
    pub call_params: Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<Type<I>, Comma<I>>>,
    /// possiable returns value.
    pub returns_param: Option<(ArrowRight<I>, Box<Type<I>>)>,
}

/// value type of `stylang`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Type<I>
where
    I: CSTInput,
{
    /// i8,i16,...
    Int(Int<I>),
    /// u8,u16,...
    Uint(Uint<I>),
    /// f16,f32,...
    Float(Float<I>),

    Bool(#[parserc(keyword = "bool")] I),
    String(#[parserc(keyword = "string")] I),
    Map(#[parserc(keyword = "map")] I),
    Set(#[parserc(keyword = "set")] I),
    Draw(#[parserc(keyword = "draw")] I),
    View(#[parserc(keyword = "view")] I),
    Color(#[parserc(keyword = "color")] I),
    Angle(#[parserc(keyword = "angle")] I),
    Length(#[parserc(keyword = "length")] I),
    Freq(#[parserc(keyword = "freq")] I),
    Time(#[parserc(keyword = "time")] I),

    /// ...type
    Variadic(#[parserc(crucial, keyword = "...")] I, Box<Type<I>>),

    /// Sequence, [T] or [T;N]
    Sequence(
        Delimiter<BracketStart<I>, BracketEnd<I>, (Box<Type<I>>, Option<(Semi<I>, Digits<I>)>)>,
    ),

    /// tuple: (T1,T2,...)
    Tuple(Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<Type<I>, Comma<I>>>),

    /// fn(T1,T2,...) -> T
    Fn(TypeFn<I>),

    /// a mutable reference.
    Mut(#[parserc(crucial, keyword = "&mut")] I, Box<Type<I>>),

    /// a immutable reference.
    Ref(#[parserc(crucial, keyword = "&")] I, Box<Type<I>>),
}

#[cfg(test)]
mod tests {
    use parserc::syntax::InputSyntaxExt;

    use crate::{input::TokenStream, syntax::Type};

    #[test]
    fn test_ty() {
        let s = TokenStream::from("&...&mut[[i32;4]]")
            .parse::<Type<_>>()
            .unwrap();

        println!("{:?}", s);

        let s = TokenStream::from("[fn() -> view]")
            .parse::<Type<_>>()
            .unwrap();

        println!("{:?}", s);
    }
}
