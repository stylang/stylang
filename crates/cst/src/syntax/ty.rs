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
    /// bool
    Bool(#[parserc(keyword = "bool")] I),
    /// string
    String(#[parserc(keyword = "string")] I),
    /// map
    Map(
        #[parserc(crucial, keyword = "map")] I,
        Delimiter<BracketStart<I>, BracketEnd<I>, (Box<Type<I>>, Comma<I>, Box<Type<I>>)>,
    ),
    /// set
    Set(
        #[parserc(crucial, keyword = "set")] I,
        Delimiter<BracketStart<I>, BracketEnd<I>, Box<Type<I>>>,
    ),
    /// draw
    Draw(#[parserc(keyword = "draw")] I),
    /// view
    View(#[parserc(keyword = "view")] I),
    /// color
    Color(#[parserc(keyword = "color")] I),
    /// angle
    Angle(#[parserc(keyword = "angle")] I),
    /// length
    Length(#[parserc(keyword = "length")] I),
    /// freq
    Freq(#[parserc(keyword = "freq")] I),
    /// time.
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
    use parserc::syntax::{Delimiter, InputSyntaxExt, Punctuated};

    use crate::{
        input::TokenStream,
        syntax::{Type, TypeFn},
        token::{
            S,
            keyword::Int,
            lit::Digits,
            punct::{ArrowRight, BracketEnd, BracketStart, Comma, ParenEnd, ParenStart, Semi},
        },
    };

    #[test]
    fn test_ty() {
        assert_eq!(
            TokenStream::from("&...&mut[[i32;4]]").parse::<Type<_>>(),
            Ok(Type::Ref(
                TokenStream::from((0, "&")),
                Box::new(Type::Variadic(
                    TokenStream::from((1, "...")),
                    Box::new(Type::Mut(
                        TokenStream::from((4, "&mut")),
                        Box::new(Type::Sequence(Delimiter {
                            start: BracketStart(None, TokenStream::from((8, "[")), None),
                            end: BracketEnd(None, TokenStream::from((16, "]")), None),
                            body: (
                                Box::new(Type::Sequence(Delimiter {
                                    start: BracketStart(None, TokenStream::from((9, "[")), None),
                                    end: BracketEnd(None, TokenStream::from((15, "]")), None),
                                    body: (
                                        Box::new(Type::Int(Int {
                                            input: TokenStream::from((10, "i32")),
                                            len: 32
                                        })),
                                        Some((
                                            Semi(None, TokenStream::from((13, ";")), None),
                                            Digits {
                                                input: TokenStream::from((14, "4")),
                                                value: 4
                                            }
                                        ))
                                    )
                                })),
                                None
                            )
                        }))
                    ))
                ))
            ))
        );

        assert_eq!(
            TokenStream::from("[fn() -> view]").parse::<Type<_>>(),
            Ok(Type::Sequence(Delimiter {
                start: BracketStart(None, TokenStream::from((0, "[")), None),
                end: BracketEnd(None, TokenStream::from((13, "]")), None),
                body: (
                    Box::new(Type::Fn(TypeFn {
                        keyword: TokenStream::from((1, "fn")),
                        call_params: Delimiter {
                            start: ParenStart(None, TokenStream::from((3, "(")), None),
                            end: ParenEnd(
                                None,
                                TokenStream::from((4, ")")),
                                Some(S(TokenStream::from((5, " "))))
                            ),
                            body: Punctuated {
                                pairs: vec![],
                                tail: None
                            }
                        },
                        returns_param: Some((
                            ArrowRight(
                                None,
                                TokenStream::from((6, "->")),
                                Some(S(TokenStream::from((8, " "))))
                            ),
                            Box::new(Type::View(TokenStream::from((9, "view"))))
                        ))
                    })),
                    None
                )
            }))
        );

        assert_eq!(
            TokenStream::from("map[string,draw]").parse(),
            Ok(Type::Map(
                TokenStream::from("map"),
                Delimiter {
                    start: BracketStart(None, TokenStream::from((3, "[")), None),
                    end: BracketEnd(None, TokenStream::from((15, "]")), None),
                    body: (
                        Box::new(Type::String(TokenStream::from((4, "string")))),
                        Comma(None, TokenStream::from((10, ",")), None),
                        Box::new(Type::Draw(TokenStream::from((11, "draw"))))
                    )
                }
            ))
        );

        assert_eq!(
            TokenStream::from("set[string]").parse(),
            Ok(Type::Set(
                TokenStream::from("set"),
                Delimiter {
                    start: BracketStart(None, TokenStream::from((3, "[")), None),
                    end: BracketEnd(None, TokenStream::from((10, "]")), None),
                    body: Box::new(Type::String(TokenStream::from((4, "string")))),
                }
            ))
        );

        assert_eq!(
            TokenStream::from("draw").parse(),
            Ok(Type::Draw(TokenStream::from("draw")))
        );

        assert_eq!(
            TokenStream::from("color").parse(),
            Ok(Type::Color(TokenStream::from("color")))
        );

        assert_eq!(
            TokenStream::from("angle").parse(),
            Ok(Type::Angle(TokenStream::from("angle")))
        );

        assert_eq!(
            TokenStream::from("length").parse(),
            Ok(Type::Length(TokenStream::from("length")))
        );

        assert_eq!(
            TokenStream::from("freq").parse(),
            Ok(Type::Freq(TokenStream::from("freq")))
        );
    }
}
