use parserc::syntax::{Delimiter, Punctuated, Syntax};

use crate::{
    input::CSTInput,
    syntax::{OuterAttribute, OuterBlockDoc, OuterLineDoc, Type, Visibility},
    token::{
        Ident, S,
        keyword::Extern,
        punct::{ArrowRight, BraceEnd, BraceStart, Colon, Comma, ParenEnd, ParenStart, Semi},
    },
};

/// field list of struct or enum variant.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Fields<I>
where
    I: CSTInput,
{
    /// named fields: { xx:tt,... }
    Named(
        Delimiter<BraceStart<I>, BraceEnd<I>, Punctuated<(Ident<I>, Colon<I>, Type<I>), Comma<I>>>,
    ),

    /// unnamed fields: (tt,...);
    Unnamed(Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<Type<I>, Comma<I>>>),
}

/// struct / tuple struct item.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ItemStruct<I>
where
    I: CSTInput,
{
    /// visibility marker `pub ..`
    pub visibility: Option<Visibility<I>>,
    /// keyword `struct`
    #[parserc(crucial, keyword = "struct")]
    pub keyword: I,
    /// struct name.
    pub ident: (S<I>, Ident<I>),
    /// struct fields named or unnamed.
    pub fields: Fields<I>,
}

/// enum item.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ItemEnum<I>
where
    I: CSTInput,
{
    /// visibility marker `pub ..`
    pub visibility: Option<Visibility<I>>,
    /// keyword `enum`
    #[parserc(crucial, keyword = "enum")]
    pub keyword: I,
    /// enum name.
    pub ident: (S<I>, Ident<I>),
    /// variant arms: ident{ ... } or ident(...)
    pub variants: Punctuated<(Ident<I>, Fields<I>), Comma<I>>,
}

/// extern fn item.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ItemExternFn<I>
where
    I: CSTInput,
{
    /// visibility marker `pub ..`
    pub visibility: Option<Visibility<I>>,
    /// keyword `extern`
    pub keyword_extern: (Extern<I>, Option<S<I>>),
    /// keyword `fn`
    #[parserc(crucial, keyword = "fn")]
    pub keyword_fn: I,
    /// function name.
    pub ident: (S<I>, Ident<I>),
    /// function body.
    pub call_params:
        Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<(Ident<I>, Colon<I>, Type<I>), Comma<I>>>,
    /// possiable returns value.
    pub returns_param: Option<(ArrowRight<I>, Box<Type<I>>)>,
    /// end punct `;`
    pub semi: Semi<I>,
}

/// directly child of one module.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Item<I>
where
    I: CSTInput,
{
    S(S<I>),
    OuterLineDoc(OuterLineDoc<I>),
    OuterBlockDoc(OuterBlockDoc<I>),
    OuterAttribute(OuterAttribute<I>),
    Struct(ItemStruct<I>),
    Enum(ItemEnum<I>),
    ExternFn(ItemExternFn<I>),
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, InputSyntaxExt, Or, Punctuated};

    use crate::{
        input::TokenStream,
        syntax::{Fields, ItemStruct, Type, Visibility},
        token::{
            Ident, S,
            keyword::Int,
            punct::{BraceEnd, BraceStart, Colon, Comma, ParenEnd, ParenStart},
        },
    };

    #[test]
    fn test_struct() {
        assert_eq!(
            TokenStream::from("pub struct A(i32,string)\n;").parse(),
            Ok(ItemStruct {
                visibility: Some(Visibility {
                    keyword: TokenStream::from((0, "pub")),
                    predicate: Or::Second(S(TokenStream::from((3, " "))))
                }),
                keyword: TokenStream::from((4, "struct")),
                ident: (
                    S(TokenStream::from((10, " "))),
                    Ident(TokenStream::from((11, "A")))
                ),
                fields: Fields::Unnamed(Delimiter {
                    start: ParenStart(None, TokenStream::from((12, "(")), None),
                    end: ParenEnd(
                        None,
                        TokenStream::from((23, ")")),
                        Some(S(TokenStream::from((24, "\n"))))
                    ),
                    body: Punctuated {
                        pairs: vec![(
                            Type::Int(Int {
                                input: TokenStream::from((13, "i32")),
                                len: 32
                            }),
                            Comma(None, TokenStream::from((16, ",")), None)
                        )],
                        tail: Some(Box::new(Type::String(TokenStream::from((17, "string")))))
                    }
                })
            })
        );

        assert_eq!(
            TokenStream::from("pub struct A { a:i32,_:\nstring }\n;").parse(),
            Ok(ItemStruct {
                visibility: Some(Visibility {
                    keyword: TokenStream::from((0, "pub")),
                    predicate: Or::Second(S(TokenStream::from((3, " "))))
                }),
                keyword: TokenStream::from((4, "struct")),
                ident: (
                    S(TokenStream::from((10, " "))),
                    Ident(TokenStream::from((11, "A")))
                ),
                fields: Fields::Named(Delimiter {
                    start: BraceStart(
                        Some(S(TokenStream::from((12, " ")))),
                        TokenStream::from((13, "{")),
                        Some(S(TokenStream::from((14, " "))))
                    ),
                    end: BraceEnd(
                        Some(S(TokenStream::from((30, " ")))),
                        TokenStream::from((31, "}")),
                        Some(S(TokenStream::from((32, "\n"))))
                    ),
                    body: Punctuated {
                        pairs: vec![(
                            (
                                Ident(TokenStream::from((15, "a"))),
                                Colon(None, TokenStream::from((16, ":")), None),
                                Type::Int(Int {
                                    input: TokenStream::from((17, "i32")),
                                    len: 32
                                })
                            ),
                            Comma(None, TokenStream::from((20, ",")), None)
                        )],
                        tail: Some(Box::new((
                            Ident(TokenStream::from((21, "_"))),
                            Colon(
                                None,
                                TokenStream::from((22, ":")),
                                Some(S(TokenStream::from((23, "\n"))))
                            ),
                            Type::String(TokenStream::from((24, "string")))
                        )))
                    }
                })
            })
        );
    }
}
