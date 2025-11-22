use parserc::syntax::{Delimiter, Punctuated, Syntax};

use crate::{
    ArrowRight, BraceEnd, BraceStart, Colon, Comma, Ident, OuterAttribute, OuterBlockDoc,
    OuterLineDoc, ParenEnd, ParenStart, S, Semi, Type, Visibility, input::CSTInput,
};

/// Meta data of item or field
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Meta<I>
where
    I: CSTInput,
{
    OuterLineDoc(OuterLineDoc<I>),
    OuterBlockDoc(OuterBlockDoc<I>),
    OuterAttribute(OuterAttribute<I>),
}

/// field list of struct or enum variant.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Fields<I>
where
    I: CSTInput,
{
    /// named fields: { xx:tt,... }
    Named(
        Delimiter<
            BraceStart<I>,
            BraceEnd<I>,
            Punctuated<(Vec<Meta<I>>, Ident<I>, Colon<I>, Type<I>), Comma<I>>,
        >,
    ),

    /// unnamed fields: (tt,...);
    Unnamed(Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<(Vec<Meta<I>>, Type<I>), Comma<I>>>),
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

/// extern keyword `extern`
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(keyword = "extern")]
pub struct Extern<I>(pub I)
where
    I: CSTInput;

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
    pub keyword_extern: (Extern<I>, S<I>),
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

    use super::*;
    use crate::input::TokenStream;

    #[test]
    fn test_struct() {
        assert_eq!(
            TokenStream::from("pub struct A(/** hello world */i32,string)\n;").parse(),
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
                        TokenStream::from((41, ")")),
                        Some(S(TokenStream::from((42, "\n"))))
                    ),
                    body: Punctuated {
                        pairs: vec![(
                            (
                                vec![Meta::OuterBlockDoc(OuterBlockDoc {
                                    start: TokenStream::from((13, "/**")),
                                    lines: TokenStream::from((16, " hello world ")),
                                    end: TokenStream::from((29, "*/"))
                                })],
                                Type::I32(TokenStream::from((31, "i32")))
                            ),
                            Comma(None, TokenStream::from((34, ",")), None)
                        )],
                        tail: Some(Box::new((
                            vec![],
                            Type::String(TokenStream::from((35, "string")))
                        )))
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
                                vec![],
                                Ident(TokenStream::from((15, "a"))),
                                Colon(None, TokenStream::from((16, ":")), None),
                                Type::I32(TokenStream::from((17, "i32")))
                            ),
                            Comma(None, TokenStream::from((20, ",")), None)
                        )],
                        tail: Some(Box::new((
                            vec![],
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
