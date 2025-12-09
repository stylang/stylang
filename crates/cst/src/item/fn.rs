use parserc::syntax::{Delimiter, Punctuated, Syntax};

use crate::{
    input::CSTInput,
    keyword::{Extern, Fn_, Mut, Self_},
    misc::Ident,
    pat::PatType,
    punct::{And, ArrowRight, Comma, ParenEnd, ParenStart, Semi},
    ty::Type,
    vs::Visibility,
};

/// The self argument of an associated method.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Receiver<I>
where
    I: CSTInput,
{
    /// optional reference char `&`
    pub reference: Option<And<I>>,
    /// optional mutability keyword `mut`
    pub mutability: Option<Mut<I>>,
    /// keyword `self` or `Self`
    pub keyword_self: Self_<I>,
}

/// A free-standing extern function: `extern fn process(n: usize) -> Result<()>;`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ItemExternFn<I>
where
    I: CSTInput,
{
    /// optional vs
    pub vs: Option<Visibility<I>>,
    /// required prefix `extern`
    #[parserc(crucial)]
    pub keyword_extern: Extern<I>,
    /// keyword `fn`
    pub keyword_fn: Fn_<I>,
    /// name of this function.
    pub ident: Ident<I>,
    /// input parameters.
    pub inputs: Delimiter<ParenStart<I>, ParenEnd<I>, Punctuated<PatType<I>, Comma<I>>>,
    /// Return type of a function signature.
    pub output: Option<(ArrowRight<I>, Box<Type<I>>)>,
    /// end token `;`
    pub semi: Semi<I>,
}

#[cfg(test)]
mod tests {
    use parserc::syntax::{Delimiter, SyntaxInput, Punctuated};

    use crate::{
        input::TokenStream,
        item::ItemExternFn,
        keyword::{Extern, Fn_},
        misc::{Ident, S},
        punct::{ArrowRight, ParenEnd, ParenStart, Semi},
        ty::Type,
    };

    #[test]
    fn test_extern_fn() {
        assert_eq!(
            TokenStream::from("extern fn test() -> u8 ;").parse::<ItemExternFn<_>>(),
            Ok(ItemExternFn {
                vs: None,
                keyword_extern: Extern(
                    TokenStream::from((0, "extern")),
                    Some(S(TokenStream::from((6, " "))))
                ),
                keyword_fn: Fn_(
                    TokenStream::from((7, "fn")),
                    Some(S(TokenStream::from((9, " "))))
                ),
                ident: Ident(TokenStream::from((10, "test"))),
                inputs: Delimiter {
                    start: ParenStart(None, TokenStream::from((14, "(")), None),
                    end: ParenEnd(
                        None,
                        TokenStream::from((15, ")")),
                        Some(S(TokenStream::from((16, " "))))
                    ),
                    body: Punctuated {
                        pairs: vec![],
                        tail: None
                    }
                },
                output: Some((
                    ArrowRight(
                        None,
                        TokenStream::from((17, "->")),
                        Some(S(TokenStream::from((19, " "))))
                    ),
                    Box::new(Type::U8(TokenStream::from((20, "u8"))))
                )),
                semi: Semi(
                    Some(S(TokenStream::from((22, " ")))),
                    TokenStream::from((23, ";")),
                    None
                )
            })
        );

        assert_eq!(
            TokenStream::from("extern fn test();").parse::<ItemExternFn<_>>(),
            Ok(ItemExternFn {
                vs: None,
                keyword_extern: Extern(
                    TokenStream::from((0, "extern")),
                    Some(S(TokenStream::from((6, " "))))
                ),
                keyword_fn: Fn_(
                    TokenStream::from((7, "fn")),
                    Some(S(TokenStream::from((9, " "))))
                ),
                ident: Ident(TokenStream::from((10, "test"))),
                inputs: Delimiter {
                    start: ParenStart(None, TokenStream::from((14, "(")), None),
                    end: ParenEnd(None, TokenStream::from((15, ")")), None),
                    body: Punctuated {
                        pairs: vec![],
                        tail: None
                    }
                },
                output: None,
                semi: Semi(None, TokenStream::from((16, ";")), None)
            })
        );
    }
}
