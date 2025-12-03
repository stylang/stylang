//! A pattern in a local binding, function signature, match expression, or various other places.

use parserc::{Parser, syntax::Syntax};

use crate::{errors::CSTError, input::CSTInput, misc::Ident, punct::Colon, ty::Type};

/// A type ascription pattern: foo: f64.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PatType<I>
where
    I: CSTInput,
{
    #[parserc(parser = parse_pat_type_pat)]
    pub pat: Box<Pat<I>>,
    pub colon: Colon<I>,
    pub ty: Box<Type<I>>,
}

#[inline]
fn parse_pat_type_pat<I>(input: &mut I) -> Result<Box<Pat<I>>, CSTError>
where
    I: CSTInput,
{
    Ident::into_parser()
        .map(|ident| Pat::Ident(ident))
        .boxed()
        .parse(input)
}

/// A pattern in a local binding, function signature, match expression, or various other places.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Pat<I>
where
    I: CSTInput,
{
    Type(PatType<I>),
    Ident(Ident<I>),
}

#[cfg(test)]
mod tests {
    use parserc::syntax::InputSyntaxExt;

    use crate::{
        input::TokenStream,
        misc::{Ident, S},
        pat::{Pat, PatType},
        punct::Colon,
        ty::Type,
    };

    #[test]
    fn test_pat_type() {
        assert_eq!(
            TokenStream::from("a: u8").parse(),
            Ok(Pat::Type(PatType {
                pat: Box::new(Pat::Ident(Ident(TokenStream::from("a")))),
                colon: Colon(
                    None,
                    TokenStream::from((1, ":")),
                    Some(S(TokenStream::from((2, " "))))
                ),
                ty: Box::new(Type::U8(TokenStream::from((3, "u8"))))
            }))
        );
    }
}
