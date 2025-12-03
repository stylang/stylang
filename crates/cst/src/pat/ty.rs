use parserc::{Parser, syntax::Syntax};

use crate::{errors::CSTError, input::CSTInput, misc::Ident, pat::Pat, punct::Colon, ty::Type};

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
