use parserc::syntax::{Or, Punctuated, Syntax};

use crate::{CSTInput, Colon, Eequal, Ident, Lit, Path, Plus, Question, Type};

/// A trait used as a bound on a type parameter.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]

pub struct TraitBound<I>
where
    I: CSTInput,
{
    /// A modifier on a trait bound, currently only used for the ? in ?Sized.
    pub modifier: Option<Question<I>>,
    /// The Foo<&'a T> in for<'a> Foo<&'a T>
    pub path: Path<I>,
}

/// An individual generic argument, like  T, or Item = T.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum GenericArgument<I>
where
    I: CSTInput,
{
    /// A binding (equality constraint) on an associated type: the Item = u8 in Iterator<Item = u8>.
    Associated {
        ident: Ident<I>,
        eq: Eequal<I>,
        ty: Or<Type<I>, Lit<I>>,
    },
    /// An associated type bound: Iterator<Item: Display>.
    Constraint {
        ident: Ident<I>,
        colon: Colon<I>,
        bounds: Punctuated<TraitBound<I>, Plus<I>>,
    },
    /// A type argument.
    Type(Type<I>),
}

#[cfg(test)]
mod test {

    use parserc::syntax::{InputSyntaxExt, Punctuated};

    use crate::{GenericArgument, Ident, Path, PathSegment, Question, TokenStream, TraitBound};

    #[test]
    fn test_trait_bound() {
        assert_eq!(
            TokenStream::from("?Sizied").parse::<TraitBound<_>>(),
            Ok(TraitBound {
                modifier: Some(Question(None, TokenStream::from((0, "?")), None)),
                path: Path {
                    leading_pathsep: None,
                    segments: Punctuated {
                        pairs: vec![],
                        tail: Some(Box::new(PathSegment {
                            ident: Ident(TokenStream::from((1, "Sizied"))),
                            arguments: None
                        }))
                    }
                }
            })
        );
    }

    #[test]
    fn test_generic_argument() {
        println!(
            "{:?}",
            TokenStream::from("T = u8").parse::<GenericArgument<_>>()
        );

        println!(
            "{:?}",
            TokenStream::from("N = 19").parse::<GenericArgument<_>>()
        );

        println!(
            "{:?}",
            TokenStream::from("T: Display + Eq + ").parse::<GenericArgument<_>>()
        );
    }
}
