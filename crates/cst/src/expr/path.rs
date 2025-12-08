use parserc::syntax::{Delimiter, Syntax};

use crate::{
    input::CSTInput,
    keyword::As,
    path::Path,
    punct::{AngleBracketEnd, AngleBracketStart},
    ty::Type,
};

/// The explicit Self type in a qualified path: the T in <T as Display>::fmt.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct QSelf<I>(
    pub  Delimiter<
        AngleBracketStart<I>,
        AngleBracketEnd<I>,
        (Box<Type<I>>, Option<(As<I>, Box<Type<I>>)>),
    >,
)
where
    I: CSTInput;

/// A path like std::mem::replace possibly containing generic parameters and a qualified self-type.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprPath<I>
where
    I: CSTInput,
{
    /// optional `QSelf`
    pub qself: Option<QSelf<I>>,
    /// A path.
    pub path: Path<I>,
}
