use parserc::{ControlFlow, Parser, syntax::Syntax};

use crate::{
    errors::{CSTError, PunctKind, SyntaxKind},
    expr::Digits,
    input::CSTInput,
    misc::Ident,
    punct::{Dot, DotDot},
};

/// A struct or tuple struct field accessed in a struct literal or field expression.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[parserc(map_err = SyntaxKind::ExprFieldDot.map())]
pub enum Member<I>
where
    I: CSTInput,
{
    Named(Ident<I>),
    Unamed(Digits<I>),
}

/// Access of a named struct field (`obj.k`) or unnamed tuple struct field (`obj.0`).
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Field<I>
where
    I: CSTInput,
{
    /// punct `.`
    #[parserc(parser = parse_field_dot)]
    pub dot: Dot<I>,
    /// member of struct.
    pub member: Member<I>,
}

#[inline]
fn parse_field_dot<I>(input: &mut I) -> Result<Dot<I>, CSTError>
where
    I: CSTInput,
{
    let None = DotDot::into_parser().ok().parse(input)? else {
        return Err(CSTError::Punct(
            PunctKind::Dot,
            ControlFlow::Recovable,
            input.to_span_at(1),
        ));
    };

    Dot::parse(input)
}
