use parserc::{ControlFlow, syntax::Syntax};

use crate::{errors::CSTError, input::CSTInput, punct::Underscore};

/// The inferred value of a const generic argument, denoted _.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprInfer<I>(pub Underscore<I>)
where
    I: CSTInput;

impl<I> Syntax<I> for ExprInfer<I>
where
    I: CSTInput,
{
    #[inline]
    fn parse(input: &mut I) -> Result<Self, <I as parserc::Input>::Error> {
        let underscore = Underscore::parse(input)?;

        if underscore.2.is_none() {
            if let Some(c) = input.iter().next() {
                if c.is_ascii_alphanumeric() {
                    return Err(CSTError::Syntax(
                        crate::errors::SyntaxKind::ExprInfer,
                        ControlFlow::Recovable,
                        underscore.to_span(),
                    ));
                }
            }
        }

        Ok(Self(underscore))
    }

    #[inline]
    fn to_span(&self) -> parserc::Span {
        self.0.to_span()
    }
}
