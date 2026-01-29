//! The types used for `unsyn` parsing error reports.

use parserc::{ControlFlow, ParseError, Span};

/// Error for punct tokens.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum PunctKind {
    #[error("punct ';'")]
    Semi,
}

impl PunctKind {
    /// Map error to `punct` error.
    #[inline]
    pub fn map(self) -> impl FnOnce(UnsynError) -> UnsynError {
        |err: UnsynError| UnsynError::Punct(self, err.control_flow(), err.to_span())
    }
}

/// Error for syntax tree.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum SyntaxKind {
    #[error("unicode literal")]
    Unicode,
}

impl SyntaxKind {
    /// Map error to `syntax` error.
    #[inline]
    pub fn map(self) -> impl FnOnce(UnsynError) -> UnsynError {
        |err: UnsynError| UnsynError::Syntax(self, err.control_flow(), err.to_span())
    }

    /// Map error to `syntax` fatal error.
    #[inline]
    pub fn map_into_fatal(self) -> impl FnOnce(UnsynError) -> UnsynError {
        |err: UnsynError| UnsynError::Syntax(self, ControlFlow::Fatal, err.to_span())
    }
}

/// Error for semantic check.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum SemanticsKind {
    #[error("unicode literal")]
    Unicode,
}

impl SemanticsKind {
    /// Map error to `semantic` error.
    #[inline]
    pub fn map(self) -> impl FnOnce(UnsynError) -> UnsynError {
        |err: UnsynError| UnsynError::Semantics(self, err.to_span())
    }
}

/// Error information container for `unsyn` parsing.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum UnsynError {
    /// Unhandle parserc `Errors`.
    #[error(transparent)]
    Kind(#[from] parserc::Kind),

    /// Reports a lexer error.
    #[error("punct error: expect {0}, {1:?},{2:?}")]
    Punct(PunctKind, ControlFlow, Span),

    /// Reports a syntax error.
    #[error("syntax error: expect {0}, {1:?},{2:?}")]
    Syntax(SyntaxKind, ControlFlow, Span),
    /// Reports a semantics error
    #[error("unexpect/invalid: {0}, {1:?}")]
    Semantics(SemanticsKind, Span),
}

impl ParseError for UnsynError {
    #[inline]
    fn to_span(&self) -> Span {
        match self {
            UnsynError::Kind(kind) => kind.to_span(),
            UnsynError::Syntax(_, _, span) => span.clone(),
            UnsynError::Punct(_, _, span) => span.clone(),
            UnsynError::Semantics(_, span) => span.clone(),
        }
    }

    #[inline]
    fn control_flow(&self) -> parserc::ControlFlow {
        match self {
            UnsynError::Kind(kind) => kind.control_flow(),
            UnsynError::Syntax(_, control_flow, _) => *control_flow,
            UnsynError::Punct(_, control_flow, _) => *control_flow,
            UnsynError::Semantics(_, _) => ControlFlow::Fatal,
        }
    }

    #[inline]
    fn into_fatal(self) -> Self {
        match self {
            UnsynError::Kind(kind) => Self::Kind(kind.into_fatal()),
            UnsynError::Syntax(syntax_kind, _, span) => {
                Self::Syntax(syntax_kind, ControlFlow::Fatal, span)
            }
            UnsynError::Punct(punct_kind, _, span) => {
                UnsynError::Punct(punct_kind, ControlFlow::Fatal, span)
            }
            UnsynError::Semantics(semantics_kind, span) => {
                UnsynError::Semantics(semantics_kind, span)
            }
        }
    }
}
