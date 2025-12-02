//! The types used for `CST` parsing error reports.

use parserc::{ControlFlow, ParseError, Span};

/// Error for parsing puncts.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum PunctKind {
    #[error("punct `{{`")]
    BraceStart,
    #[error("punct `}}`")]
    BraceEnd,
    #[error("punct `[`")]
    BracketStart,
    #[error("punct `]`")]
    BracketEnd,
    #[error("punct `(`")]
    ParenStart,
    #[error("punct `)`")]
    ParenEnd,
    #[error("punct `@`")]
    At,
    #[error("punct `->`")]
    ArrowRight,
    #[error("punct `:`")]
    Colon,
    #[error("punct `,`")]
    Comma,
    #[error("punct `;`")]
    Semi,
    #[error("punct `::`")]
    PathSep,
}

impl PunctKind {
    /// Map error to `TokenKind`
    pub fn map(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| CSTError::Punct(self, err.control_flow(), err.to_span())
    }

    /// Map unhandle error
    pub fn map_unhandle(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| {
            if let CSTError::Kind(kind) = &err {
                CSTError::Punct(self, kind.control_flow(), kind.to_span())
            } else {
                err
            }
        }
    }
}

/// Error for parsing keyword.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum KeywordKind {
    #[error("keyword `struct`")]
    Struct,
    #[error("keyword `enum`")]
    Enum,
    #[error("keyword `fn`")]
    Fn,
    #[error("keyword `mut`")]
    Mut,
    #[error("keyword `self`")]
    SelfObject,
    #[error("keyword `Self`")]
    SelfClass,
}

impl KeywordKind {
    /// Map error to `TokenKind`
    pub fn map(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| CSTError::Keyword(self, err.control_flow(), err.to_span())
    }

    /// Map unhandle error
    pub fn map_unhandle(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| {
            if let CSTError::Kind(kind) = &err {
                CSTError::Keyword(self, kind.control_flow(), kind.to_span())
            } else {
                err
            }
        }
    }
}

/// Error for syntax tree.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum SyntaxKind {}

impl SyntaxKind {
    /// Map unhandle error
    pub fn map_unhandle(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| {
            if let CSTError::Kind(kind) = &err {
                CSTError::Syntax(self, kind.control_flow(), kind.to_span())
            } else {
                err
            }
        }
    }

    /// Map error to this kind.
    pub fn map(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| CSTError::Syntax(self, err.control_flow(), err.to_span())
    }

    /// Map error to `SyntaxKind`
    pub fn map_fatal(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| CSTError::Syntax(self, ControlFlow::Fatal, err.to_span())
    }
}

/// Overflow kind
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum OverflowKind {
    #[error("`rgb component`")]
    RgbComponent,
}

impl OverflowKind {
    /// Map error to this kind.
    pub fn map(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| CSTError::Overflow(self, err.to_span())
    }

    /// Map error to `SyntaxKind`
    pub fn map_fatal(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| CSTError::Overflow(self, err.to_span())
    }
}

/// Semantics error.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum SemanticsKind {}

impl SemanticsKind {
    /// Map error to this kind.
    pub fn map(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| CSTError::Semantics(self, err.to_span())
    }

    /// Map error to `SyntaxKind`
    pub fn map_fatal(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| CSTError::Semantics(self, err.to_span())
    }
}

/// Error information container for `CST` parsing.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum CSTError {
    /// Unhandle parserc `Errors`.
    #[error(transparent)]
    Kind(#[from] parserc::Kind),

    /// Reports a lexer error.
    #[error("punct error: expect {0}, {1:?},{2:?}")]
    Punct(PunctKind, ControlFlow, Span),

    /// Reports a lexer error.
    #[error("punct error: expect {0}, {1:?},{2:?}")]
    Keyword(KeywordKind, ControlFlow, Span),

    /// Reports a syntax error.
    #[error("syntax error: expect {0}, {1:?},{2:?}")]
    Syntax(SyntaxKind, ControlFlow, Span),

    /// Reports a literal value is overflow.
    #[error("literal value {0:?} is overflow: {1:?}")]
    Overflow(OverflowKind, Span),

    /// Reports a semantics error
    #[error("unexpect/invalid: {0}, {1:?}")]
    Semantics(SemanticsKind, Span),
}

impl ParseError for CSTError {
    #[inline]
    fn to_span(&self) -> Span {
        match self {
            CSTError::Kind(kind) => kind.to_span(),
            CSTError::Punct(_, _, span) => span.clone(),
            CSTError::Keyword(_, _, span) => span.clone(),
            CSTError::Syntax(_, _, span) => span.clone(),
            CSTError::Semantics(_, span) => span.clone(),
            CSTError::Overflow(_, span) => span.clone(),
        }
    }

    #[inline]
    fn control_flow(&self) -> parserc::ControlFlow {
        match self {
            CSTError::Kind(kind) => kind.control_flow(),
            CSTError::Punct(_, control_flow, _) => *control_flow,
            CSTError::Keyword(_, control_flow, _) => *control_flow,
            CSTError::Syntax(_, control_flow, _) => *control_flow,
            CSTError::Overflow(_, _) => ControlFlow::Fatal,
            CSTError::Semantics(_, _) => ControlFlow::Fatal,
        }
    }

    #[inline]
    fn into_fatal(self) -> Self {
        match self {
            CSTError::Kind(kind) => CSTError::Kind(kind.into_fatal()),
            CSTError::Punct(token_kind, _, span) => {
                CSTError::Punct(token_kind, ControlFlow::Fatal, span)
            }
            CSTError::Keyword(token_kind, _, span) => {
                CSTError::Keyword(token_kind, ControlFlow::Fatal, span)
            }
            CSTError::Syntax(kind, _, span) => CSTError::Syntax(kind, ControlFlow::Fatal, span),
            CSTError::Overflow(kind, span) => CSTError::Overflow(kind, span),
            CSTError::Semantics(kind, span) => CSTError::Semantics(kind, span),
        }
    }
}
