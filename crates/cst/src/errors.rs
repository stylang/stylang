use parserc::{ControlFlow, ParseError, Span};

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum TokenKind {
    /// Keyword `fn`
    #[error("keyword `fn`")]
    Fn,
    /// Punct `{`
    #[error("Punct `{{`")]
    BraceStart,
    /// Punct `}`
    #[error("Punct `}}`")]
    BraceEnd,
    /// Punct `[`
    #[error("Punct `[`")]
    BracketStart,
    /// Punct `]`
    #[error("Punct `]`")]
    BracketEnd,
    /// Punct `(`
    #[error("Punct `(`")]
    ParenStart,
    /// Punct `)`
    #[error("Punct `)`")]
    ParenEnd,
    /// Punct `@`
    #[error("Punct `@`")]
    At,
    /// Punct `->`
    #[error("Punct `->`")]
    ArrowRight,
}

impl TokenKind {
    /// Map error to this kind.
    pub fn map(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| CSTError::Token(self, err.control_flow(), err.to_span())
    }
}

/// Error returns by CST parsers.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum CSTError {
    /// Unhandle parserc `Errors`.
    #[error(transparent)]
    Kind(#[from] parserc::Kind),

    /// Failed to parse token.
    #[error("lexer error: expect {0}, {1:?},{2:?}")]
    Token(TokenKind, ControlFlow, Span),
}

impl ParseError for CSTError {
    #[inline]
    fn to_span(&self) -> Span {
        match self {
            CSTError::Kind(kind) => kind.to_span(),
            CSTError::Token(_, _, span) => span.clone(),
        }
    }

    #[inline]
    fn control_flow(&self) -> parserc::ControlFlow {
        match self {
            CSTError::Kind(kind) => kind.control_flow(),
            CSTError::Token(_, control_flow, _) => *control_flow,
        }
    }

    #[inline]
    fn into_fatal(self) -> Self {
        match self {
            CSTError::Kind(kind) => CSTError::Kind(kind.into_fatal()),
            CSTError::Token(token_kind, _, span) => {
                CSTError::Token(token_kind, ControlFlow::Fatal, span)
            }
        }
    }
}
