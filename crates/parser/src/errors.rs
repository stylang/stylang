use parserc::{ControlFlow, ParseError, Span};

/// Error kind for `stylang` parser.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum TokenKind {
    #[error("whitespaces")]
    S,
    #[error("&")]
    And,
    #[error("&&")]
    AndAnd,
    #[error("&=")]
    AndEq,
    #[error("|")]
    Or,
    #[error("||")]
    OrOr,
    #[error("|=")]
    OrEq,
    #[error("^")]
    Caret,
    #[error("^=")]
    CaretEq,
}

impl TokenKind {
    /// Map error to this kind.
    pub fn map(self) -> impl FnOnce(StylangError) -> StylangError {
        |err: StylangError| StylangError::Token(self, err.control_flow(), err.span())
    }
}

/// Error returns by syntax parsers.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum StylangError {
    #[error("{0:?}")]
    Other(#[from] parserc::Kind),
    #[error("{1:?}: Parsing `{0:?}` error, {1:?}")]
    Token(TokenKind, ControlFlow, Span),
}

impl ParseError for StylangError {
    fn control_flow(&self) -> parserc::ControlFlow {
        match self {
            StylangError::Other(kind) => kind.control_flow(),
            StylangError::Token(_, control_flow, _) => *control_flow,
        }
    }

    fn into_fatal(self) -> Self {
        match self {
            StylangError::Other(kind) => StylangError::Other(kind.into_fatal()),
            StylangError::Token(kind, _, span) => {
                StylangError::Token(kind, ControlFlow::Fatal, span)
            }
        }
    }

    fn span(&self) -> Span {
        match self {
            StylangError::Other(kind) => kind.span(),
            StylangError::Token(_, _, span) => span.clone(),
        }
    }
}
