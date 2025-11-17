use parserc::{ControlFlow, ParseError, Span};

/// Error kind for `stylang` parser.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum TokenKind {
    #[error("`whitespaces`")]
    S,
    #[error("keyword `fn`")]
    Fn,
    #[error("keyword `struct`")]
    Struct,
    #[error("keyword `enum`")]
    Enum,
    #[error("keyword `pub`")]
    Pub,
    #[error("keyword `native`")]
    Native,
    #[error("keyword `use`")]
    Use,
    #[error("keyword `mod`")]
    Mod,
    #[error("punctuation `{{`")]
    BraceStart,
    #[error("punctuation `}}`")]
    BraceEnd,
    #[error("punctuation `[`")]
    BracketStart,
    #[error("punctuation `]`")]
    BracketEnd,
    #[error("punctuation `(`")]
    ParenStart,
    #[error("punctuation `)`")]
    ParenEnd,
}

impl TokenKind {
    /// Map error to this kind.
    pub fn map(self) -> impl FnOnce(StylangError) -> StylangError {
        |err: StylangError| StylangError::Token(self, err.control_flow(), err.to_span())
    }
}

/// Error kind for operator tokens.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum OpKind {
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
    #[error("->")]
    RightArrow,
}

impl OpKind {
    /// Map error to this kind.
    pub fn map(self) -> impl FnOnce(StylangError) -> StylangError {
        |err: StylangError| StylangError::Op(self, err.control_flow(), err.to_span())
    }
}

/// Error returns by syntax parsers.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum StylangError {
    #[error("{0:?}")]
    Other(#[from] parserc::Kind),
    #[error("{1:?}: failed to parse {0:?} error, {1:?}")]
    Token(TokenKind, ControlFlow, Span),
    #[error("{1:?}: failed to parse operator `{0:?}`, {1:?}")]
    Op(OpKind, ControlFlow, Span),
}

impl ParseError for StylangError {
    fn control_flow(&self) -> parserc::ControlFlow {
        match self {
            StylangError::Other(kind) => kind.control_flow(),
            StylangError::Token(_, control_flow, _) => *control_flow,
            StylangError::Op(_, control_flow, _) => *control_flow,
        }
    }

    fn into_fatal(self) -> Self {
        match self {
            StylangError::Other(kind) => StylangError::Other(kind.into_fatal()),
            StylangError::Token(kind, _, span) => {
                StylangError::Token(kind, ControlFlow::Fatal, span)
            }
            StylangError::Op(kind, _, span) => StylangError::Op(kind, ControlFlow::Fatal, span),
        }
    }

    fn to_span(&self) -> Span {
        match self {
            StylangError::Other(kind) => kind.to_span(),
            StylangError::Token(_, _, span) => span.clone(),
            StylangError::Op(_, _, span) => span.clone(),
        }
    }
}
