use parserc::{ControlFlow, ParseError, Span};

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum TokenKind {
    /// Keyword `fn`
    #[error("keyword `fn`")]
    Fn,
    /// Keyword `struct`
    #[error("keyword `struct`")]
    Struct,
    /// Keyword `enum`
    #[error("keyword `enum`")]
    Enum,
    /// Keyword `view`
    #[error("keyword `view`")]
    View,
    /// punct `{`
    #[error("punct `{{`")]
    BraceStart,
    /// punct `}`
    #[error("punct `}}`")]
    BraceEnd,
    /// punct `[`
    #[error("punct `[`")]
    BracketStart,
    /// punct `]`
    #[error("punct `]`")]
    BracketEnd,
    /// punct `(`
    #[error("punct `(`")]
    ParenStart,
    /// punct `)`
    #[error("punct `)`")]
    ParenEnd,
    /// punct `:`
    #[error("punct `:`")]
    Colon,
    /// punct `,`
    #[error("punct `,`")]
    Comma,
    /// punct `;`
    #[error("punct `;`")]
    Semi,
    /// punct `::`
    #[error("punct `::`")]
    PathSep,
    /// punct `@`
    #[error("punct `@`")]
    At,
    /// punct `->`
    #[error("punct `->`")]
    ArrowRight,
    /// Operator `&`
    #[error("operator `&`")]
    And,
    /// Operator `&&`
    #[error("operator `&&`")]
    AndAnd,
    /// Operator `&=`
    #[error("operator `&=`")]
    AndEq,
    /// Operator `|`
    #[error("operator `|`")]
    Or,
    /// Operator `||`
    #[error("operator `||`")]
    OrOr,
    /// Operator `|=`
    #[error("operator `|=`")]
    OrEq,
    /// Operator `^`
    #[error("operator `^`")]
    Caret,
    /// Operator `^=`
    #[error("operator `^=`")]
    CaretEq,
    /// Operator `%`
    #[error("operator `%`")]
    Percent,
    /// Operator `%=`
    #[error("operator `%=`")]
    PercentEq,
    /// Operator `/`
    #[error("operator `/`")]
    Slash,
    /// Operator `/=`
    #[error("operator `/=`")]
    SlashEq,
    /// Operator `*`
    #[error("operator `*`")]
    Star,
    /// Operator `**`
    #[error("operator `**`")]
    StarStar,
    /// Operator `*=`
    #[error("operator `*=`")]
    StarEq,
    /// Operator `+`
    #[error("operator `+`")]
    Plus,
    /// Operator `+=`
    #[error("operator `+=`")]
    PlusEq,
    /// Operator `-`
    #[error("operator `-`")]
    Minus,
    /// Operator `-=`
    #[error("operator `-=`")]
    MinusEq,
    /// Operator `>>`
    #[error("operator `>>`")]
    Shr,
    /// Operator `>>=`
    #[error("operator `>>=`")]
    ShrEq,
    /// Operator `<<`
    #[error("operator `<<`")]
    Shl,
    /// Operator `<<=`
    #[error("operator `<<=`")]
    ShlEq,
    /// Operator `=`
    #[error("operator `=`")]
    Eq,
    /// Operator `==`
    #[error("operator `==`")]
    EqEq,
    /// Operator `=>`
    #[error("operator `=>`")]
    FatArrowRight,
    /// Operator `>`
    #[error("operator `>`")]
    Gt,
    /// Operator `>=`
    #[error("operator `>=`")]
    Ge,
    /// Operator `/>`
    #[error("operator `/>`")]
    Sg,
    /// Operator `<`
    #[error("operator `<`")]
    Lt,
    /// Operator `<=`
    #[error("operator `<=`")]
    Le,
    /// Operator `</`
    #[error("operator `</`")]
    Ls,
    /// Operator `!`
    #[error("operator `!`")]
    Not,
    /// Operator `!=`
    #[error("operator `!=`")]
    NotEq,
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
