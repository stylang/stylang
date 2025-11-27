use parserc::{ControlFlow, ParseError, Span};

/// Error for tokens.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum TokenKind {
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
    #[error("punct `::`")]
    ColonColon,
    #[error("punct `,`")]
    Comma,
    #[error("punct `;`")]
    Semi,
    #[error("punct `::`")]
    PathSep,
    #[error("punct `|`")]
    Or,
    #[error("punct `<`")]
    AngleBracketStart,
    #[error("punct `>`")]
    AngleBracketEnd,
    #[error("punct `=`")]
    Eequal,
    #[error("punct `?`")]
    Question,
    #[error("keyword `fn`")]
    Fn,
    #[error("keyword `struct`")]
    Struct,
    #[error("keyword `enum`")]
    Enum,
    #[error("keyword `impl`")]
    Impl,
    #[error("keyword `trait`")]
    Trait,
    #[error("keyword `extern`")]
    Extern,
    #[error("keyword `type`")]
    Type,
    #[error("keyword `use`")]
    Use,
    #[error("keyword `mod`")]
    Mod,
    #[error("keyword `pub`")]
    Pub,
    /// ident character sequence.
    #[error("type `name`")]
    Ident,
    /// xml ident character sequence.
    #[error("xml tag `name`")]
    XmlIdent,
    /// literial digits.
    #[error("literial `digits`")]
    LitDigits,
    /// literial hex-digits.
    #[error("literial `hex-digits`")]
    LitHexDigits,
}

impl TokenKind {
    /// Map error to this kind.
    pub fn map(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| CSTError::Token(self, err.control_flow(), err.to_span())
    }
}

/// Error for syntax tree.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum SyntaxKind {
    /// line comment
    #[error("`line comment`")]
    LineComment,
    /// block comment
    #[error("`block comment`")]
    BlockComment,
    /// outer line document
    #[error("`outer line doc`")]
    OuterLineDoc,
    /// outer block document.
    #[error("`outer block doc`")]
    OuterBlockDoc,
    /// inner line document
    #[error("`inner line doc`")]
    InnerLineDoc,
    /// outer block document.
    #[error("`inner block doc`")]
    InnerBlockDoc,
    #[error("literial string tailing `\"`")]
    TailingQuote,
    #[error("`literal expr`")]
    Lit,
    #[error("`literal bool value`")]
    LitBool,
    #[error("literal `number`")]
    LitNumber,
    #[error("literal `hex-number`")]
    LitHexNumber,
    #[error("`literal string`")]
    LitStr,
    #[error("Visibility predicates `crate` or `super`")]
    VisibilityPredicate,
    #[error("type union expr `right operand`.")]
    TypeUnionOperand,
    #[error("type `bare fn`.")]
    BareFn,
    #[error("type `path`")]
    Path,
}

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
pub enum SemanticsKind {
    #[error("`number unit`")]
    Unit,
}

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

/// Error returns by CST parsers.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum CSTError {
    /// Unhandle parserc `Errors`.
    #[error(transparent)]
    Kind(#[from] parserc::Kind),

    /// Failed to parse token.
    #[error("token error: expect {0}, {1:?},{2:?}")]
    Token(TokenKind, ControlFlow, Span),

    /// Failed to parse syntax tree.
    #[error("syntax error: expect {0}, {1:?},{2:?}")]
    Syntax(SyntaxKind, ControlFlow, Span),

    /// literial value is overflow
    #[error("literial {0:?} is overflow: {1:?}")]
    Overflow(OverflowKind, Span),

    /// Semantics error
    #[error("unexpect/invalid: {0}, {1:?}")]
    Semantics(SemanticsKind, Span),
}

impl ParseError for CSTError {
    #[inline]
    fn to_span(&self) -> Span {
        match self {
            CSTError::Kind(kind) => kind.to_span(),
            CSTError::Token(_, _, span) => span.clone(),
            CSTError::Syntax(_, _, span) => span.clone(),
            CSTError::Semantics(_, span) => span.clone(),
            CSTError::Overflow(_, span) => span.clone(),
        }
    }

    #[inline]
    fn control_flow(&self) -> parserc::ControlFlow {
        match self {
            CSTError::Kind(kind) => kind.control_flow(),
            CSTError::Token(_, control_flow, _) => *control_flow,
            CSTError::Syntax(_, control_flow, _) => *control_flow,
            CSTError::Overflow(_, _) => ControlFlow::Fatal,
            CSTError::Semantics(_, _) => ControlFlow::Fatal,
        }
    }

    #[inline]
    fn into_fatal(self) -> Self {
        match self {
            CSTError::Kind(kind) => CSTError::Kind(kind.into_fatal()),
            CSTError::Token(token_kind, _, span) => {
                CSTError::Token(token_kind, ControlFlow::Fatal, span)
            }
            CSTError::Syntax(kind, _, span) => CSTError::Syntax(kind, ControlFlow::Fatal, span),
            CSTError::Overflow(kind, span) => CSTError::Overflow(kind, span),
            CSTError::Semantics(kind, span) => CSTError::Semantics(kind, span),
        }
    }
}
