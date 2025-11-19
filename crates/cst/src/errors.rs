use parserc::{ControlFlow, ParseError, Span};

/// Error for tokens.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum TokenKind {
    /// Keyword `rgb`
    #[error("keyword `rgb`")]
    Rgb,
    /// Keyword `color`
    #[error("keyword `color`")]
    Color,
    /// Keyword `set`
    #[error("keyword `set`")]
    Set,
    /// Keyword `map`
    #[error("keyword `map`")]
    Map,
    /// Keyword `string`
    #[error("keyword `string`")]
    String,
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
    /// punct `#`
    #[error("punct `#`")]
    Pound,
    /// punct `"`
    #[error("punct `\"`")]
    DoubleQuote,
    /// punct `'`
    #[error("punct `'`")]
    SingleQuote,
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
    /// Operator `/>`
    #[error("operator `/>`")]
    Sg,
    /// Operator `//`
    #[error("operator `//`")]
    SlashSlash,
    /// Operator `///`
    #[error("operator `///`")]
    SlashSlashSlash,
    /// Operator `//!`
    #[error("operator `//!`")]
    SlashSlashNot,
    /// Operator `/**`
    #[error("operator `/**`")]
    SlashStarStar,
    /// Operator `/*!`
    #[error("operator `/*!`")]
    SlashStarNot,
    /// Operator `/*`
    #[error("operator `/*`")]
    SlashStar,
    /// Operator `*/`
    #[error("operator `*/`")]
    StarSlash,
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
    /// ident character sequence.
    #[error("`ident` string")]
    Ident,
    /// xml ident character sequence.
    #[error("`xml ident` string")]
    XmlIdent,
    /// keyword `i8,...`
    #[error("keyword `i8,i16,...`")]
    Int,
    /// keyword `u8,...`
    #[error("keyword `u8,u16,...`")]
    Uint,
    /// literial digits.
    #[error("literial `digits`")]
    LitDigits,
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
    #[error("`literal rgb value`")]
    LitRgb,
    #[error("`literal hex rgb value`")]
    LitHexRgb,
    #[error("`rgb component`")]
    RgbComponent,
    #[error("`literal string`")]
    LitStr,
    #[error("`literal string tailing quote`")]
    TailingQuote,
}

impl SyntaxKind {
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
}

impl ParseError for CSTError {
    #[inline]
    fn to_span(&self) -> Span {
        match self {
            CSTError::Kind(kind) => kind.to_span(),
            CSTError::Token(_, _, span) => span.clone(),
            CSTError::Syntax(_, _, span) => span.clone(),
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
        }
    }
}
