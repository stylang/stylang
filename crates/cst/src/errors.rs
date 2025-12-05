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
    #[error("punct `|`")]
    Or,
    #[error("punct `<`")]
    AngleBracketStart,
    #[error("punct `>`")]
    AngleBracketEnd,
    #[error("punct `?`")]
    Question,
    #[error("punct `+`")]
    Plus,
    #[error("punct `-`")]
    Minus,
    #[error("punct `+=`")]
    PlusEq,
    #[error("punct `-=`")]
    MinusEq,
    #[error("punct `=`")]
    Equal,
    #[error("punct `&`")]
    And,
    #[error("punct `&&`")]
    AndAnd,
    #[error("punct `&=`")]
    AndEq,
    #[error("punct `||`")]
    OrOr,
    #[error("punct `|=`")]
    OrEq,
    #[error("punct `<`")]
    Lt,
    #[error("punct `<=`")]
    LtEq,
    #[error("punct `<<`")]
    Shl,
    #[error("punct `<<=`")]
    ShlEq,
    #[error("punct `>`")]
    Gt,
    #[error("punct `>=`")]
    GtEq,
    #[error("punct `>>`")]
    Shr,
    #[error("punct `>>=`")]
    ShrEq,
    #[error("punct `*`")]
    Star,
    #[error("punct `*=`")]
    StarEq,
    #[error("punct `**`")]
    StarStar,
    #[error("punct `**=`")]
    StarStarEq,
    #[error("punct `/`")]
    Slash,
    #[error("punct `/=`")]
    SlashEq,
    #[error("punct `%`")]
    Rem,
    #[error("punct `%=`")]
    RemEq,
    #[error("punct `==`")]
    EqEq,
    #[error("punct `!=`")]
    NotEq,
    #[error("punct `^`")]
    Caret,
    #[error("punct `^=`")]
    CaretEq,
    #[error("punct `!`")]
    Not,
    #[error("punct `.`")]
    Dot,
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
    Fn_,
    #[error("keyword `mut`")]
    Mut,
    #[error("keyword `self`")]
    SelfObject,
    #[error("keyword `Self`")]
    SelfClass,
    #[error("keyword `const`")]
    Const,
    #[error("keyword `where`")]
    Where,
    #[error("keyword `extern`")]
    Extern,
    #[error("keyword `as`")]
    As,
    #[error("keyword `let`")]
    Let,
    #[error("keyword `else`")]
    Else,
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
pub enum SyntaxKind {
    #[error("`line comment`")]
    LineComment,
    #[error("`outer line document`")]
    OuterLineDoc,
    #[error("`inner line document`")]
    InnerLineDoc,
    #[error("`block comment`")]
    BlockComment,
    #[error("`outer block document`")]
    OuterBlockDoc,
    #[error("`inner block document`")]
    InnerBlockDoc,
    #[error("`ident`")]
    Ident,
    #[error("`xml ident`")]
    XmlIdent,
    #[error("`literal string`")]
    LitStr,
    #[error("`tailing quote token`")]
    TailingQuote,
    #[error("`literal digits`")]
    LitDigits,
    #[error("`literal number`")]
    LitNumber,
    #[error("`literal hex-number`")]
    LitHexNumber,
    #[error("`literal bool`")]
    LitBool,
    #[error("`A bare function type`")]
    BareFn,
    #[error("`item path`")]
    Path,
    #[error("`attribute argument`")]
    AttrArgument,
    #[error("pub `super/crate`")]
    VisibilityPredicate,
    #[error("expr `assign left operand`")]
    AssignLeftOperand,
    #[error("expr `assign right operand`")]
    AssignRightOperand,
    #[error("expr `let init`")]
    LetInitExpr,
    #[error("expr `call`")]
    ExprCall,
    #[error("expr binary `left operand`")]
    ExprBinaryLeftOperand,
    #[error("expr binary `right operand`")]
    ExprBinaryRightOperand,
    #[error("expr binary `op`")]
    ExprBinaryOp,
    #[error("expr unary `right operand`")]
    ExprUnaryRightOprand,
    #[error("expr `field`")]
    ExprField,
    #[error("expr `field dot`")]
    ExprFieldDot,
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
    #[error("method call `turbofish`")]
    TurboFish,
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
