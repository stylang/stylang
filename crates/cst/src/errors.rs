//! The types used for `CST` parsing error reports.

use parserc::{ControlFlow, ParseError, Span};

/// Error for parsing puncts.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum PunctKind {
    #[error("punct `::`")]
    PathSep,
    #[error("punct `'`")]
    Quote,
    #[error("punct `b'`")]
    BQuote,
    #[error("punct `\"`")]
    DoubleQuote,
    #[error("punct '+'")]
    Plus,
    #[error("punct '-'")]
    Minus,
    #[error("punct '*'")]
    Star,
    #[error("punct '/'")]
    Slash,
    #[error("punct '%'")]
    Percent,
    #[error("punct '^'")]
    Caret,
    #[error("punct '!'")]
    Not,
    #[error("punct '&'")]
    And,
    #[error("punct '|'")]
    Or,
    #[error("punct '&&'")]
    AndAnd,
    #[error("punct '||'")]
    OrOr,
    #[error("punct '<<'")]
    Shl,
    #[error("punct '>>'")]
    Shr,
    #[error("punct '+='")]
    PlusEq,
    #[error("punct '-='")]
    MinusEq,
    #[error("punct '*='")]
    StarEq,
    #[error("punct '/='")]
    SlashEq,
    #[error("punct '%='")]
    PercentEq,
    #[error("punct '^='")]
    CaretEq,
    #[error("punct '&='")]
    AndEq,
    #[error("punct '|='")]
    OrEq,
    #[error("punct '<<='")]
    ShlEq,
    #[error("punct '>>='")]
    ShrEq,
    #[error("punct '='")]
    Eq,
    #[error("punct '=='")]
    EqEq,
    #[error("punct '!='")]
    Ne,
    #[error("punct '>'")]
    Gt,
    #[error("punct '<'")]
    Lt,
    #[error("punct '<='")]
    Ge,
    #[error("punct '>='")]
    Le,
    #[error("punct '@'")]
    At,
    #[error("punct '_'")]
    Underscore,
    #[error("punct '.'")]
    Dot,
    #[error("punct '..'")]
    DotDot,
    #[error("punct '...'")]
    DotDotDot,
    #[error("punct '..='")]
    DotDotEq,
    #[error("punct ','")]
    Comma,
    #[error("punct ';'")]
    Semi,
    #[error("punct ':'")]
    Colon,
    #[error("punct '->'")]
    RArrow,
    #[error("punct '=>'")]
    FatArrow,
    #[error("punct '<-'")]
    LArrow,
    #[error("punct '#'")]
    Pound,
    #[error("punct '$'")]
    Dollar,
    #[error("punct '?'")]
    Question,
    #[error("punct '~'")]
    Tilde,
    #[error("punct '{{'")]
    BraceStart,
    #[error("punct '}}'")]
    BraceEnd,
    #[error("punct '['")]
    BracketStart,
    #[error("punct ']'")]
    BracketEnd,
    #[error("punct '('")]
    ParenStart,
    #[error("punct ')'")]
    ParenEnd,
}

impl PunctKind {
    /// Map error to `TokenKind`
    #[inline]
    pub fn map(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| CSTError::Punct(self, err.control_flow(), err.to_span())
    }

    /// Map unhandle error
    #[inline]
    pub fn map_unhandle(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| {
            if let CSTError::Kind(kind) = &err {
                CSTError::Punct(self, kind.control_flow(), kind.to_span())
            } else {
                err
            }
        }
    }

    /// Map error to `PunctKind` fatal error.
    #[inline]
    pub fn map_into_fatal(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| CSTError::Punct(self, ControlFlow::Fatal, err.to_span())
    }
}

/// Error for parsing keyword.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum KeywordKind {
    #[error("keyword `as`")]
    As,
    #[error("keyword `async`")]
    Async,
    #[error("keyword `await`")]
    Await,
    #[error("keyword `break`")]
    Break,
    #[error("keyword `const`")]
    Const,
    #[error("keyword `continue`")]
    Continue,
    #[error("keyword `crate`")]
    Crate,
    #[error("keyword `dyn`")]
    Dyn,
    #[error("keyword `else`")]
    Else,
    #[error("keyword `enum`")]
    Enum,
    #[error("keyword `extern`")]
    Extern,
    #[error("keyword `false`")]
    False,
    #[error("keyword `fn`")]
    Fn,
    #[error("keyword `for`")]
    For,
    #[error("keyword `if`")]
    If,
    #[error("keyword `impl`")]
    Impl,
    #[error("keyword `in`")]
    In,
    #[error("keyword `let`")]
    Let,
    #[error("keyword `loop`")]
    Loop,
    #[error("keyword `match`")]
    Match,
    #[error("keyword `mod`")]
    Mod,
    #[error("keyword `move`")]
    Move,
    #[error("keyword `mut`")]
    Mut,
    #[error("keyword `pub`")]
    Pub,
    #[error("keyword `ref`")]
    Ref,
    #[error("keyword `return`")]
    Return,
    #[error("keyword `self`")]
    SelfLower,
    #[error("keyword `Self`")]
    SelfUpper,
    #[error("keyword `static`")]
    Static,
    #[error("keyword `struct`")]
    Struct,
    #[error("keyword `super`")]
    Super,
    #[error("keyword `trait`")]
    Trait,
    #[error("keyword `true`")]
    True,
    #[error("keyword `type`")]
    Type,
    #[error("keyword `unsafe`")]
    Unsafe,
    #[error("keyword `use`")]
    Use,
    #[error("keyword `where`")]
    Where,
    #[error("keyword `where`")]
    While,
    #[error("reserved keyword `abstract`")]
    Abstract,
    #[error("reserved keyword `become`")]
    Become,
    #[error("reserved keyword `box`")]
    Box,
    #[error("reserved keyword `do`")]
    Do,
    #[error("reserved keyword `final`")]
    Final,
    #[error("reserved keyword `gen`")]
    Gen,
    #[error("reserved keyword `macro`")]
    Macro,
    #[error("reserved keyword `override`")]
    Override,
    #[error("reserved keyword `priv`")]
    Priv,
    #[error("reserved keyword `try`")]
    Try,
    #[error("reserved keyword `typeof`")]
    TypeOf,
    #[error("reserved keyword `unsized`")]
    Unsized,
    #[error("reserved keyword `virtual`")]
    Virtual,
    #[error("reserved keyword `yield`")]
    Yied,
    #[error("weak keyword `'static`")]
    StaticLifetime,
    #[error("weak keyword `macro_rules`")]
    MacroRules,
    #[error("weak keyword `raw`")]
    Raw,
    #[error("weak keyword `safe`")]
    Safe,
    #[error("weak keyword `union`")]
    Union,
}

impl KeywordKind {
    /// Map error to `TokenKind`
    #[inline]
    pub fn map(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| CSTError::Keyword(self, err.control_flow(), err.to_span())
    }

    /// Map unhandle error
    #[inline]
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
    #[error("identifier or keyword")]
    IdentOrKeyword,
    #[error("raw identifier")]
    RawIdent,
    #[error("reserved raw identifier")]
    ReservedRawIdent,
    #[error("non keyword identifier")]
    NonKeywordIdentifer,
    #[error("line comment")]
    LineComment,
    #[error("block comment")]
    BlockComment,
    #[error("outer line document")]
    OuterLineDoc,
    #[error("inner block document")]
    InnerBlockDoc,
    #[error("inner line document")]
    InnerLineDoc,
    #[error("outer block document")]
    OuterBlockDoc,
    #[error("simple path segment")]
    SimplePathSegment,
    #[error("simple path")]
    SimplePath,
    #[error("ascii escape")]
    ASCIIEscape,
    #[error("char of literal string")]
    StrChar,
    #[error("char of literal byte string")]
    ByteStrChar,
    #[error("raw string delimiter end token `\"#{{0..256}}`")]
    RawStringDelimiterEnd,
    #[error("unicode escape")]
    UnicodeEscape,
    #[error("quote escape")]
    QuoteEscape,
    #[error("character literal content")]
    Character,
    #[error("byte literal content")]
    Byte,
    #[error("string listeral content")]
    StrContent,
    #[error("byte string listeral content")]
    ByteStrContent,
    #[error("dec literal")]
    Dec,
    #[error("bin literal")]
    Bin,
    #[error("oct literal")]
    Oct,
    #[error("hex literal")]
    Hex,
    #[error("float literal")]
    Float,
    #[error("raw lifetime token")]
    RawLifeTime,
    #[error("lifetime token")]
    LifeTime,
    #[error("lifetime or label token")]
    LifeTimeOrLabel,
    #[error("reserved lifetime token")]
    ReservedLifeTime,
    #[error("placeholder lifetime token")]
    PlaceHolderLifeTime,
    #[error("token of macro invocation")]
    TokenTreeToken,
    #[error("token of macros by example")]
    MacroMatchToken,
    #[error("macros by example fragment type.")]
    MacroFragSpec,
    #[error("macros by example fragment ident.")]
    MacroFragIdent,
    #[error("macros repeat seperator")]
    MacroRepSep,
    #[error("macros repeat operator")]
    MacroRepOp,
}

impl SyntaxKind {
    /// Map unhandle error
    #[inline]
    pub fn map_unhandle(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| match err {
            CSTError::Kind(kind) => CSTError::Syntax(self, kind.control_flow(), kind.to_span()),
            err => err,
        }
    }

    #[inline]
    pub fn map_non_fatal(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| {
            if err.is_fatal() {
                err
            } else {
                CSTError::Syntax(self, ControlFlow::Fatal, err.to_span())
            }
        }
    }

    /// Map error to this kind.
    #[inline]
    pub fn map(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| CSTError::Syntax(self, err.control_flow(), err.to_span())
    }

    /// Map error to `SyntaxKind`
    pub fn map_into_fatal(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| CSTError::Syntax(self, ControlFlow::Fatal, err.to_span())
    }
}

/// Overflow kind
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum OverflowKind {}

impl OverflowKind {
    /// Map error to this kind.
    #[inline]
    pub fn map(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| CSTError::Overflow(self, err.to_span())
    }

    /// Map error to `SyntaxKind`
    #[inline]
    pub fn map_fatal(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| CSTError::Overflow(self, err.to_span())
    }
}

/// Semantics error.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum SemanticsKind {
    #[error("raw identifier")]
    RawIdent,
    #[error("non keyword identifier")]
    NonKeywordIdent,
    #[error("byte literal")]
    ByteLiteral,
    #[error("empty byte literal")]
    EmptyByteLiteral,
    #[error("`'`")]
    Quote,
    #[error("`\"`")]
    DoubleQuote,
    #[error("char of literal string")]
    StrChar,
    #[error("raw string leading `#`s too long")]
    RawStringLeadingPounds,
    #[error("char of literal byte string")]
    ByteStrChar,
    #[error("7bit char code")]
    Char7BitEscapeOutOfRange,
    #[error("7bit char code")]
    Char7BitEscapeTooShort,
    #[error("8bit char code")]
    Byte8BitEscapeTooShort,
    #[error("hex-digit")]
    HexDigit,
    #[error("unicode escape hex-digits length is out of range")]
    UnicodeEscapeLength,
    #[error("character literal value")]
    Character,
    #[error("byte literal value")]
    Byte,
    #[error("string listeral content")]
    StrContent,
    #[error("byte string listeral content")]
    ByteStrContent,
    #[error("raw byte string listeral content")]
    RawByteStrContent,
    #[error("c string listeral content")]
    CStrContent,
    #[error("raw c string listeral content")]
    RawCStrContent,
    #[error("raw string leading pounds out of range.")]
    Pounds,
    #[error("bin literal")]
    Bin,
    #[error("oct literal")]
    Oct,
    #[error("hex literal")]
    Hex,
    #[error("exponent decimal digit part")]
    Exponent,
    #[error("float literal")]
    Float,
    #[error("reserved num")]
    ReservedNum,
}

impl SemanticsKind {
    /// Map error to this kind.
    #[inline]
    pub fn map(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| CSTError::Semantics(self, err.to_span())
    }

    #[inline]
    pub fn map_non_fatal(self) -> impl FnOnce(CSTError) -> CSTError {
        |err: CSTError| {
            if err.is_fatal() {
                err
            } else {
                CSTError::Semantics(self, err.to_span())
            }
        }
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
