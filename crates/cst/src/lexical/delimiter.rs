//! lexical `brackets`, more information see [`The Rust Reference`]
//!
//! [`The Rust Reference`]: https://doc.rust-lang.org/reference/tokens.html#delimiters

use parserc::syntax::Delimiter;

use crate::lexical::punct::{BraceEnd, BraceStart, BracketEnd, BracketStart, ParenEnd, ParenStart};

/// Curly braces
pub type Brace<I, T> = Delimiter<BraceStart<I>, BraceEnd<I>, T>;
/// Square brackets
pub type Bracket<I, T> = Delimiter<BracketStart<I>, BracketEnd<I>, T>;
/// Parentheses
pub type Paren<I, T> = Delimiter<ParenStart<I>, ParenEnd<I>, T>;
