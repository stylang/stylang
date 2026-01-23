//! A statement is a component of a block, which is in turn a component of an outer expression or function.
//!
//! More information see [`The Rust Reference`]
//!
//! [`The Rust Reference`]: https://doc.rust-lang.org/stable/reference/statements.html

use parserc::syntax::Syntax;

use crate::{attr::OuterAttribute, input::CSTInput};

/// Each form of conditional compilation takes a configuration predicate
/// that evaluates to true or false
///
/// More information see [`The Rust Reference`]
///
/// [`The Rust Reference`]: https://doc.rust-lang.org/reference/conditional-compilation.html#r-cfg.predicate
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Stmt<I>
where
    I: CSTInput,
{
    A(OuterAttribute<I>),
}
