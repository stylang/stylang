//! Concrete Syntax Tree for `stylang` based on `parserc` infrastructure.
//!
//! It also serves as a Rust source code-compatible front-end parser.
#![cfg_attr(docsrs, feature(doc_cfg))]

pub mod attr;
pub mod cfg;
pub mod errors;
pub mod expr;
pub mod input;
pub mod lexical;
pub mod macros;
pub mod names;
pub mod types;
