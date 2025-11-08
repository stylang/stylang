//! Parser frontend for `stylang` programming language.
#![cfg_attr(docsrs, feature(doc_cfg))]

mod errors;
pub use errors::*;

pub mod input;
pub mod syntax;
pub mod token;
