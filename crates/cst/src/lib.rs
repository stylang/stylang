//! Concrete Syntax Tree for `stylang` based on `parserc` infrastructure
#![cfg_attr(docsrs, feature(doc_cfg))]

pub mod block;
pub mod errors;
pub mod expr;
pub mod generics;
pub mod input;
pub mod item;
pub mod keyword;
pub mod misc;
pub mod pat;
pub mod path;
pub mod punct;
pub mod ty;
pub mod vs;
