//! The input types for `CST` parsing.

use std::fmt::Debug;

use parserc::{AsBytes, AsStr, Find, Input, StartWith};

use crate::errors::CSTError;

/// The `Input` trait for `CST` parsing.
pub trait CSTInput:
    Input<Item = u8, Error = CSTError>
    + AsBytes
    + AsStr
    + StartWith<&'static str>
    + StartWith<&'static [u8]>
    + Find<&'static str>
    + Find<&'static [u8]>
    + Clone
    + Debug
    + PartialEq
{
}

/// A implementation of `CSTInput`.
pub type TokenStream<'a> = parserc::bytes::TokenStream<'a, CSTError>;

impl<'a> CSTInput for TokenStream<'a> {}
