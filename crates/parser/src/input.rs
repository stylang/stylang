//! The input types for `stylang` parsers.

use std::fmt::Debug;

use parserc::{AsBytes, AsStr, Find, Input, StartWith};

use crate::StylangError;

/// The `Input` trait for `stylang` parsers.
pub trait StylangInput:
    Input<Item = u8, Error = StylangError>
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

pub type TokenStream<'a> = parserc::bytes::TokenStream<'a, StylangError>;

impl<'a> StylangInput for TokenStream<'a> {}
