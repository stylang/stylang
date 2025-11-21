use crate::CSTInput;

/// A literal string value.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LitStr<I>
where
    I: CSTInput,
{
    /// raw string flag `r`
    pub leading_flag: I,
    /// leading `#` characters.
    pub leading_pounds: I,
    /// char `"`
    pub delimiter_start: I,
    /// string content.
    pub content: I,
    /// char `"`
    pub delimiter_end: I,
    /// tailing `#` characters.
    pub tailing_pounds: I,
}
