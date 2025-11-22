use parserc::syntax::Syntax;

use crate::CSTInput;

/// Type of `stylang` value.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Type<I>
where
    I: CSTInput,
{
    I8(#[parserc(keyword = "i8")] I),
    I16(#[parserc(keyword = "i16")] I),
    I32(#[parserc(keyword = "i32")] I),
    I64(#[parserc(keyword = "i64")] I),
    I128(#[parserc(keyword = "i128")] I),
    I256(#[parserc(keyword = "i256")] I),
    U8(#[parserc(keyword = "u8")] I),
    U16(#[parserc(keyword = "u16")] I),
    U32(#[parserc(keyword = "u32")] I),
    U64(#[parserc(keyword = "u64")] I),
    U128(#[parserc(keyword = "u128")] I),
    U256(#[parserc(keyword = "u256")] I),
    F16(#[parserc(keyword = "f16")] I),
    F32(#[parserc(keyword = "f32")] I),
    F64(#[parserc(keyword = "f64")] I),
    Time(#[parserc(keyword = "time")] I),
    Length(#[parserc(keyword = "length")] I),
    Color(#[parserc(keyword = "color")] I),
    Angle(#[parserc(keyword = "angle")] I),
    Freq(#[parserc(keyword = "freq")] I),
}
