//! Types used by unit syntax.

use crate::attribute::Target::*;


/// Convert value by `unit` suffix.
@marker
pub trait Unit<const static NAME: string, T> {
    fn from(value: T) -> Self;
}

/// The built-in attribute `unit` is used to derive [`Unit`] for struct/enum.
@builtin_attribute(Variant | Struct)
pub struct Unit(pub string);
