//! Types used by unit syntax.

use crate::attribute::Target::*;


/// Convert value by `unit` suffix.
pub trait Unit<const static NAME: string, T> {
    fn from(value: T) -> Self;
}

/// The built-in attribute `unit` is used to derive [`Unit`] for struct/enum.
@compile_builtin
@attribute(Variant | Struct)
pub struct Unit(pub string);
