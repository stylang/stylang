//! Types used by unit syntax.

use crate::attribute::Target::*;

/// The built-in attribute `unit` is used to rename struct/variant identifiers
/// used as unit markers.
@builtin_attribute(Variant | Struct)
pub struct Unit(pub string);
