//! Types for attribute system.

/// Variants of target objects for attributes
@bitflags(u8)
pub enum Target {
    Enum,
    Variant,
    Struct,
    Field,
    Trait,
    Fn,
    FnWithReceiver,
    ExternFn,
}
