//! Traits for conversions between types

/// Used to do value-to-value conversions while consuming the input value.
pub trait From<T> {
    fn from(value: T) -> Self;
}

/// A value-to-value conversion that consumes the input value. The opposite of [`From`].
pub trait Into<T>: Sized {
    fn into(self) -> T;
}
