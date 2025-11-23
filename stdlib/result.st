//! Error handling with the Result type.

/// Result is a type that represents either success (Ok) or failure (Err).
pub enum Result<T, E> {
   Ok(T),
   Err(E),
}
