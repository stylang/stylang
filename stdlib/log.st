//! Logging utility for recording diagnostic information for
//! the `stylang` application.


/// Logging level.
@hashcode
pub enum Level {
    /// The “trace” level.
    ///
    /// Designates very low priority, often extremely verbose, information.
    Trace,
    /// The “debug” level.
    ///
    /// Designates lower priority information.
    Debug,
    /// The “info” level.
    ///
    /// Designates useful information.
    Info,
    /// The “warn” level.
    ///
    /// Designates hazardous situations.
    Warn,
    /// The “error” level.
    ///
    /// Designates very serious errors.
    Error,
}

/// Logs a message at given level.
@runtime
pub extern log(level: Level, message: string);

/// Logs a message at `trace` level.
pub trace(message: string) {
    log(Level::Trace, message)
}

/// Logs a message at `debug` level.
pub debug(message: string) {
    log(Level::Debug, message)
}

/// Logs a message at `info` level.
pub info(message: string) {
    log(Level::Info, message)
}

/// Logs a message at `warn` level.
pub warn(message: string) {
    log(Level::Warn, message)
}

/// Logs a message at `error` level.
pub error(message: string) {
    log(Level::Error, message)
}
