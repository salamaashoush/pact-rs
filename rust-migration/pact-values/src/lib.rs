//! Runtime value system for Pact
//!
//! This crate provides the runtime value representation, guards, and
//! authorization system used during Pact evaluation and storage.

pub mod collections;
pub mod guards;
pub mod numeric;
pub mod time;
pub mod values;

#[cfg(test)]
mod tests;

// Re-export main types
pub use collections::{Object, PactList};
pub use guards::{enforce_guard, eval_guard, Guard, Keyset, KeySetPredicate};
pub use numeric::{Decimal, Integer};
pub use time::PactTime;
pub use values::{CapToken, Closure, ModRef, PactValue};

// Re-export shared crypto types
pub use pact_shared_types::{CryptoError, KeySetName, Principal, PublicKeyText};

/// Result type for value operations
pub type ValueResult<T> = Result<T, ValueError>;

/// Errors that can occur during value operations
#[derive(Debug, Clone, thiserror::Error, PartialEq)]
pub enum ValueError {
    #[error("Type mismatch: expected {expected}, got {actual}")]
    TypeMismatch { expected: String, actual: String },

    #[error("Invalid field access: {field}")]
    InvalidField { field: String },

    #[error("Index out of bounds: {index}")]
    IndexOutOfBounds { index: usize },

    #[error("Serialization error: {message}")]
    Serialization { message: String },

    #[error("Guard evaluation failed: {message}")]
    GuardEvaluation { message: String },

    #[error("Crypto error: {message}")]
    Crypto { message: String },
}

// Crypto error conversion
impl From<CryptoError> for ValueError {
    fn from(err: CryptoError) -> Self {
        ValueError::Crypto {
            message: err.to_string(),
        }
    }
}
