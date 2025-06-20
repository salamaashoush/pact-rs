//! Shared types between Pact layers
//!
//! This crate provides fundamental types that are shared between multiple layers
//! of the Pact architecture to avoid circular dependencies. These types are used
//! by both the crypto foundation layer and the values type system layer.

use serde::{Deserialize, Serialize};
use std::fmt;

/// Principal type for cryptographic authorization
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Principal {
    /// Key principal (public key)
    K(String),
    /// Role principal (named keyset)
    R(String),
}

impl Principal {
    /// Create a key principal
    pub fn key(key: String) -> Self {
        Principal::K(key)
    }

    /// Create a role principal
    pub fn role(role: String) -> Self {
        Principal::R(role)
    }

    /// Get the underlying string value
    pub fn as_str(&self) -> &str {
        match self {
            Principal::K(k) => k,
            Principal::R(r) => r,
        }
    }

    /// Check if this is a key principal
    pub fn is_key(&self) -> bool {
        matches!(self, Principal::K(_))
    }

    /// Check if this is a role principal
    pub fn is_role(&self) -> bool {
        matches!(self, Principal::R(_))
    }
}

impl fmt::Display for Principal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Principal::K(k) => write!(f, "k:{}", k),
            Principal::R(r) => write!(f, "r:{}", r),
        }
    }
}

/// Wrapper for validated public key text
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PublicKeyText(pub String);

impl PublicKeyText {
    /// Create a new public key text wrapper
    pub fn new(key: String) -> Self {
        PublicKeyText(key)
    }

    /// Get the underlying key string
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Into the underlying string
    pub fn into_string(self) -> String {
        self.0
    }
}

impl fmt::Display for PublicKeyText {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for PublicKeyText {
    fn from(s: String) -> Self {
        PublicKeyText(s)
    }
}

impl From<&str> for PublicKeyText {
    fn from(s: &str) -> Self {
        PublicKeyText(s.to_string())
    }
}

/// Wrapper for keyset names
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct KeySetName(pub String);

impl KeySetName {
    /// Create a new keyset name wrapper
    pub fn new(name: String) -> Self {
        KeySetName(name)
    }

    /// Get the underlying name string
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Into the underlying string
    pub fn into_string(self) -> String {
        self.0
    }
}

impl fmt::Display for KeySetName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for KeySetName {
    fn from(s: String) -> Self {
        KeySetName(s)
    }
}

impl From<&str> for KeySetName {
    fn from(s: &str) -> Self {
        KeySetName(s.to_string())
    }
}

/// Chain ID for blockchain identification
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ChainId(String);

impl ChainId {
    /// Create a new chain ID
    pub fn new(id: String) -> Self {
        ChainId(id)
    }

    /// Get the chain ID as a string
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for ChainId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Cryptographic errors that can occur in validation
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum CryptoError {
    /// Invalid key format
    #[error("Invalid key format: {message}")]
    InvalidKeyFormat {
        /// Error message describing the invalid format
        message: String,
    },

    /// Key validation failed
    #[error("Key validation failed: {message}")]
    KeyValidation {
        /// Error message describing the validation failure
        message: String,
    },

    /// Principal creation failed
    #[error("Principal creation failed: {message}")]
    PrincipalCreation {
        /// Error message describing the creation failure
        message: String,
    },

    /// Signature verification failed
    #[error("Signature verification failed: {message}")]
    SignatureVerification {
        /// Error message describing the verification failure
        message: String,
    },

    /// Hash computation failed
    #[error("Hash computation failed: {message}")]
    HashComputation {
        /// Error message describing the computation failure
        message: String,
    },

    /// General cryptographic error
    #[error("Cryptographic error: {message}")]
    General {
        /// Error message
        message: String,
    },
}

impl CryptoError {
    /// Create an invalid key format error
    pub fn invalid_key_format<S: Into<String>>(msg: S) -> Self {
        CryptoError::InvalidKeyFormat {
            message: msg.into(),
        }
    }

    /// Create a key validation error
    pub fn key_validation<S: Into<String>>(msg: S) -> Self {
        CryptoError::KeyValidation {
            message: msg.into(),
        }
    }

    /// Create a principal creation error
    pub fn principal_creation<S: Into<String>>(msg: S) -> Self {
        CryptoError::PrincipalCreation {
            message: msg.into(),
        }
    }

    /// Create a signature verification error
    pub fn signature_verification<S: Into<String>>(msg: S) -> Self {
        CryptoError::SignatureVerification {
            message: msg.into(),
        }
    }

    /// Create a hash computation error
    pub fn hash_computation<S: Into<String>>(msg: S) -> Self {
        CryptoError::HashComputation {
            message: msg.into(),
        }
    }

    /// Create a general cryptographic error
    pub fn general<S: Into<String>>(msg: S) -> Self {
        CryptoError::General {
            message: msg.into(),
        }
    }
}

/// Result type for cryptographic operations
pub type CryptoResult<T> = Result<T, CryptoError>;

/// Unique identifier for a defpact execution instance
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct DefPactId(pub String);

impl DefPactId {
    /// Create a new DefPactId
    pub fn new<S: Into<String>>(id: S) -> Self {
        DefPactId(id.into())
    }
    
    /// Get the string representation
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for DefPactId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Source position information shared across parser and IR
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SpanInfo {
    /// Start position in source
    pub start: usize,
    /// End position in source
    pub end: usize,
}

impl SpanInfo {
    /// Create a new span info
    pub fn new(start: usize, end: usize) -> Self {
        SpanInfo { start, end }
    }
    
    /// Combine two spans to create a larger span
    pub fn combine(start: SpanInfo, end: SpanInfo) -> Self {
        SpanInfo {
            start: start.start,
            end: end.end,
        }
    }
    
    /// Create an empty/null span (for generated code)
    pub fn empty() -> Self {
        SpanInfo { start: 0, end: 0 }
    }
    
    /// Create an unknown span (for generated/synthetic code)
    pub fn unknown() -> Self {
        SpanInfo { start: 0, end: 0 }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_principal_creation() {
        let key_principal = Principal::key("abc123".to_string());
        assert!(key_principal.is_key());
        assert!(!key_principal.is_role());
        assert_eq!(key_principal.as_str(), "abc123");

        let role_principal = Principal::role("admin".to_string());
        assert!(role_principal.is_role());
        assert!(!role_principal.is_key());
        assert_eq!(role_principal.as_str(), "admin");
    }

    #[test]
    fn test_principal_display() {
        let key_principal = Principal::K("abc123".to_string());
        assert_eq!(format!("{}", key_principal), "k:abc123");

        let role_principal = Principal::R("admin".to_string());
        assert_eq!(format!("{}", role_principal), "r:admin");
    }

    #[test]
    fn test_public_key_text() {
        let key = PublicKeyText::new("abc123".to_string());
        assert_eq!(key.as_str(), "abc123");
        assert_eq!(format!("{}", key), "abc123");

        let key_from_str: PublicKeyText = "def456".into();
        assert_eq!(key_from_str.as_str(), "def456");
    }

    #[test]
    fn test_keyset_name() {
        let name = KeySetName::new("admin-keys".to_string());
        assert_eq!(name.as_str(), "admin-keys");
        assert_eq!(format!("{}", name), "admin-keys");

        let name_from_str: KeySetName = "user-keys".into();
        assert_eq!(name_from_str.as_str(), "user-keys");
    }

    #[test]
    fn test_crypto_errors() {
        let err = CryptoError::invalid_key_format("bad key");
        assert_eq!(err.to_string(), "Invalid key format: bad key");

        let err = CryptoError::key_validation("validation failed");
        assert_eq!(err.to_string(), "Key validation failed: validation failed");

        let err = CryptoError::principal_creation("creation failed");
        assert_eq!(
            err.to_string(),
            "Principal creation failed: creation failed"
        );
    }

    #[test]
    fn test_serde() {
        let principal = Principal::K("test-key".to_string());
        let json = serde_json::to_string(&principal).unwrap();
        let deserialized: Principal = serde_json::from_str(&json).unwrap();
        assert_eq!(principal, deserialized);

        let key = PublicKeyText::new("test-key".to_string());
        let json = serde_json::to_string(&key).unwrap();
        let deserialized: PublicKeyText = serde_json::from_str(&json).unwrap();
        assert_eq!(key, deserialized);

        let name = KeySetName::new("test-name".to_string());
        let json = serde_json::to_string(&name).unwrap();
        let deserialized: KeySetName = serde_json::from_str(&json).unwrap();
        assert_eq!(name, deserialized);
    }
}
