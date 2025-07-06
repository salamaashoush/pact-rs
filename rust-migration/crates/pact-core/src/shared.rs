//! Shared types between Pact layers
//!
//! This module provides fundamental types that are shared between multiple layers
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
/// Matches Haskell implementation with line/column tracking
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SpanInfo {
    /// Start line number (1-based)
    pub start_line: usize,
    /// Start column number (0-based)
    pub start_column: usize,
    /// End line number (1-based)
    pub end_line: usize,
    /// End column number (0-based)
    pub end_column: usize,
}

impl SpanInfo {
    /// Create a new span info with line/column information
    pub fn new(start_line: usize, start_column: usize, end_line: usize, end_column: usize) -> Self {
        SpanInfo { 
            start_line, 
            start_column, 
            end_line, 
            end_column 
        }
    }
    
    /// Create from byte offsets (compatibility method)
    /// Creates a span with line 1 and uses byte offsets as columns
    /// This is temporary until all code is migrated to proper line/column tracking
    pub fn from_offsets(start: usize, end: usize) -> Self {
        SpanInfo {
            start_line: 1,
            start_column: start,
            end_line: 1,
            end_column: end,
        }
    }
    
    /// Combine two spans to create a larger span
    pub fn combine(start: SpanInfo, end: SpanInfo) -> Self {
        SpanInfo {
            start_line: start.start_line,
            start_column: start.start_column,
            end_line: end.end_line,
            end_column: end.end_column,
        }
    }
    
    /// Create an empty/null span (for generated code)
    pub fn empty() -> Self {
        SpanInfo { 
            start_line: 0, 
            start_column: 0, 
            end_line: 0, 
            end_column: 0 
        }
    }
    
    /// Create an unknown span (for generated/synthetic code)
    pub fn unknown() -> Self {
        SpanInfo { 
            start_line: 0, 
            start_column: 0, 
            end_line: 0, 
            end_column: 0 
        }
    }
    
    /// Create span info from byte offsets by converting to line/column
    /// This is a compatibility helper for migrating from byte offsets
    pub fn from_byte_offsets(source: &str, start: usize, end: usize) -> Self {
        let (start_line, start_col) = byte_offset_to_line_col(source, start);
        let (end_line, end_col) = byte_offset_to_line_col(source, end);
        
        SpanInfo {
            start_line,
            start_column: start_col,
            end_line,
            end_column: end_col,
        }
    }
}

/// Convert byte offset to line and column numbers
fn byte_offset_to_line_col(source: &str, offset: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 0;
    let mut byte_count = 0;
    
    for ch in source.chars() {
        if byte_count >= offset {
            break;
        }
        
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
        
        byte_count += ch.len_utf8();
    }
    
    (line, col)
}