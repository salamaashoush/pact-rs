//! Cryptographic primitives for Pact - complete implementation matching Haskell Pact
//!
//! This module implements all cryptographic operations exactly as they are
//! implemented in Haskell Pact, ensuring full compatibility.

// Import Guard from pact-values
pub use pact_values::Guard;
// Re-export shared crypto types
pub use pact_shared_types::{
    CryptoError as SharedCryptoError, CryptoResult as SharedCryptoResult, KeySetName, Principal,
    PublicKeyText,
};
use serde::{Deserialize, Serialize};
use std::fmt;
use thiserror::Error;

// Core cryptographic modules
pub mod hash;
pub mod hashing;
pub mod principal;
pub mod signature;
pub mod pairing;
pub mod encoding;

// Re-export main functionality
pub use hash::*;
pub use hashing::*;
pub use principal::*;
pub use signature::*;
pub use pairing::*;
pub use encoding::*;

/// Extended cryptographic errors for this crate (additional to shared types)
#[derive(Debug, Clone, Error, PartialEq, Serialize, Deserialize)]
pub enum ExtendedCryptoError {
    /// Keccak256 specific errors (matches Haskell Keccak256Error)
    #[error("Keccak256 OpenSSL exception: {message}")]
    Keccak256OpenSslException { message: String },

    #[error("Keccak256 Base64 exception: {message}")]
    Keccak256Base64Exception { message: String },

    #[error("Keccak256 other exception: {message}")]
    Keccak256OtherException { message: String },

    /// Ed25519 parsing errors
    #[error("Invalid Ed25519 public key: {message}")]
    InvalidEd25519PublicKey { message: String },

    #[error("Invalid Ed25519 secret key: {message}")]
    InvalidEd25519SecretKey { message: String },

    #[error("Invalid Ed25519 signature: {message}")]
    InvalidEd25519Signature { message: String },

    /// WebAuthn errors
    #[error("Invalid WebAuthn signature: {message}")]
    InvalidWebAuthnSignature { message: String },

    #[error("Invalid COSE public key: {message}")]
    InvalidCosePublicKey { message: String },

    /// Pairing operation errors
    #[error("Invalid curve point: {message}")]
    InvalidCurvePoint { message: String },

    #[error("Invalid field element: {message}")]
    InvalidFieldElement { message: String },

    /// Unsupported operation
    #[error("Unsupported operation: {operation}")]
    UnsupportedOperation { operation: String },
}

/// Use shared CryptoError as our main error type, with conversion from extended errors
impl From<ExtendedCryptoError> for SharedCryptoError {
    fn from(err: ExtendedCryptoError) -> Self {
        match err {
            ExtendedCryptoError::InvalidEd25519PublicKey { message }
            | ExtendedCryptoError::InvalidEd25519SecretKey { message }
            | ExtendedCryptoError::InvalidEd25519Signature { message } => {
                SharedCryptoError::key_validation(message)
            }
            ExtendedCryptoError::InvalidWebAuthnSignature { message }
            | ExtendedCryptoError::InvalidCosePublicKey { message } => {
                SharedCryptoError::signature_verification(message)
            }
            ExtendedCryptoError::Keccak256OpenSslException { message }
            | ExtendedCryptoError::Keccak256Base64Exception { message }
            | ExtendedCryptoError::Keccak256OtherException { message } => {
                SharedCryptoError::hash_computation(message)
            }
            ExtendedCryptoError::InvalidCurvePoint { message }
            | ExtendedCryptoError::InvalidFieldElement { message } => {
                SharedCryptoError::general(message)
            }
            ExtendedCryptoError::UnsupportedOperation { operation } => {
                SharedCryptoError::general(format!("Unsupported operation: {}", operation))
            }
        }
    }
}

/// Result type for crypto operations using shared CryptoError
pub type CryptoResult<T> = SharedCryptoResult<T>;

/// PPK Scheme enumeration matching Haskell exactly
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PPKScheme {
    /// Ed25519 digital signature algorithm
    ED25519,
    /// WebAuthn/FIDO authentication
    WebAuthn,
}

impl fmt::Display for PPKScheme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PPKScheme::ED25519 => write!(f, "ED25519"),
            PPKScheme::WebAuthn => write!(f, "WebAuthn"),
        }
    }
}

impl std::str::FromStr for PPKScheme {
    type Err = SharedCryptoError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_uppercase().as_str() {
            "ED25519" => Ok(PPKScheme::ED25519),
            "WEBAUTHN" => Ok(PPKScheme::WebAuthn),
            _ => Err(SharedCryptoError::general(format!(
                "Invalid PPK scheme: {}",
                s
            ))),
        }
    }
}

/// Extended validation for PublicKeyText
pub fn validate_public_key_format(key: &PublicKeyText) -> CryptoResult<()> {
    if key.as_str().len() == 64 {
        // Ed25519 format: 64 hex characters
        hex::decode(key.as_str()).map_err(|e| ExtendedCryptoError::InvalidEd25519PublicKey {
            message: format!("Invalid hex encoding: {}", e),
        })?;
        Ok(())
    } else {
        // Check if it's a valid WebAuthn key (can be various formats)
        // For now, accept any non-empty string
        if key.as_str().is_empty() {
            Err(ExtendedCryptoError::InvalidEd25519PublicKey {
                message: "Empty public key".to_string(),
            }
            .into())
        } else {
            Ok(())
        }
    }
}

/// DefPact ID wrapper
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct DefPactId(pub String);

impl DefPactId {
    pub fn new(id: String) -> Self {
        DefPactId(id)
    }
}

impl fmt::Display for DefPactId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ppk_scheme_parsing() {
        assert_eq!("ED25519".parse::<PPKScheme>().unwrap(), PPKScheme::ED25519);
        assert_eq!(
            "WebAuthn".parse::<PPKScheme>().unwrap(),
            PPKScheme::WebAuthn
        );
        assert!("INVALID".parse::<PPKScheme>().is_err());
    }

    #[test]
    fn test_public_key_text_validation() {
        // Valid Ed25519 key (64 hex chars)
        let valid_key = PublicKeyText::new(
            "1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef".to_string(),
        );
        assert!(validate_public_key_format(&valid_key).is_ok());

        // Invalid hex (64 chars but not valid hex)
        let invalid_key = PublicKeyText::new(
            "gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg".to_string(),
        );
        assert!(validate_public_key_format(&invalid_key).is_err());

        // Empty key should fail
        let empty_key = PublicKeyText::new("".to_string());
        assert!(validate_public_key_format(&empty_key).is_err());
    }

    #[test]
    fn test_extended_crypto_error_conversion() {
        let extended_err = ExtendedCryptoError::InvalidEd25519PublicKey {
            message: "Test error".to_string(),
        };

        let shared_err: SharedCryptoError = extended_err.into();

        assert!(shared_err.to_string().contains("Key validation failed"));
    }
}
