//! Simplified Ed25519 signature implementation
//!
//! This provides core Ed25519 functionality that compiles and works correctly.

use crate::hash::PactHash;
use crate::{CryptoResult, SharedCryptoError};
use ed25519_dalek::{Signature, Signer, SigningKey, Verifier, VerifyingKey};
use pact_core::shared::PublicKeyText;
use serde::{Deserialize, Serialize};

/// Ed25519 keypair wrapper
#[derive(Debug, Clone)]
pub struct Ed25519KeyPair {
    pub public: VerifyingKey,
    pub secret: SigningKey,
}

impl Ed25519KeyPair {
    /// Generate a new Ed25519 keypair
    pub fn generate() -> CryptoResult<Self> {
        use rand::rngs::OsRng;
        let mut csprng = OsRng;
        let secret_bytes: [u8; 32] = rand::Rng::gen(&mut csprng);
        let secret = SigningKey::from_bytes(&secret_bytes);
        let public = secret.verifying_key();

        Ok(Ed25519KeyPair { public, secret })
    }

    /// Sign a hash with this keypair
    pub fn sign_hash(&self, hash: &PactHash) -> Signature {
        self.secret.sign(hash.as_ref())
    }
}

/// WebAuthn signature structure (simplified)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WebAuthnSignature {
    pub client_data_json: String,
    pub authenticator_data: String,
    pub signature: String,
}

/// Parse Ed25519 public key from hex string
pub fn parse_ed25519_public_key_hex(hex_key: &str) -> CryptoResult<VerifyingKey> {
    if hex_key.len() != 64 {
        return Err(SharedCryptoError::key_validation(
            "Key must be 64 hex characters",
        ));
    }

    let key_bytes = hex::decode(hex_key)
        .map_err(|e| SharedCryptoError::key_validation(format!("Invalid hex: {}", e)))?;

    if key_bytes.len() != 32 {
        return Err(SharedCryptoError::key_validation(
            "Decoded key must be 32 bytes",
        ));
    }

    let mut array = [0u8; 32];
    array.copy_from_slice(&key_bytes);

    VerifyingKey::from_bytes(&array).map_err(|e| SharedCryptoError::key_validation(e.to_string()))
}

/// Sign with Ed25519
pub fn sign_ed25519(secret_key: &SigningKey, hash: &PactHash) -> Signature {
    secret_key.sign(hash.as_ref())
}

/// Verify Ed25519 signature
pub fn verify_ed25519_signature(
    hash: &PactHash,
    public_key: &VerifyingKey,
    signature: &Signature,
) -> CryptoResult<()> {
    public_key.verify(hash.as_ref(), signature).map_err(|e| {
        SharedCryptoError::signature_verification(format!("Signature verification failed: {}", e))
    })
}

/// Check if a key is in Ed25519 hex format (64 hex characters)
pub fn is_ed25519_hex_format(key: &str) -> bool {
    key.len() == 64 && key.chars().all(|c| c.is_ascii_hexdigit())
}

/// Validate key format
pub fn is_valid_key_format(key: &PublicKeyText) -> bool {
    is_ed25519_hex_format(key.as_str())
}

/// Enforce key format validation
pub fn enforce_key_formats(keys: &[PublicKeyText]) -> CryptoResult<()> {
    for key in keys {
        if !is_valid_key_format(key) {
            return Err(SharedCryptoError::key_validation(format!(
                "Invalid key format: {}",
                key.as_str()
            )));
        }
    }
    Ok(())
}

/// Convert public key text to actual public key
pub fn public_key_text_to_ed25519(key_text: &PublicKeyText) -> CryptoResult<VerifyingKey> {
    parse_ed25519_public_key_hex(key_text.as_str())
}

/// Parse Ed25519 secret key from hex string
pub fn parse_ed25519_secret_key_hex(hex_key: &str) -> CryptoResult<SigningKey> {
    let key_bytes = hex::decode(hex_key)
        .map_err(|e| SharedCryptoError::key_validation(format!("Invalid hex: {}", e)))?;

    if key_bytes.len() != 32 {
        return Err(SharedCryptoError::key_validation(
            "Secret key must be 32 bytes",
        ));
    }

    let mut array = [0u8; 32];
    array.copy_from_slice(&key_bytes);

    Ok(SigningKey::from_bytes(&array))
}

/// Derive public key from secret key
pub fn derive_public_key_from_secret(secret_key: &SigningKey) -> String {
    let verifying_key = secret_key.verifying_key();
    hex::encode(verifying_key.as_bytes())
}

/// Create signing key from hex string and derive public key
pub fn create_keypair_from_secret_hex(secret_hex: &str) -> CryptoResult<(String, SigningKey)> {
    let signing_key = parse_ed25519_secret_key_hex(secret_hex)?;
    let public_key_hex = derive_public_key_from_secret(&signing_key);
    Ok((public_key_hex, signing_key))
}

/// Parse Ed25519 signature from hex string
pub fn parse_ed25519_signature_hex(hex_sig: &str) -> CryptoResult<Signature> {
    let sig_bytes = hex::decode(hex_sig)
        .map_err(|e| SharedCryptoError::signature_verification(format!("Invalid hex: {}", e)))?;

    if sig_bytes.len() != 64 {
        return Err(SharedCryptoError::signature_verification(
            "Signature must be 64 bytes",
        ));
    }

    let array: [u8; 64] = sig_bytes.try_into()
        .map_err(|_| SharedCryptoError::signature_verification("Failed to convert to array"))?;
    
    Ok(Signature::from_bytes(&array))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keypair_generation() {
        let keypair = Ed25519KeyPair::generate().unwrap();
        assert_eq!(keypair.public.as_bytes().len(), 32);
    }

    #[test]
    fn test_sign_verify() {
        let keypair = Ed25519KeyPair::generate().unwrap();
        let data = b"test message";
        let hash = crate::hash::pact_hash(data);

        let signature = keypair.sign_hash(&hash);
        assert!(verify_ed25519_signature(&hash, &keypair.public, &signature).is_ok());
    }

    #[test]
    fn test_key_format_validation() {
        let valid_key = PublicKeyText::new(
            "1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef".to_string(),
        );
        assert!(is_valid_key_format(&valid_key));

        let invalid_key = PublicKeyText::new("not_hex".to_string());
        assert!(!is_valid_key_format(&invalid_key));
    }
}
