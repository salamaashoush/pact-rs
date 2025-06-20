//! Encoding utilities for crypto operations
//!
//! This module provides centralized encoding/decoding functionality for
//! base64 and hex encoding used throughout the crypto operations.

use crate::{CryptoResult, SharedCryptoError};
use base64ct::{Base64UrlUnpadded, Base64, Encoding};
use num_bigint::{BigInt, Sign};
use num_traits::Signed;

/// Base64 URL-safe encoding without padding
pub fn base64_encode(data: &[u8]) -> String {
    Base64UrlUnpadded::encode_string(data)
}

/// Base64 URL-safe decoding without padding
pub fn base64_decode(data: &str) -> CryptoResult<Vec<u8>> {
    Base64UrlUnpadded::decode_vec(data)
        .map_err(|e| SharedCryptoError::general(format!("Base64 decode error: {}", e)))
}

/// Base64 standard encoding with padding
pub fn base64_encode_std(data: &[u8]) -> String {
    Base64::encode_string(data)
}

/// Base64 standard decoding with padding
pub fn base64_decode_std(data: &str) -> CryptoResult<Vec<u8>> {
    Base64::decode_vec(data)
        .map_err(|e| SharedCryptoError::general(format!("Base64 decode error: {}", e)))
}

/// Hex encoding
pub fn hex_encode(data: &[u8]) -> String {
    hex::encode(data)
}

/// Hex decoding
pub fn hex_decode(data: &str) -> CryptoResult<Vec<u8>> {
    hex::decode(data)
        .map_err(|e| SharedCryptoError::general(format!("Hex decode error: {}", e)))
}

/// Convert BigInt to bytes with proper padding
pub fn bigint_to_bytes_be(value: &BigInt, size: usize) -> CryptoResult<Vec<u8>> {
    if value.is_negative() {
        return Err(SharedCryptoError::general("Cannot convert negative BigInt to bytes"));
    }
    
    let (_, mut bytes) = value.to_bytes_be();
    
    if bytes.len() > size {
        return Err(SharedCryptoError::general(format!(
            "BigInt too large: {} bytes, max {}", 
            bytes.len(), 
            size
        )));
    }
    
    // Pad to required size
    if bytes.len() < size {
        let mut padded = vec![0u8; size - bytes.len()];
        padded.append(&mut bytes);
        bytes = padded;
    }
    
    Ok(bytes)
}

/// Convert bytes to BigInt (big-endian)
pub fn bytes_to_bigint_be(bytes: &[u8]) -> BigInt {
    BigInt::from_bytes_be(Sign::Plus, bytes)
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_base64_roundtrip() {
        let data = b"Hello, Pact!";
        let encoded = base64_encode(data);
        let decoded = base64_decode(&encoded).unwrap();
        assert_eq!(data.as_slice(), decoded.as_slice());
    }
    
    #[test]
    fn test_hex_roundtrip() {
        let data = b"Hello, Pact!";
        let encoded = hex_encode(data);
        let decoded = hex_decode(&encoded).unwrap();
        assert_eq!(data.as_slice(), decoded.as_slice());
    }
    
    #[test]
    fn test_bigint_to_bytes() {
        use num_bigint::BigInt;
        
        let value = BigInt::from(12345u32);
        let bytes = bigint_to_bytes_be(&value, 32).unwrap();
        assert_eq!(bytes.len(), 32);
        
        // First 28 bytes should be zeros
        assert!(bytes[..28].iter().all(|&b| b == 0));
        
        // Last 4 bytes should contain the value
        let reconstructed = bytes_to_bigint_be(&bytes);
        assert_eq!(reconstructed, value);
    }
}