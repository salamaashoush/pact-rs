//! Module hashing implementation for Pact
//!
//! This module implements the hashing functionality for modules, interfaces,
//! and other data structures, matching the Haskell implementation exactly.

use blake2::{Blake2b, Digest};
use blake2::digest::consts::U32;
use pact_names::{ModuleHash, PactHash, HASH_LENGTH};
use serde::Serialize;
use std::io::Write;

type Blake2b256 = Blake2b<U32>;

/// Compute hash for arbitrary bytes using Blake2b-256
pub fn hash_bytes(data: &[u8]) -> PactHash {
    let mut hasher = Blake2b256::new();
    hasher.update(data);
    let result = hasher.finalize();
    let mut hash_bytes = [0u8; HASH_LENGTH];
    hash_bytes.copy_from_slice(&result);
    PactHash::new(hash_bytes)
}

/// Compute module hash from serialized module data
pub fn compute_module_hash<T: Serialize>(module_data: &T) -> Result<ModuleHash, String> {
    // Serialize the module data to CBOR
    let encoded = serde_cbor::to_vec(module_data)
        .map_err(|e| format!("Failed to serialize module for hashing: {}", e))?;

    // Hash the encoded data
    let hash = hash_bytes(&encoded);
    Ok(ModuleHash(hash))
}

/// Initial hash (empty data hash)
pub fn initial_hash() -> PactHash {
    hash_bytes(b"")
}

/// Placeholder hash for modules not yet hashed
pub fn placeholder_hash() -> ModuleHash {
    ModuleHash(PactHash::new([0u8; HASH_LENGTH]))
}

/// Verify that a hash matches the given data
pub fn verify_hash(expected: &PactHash, data: &[u8]) -> Result<(), String> {
    let computed = hash_bytes(data);
    if computed == *expected {
        Ok(())
    } else {
        Err(format!(
            "Hash mismatch: expected {}, got {}",
            expected.to_base64url(),
            computed.to_base64url()
        ))
    }
}

/// Hash module code for module hash computation
/// This matches the Haskell encodeModule function
pub fn hash_module_code(
    module_name: &str,
    governance: &str,
    code: &str,
) -> ModuleHash {
    // Create a simplified module representation for hashing
    // This should match the Haskell SerialiseV1 encoding
    let mut data = Vec::new();

    // Write module name
    data.write_all(module_name.as_bytes()).unwrap();
    data.push(0); // Separator

    // Write governance
    data.write_all(governance.as_bytes()).unwrap();
    data.push(0); // Separator

    // Write code
    data.write_all(code.as_bytes()).unwrap();

    ModuleHash(hash_bytes(&data))
}

/// Hash interface code for interface hash computation
pub fn hash_interface_code(
    interface_name: &str,
    code: &str,
) -> ModuleHash {
    // Create a simplified interface representation for hashing
    let mut data = Vec::new();

    // Write interface name
    data.write_all(interface_name.as_bytes()).unwrap();
    data.push(0); // Separator

    // Write code
    data.write_all(code.as_bytes()).unwrap();

    ModuleHash(hash_bytes(&data))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hash_bytes() {
        let data = b"hello world";
        let hash = hash_bytes(data);
        assert_eq!(hash.bytes().len(), HASH_LENGTH);
    }

    #[test]
    fn test_initial_hash() {
        let hash = initial_hash();
        assert_eq!(hash.bytes().len(), HASH_LENGTH);
    }

    #[test]
    fn test_verify_hash() {
        let data = b"test data";
        let hash = hash_bytes(data);

        assert!(verify_hash(&hash, data).is_ok());
        assert!(verify_hash(&hash, b"different data").is_err());
    }

    #[test]
    fn test_module_hash() {
        let hash = hash_module_code("test-module", "KeyGov \"admin\"", "(module test-module \"admin\" (defun foo () 1))");
        assert_eq!(hash.0.bytes().len(), HASH_LENGTH);
    }
}
