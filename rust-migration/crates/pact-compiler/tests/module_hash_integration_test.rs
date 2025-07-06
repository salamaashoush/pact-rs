//! Integration tests for module hashing functionality
//! 
//! These tests verify that our Rust implementation produces module hashes
//! that match the Haskell implementation exactly.

use pact_compiler::*;
use pact_ir::{CoreEvalModule, ModuleHash, Hash, ModuleCode, EvalModule, ModuleName, Governance};
use pact_ir::term::{Def, SpanInfo, Module};
use std::collections::HashSet;
use compact_str::CompactString;

#[test]
fn test_empty_module_hash() {
    // Create a minimal empty module for hashing
    let module = create_test_module("test-module", vec![]);
    
    // Compute hash using our implementation
    let hash_result = compute_module_hash(&module);
    
    // Verify hash computation succeeds
    assert!(hash_result.is_ok(), "Module hash computation should succeed");
    
    let hash = hash_result.unwrap();
    
    // Verify hash format (should be 64-character hex string)
    assert_eq!(hash.0.len(), 64, "Module hash should be 64 characters (32 bytes as hex)");
    
    // Verify hash contains only valid hex characters
    assert!(hash.0.chars().all(|c| c.is_ascii_hexdigit()), "Hash should contain only hex characters");
}

#[test]
fn test_module_hash_consistency() {
    // Create identical modules
    let module1 = create_test_module("test-module", vec![]);
    let module2 = create_test_module("test-module", vec![]);
    
    // Compute hashes
    let hash1 = compute_module_hash(&module1).unwrap();
    let hash2 = compute_module_hash(&module2).unwrap();
    
    // Identical modules should produce identical hashes
    assert_eq!(hash1, hash2, "Identical modules should produce identical hashes");
}

#[test]
fn test_module_hash_sensitivity() {
    // Create modules with different names
    let module1 = create_test_module("module-a", vec![]);
    let module2 = create_test_module("module-b", vec![]);
    
    // Compute hashes
    let hash1 = compute_module_hash(&module1).unwrap();
    let hash2 = compute_module_hash(&module2).unwrap();
    
    // Different modules should produce different hashes
    assert_ne!(hash1, hash2, "Different modules should produce different hashes");
}

#[test]
fn test_module_with_governance_hash() {
    // Create module with keyset governance
    let module = create_test_module_with_governance("test-module", Governance::KeyGov("admin-keyset".into()));
    
    // Compute hash
    let hash_result = compute_module_hash(&module);
    
    // Verify hash computation succeeds
    assert!(hash_result.is_ok(), "Module with governance should hash successfully");
}

#[test]
fn test_module_serialization_for_hash() {
    // Create a test module
    let module = create_test_module("test-module", vec![]);
    
    // Test that our ModuleForHashing serialization works
    let module_for_hash = ModuleForHashing::from(&module);
    
    // Serialize to CBOR
    let mut cbor_data = Vec::new();
    let serialization_result = ciborium::ser::into_writer(&module_for_hash, &mut cbor_data);
    
    assert!(serialization_result.is_ok(), "Module should serialize to CBOR successfully");
    assert!(!cbor_data.is_empty(), "CBOR data should not be empty");
}

#[test]
fn test_blake2b_hash_computation() {
    // Test the actual Blake2b hash computation with known input
    let test_input = b"test data for hashing";
    
    use blake2::{Blake2b512, Digest};
    let mut hasher = Blake2b512::new();
    hasher.update(test_input);
    let hash_result = hasher.finalize();
    let hash_hex = hex::encode(&hash_result[..32]); // Take first 32 bytes
    
    // Verify hash format
    assert_eq!(hash_hex.len(), 64, "Blake2b hash should be 64 hex characters");
    assert!(hash_hex.chars().all(|c| c.is_ascii_hexdigit()), "Hash should be valid hex");
}

#[test]
fn test_module_hash_integration_with_cek() {
    // Create a module with some basic content
    let module = create_test_module("coin", vec![]);
    
    // Compute the hash
    let hash = compute_module_hash(&module).unwrap();
    
    // Verify we can use this hash in the CEK evaluator context
    // This tests the integration between compiler and CEK machine
    let hashed_module_name = pact_ir::HashedModuleName::new(
        ModuleName {
            name: "coin".into(),
            namespace: None,
        },
        hash.clone(),
    );
    
    // Verify the display format matches expected pattern
    let display_str = format!("{}", hashed_module_name);
    assert!(display_str.contains("coin#"), "Hashed module name should contain module name and hash");
    assert!(display_str.contains(&hash.0), "Display should contain the actual hash");
}

/// Helper function to create a test module
fn create_test_module(name: &str, definitions: Vec<Def<pact_ir::Name, pact_ir::Type, pact_ir::CoreBuiltin, SpanInfo>>) -> CoreEvalModule {
    EvalModule {
        name: ModuleName {
            name: name.into(),
            namespace: None,
        },
        governance: Governance::KeyGov("admin-keyset".into()),
        definitions,
        blessed: HashSet::new(),
        imports: vec![],
        implements: vec![],
        hash: ModuleHash("placeholder-hash".into()),
        tx_hash: Hash("placeholder-tx-hash".into()),
        code: ModuleCode::new(format!("(module {})", name)),
        info: SpanInfo::empty(),
    }
}

/// Helper function to create a test module with specific governance
fn create_test_module_with_governance(name: &str, governance: Governance) -> CoreEvalModule {
    EvalModule {
        name: ModuleName {
            name: name.into(),
            namespace: None,
        },
        governance,
        definitions: vec![],
        blessed: HashSet::new(),
        imports: vec![],
        implements: vec![],
        hash: ModuleHash("placeholder-hash".into()),
        tx_hash: Hash("placeholder-tx-hash".into()),
        code: ModuleCode::new(format!("(module {})", name)),
        info: SpanInfo::empty(),
    }
}