//! Comprehensive tests for pact-names
//!
//! Tests cover module names, qualified names, namespaces, and hashing.

use crate::*;

#[cfg(test)]
mod module_name_tests {
    use super::*;

    #[test]
    fn test_simple_module_creation() {
        let module = ModuleName::simple("coin");
        assert_eq!(module.name, "coin");
        assert_eq!(module.namespace, None);
        assert_eq!(module.render(), "coin");
        assert_eq!(module.to_string(), "coin");
    }

    #[test]
    fn test_namespaced_module_creation() {
        let module = ModuleName::namespaced("kadena", "coin");
        assert_eq!(module.name, "coin");
        assert_eq!(module.namespace, Some("kadena".to_string()));
        assert_eq!(module.render(), "kadena.coin");
        assert_eq!(module.to_string(), "kadena.coin");
    }

    #[test]
    fn test_module_name_parsing() {
        // Simple module name
        let simple = ModuleName::parse("fungible-v2");
        assert_eq!(simple.name, "fungible-v2");
        assert_eq!(simple.namespace, None);

        // Namespaced module name
        let namespaced = ModuleName::parse("kadena.coin");
        assert_eq!(namespaced.name, "coin");
        assert_eq!(namespaced.namespace, Some("kadena".to_string()));

        // Multi-level namespace (treated as single namespace)
        let multi = ModuleName::parse("org.kadena.coin");
        assert_eq!(multi.name, "coin");
        assert_eq!(multi.namespace, Some("org.kadena".to_string()));
    }

    #[test]
    fn test_module_name_edge_cases() {
        // Empty string
        let empty = ModuleName::parse("");
        assert_eq!(empty.name, "");
        assert_eq!(empty.namespace, None);

        // Only dot
        let dot = ModuleName::parse(".");
        assert_eq!(dot.name, "");
        assert_eq!(dot.namespace, Some("".to_string()));

        // Multiple dots
        let dots = ModuleName::parse("a.b.c.d");
        assert_eq!(dots.name, "d");
        assert_eq!(dots.namespace, Some("a.b.c".to_string()));

        // Trailing dot
        let trailing = ModuleName::parse("test.");
        assert_eq!(trailing.name, "");
        assert_eq!(trailing.namespace, Some("test".to_string()));
    }

    #[test]
    fn test_module_name_from_string_types() {
        // From &str
        let from_str = ModuleName::simple("test");
        assert_eq!(from_str.name, "test");

        // From String
        let from_string = ModuleName::simple(String::from("test"));
        assert_eq!(from_string.name, "test");

        // Namespaced with different string types
        let ns_mixed = ModuleName::namespaced("ns", String::from("module"));
        assert_eq!(ns_mixed.namespace, Some("ns".to_string()));
        assert_eq!(ns_mixed.name, "module");
    }

    #[test]
    fn test_module_name_equality() {
        let m1 = ModuleName::simple("test");
        let m2 = ModuleName::simple("test");
        let m3 = ModuleName::simple("other");
        let m4 = ModuleName::namespaced("ns", "test");

        assert_eq!(m1, m2);
        assert_ne!(m1, m3);
        assert_ne!(m1, m4);
    }

    #[test]
    fn test_module_name_hash() {
        use std::collections::HashSet;

        let mut set = HashSet::new();
        set.insert(ModuleName::simple("coin"));
        set.insert(ModuleName::namespaced("kadena", "coin"));
        set.insert(ModuleName::simple("coin")); // Duplicate

        assert_eq!(set.len(), 2); // Only 2 unique modules
        assert!(set.contains(&ModuleName::simple("coin")));
        assert!(set.contains(&ModuleName::namespaced("kadena", "coin")));
    }
}

#[cfg(test)]
mod qualified_name_tests {
    use super::*;

    #[test]
    fn test_qualified_name_creation() {
        let module = ModuleName::simple("coin");
        let qname = QualifiedName::new(module.clone(), "transfer".to_string());

        assert_eq!(qname.module, module);
        assert_eq!(qname.name, "transfer");
        assert_eq!(qname.render(), "coin.transfer");
        assert_eq!(qname.to_string(), "coin.transfer");
    }

    #[test]
    fn test_qualified_name_with_namespace() {
        let module = ModuleName::namespaced("kadena", "coin");
        let qname = QualifiedName::new(module, "get-balance".to_string());

        assert_eq!(qname.render(), "kadena.coin.get-balance");
    }

    #[test]
    fn test_qualified_name_from_strs() {
        // Simple module
        let q1 = QualifiedName::from_strs("coin", "transfer");
        assert_eq!(q1.module, ModuleName::simple("coin"));
        assert_eq!(q1.name, "transfer");

        // Namespaced module
        let q2 = QualifiedName::from_strs("kadena.coin", "transfer");
        assert_eq!(q2.module, ModuleName::namespaced("kadena", "coin"));
        assert_eq!(q2.name, "transfer");
    }

    #[test]
    fn test_qualified_name_equality() {
        let q1 = QualifiedName::from_strs("coin", "transfer");
        let q2 = QualifiedName::from_strs("coin", "transfer");
        let q3 = QualifiedName::from_strs("coin", "get-balance");
        let q4 = QualifiedName::from_strs("kadena.coin", "transfer");

        assert_eq!(q1, q2);
        assert_ne!(q1, q3); // Different function
        assert_ne!(q1, q4); // Different module
    }

    #[test]
    fn test_qualified_name_hash() {
        use std::collections::HashMap;

        let mut map = HashMap::new();
        let q1 = QualifiedName::from_strs("coin", "transfer");
        let q2 = QualifiedName::from_strs("coin", "transfer");

        map.insert(q1.clone(), 1);
        map.insert(q2, 2); // Should overwrite

        assert_eq!(map.len(), 1);
        assert_eq!(map.get(&q1), Some(&2));
    }

    #[test]
    fn test_qualified_name_special_chars() {
        let qname = QualifiedName::new(
            ModuleName::simple("my-module"),
            "get-user-info!".to_string(),
        );

        assert_eq!(qname.render(), "my-module.get-user-info!");
    }
}

#[cfg(test)]
mod namespace_name_tests {
    use super::*;

    #[test]
    fn test_namespace_creation() {
        let ns = NamespaceName::new("kadena");
        assert_eq!(ns.0, "kadena");
        assert_eq!(ns.to_string(), "kadena");
    }

    #[test]
    fn test_namespace_from_different_types() {
        let ns1 = NamespaceName::new("test");
        let ns2 = NamespaceName::new(String::from("test"));

        assert_eq!(ns1, ns2);
    }

    #[test]
    fn test_namespace_equality() {
        let ns1 = NamespaceName::new("kadena");
        let ns2 = NamespaceName::new("kadena");
        let ns3 = NamespaceName::new("other");

        assert_eq!(ns1, ns2);
        assert_ne!(ns1, ns3);
    }

    #[test]
    fn test_namespace_special_names() {
        // Empty namespace
        let empty = NamespaceName::new("");
        assert_eq!(empty.0, "");

        // Namespace with special characters
        let special = NamespaceName::new("org.kadena-io");
        assert_eq!(special.0, "org.kadena-io");
    }
}

#[cfg(test)]
mod pact_hash_tests {
    use super::*;

    #[test]
    fn test_pact_hash_creation() {
        // Create a proper 32-byte hash
        let mut bytes = [0u8; HASH_LENGTH];
        bytes[0] = 0xab;
        bytes[1] = 0xcd;
        let hash = PactHash::new(bytes);
        assert_eq!(hash.bytes()[0], 0xab);
        assert_eq!(hash.bytes()[1], 0xcd);
        // Test base64url encoding
        let encoded = hash.to_base64url();
        assert!(!encoded.is_empty());
    }

    #[test]
    fn test_pact_hash_equality() {
        let bytes1 = [1u8; HASH_LENGTH];
        let bytes2 = [1u8; HASH_LENGTH];
        let bytes3 = [2u8; HASH_LENGTH];

        let h1 = PactHash::new(bytes1);
        let h2 = PactHash::new(bytes2);
        let h3 = PactHash::new(bytes3);

        assert_eq!(h1, h2);
        assert_ne!(h1, h3);
    }

    #[test]
    fn test_pact_hash_as_key() {
        use std::collections::HashMap;

        let mut map = HashMap::new();
        let bytes = [42u8; HASH_LENGTH];
        let hash = PactHash::new(bytes);

        map.insert(hash.clone(), "value");
        assert_eq!(map.get(&hash), Some(&"value"));
    }

    #[test]
    fn test_pact_hash_base64_roundtrip() {
        let original_bytes = [99u8; HASH_LENGTH];
        let hash = PactHash::new(original_bytes);

        let encoded = hash.to_base64url();
        let decoded = PactHash::from_base64url(&encoded).unwrap();

        assert_eq!(hash, decoded);
    }

    #[test]
    fn test_pact_hash_hex_roundtrip() {
        let original_bytes = [
            0xDE, 0xAD, 0xBE, 0xEF, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        ];
        let hash = PactHash::new(original_bytes);

        let hex = hash.to_hex();
        let decoded = PactHash::from_hex(&hex).unwrap();

        assert_eq!(hash, decoded);
    }
}

#[cfg(test)]
mod module_hash_tests {
    use super::*;

    #[test]
    fn test_module_hash_creation() {
        let bytes = [123u8; HASH_LENGTH];
        let pact_hash = PactHash::new(bytes);
        let mod_hash = ModuleHash::new(pact_hash.clone());

        assert_eq!(mod_hash.0, pact_hash);
        // ModuleHash displays as base64url encoded
        assert_eq!(mod_hash.to_string(), pact_hash.to_base64url());
    }

    #[test]
    fn test_module_hash_equality() {
        let bytes1 = [1u8; HASH_LENGTH];
        let bytes2 = [1u8; HASH_LENGTH];
        let bytes3 = [2u8; HASH_LENGTH];

        let h1 = ModuleHash::new(PactHash::new(bytes1));
        let h2 = ModuleHash::new(PactHash::new(bytes2));
        let h3 = ModuleHash::new(PactHash::new(bytes3));

        assert_eq!(h1, h2);
        assert_ne!(h1, h3);
    }
}

#[cfg(test)]
mod serialization_tests {
    use super::*;

    #[test]
    fn test_module_name_serialization() {
        let module = ModuleName::namespaced("kadena", "coin");
        let serialized = serde_json::to_string(&module).unwrap();
        let deserialized: ModuleName = serde_json::from_str(&serialized).unwrap();

        assert_eq!(module, deserialized);
    }

    #[test]
    fn test_qualified_name_serialization() {
        let qname = QualifiedName::from_strs("kadena.coin", "transfer");
        let serialized = serde_json::to_string(&qname).unwrap();
        let deserialized: QualifiedName = serde_json::from_str(&serialized).unwrap();

        assert_eq!(qname, deserialized);
    }

    #[test]
    fn test_namespace_name_serialization() {
        let ns = NamespaceName::new("kadena");
        let serialized = serde_json::to_string(&ns).unwrap();
        let deserialized: NamespaceName = serde_json::from_str(&serialized).unwrap();

        assert_eq!(ns, deserialized);
    }

    #[test]
    fn test_pact_hash_serialization() {
        let bytes = [
            0xAB, 0xCD, 0xEF, 0x12, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        ];
        let hash = PactHash::new(bytes);
        let serialized = serde_json::to_string(&hash).unwrap();
        let deserialized: PactHash = serde_json::from_str(&serialized).unwrap();

        assert_eq!(hash, deserialized);
    }

    #[test]
    fn test_module_hash_serialization() {
        let bytes = [123u8; HASH_LENGTH];
        let mod_hash = ModuleHash::new(PactHash::new(bytes));
        let serialized = serde_json::to_string(&mod_hash).unwrap();
        let deserialized: ModuleHash = serde_json::from_str(&serialized).unwrap();

        assert_eq!(mod_hash, deserialized);
    }
}

#[cfg(test)]
mod integration_tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn test_module_registry() {
        // Simulate a module registry
        let mut registry: HashMap<ModuleName, ModuleHash> = HashMap::new();

        // Register some modules
        let hash1 = [1u8; HASH_LENGTH];
        let hash2 = [2u8; HASH_LENGTH];

        registry.insert(
            ModuleName::simple("coin"),
            ModuleHash::new(PactHash::new(hash1)),
        );

        registry.insert(
            ModuleName::namespaced("kadena", "coin"),
            ModuleHash::new(PactHash::new(hash2)),
        );

        // Look up modules
        assert!(registry.contains_key(&ModuleName::simple("coin")));
        assert!(registry.contains_key(&ModuleName::namespaced("kadena", "coin")));
        assert!(!registry.contains_key(&ModuleName::simple("token")));
    }

    #[test]
    fn test_function_registry() {
        // Simulate a function registry
        let mut functions: HashMap<QualifiedName, String> = HashMap::new();

        // Register functions
        functions.insert(
            QualifiedName::from_strs("coin", "transfer"),
            "Transfer coins between accounts".to_string(),
        );

        functions.insert(
            QualifiedName::from_strs("coin", "get-balance"),
            "Get account balance".to_string(),
        );

        // Look up functions
        let transfer = QualifiedName::from_strs("coin", "transfer");
        assert_eq!(
            functions.get(&transfer),
            Some(&"Transfer coins between accounts".to_string())
        );
    }

    #[test]
    fn test_namespace_modules() {
        // Track modules by namespace
        let mut by_namespace: HashMap<Option<String>, Vec<ModuleName>> = HashMap::new();

        let modules = vec![
            ModuleName::simple("coin"),
            ModuleName::simple("token"),
            ModuleName::namespaced("kadena", "coin"),
            ModuleName::namespaced("kadena", "fungible"),
            ModuleName::namespaced("free", "util"),
        ];

        for module in modules {
            by_namespace
                .entry(module.namespace.clone())
                .or_insert_with(Vec::new)
                .push(module);
        }

        // Check grouping
        assert_eq!(by_namespace.get(&None).unwrap().len(), 2);
        assert_eq!(
            by_namespace.get(&Some("kadena".to_string())).unwrap().len(),
            2
        );
        assert_eq!(
            by_namespace.get(&Some("free".to_string())).unwrap().len(),
            1
        );
    }
}

#[cfg(test)]
mod display_tests {
    use super::*;

    #[test]
    fn test_all_display_implementations() {
        // ModuleName
        let module = ModuleName::namespaced("ns", "mod");
        assert_eq!(format!("{}", module), "ns.mod");

        // QualifiedName
        let qname = QualifiedName::new(module, "func".to_string());
        assert_eq!(format!("{}", qname), "ns.mod.func");

        // NamespaceName
        let ns = NamespaceName::new("kadena");
        assert_eq!(format!("{}", ns), "kadena");

        // PactHash
        let bytes = [
            0xAB, 0xC1, 0x23, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
            0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
        ];
        let hash = PactHash::new(bytes);
        let base64_display = format!("{}", hash);
        assert!(!base64_display.is_empty());

        // ModuleHash
        let mod_hash = ModuleHash::new(hash.clone());
        assert_eq!(format!("{}", mod_hash), base64_display);
    }
}
