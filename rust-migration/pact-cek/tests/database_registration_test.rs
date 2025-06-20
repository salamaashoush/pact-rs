use pact_cek::{database_ops, BuiltinEnv};
use pact_shared_types::SpanInfo;
use pact_ir::CoreBuiltin;

/// Integration tests for database builtin registration
/// Validates that all database operations can be registered and looked up successfully

#[cfg(test)]
mod database_registration_tests {
    use super::*;

    fn dummy_span() -> SpanInfo {
        SpanInfo::empty()
    }

    #[test]
    fn test_database_builtins_register_successfully() {
        let mut builtin_env = BuiltinEnv::new();
        let result = database_ops::register_database_builtins(&mut builtin_env);
        assert!(result.is_ok(), "Database builtins should register without error");
    }

    #[test]
    fn test_all_database_operations_are_registered() {
        let mut builtin_env = BuiltinEnv::new();
        database_ops::register_database_builtins(&mut builtin_env).unwrap();

        // Test all database operations are properly registered
        let all_ops = vec![
            // Table operations
            CoreBuiltin::CoreCreateTable,
            CoreBuiltin::CoreDescribeTable,
            // Write operations
            CoreBuiltin::CoreInsert,
            CoreBuiltin::CoreWrite,
            CoreBuiltin::CoreUpdate,
            // Read operations
            CoreBuiltin::CoreRead,
            CoreBuiltin::CoreSelect,
            CoreBuiltin::CoreSelectWithFields,
            CoreBuiltin::CoreKeys,
            // Advanced operations
            CoreBuiltin::CoreWithDefaultRead,
            CoreBuiltin::CoreWithRead,
            CoreBuiltin::CoreFoldDb,
        ];

        for op in all_ops {
            assert!(builtin_env.lookup(op, dummy_span()).is_ok(),
                    "Operation {:?} should be registered", op);
        }
    }

    #[test]
    fn test_database_operations_have_correct_arity() {
        let mut builtin_env = BuiltinEnv::new();
        database_ops::register_database_builtins(&mut builtin_env).unwrap();

        // Test operations with arity 1
        let unary_ops = vec![
            (CoreBuiltin::CoreCreateTable, "create-table"),
            (CoreBuiltin::CoreDescribeTable, "describe-table"),
            (CoreBuiltin::CoreKeys, "keys"),
        ];

        for (op, name) in unary_ops {
            match builtin_env.lookup(op, dummy_span()) {
                Ok(native_fn) => {
                    assert_eq!(native_fn.arity, 1,
                              "Operation {} ({:?}) should have arity 1", name, op);
                }
                Err(_) => panic!("Operation {} ({:?}) not found", name, op),
            }
        }

        // Test operations with arity 2
        let binary_ops = vec![
            (CoreBuiltin::CoreRead, "read"),
            (CoreBuiltin::CoreSelect, "select"),
        ];

        for (op, name) in binary_ops {
            match builtin_env.lookup(op, dummy_span()) {
                Ok(native_fn) => {
                    assert_eq!(native_fn.arity, 2,
                              "Operation {} ({:?}) should have arity 2", name, op);
                }
                Err(_) => panic!("Operation {} ({:?}) not found", name, op),
            }
        }

        // Test operations with arity 3
        let ternary_ops = vec![
            (CoreBuiltin::CoreInsert, "insert"),
            (CoreBuiltin::CoreWrite, "write"),
            (CoreBuiltin::CoreUpdate, "update"),
            (CoreBuiltin::CoreWithRead, "with-read"),
            (CoreBuiltin::CoreFoldDb, "fold-db"),
            (CoreBuiltin::CoreSelectWithFields, "select"),
        ];

        for (op, name) in ternary_ops {
            match builtin_env.lookup(op, dummy_span()) {
                Ok(native_fn) => {
                    assert_eq!(native_fn.arity, 3,
                              "Operation {} ({:?}) should have arity 3", name, op);
                }
                Err(_) => panic!("Operation {} ({:?}) not found", name, op),
            }
        }

        // Test operations with arity 4
        let quaternary_ops = vec![
            (CoreBuiltin::CoreWithDefaultRead, "with-default-read"),
        ];

        for (op, name) in quaternary_ops {
            match builtin_env.lookup(op, dummy_span()) {
                Ok(native_fn) => {
                    assert_eq!(native_fn.arity, 4,
                              "Operation {} ({:?}) should have arity 4", name, op);
                }
                Err(_) => panic!("Operation {} ({:?}) not found", name, op),
            }
        }
    }

    #[test]
    fn test_database_operations_are_mapped_to_correct_builtins() {
        let mut builtin_env = BuiltinEnv::new();
        database_ops::register_database_builtins(&mut builtin_env).unwrap();

        // Test that operations are properly mapped to their CoreBuiltin variants
        let builtin_mappings = vec![
            CoreBuiltin::CoreCreateTable,
            CoreBuiltin::CoreDescribeTable,
            CoreBuiltin::CoreInsert,
            CoreBuiltin::CoreWrite,
            CoreBuiltin::CoreUpdate,
            CoreBuiltin::CoreRead,
            CoreBuiltin::CoreSelect,
            CoreBuiltin::CoreKeys,
            CoreBuiltin::CoreWithDefaultRead,
            CoreBuiltin::CoreWithRead,
            CoreBuiltin::CoreFoldDb,
            CoreBuiltin::CoreSelectWithFields,
        ];

        for builtin in builtin_mappings {
            match builtin_env.lookup(builtin, dummy_span()) {
                Ok(native_fn) => {
                    assert_eq!(native_fn.builtin, builtin,
                              "NativeFn should have correct builtin variant");
                }
                Err(_) => panic!("Builtin {:?} not found", builtin),
            }
        }
    }

    #[test]
    fn test_select_overload_registration() {
        let mut builtin_env = BuiltinEnv::new();
        database_ops::register_database_builtins(&mut builtin_env).unwrap();

        // Verify both select variants are registered
        // Normal select with arity 2
        match builtin_env.lookup(CoreBuiltin::CoreSelect, dummy_span()) {
            Ok(native_fn) => {
                assert_eq!(native_fn.arity, 2,
                          "CoreSelect should have arity 2");
                assert_eq!(native_fn.builtin, CoreBuiltin::CoreSelect,
                          "NativeFn should have CoreSelect builtin");
            }
            Err(_) => panic!("CoreSelect not found"),
        }

        // Select with fields with arity 3
        match builtin_env.lookup(CoreBuiltin::CoreSelectWithFields, dummy_span()) {
            Ok(native_fn) => {
                assert_eq!(native_fn.arity, 3,
                          "CoreSelectWithFields should have arity 3");
                assert_eq!(native_fn.builtin, CoreBuiltin::CoreSelectWithFields,
                          "NativeFn should have CoreSelectWithFields builtin");
            }
            Err(_) => panic!("CoreSelectWithFields not found"),
        }
    }
}