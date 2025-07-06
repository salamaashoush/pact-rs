use pact_cek::*;
use pact_shared_types::SpanInfo;
use pact_ir::CoreBuiltin;

/// Simple integration test for boolean builtin registration
/// Validates that all boolean operations can be registered and looked up successfully

#[cfg(test)]
mod boolean_registration_tests {
    use super::*;

    fn dummy_span() -> SpanInfo {
        SpanInfo::empty()
    }

    #[test]
    fn test_boolean_builtins_register_successfully() {
        let mut builtin_env = BuiltinEnv::new();
        let result = pact_cek::boolean_ops::register_boolean_builtins(&mut builtin_env);
        assert!(result.is_ok(), "Boolean builtins should register without error");
    }

    #[test]
    fn test_all_boolean_operations_are_registered() {
        let mut builtin_env = BuiltinEnv::new();
        pact_cek::boolean_ops::register_boolean_builtins(&mut builtin_env).unwrap();
        
        // Test all boolean operations are properly registered
        let all_ops = vec![
            CoreBuiltin::CoreNot,
            CoreBuiltin::CoreNotQ,
            CoreBuiltin::CoreAndQ,
            CoreBuiltin::CoreOrQ,
        ];
        
        for op in all_ops {
            assert!(builtin_env.lookup(op, dummy_span()).is_ok(),
                    "Operation {:?} should be registered", op);
        }
    }

    #[test]
    fn test_boolean_operations_have_correct_arity() {
        let mut builtin_env = BuiltinEnv::new();
        pact_cek::boolean_ops::register_boolean_builtins(&mut builtin_env).unwrap();
        
        // Test operations have correct arity
        let arity_tests = vec![
            (CoreBuiltin::CoreNot, 1),
            (CoreBuiltin::CoreNotQ, 2),
            (CoreBuiltin::CoreAndQ, 3),
            (CoreBuiltin::CoreOrQ, 3),
        ];
        
        for (op, expected_arity) in arity_tests {
            match builtin_env.lookup(op, dummy_span()) {
                Ok(native_fn) => {
                    assert_eq!(native_fn.arity, expected_arity,
                              "Operation {:?} should have arity {}", op, expected_arity);
                }
                Err(_) => panic!("Operation {:?} not found", op),
            }
        }
    }

    #[test]
    fn test_boolean_operations_return_correct_builtin_enum() {
        let mut builtin_env = BuiltinEnv::new();
        pact_cek::boolean_ops::register_boolean_builtins(&mut builtin_env).unwrap();
        
        // Test that lookup returns the correct builtin enum
        let test_ops = vec![
            CoreBuiltin::CoreNot,
            CoreBuiltin::CoreNotQ,
            CoreBuiltin::CoreAndQ,
            CoreBuiltin::CoreOrQ,
        ];
        
        for op in test_ops {
            match builtin_env.lookup(op, dummy_span()) {
                Ok(native_fn) => {
                    assert_eq!(native_fn.builtin, op,
                              "Lookup should return the correct builtin enum for {:?}", op);
                }
                Err(_) => panic!("Operation {:?} not found", op),
            }
        }
    }

    #[test]
    fn test_lookup_nonexistent_boolean_builtin_returns_error() {
        let builtin_env = BuiltinEnv::new(); // Empty environment
        
        // Should return an error for unregistered builtins
        let result = builtin_env.lookup(CoreBuiltin::CoreNot, dummy_span());
        assert!(result.is_err(), "Lookup of unregistered builtin should return error");
    }

    #[test]
    fn test_boolean_builtins_integration() {
        // Comprehensive integration test that all boolean operations are present
        // and correctly integrated into the builtin environment
        let mut builtin_env = BuiltinEnv::new();
        
        // Registration should succeed
        let registration_result = pact_cek::boolean_ops::register_boolean_builtins(&mut builtin_env);
        assert!(registration_result.is_ok(), "Boolean builtin registration should succeed");
        
        // All 4 boolean operations should be available
        let operation_count = vec![
            CoreBuiltin::CoreNot,
            CoreBuiltin::CoreNotQ,
            CoreBuiltin::CoreAndQ,
            CoreBuiltin::CoreOrQ,
        ];
        
        // Verify each operation is correctly registered and can be looked up
        for (index, op) in operation_count.iter().enumerate() {
            match builtin_env.lookup(*op, dummy_span()) {
                Ok(_) => {
                    // Successfully found - this is what we expect
                }
                Err(e) => {
                    panic!("Failed to lookup boolean operation #{} {:?}: {:?}", 
                           index + 1, op, e);
                }
            }
        }
        
        // All 4 operations should be registered
        assert_eq!(operation_count.len(), 4, "Should have 4 boolean operations");
    }
}