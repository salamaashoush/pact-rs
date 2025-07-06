use pact_cek::*;
use pact_shared_types::SpanInfo;
use pact_ir::CoreBuiltin;

/// Simple integration test for comparison builtin registration
/// Validates that all comparison operations can be registered and looked up successfully

#[cfg(test)]
mod comparison_registration_tests {
    use super::*;

    fn dummy_span() -> SpanInfo {
        SpanInfo::empty()
    }

    #[test]
    fn test_comparison_builtins_register_successfully() {
        let mut builtin_env = BuiltinEnv::new();
        let result = pact_cek::comparison::register_comparison_builtins(&mut builtin_env);
        assert!(result.is_ok(), "Comparison builtins should register without error");
    }

    #[test]
    fn test_all_comparison_operations_are_registered() {
        let mut builtin_env = BuiltinEnv::new();
        pact_cek::comparison::register_comparison_builtins(&mut builtin_env).unwrap();
        
        // Test all comparison operations are properly registered
        let all_ops = vec![
            CoreBuiltin::CoreEq,
            CoreBuiltin::CoreNeq,
            CoreBuiltin::CoreGT,
            CoreBuiltin::CoreGEQ,
            CoreBuiltin::CoreLT,
            CoreBuiltin::CoreLEQ,
        ];
        
        for op in all_ops {
            assert!(builtin_env.lookup(op, dummy_span()).is_ok(),
                    "Operation {:?} should be registered", op);
        }
    }

    #[test]
    fn test_comparison_operations_have_correct_arity() {
        let mut builtin_env = BuiltinEnv::new();
        pact_cek::comparison::register_comparison_builtins(&mut builtin_env).unwrap();
        
        // Test operations have correct arity (all comparison operations take 2 arguments)
        let arity_tests = vec![
            (CoreBuiltin::CoreEq, 2),
            (CoreBuiltin::CoreNeq, 2),
            (CoreBuiltin::CoreGT, 2),
            (CoreBuiltin::CoreGEQ, 2),
            (CoreBuiltin::CoreLT, 2),
            (CoreBuiltin::CoreLEQ, 2),
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
    fn test_comparison_operations_return_correct_builtin_enum() {
        let mut builtin_env = BuiltinEnv::new();
        pact_cek::comparison::register_comparison_builtins(&mut builtin_env).unwrap();
        
        // Test that lookup returns the correct builtin enum
        let test_ops = vec![
            CoreBuiltin::CoreEq,
            CoreBuiltin::CoreNeq,
            CoreBuiltin::CoreGT,
            CoreBuiltin::CoreGEQ,
            CoreBuiltin::CoreLT,
            CoreBuiltin::CoreLEQ,
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
    fn test_lookup_nonexistent_comparison_builtin_returns_error() {
        let builtin_env = BuiltinEnv::new(); // Empty environment
        
        // Should return an error for unregistered builtins
        let result = builtin_env.lookup(CoreBuiltin::CoreEq, dummy_span());
        assert!(result.is_err(), "Lookup of unregistered builtin should return error");
    }

    #[test]
    fn test_comparison_builtins_integration() {
        // Comprehensive integration test that all comparison operations are present
        // and correctly integrated into the builtin environment
        let mut builtin_env = BuiltinEnv::new();
        
        // Registration should succeed
        let registration_result = pact_cek::comparison::register_comparison_builtins(&mut builtin_env);
        assert!(registration_result.is_ok(), "Comparison builtin registration should succeed");
        
        // All 6 comparison operations should be available
        let operation_count = vec![
            CoreBuiltin::CoreEq,
            CoreBuiltin::CoreNeq,
            CoreBuiltin::CoreGT,
            CoreBuiltin::CoreGEQ,
            CoreBuiltin::CoreLT,
            CoreBuiltin::CoreLEQ,
        ];
        
        // Verify each operation is correctly registered and can be looked up
        for (index, op) in operation_count.iter().enumerate() {
            match builtin_env.lookup(*op, dummy_span()) {
                Ok(_) => {
                    // Successfully found - this is what we expect
                }
                Err(e) => {
                    panic!("Failed to lookup comparison operation #{} {:?}: {:?}", 
                           index + 1, op, e);
                }
            }
        }
        
        // All 6 operations should be registered
        assert_eq!(operation_count.len(), 6, "Should have 6 comparison operations");
    }

    #[test]
    fn test_equality_vs_ordering_operations() {
        let mut builtin_env = BuiltinEnv::new();
        pact_cek::comparison::register_comparison_builtins(&mut builtin_env).unwrap();
        
        // Equality operations (work with any PactValue)
        let equality_ops = vec![CoreBuiltin::CoreEq, CoreBuiltin::CoreNeq];
        
        // Ordering operations (work with literals and time only)
        let ordering_ops = vec![
            CoreBuiltin::CoreGT, 
            CoreBuiltin::CoreGEQ, 
            CoreBuiltin::CoreLT, 
            CoreBuiltin::CoreLEQ
        ];
        
        // All should be registered and have arity 2
        for op in equality_ops.iter().chain(ordering_ops.iter()) {
            let result = builtin_env.lookup(*op, dummy_span());
            assert!(result.is_ok(), "Operation {:?} should be registered", op);
            
            if let Ok(native_fn) = result {
                assert_eq!(native_fn.arity, 2, "All comparison operations should have arity 2");
            }
        }
        
        // Total count should be 6
        assert_eq!(equality_ops.len() + ordering_ops.len(), 6);
    }
}