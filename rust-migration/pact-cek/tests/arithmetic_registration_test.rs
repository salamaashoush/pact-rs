use pact_cek::*;
use pact_shared_types::SpanInfo;
use pact_ir::CoreBuiltin;

/// Simple integration test for arithmetic builtin registration
/// Validates that all arithmetic operations can be registered and looked up successfully

#[cfg(test)]
mod arithmetic_registration_tests {
    use super::*;

    fn dummy_span() -> SpanInfo {
        SpanInfo::empty()
    }

    #[test]
    fn test_arithmetic_builtins_register_successfully() {
        let mut builtin_env = BuiltinEnv::new();
        let result = arithmetic::register_arithmetic_builtins(&mut builtin_env);
        assert!(result.is_ok(), "Arithmetic builtins should register without error");
    }

    #[test]
    fn test_all_arithmetic_operations_are_registered() {
        let mut builtin_env = BuiltinEnv::new();
        arithmetic::register_arithmetic_builtins(&mut builtin_env).unwrap();

        // Test all arithmetic operations are properly registered
        let all_ops = vec![
            // Basic arithmetic
            CoreBuiltin::CoreAdd,
            CoreBuiltin::CoreSub,
            CoreBuiltin::CoreMultiply,
            CoreBuiltin::CoreDivide,
            CoreBuiltin::CoreNegate,
            CoreBuiltin::CoreAbs,
            CoreBuiltin::CorePow,
            CoreBuiltin::CoreMod,
            // Rounding
            CoreBuiltin::CoreRound,
            CoreBuiltin::CoreCeiling,
            CoreBuiltin::CoreFloor,
            // Transcendental
            CoreBuiltin::CoreExp,
            CoreBuiltin::CoreLn,
            CoreBuiltin::CoreSqrt,
            CoreBuiltin::CoreLogBase,
            // Bitwise
            CoreBuiltin::CoreBitwiseAnd,
            CoreBuiltin::CoreBitwiseOr,
            CoreBuiltin::CoreBitwiseXor,
            CoreBuiltin::CoreBitwiseFlip,
            CoreBuiltin::CoreBitShift,
        ];

        for op in all_ops {
            assert!(builtin_env.lookup(op, dummy_span()).is_ok(),
                    "Operation {:?} should be registered", op);
        }
    }

    #[test]
    fn test_arithmetic_operations_have_correct_arity() {
        let mut builtin_env = BuiltinEnv::new();
        arithmetic::register_arithmetic_builtins(&mut builtin_env).unwrap();

        // Test binary operations have arity 2
        let binary_ops = vec![
            CoreBuiltin::CoreAdd,
            CoreBuiltin::CoreSub,
            CoreBuiltin::CoreMultiply,
            CoreBuiltin::CoreDivide,
            CoreBuiltin::CorePow,
            CoreBuiltin::CoreMod,
            CoreBuiltin::CoreLogBase,
            CoreBuiltin::CoreBitwiseAnd,
            CoreBuiltin::CoreBitwiseOr,
            CoreBuiltin::CoreBitwiseXor,
            CoreBuiltin::CoreBitShift,
        ];

        for op in binary_ops {
            match builtin_env.lookup(op, dummy_span()) {
                Ok(native_fn) => {
                    assert_eq!(native_fn.arity, 2,
                              "Binary operation {:?} should have arity 2", op);
                }
                Err(_) => panic!("Binary operation {:?} not found", op),
            }
        }

        // Test unary operations have arity 1
        let unary_ops = vec![
            CoreBuiltin::CoreNegate,
            CoreBuiltin::CoreAbs,
            CoreBuiltin::CoreRound,
            CoreBuiltin::CoreCeiling,
            CoreBuiltin::CoreFloor,
            CoreBuiltin::CoreExp,
            CoreBuiltin::CoreLn,
            CoreBuiltin::CoreSqrt,
            CoreBuiltin::CoreBitwiseFlip,
        ];

        for op in unary_ops {
            match builtin_env.lookup(op, dummy_span()) {
                Ok(native_fn) => {
                    assert_eq!(native_fn.arity, 1,
                              "Unary operation {:?} should have arity 1", op);
                }
                Err(_) => panic!("Unary operation {:?} not found", op),
            }
        }
    }

    #[test]
    fn test_arithmetic_operations_return_correct_builtin_enum() {
        let mut builtin_env = BuiltinEnv::new();
        arithmetic::register_arithmetic_builtins(&mut builtin_env).unwrap();

        // Test that lookup returns the correct builtin enum
        let test_ops = vec![
            CoreBuiltin::CoreAdd,
            CoreBuiltin::CoreSub,
            CoreBuiltin::CoreMultiply,
            CoreBuiltin::CoreDivide,
            CoreBuiltin::CoreNegate,
            CoreBuiltin::CoreAbs,
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
    fn test_lookup_nonexistent_builtin_returns_error() {
        let builtin_env = BuiltinEnv::new(); // Empty environment

        // Should return an error for unregistered builtins
        let result = builtin_env.lookup(CoreBuiltin::CoreAdd, dummy_span());
        assert!(result.is_err(), "Lookup of unregistered builtin should return error");
    }

    #[test]
    fn test_arithmetic_builtins_integration() {
        // Comprehensive integration test that all arithmetic operations are present
        // and correctly integrated into the builtin environment
        let mut builtin_env = BuiltinEnv::new();

        // Registration should succeed
        let registration_result = arithmetic::register_arithmetic_builtins(&mut builtin_env);
        assert!(registration_result.is_ok(), "Arithmetic builtin registration should succeed");

        // All 20 arithmetic operations should be available
        let operation_count = vec![
            CoreBuiltin::CoreAdd, CoreBuiltin::CoreSub, CoreBuiltin::CoreMultiply, CoreBuiltin::CoreDivide,
            CoreBuiltin::CoreNegate, CoreBuiltin::CoreAbs, CoreBuiltin::CorePow, CoreBuiltin::CoreMod,
            CoreBuiltin::CoreRound, CoreBuiltin::CoreCeiling, CoreBuiltin::CoreFloor,
            CoreBuiltin::CoreExp, CoreBuiltin::CoreLn, CoreBuiltin::CoreSqrt, CoreBuiltin::CoreLogBase,
            CoreBuiltin::CoreBitwiseAnd, CoreBuiltin::CoreBitwiseOr, CoreBuiltin::CoreBitwiseXor,
            CoreBuiltin::CoreBitwiseFlip, CoreBuiltin::CoreBitShift,
        ];

        // Verify each operation is correctly registered and can be looked up
        for (index, op) in operation_count.iter().enumerate() {
            match builtin_env.lookup(*op, dummy_span()) {
                Ok(_) => {
                    // Successfully found - this is what we expect
                }
                Err(e) => {
                    panic!("Failed to lookup arithmetic operation #{} {:?}: {:?}",
                           index + 1, op, e);
                }
            }
        }

        // All 20 operations should be registered
        assert_eq!(operation_count.len(), 20, "Should have 20 arithmetic operations");
    }
}
