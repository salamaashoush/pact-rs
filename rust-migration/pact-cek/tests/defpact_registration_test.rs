use pact_cek::{defpact_ops, BuiltinEnv};
use pact_shared_types::SpanInfo;
use pact_ir::CoreBuiltin;

/// Integration tests for defpact builtin registration
/// Validates that all defpact operations can be registered and looked up successfully

#[cfg(test)]
mod defpact_registration_tests {
    use super::*;

    fn dummy_span() -> SpanInfo {
        SpanInfo::empty()
    }

    #[test]
    fn test_defpact_builtins_register_successfully() {
        let mut builtin_env = BuiltinEnv::new();
        let result = defpact_ops::register_defpact_builtins(&mut builtin_env);
        assert!(result.is_ok(), "Defpact builtins should register without error");
    }

    #[test]
    fn test_all_defpact_operations_are_registered() {
        let mut builtin_env = BuiltinEnv::new();
        defpact_ops::register_defpact_builtins(&mut builtin_env).unwrap();

        // Test all defpact operations are properly registered
        let all_ops = vec![
            CoreBuiltin::CoreYield,
            CoreBuiltin::CoreYieldToChain,
            CoreBuiltin::CoreResume,
            CoreBuiltin::CorePactId,
        ];

        for op in all_ops {
            assert!(builtin_env.lookup(op, dummy_span()).is_ok(),
                    "Operation {:?} should be registered", op);
        }
    }

    #[test]
    fn test_defpact_operations_have_correct_arity() {
        let mut builtin_env = BuiltinEnv::new();
        defpact_ops::register_defpact_builtins(&mut builtin_env).unwrap();

        // Test yield has arity 1
        match builtin_env.lookup(CoreBuiltin::CoreYield, dummy_span()) {
            Ok(native_fn) => {
                assert_eq!(native_fn.arity, 1,
                          "yield should have arity 1 (takes object to yield)");
            }
            Err(_) => panic!("yield operation not found"),
        }

        // Test yield-to-chain has arity 2 (overloaded yield)
        match builtin_env.lookup(CoreBuiltin::CoreYieldToChain, dummy_span()) {
            Ok(native_fn) => {
                assert_eq!(native_fn.arity, 2,
                          "yield (to chain) should have arity 2 (takes object and chain-id)");
            }
            Err(_) => panic!("yield-to-chain operation not found"),
        }

        // Test resume has arity 1
        match builtin_env.lookup(CoreBuiltin::CoreResume, dummy_span()) {
            Ok(native_fn) => {
                assert_eq!(native_fn.arity, 1,
                          "resume should have arity 1 (takes binding closure)");
            }
            Err(_) => panic!("resume operation not found"),
        }

        // Test pact-id has arity 0
        match builtin_env.lookup(CoreBuiltin::CorePactId, dummy_span()) {
            Ok(native_fn) => {
                assert_eq!(native_fn.arity, 0,
                          "pact-id should have arity 0 (no arguments)");
            }
            Err(_) => panic!("pact-id operation not found"),
        }
    }

    #[test]
    fn test_defpact_operations_are_mapped_correctly() {
        let mut builtin_env = BuiltinEnv::new();
        defpact_ops::register_defpact_builtins(&mut builtin_env).unwrap();

        // Test that operations are properly mapped to their CoreBuiltin variants
        let builtin_mappings = vec![
            CoreBuiltin::CoreYield,
            CoreBuiltin::CoreYieldToChain,
            CoreBuiltin::CoreResume,
            CoreBuiltin::CorePactId,
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
}