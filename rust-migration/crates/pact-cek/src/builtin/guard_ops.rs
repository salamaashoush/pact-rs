use pact_core::values::{PactValue, Guard, Keyset};
use crate::types::{CEKValue, CEKEnv, BuiltinEnv, BuiltinSpec, NativeFunction};
use crate::monad::{charge_gas_with_args, EvalM};
use crate::eval::{return_cek_value, EvalResult};
use pact_core::gas::MilliGas;
use pact_ir::CoreBuiltin;
use std::collections::HashMap;
use crate::builtin::{throw_argument_count_error, throw_type_mismatch_error, builtin_error};

/// Register all guard-related builtin functions
pub fn register_guard_builtins(builtin_env: &mut BuiltinEnv) -> Result<(), pact_core::errors::PactErrorI> {
    // Guard creation operations that exist in CoreBuiltin
    builtin_env.register(CoreBuiltin::CoreCreateCapabilityGuard, BuiltinSpec {
        name: "create-capability-guard",
        arity: 1,
        implementation: create_capability_guard_implementation(),
    });

    builtin_env.register(CoreBuiltin::CoreCreateCapabilityPactGuard, BuiltinSpec {
        name: "create-capability-pact-guard",
        arity: 1,
        implementation: create_capability_pact_guard_implementation(),
    });

    builtin_env.register(CoreBuiltin::CoreCreateDefPactGuard, BuiltinSpec {
        name: "create-pact-guard",
        arity: 1,
        implementation: create_pact_guard_implementation(),
    });

    builtin_env.register(CoreBuiltin::CoreKeysetRefGuard, BuiltinSpec {
        name: "keyset-ref-guard",
        arity: 1,
        implementation: keyset_ref_guard_implementation(),
    });

    builtin_env.register(CoreBuiltin::CoreCreateModuleGuard, BuiltinSpec {
        name: "create-module-guard",
        arity: 1,
        implementation: create_module_guard_implementation(),
    });

    // Guard enforcement
    builtin_env.register(CoreBuiltin::CoreEnforceGuard, BuiltinSpec {
        name: "enforce-guard",
        arity: 1,
        implementation: enforce_guard_implementation(),
    });

    // Principal operations
    builtin_env.register(CoreBuiltin::CoreCreatePrincipal, BuiltinSpec {
        name: "create-principal",
        arity: 1,
        implementation: create_principal_implementation(),
    });

    builtin_env.register(CoreBuiltin::CoreValidatePrincipal, BuiltinSpec {
        name: "validate-principal",
        arity: 2,
        implementation: validate_principal_implementation(),
    });

    builtin_env.register(CoreBuiltin::CoreIsPrincipal, BuiltinSpec {
        name: "is-principal",
        arity: 1,
        implementation: is_principal_implementation(),
    });

    builtin_env.register(CoreBuiltin::CoreTypeOfPrincipal, BuiltinSpec {
        name: "typeof-principal",
        arity: 1,
        implementation: typeof_principal_implementation(),
    });

    // Keyset operations
    builtin_env.register(CoreBuiltin::CoreEnforceKeyset, BuiltinSpec {
        name: "enforce-keyset",
        arity: 1,
        implementation: enforce_keyset_implementation(),
    });

    builtin_env.register(CoreBuiltin::CoreEnforceKeysetName, BuiltinSpec {
        name: "enforce-keyset",
        arity: 2,  // Overloaded with 2 args for predicate
        implementation: enforce_keyset_name_implementation(),
    });

    builtin_env.register(CoreBuiltin::CoreReadKeyset, BuiltinSpec {
        name: "read-keyset",
        arity: 1,
        implementation: read_keyset_implementation(),
    });

    builtin_env.register(CoreBuiltin::CoreDefineKeySet, BuiltinSpec {
        name: "define-keyset",
        arity: 2,
        implementation: define_keyset_implementation(),
    });

    builtin_env.register(CoreBuiltin::CoreDefineKeysetData, BuiltinSpec {
        name: "define-keyset",
        arity: 3,  // Overloaded with data
        implementation: define_keyset_data_implementation(),
    });

    builtin_env.register(CoreBuiltin::CoreDescribeKeyset, BuiltinSpec {
        name: "describe-keyset",
        arity: 1,
        implementation: describe_keyset_implementation(),
    });

    Ok(())
}


/// Implementation of create-capability-guard builtin
/// Creates a guard that enforces a capability is in scope
fn create_capability_guard_implementation() -> NativeFunction {
    Box::new(|info, builtin, cont, handler, env, args| {
        if args.len() != 1 {
            return throw_argument_count_error(info, "create-capability-guard", 1, args.len());
        }

        // Extract capability token argument
        let arg0 = args[0].clone();

        charge_gas_with_args("create-capability-guard", &args, MilliGas(50))
            .bind(move |_| {
                match arg0 {
                    CEKValue::VPactValue(PactValue::CapToken(cap_token)) => {
                        // Create a capability guard
                        let guard = Guard::Capability {
                            name: cap_token.name,
                            args: cap_token.args,
                        };

                        let result = CEKValue::VPactValue(PactValue::Guard(guard));
                        return_cek_value(cont, handler, result)
                    }
                    _ => throw_type_mismatch_error(info, "capability token", "other", "create-capability-guard")
                }
            })
    })
}

/// Implementation of create-capability-pact-guard builtin
/// Creates a guard that enforces a capability within a specific pact context
fn create_capability_pact_guard_implementation() -> NativeFunction {
    Box::new(|info, builtin, cont, handler, env, args| {
        if args.len() != 1 {
            return throw_argument_count_error(info, "create-capability-pact-guard", 1, args.len());
        }

        // Extract capability token argument
        let arg0 = args[0].clone();

        charge_gas_with_args("create-capability-pact-guard", &args, MilliGas(50))
            .bind(move |_| {
                match arg0 {
                    CEKValue::VPactValue(PactValue::CapToken(cap_token)) => {
                        // Get current pact ID from environment
                        let _current_pact_id = env.get_current_pact_id();

                        // Create a capability guard (note: pact context handled differently)
                        let guard = Guard::Capability {
                            name: cap_token.name,
                            args: cap_token.args,
                        };

                        let result = CEKValue::VPactValue(PactValue::Guard(guard));
                        return_cek_value(cont, handler, result)
                    }
                    _ => throw_type_mismatch_error(info, "capability token", "other", "create-capability-pact-guard")
                }
            })
    })
}

/// Implementation of create-pact-guard builtin (DEPRECATED)
/// Creates a guard tied to a specific defpact execution
fn create_pact_guard_implementation() -> NativeFunction {
    Box::new(|info, builtin, cont, handler, env, args| {
        if args.len() != 1 {
            return throw_argument_count_error(info, "create-pact-guard", 1, args.len());
        }

        // Extract pact name argument
        let arg0 = args[0].clone();

        charge_gas_with_args("create-pact-guard", &args, MilliGas(50))
            .bind(move |_| {
                match arg0 {
                    CEKValue::VPactValue(PactValue::String(pact_name)) => {
                        // Issue deprecation warning
                        eprintln!("Warning: create-pact-guard is deprecated, use capability guards instead");

                        // Create a module guard (deprecated functionality, using Module variant)
                        let guard = Guard::Module {
                            name: pact_name, // Using pact name as module name
                            governance: None,
                        };

                        let result = CEKValue::VPactValue(PactValue::Guard(guard));
                        return_cek_value(cont, handler, result)
                    }
                    _ => throw_type_mismatch_error(info, "string", "other", "create-pact-guard")
                }
            })
    })
}

/// Implementation of create-module-guard builtin
/// Creates a guard that enforces module governance
fn create_module_guard_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        if args.len() != 1 {
            return throw_argument_count_error(info, "create-module-guard", 1, args.len());
        }

        // Extract module name argument
        let arg0 = args[0].clone();

        charge_gas_with_args("create-module-guard", &args, MilliGas(50))
            .bind(move |_| {
                match arg0 {
                    CEKValue::VPactValue(PactValue::String(module_name)) => {
                        // Create a module guard
                        let guard = Guard::Module {
                            name: module_name,
                            governance: None, // Simple version without governance
                        };

                        let result = CEKValue::VPactValue(PactValue::Guard(guard));
                        return_cek_value(cont, handler, result)
                    }
                    _ => throw_type_mismatch_error(info, "string", "other", "create-module-guard")
                }
            })
    })
}

/// Implementation of keyset-ref-guard builtin
/// Creates a guard that references a defined keyset by name
fn keyset_ref_guard_implementation() -> NativeFunction {
    Box::new(|info, builtin, cont, handler, env, args| {
        if args.len() != 1 {
            return throw_argument_count_error(info, "keyset-ref-guard", 1, args.len());
        }

        // Extract keyset name argument
        let arg0 = args[0].clone();

        charge_gas_with_args("keyset-ref-guard", &args, MilliGas(25))
            .bind(move |_| {
                match arg0 {
                    CEKValue::VPactValue(PactValue::String(keyset_name)) => {
                        // Create a keyset reference guard
                        let guard = Guard::KeySet {
                            name: keyset_name,
                            keys: vec![], // Empty keys means reference to named keyset
                            pred: "keys-all".to_string(), // Default predicate
                        };

                        let result = CEKValue::VPactValue(PactValue::Guard(guard));
                        return_cek_value(cont, handler, result)
                    }
                    _ => throw_type_mismatch_error(info, "string", "other", "keyset-ref-guard")
                }
            })
    })
}

/// Implementation of enforce-guard builtin
/// Enforces a guard or keyset by name, throws on failure
fn enforce_guard_implementation() -> NativeFunction {
    Box::new(|info, builtin, cont, handler, env, args| {
        if args.len() != 1 {
            return throw_argument_count_error(info, "enforce-guard", 1, args.len());
        }

        // Extract guard or keyset name argument
        let arg0 = args[0].clone();

        charge_gas_with_args("enforce-guard", &args, MilliGas(100))
            .bind(move |_| {
                match arg0 {
                    CEKValue::VPactValue(PactValue::Guard(guard)) => {
                        // Enforce the guard
                        match enforce_guard_value(&guard, &env) {
                            Ok(()) => {
                                let result = CEKValue::VPactValue(PactValue::Bool(true));
                                return_cek_value(cont, handler, result)
                            }
                            Err(e) => EvalM::pure_value(EvalResult::EvalError(e))
                        }
                    }
                    CEKValue::VPactValue(PactValue::String(keyset_name)) => {
                        // Treat as keyset name and enforce
                        match enforce_keyset_by_name(&keyset_name, &env) {
                            Ok(()) => {
                                let result = CEKValue::VPactValue(PactValue::Bool(true));
                                return_cek_value(cont, handler, result)
                            }
                            Err(e) => EvalM::pure_value(EvalResult::EvalError(e))
                        }
                    }
                    CEKValue::VPactValue(PactValue::Keyset(keyset)) => {
                        // Direct keyset enforcement
                        match enforce_keyset_value(&keyset, &env) {
                            Ok(()) => {
                                let result = CEKValue::VPactValue(PactValue::Bool(true));
                                return_cek_value(cont, handler, result)
                            }
                            Err(e) => EvalM::pure_value(EvalResult::EvalError(e))
                        }
                    }
                    _ => throw_type_mismatch_error(info, "guard, keyset, or string", "other", "enforce-guard")
                }
            })
    })
}

/// Implementation of create-principal builtin
/// Creates a principal identifier string from a guard
fn create_principal_implementation() -> NativeFunction {
    Box::new(|info, builtin, cont, handler, env, args| {
        if args.len() != 1 {
            return throw_argument_count_error(info, "create-principal", 1, args.len());
        }

        // Extract guard argument
        let arg0 = args[0].clone();

        charge_gas_with_args("create-principal", &args, MilliGas(50))
            .bind(move |_| {
                match arg0 {
                    CEKValue::VPactValue(PactValue::Guard(guard)) => {
                        // Create principal from guard
                        let principal = create_principal_for_guard(&guard);

                        let result = CEKValue::VPactValue(PactValue::String(principal));
                        return_cek_value(cont, handler, result)
                    }
                    _ => throw_type_mismatch_error(info, "guard", "other", "create-principal")
                }
            })
    })
}

/// Implementation of validate-principal builtin
/// Validates that a guard corresponds to a principal string
fn validate_principal_implementation() -> NativeFunction {
    Box::new(|info, builtin, cont, handler, env, args| {
        if args.len() != 2 {
            return throw_argument_count_error(info, "validate-principal", 2, args.len());
        }

        // Extract guard and principal arguments
        let arg0 = args[0].clone();
        let arg1 = args[1].clone();

        charge_gas_with_args("validate-principal", &args, MilliGas(50))
            .bind(move |_| {
                match (arg0, arg1) {
                    (CEKValue::VPactValue(PactValue::Guard(guard)),
                     CEKValue::VPactValue(PactValue::String(principal_str))) => {

                        // Create principal from guard and compare
                        let computed_principal = create_principal_for_guard(&guard);
                        let is_valid = computed_principal == principal_str;

                        let result = CEKValue::VPactValue(PactValue::Bool(is_valid));
                        return_cek_value(cont, handler, result)
                    }
                    _ => throw_type_mismatch_error(info, "guard, string", "incorrect types", "validate-principal")
                }
            })
    })
}

/// Helper function to enforce a guard value
fn enforce_guard_value(guard: &Guard, env: &CEKEnv) -> Result<(), pact_core::errors::PactErrorI> {
    match guard {
        Guard::KeySet { name, keys, pred } => {
            if keys.is_empty() {
                // Reference to named keyset
                enforce_keyset_by_name(name, env)
            } else {
                // Direct keyset with keys
                let keyset = Keyset::new(keys.clone(), pact_core::values::KeySetPredicate::parse(pred));
                enforce_keyset_value(&keyset, env)
            }
        }
        Guard::User { fun, args } => {
            // Execute user guard function
            // In practice, this would call the function with args
            // For now, return success (placeholder)
            Ok(())
        }
        Guard::Capability { name, args } => {
            // Check if capability is granted in current scope
            if env.has_capability(name, args) {
                Ok(())
            } else {
                builtin_error(pact_core::errors::EvalError::CapabilityNotGranted(name.clone()))
            }
        }
        Guard::Module { name, governance: _ } => {
            // Check module admin privileges
            if env.has_module_admin(name) {
                Ok(())
            } else {
                Err(pact_core::errors::PactError::PEExecutionError(
                    pact_core::errors::EvalError::GuardFailure(format!("module admin required for {}", name)),
                    vec![],
                    pact_core::shared::SpanInfo::empty()
                ))
            }
        }
    }
}

/// Helper function to enforce a keyset value
fn enforce_keyset_value(keyset: &Keyset, env: &CEKEnv) -> Result<(), pact_core::errors::PactErrorI> {
    // In practice, this would check keyset signatures
    // For now, return success (placeholder)
    Ok(())
}

/// Helper function to enforce a keyset by name
fn enforce_keyset_by_name(keyset_name: &str, env: &CEKEnv) -> Result<(), pact_core::errors::PactErrorI> {
    // In practice, this would look up the keyset by name and enforce it
    // For now, return success (placeholder)
    Ok(())
}

/// Helper function to create a principal identifier from a guard
fn create_principal_for_guard(guard: &Guard) -> String {
    match guard {
        Guard::KeySet { name, keys, pred } => {
            if keys.is_empty() {
                // Keyset reference principal: r:keyset-name
                format!("r:{}", name)
            } else if keys.len() == 1 {
                // Single key principal: k:public-key
                format!("k:{}", keys[0])
            } else {
                // Multi-key principal: w:hash:predicate
                let keyset = Keyset::new(keys.clone(), pact_core::values::KeySetPredicate::parse(pred));
                let hash = compute_keyset_hash(&keyset);
                format!("w:{}:{}", hash, pred)
            }
        }
        Guard::User { fun, args } => {
            // User guard principal: u:function:args-hash
            let args_hash = compute_args_hash(args);
            format!("u:{}:{}", fun, args_hash)
        }
        Guard::Capability { name, args } => {
            // Capability guard principal: c:hash
            let cap_token = pact_core::values::CapToken {
                name: name.clone(),
                args: args.clone(),
            };
            let cap_hash = compute_capability_hash(&cap_token);
            format!("c:{}", cap_hash)
        }
        Guard::Module { name, governance: _ } => {
            // Module guard principal: m:module:name
            format!("m:{}", name)
        }
    }
}

/// Helper function to compute keyset hash
fn compute_keyset_hash(keyset: &Keyset) -> String {
    // In practice, this would compute a cryptographic hash
    format!("keyset-hash-{}", keyset.keys.len())
}

/// Helper function to compute args hash
fn compute_args_hash(args: &[PactValue]) -> String {
    // In practice, this would compute a cryptographic hash
    format!("args-hash-{}", args.len())
}

/// Helper function to compute capability hash
fn compute_capability_hash(capability: &pact_core::values::CapToken) -> String {
    // In practice, this would compute a cryptographic hash
    format!("cap-hash-{}", capability.name.len())
}

/// Implementation of is-principal builtin
/// Checks if a string is a valid principal
fn is_principal_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        if args.len() != 1 {
            return throw_argument_count_error(info, "is-principal", 1, args.len());
        }

        let arg0 = args[0].clone();

        charge_gas_with_args("is-principal", &args, MilliGas(20))
            .bind(move |_| {
                match arg0 {
                    CEKValue::VPactValue(PactValue::String(principal_str)) => {
                        // Check if string is a valid principal format
                        let is_valid = is_valid_principal(&principal_str);
                        let result = CEKValue::VPactValue(PactValue::Bool(is_valid));
                        return_cek_value(cont, handler, result)
                    }
                    _ => throw_type_mismatch_error(info, "string", "other", "is-principal")
                }
            })
    })
}

/// Implementation of typeof-principal builtin
/// Returns the type of a principal (k, r, w, u, c, m)
fn typeof_principal_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        if args.len() != 1 {
            return throw_argument_count_error(info, "typeof-principal", 1, args.len());
        }

        let arg0 = args[0].clone();

        charge_gas_with_args("typeof-principal", &args, MilliGas(20))
            .bind(move |_| {
                match arg0 {
                    CEKValue::VPactValue(PactValue::String(principal_str)) => {
                        match get_principal_type(&principal_str) {
                            Some(principal_type) => {
                                let result = CEKValue::VPactValue(PactValue::String(principal_type));
                                return_cek_value(cont, handler, result)
                            }
                            None => {
                                EvalM::pure_value(EvalResult::EvalError(
                                    pact_core::errors::PactError::PEExecutionError(
                                        pact_core::errors::EvalError::InvalidArgument(format!("typeof-principal: Invalid principal: {}", principal_str)),
                                        vec![],
                                        info.clone()
                                    )
                                ))
                            }
                        }
                    }
                    _ => throw_type_mismatch_error(info, "string", "other", "typeof-principal")
                }
            })
    })
}

/// Implementation of enforce-keyset builtin
/// Enforces a keyset by name or value
fn enforce_keyset_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, env, args| {
        if args.len() != 1 {
            return throw_argument_count_error(info, "enforce-keyset", 1, args.len());
        }

        let arg0 = args[0].clone();

        charge_gas_with_args("enforce-keyset", &args, MilliGas(100))
            .bind(move |_| {
                match arg0 {
                    CEKValue::VPactValue(PactValue::String(keyset_name)) => {
                        // Enforce keyset by name
                        match enforce_keyset_by_name(&keyset_name, &env) {
                            Ok(()) => {
                                let result = CEKValue::VPactValue(PactValue::Bool(true));
                                return_cek_value(cont, handler, result)
                            }
                            Err(e) => EvalM::pure_value(EvalResult::EvalError(e))
                        }
                    }
                    CEKValue::VPactValue(PactValue::Keyset(keyset)) => {
                        // Enforce keyset value directly
                        match enforce_keyset_value(&keyset, &env) {
                            Ok(()) => {
                                let result = CEKValue::VPactValue(PactValue::Bool(true));
                                return_cek_value(cont, handler, result)
                            }
                            Err(e) => EvalM::pure_value(EvalResult::EvalError(e))
                        }
                    }
                    _ => throw_type_mismatch_error(info, "keyset or string", "other", "enforce-keyset")
                }
            })
    })
}

/// Implementation of enforce-keyset with predicate override
fn enforce_keyset_name_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, env, args| {
        if args.len() != 2 {
            return throw_argument_count_error(info, "enforce-keyset", 2, args.len());
        }

        let arg0 = args[0].clone();
        let arg1 = args[1].clone();

        charge_gas_with_args("enforce-keyset", &args, MilliGas(100))
            .bind(move |_| {
                match (arg0, arg1) {
                    (CEKValue::VPactValue(PactValue::String(keyset_name)),
                     CEKValue::VPactValue(PactValue::String(predicate))) => {
                        // Enforce keyset by name with predicate override
                        // In full implementation, would apply the predicate
                        match enforce_keyset_by_name(&keyset_name, &env) {
                            Ok(()) => {
                                let result = CEKValue::VPactValue(PactValue::Bool(true));
                                return_cek_value(cont, handler, result)
                            }
                            Err(e) => EvalM::pure_value(EvalResult::EvalError(e))
                        }
                    }
                    _ => throw_type_mismatch_error(info, "string, string", "other", "enforce-keyset")
                }
            })
    })
}

/// Implementation of read-keyset builtin
/// Reads a keyset from transaction data
fn read_keyset_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        if args.len() != 1 {
            return throw_argument_count_error(info, "read-keyset", 1, args.len());
        }

        let arg0 = args[0].clone();

        charge_gas_with_args("read-keyset", &args, MilliGas(30))
            .bind(move |_| {
                match arg0 {
                    CEKValue::VPactValue(PactValue::String(keyset_name)) => {
                        // In full implementation, would read from transaction data
                        // For now, return a placeholder keyset
                        let keyset = Keyset::new(
                            vec!["test-key".to_string()],
                            pact_core::values::KeySetPredicate::KeysAll
                        );
                        let result = CEKValue::VPactValue(PactValue::Keyset(keyset));
                        return_cek_value(cont, handler, result)
                    }
                    _ => throw_type_mismatch_error(info, "string", "other", "read-keyset")
                }
            })
    })
}

/// Implementation of define-keyset builtin
/// Defines a new keyset in the database
fn define_keyset_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, env, args| {
        if args.len() != 2 {
            return throw_argument_count_error(info, "define-keyset", 2, args.len());
        }

        let arg0 = args[0].clone();
        let arg1 = args[1].clone();

        charge_gas_with_args("define-keyset", &args, MilliGas(100))
            .bind(move |_| {
                match (arg0, arg1) {
                    (CEKValue::VPactValue(PactValue::String(keyset_name)),
                     CEKValue::VPactValue(PactValue::Keyset(keyset))) => {
                        // In full implementation, would write to database
                        // For now, return success
                        let result = CEKValue::VPactValue(PactValue::String(
                            format!("Keyset {} defined", keyset_name)
                        ));
                        return_cek_value(cont, handler, result)
                    }
                    _ => throw_type_mismatch_error(info, "string, keyset", "other", "define-keyset")
                }
            })
    })
}

/// Implementation of define-keyset with data
fn define_keyset_data_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        if args.len() != 3 {
            return throw_argument_count_error(info, "define-keyset", 3, args.len());
        }

        let arg0 = args[0].clone();
        let arg1 = args[1].clone();
        let arg2 = args[2].clone();

        charge_gas_with_args("define-keyset", &args, MilliGas(100))
            .bind(move |_| {
                match (arg0, arg1, arg2) {
                    (CEKValue::VPactValue(PactValue::String(keyset_name)),
                     CEKValue::VPactValue(PactValue::List(keys)),
                     CEKValue::VPactValue(PactValue::String(predicate))) => {
                        // In full implementation, would create keyset and write to database
                        let result = CEKValue::VPactValue(PactValue::String(
                            format!("Keyset {} defined", keyset_name)
                        ));
                        return_cek_value(cont, handler, result)
                    }
                    _ => throw_type_mismatch_error(info, "string, list, string", "other", "define-keyset")
                }
            })
    })
}

/// Implementation of describe-keyset builtin
/// Retrieves keyset information from the database
fn describe_keyset_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        if args.len() != 1 {
            return throw_argument_count_error(info, "describe-keyset", 1, args.len());
        }

        let arg0 = args[0].clone();

        charge_gas_with_args("describe-keyset", &args, MilliGas(50))
            .bind(move |_| {
                match arg0 {
                    CEKValue::VPactValue(PactValue::String(keyset_name)) => {
                        // In full implementation, would read from database
                        // For now, return a placeholder object
                        let mut keyset_info = HashMap::new();
                        keyset_info.insert(
                            "name".to_string(),
                            PactValue::String(keyset_name)
                        );
                        keyset_info.insert(
                            "keys".to_string(),
                            PactValue::List(vec![PactValue::String("test-key".to_string())])
                        );
                        keyset_info.insert(
                            "pred".to_string(),
                            PactValue::String("keys-all".to_string())
                        );

                        let result = CEKValue::VPactValue(PactValue::Object(
                            pact_core::values::Object::from_hashmap(keyset_info)
                        ));
                        return_cek_value(cont, handler, result)
                    }
                    _ => throw_type_mismatch_error(info, "string", "other", "describe-keyset")
                }
            })
    })
}

/// Helper to check if a string is a valid principal
fn is_valid_principal(principal: &str) -> bool {
    // Check principal format: TYPE:DATA
    if principal.len() < 3 || !principal.contains(':') {
        return false;
    }

    let parts: Vec<&str> = principal.splitn(2, ':').collect();
    if parts.len() != 2 {
        return false;
    }

    let prefix = parts[0];
    let data = parts[1];

    match prefix {
        "k" | "r" | "w" | "u" | "c" | "m" => !data.is_empty(),
        _ => false
    }
}

/// Helper to get principal type
fn get_principal_type(principal: &str) -> Option<String> {
    if !is_valid_principal(principal) {
        return None;
    }

    let prefix = principal.chars().next()?;
    let type_name = match prefix {
        'k' => "k:key",
        'r' => "r:keyset",
        'w' => "w:keyset",
        'u' => "u:user",
        'c' => "c:capability",
        'm' => "m:module",
        _ => return None,
    };

    Some(type_name.to_string())
}
