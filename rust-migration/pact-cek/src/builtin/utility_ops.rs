//! Utility Operations Builtin Implementation
//!
//! This module implements utility operations like identity, bind, compose, and cond
//! following the Haskell reference implementation exactly.

use crate::types::{CEKValue, BuiltinEnv, BuiltinSpec, NativeFunction};
use crate::monad::charge_gas_with_args;
use crate::eval::return_cek_value;
use crate::builtin::{args_error, throw_execution_error};
use pact_ir::CoreBuiltin;
use pact_values::PactValue;
use pact_gas::MilliGas;
use pact_errors::EvalError;

/// Register all utility builtin functions
pub fn register_utility_builtins(builtin_env: &mut BuiltinEnv) -> Result<(), pact_errors::PactErrorI> {
    // Identity function
    builtin_env.register(
        CoreBuiltin::CoreIdentity,
        BuiltinSpec {
            name: "identity",
            arity: 1,
            implementation: identity_implementation(),
        },
    );

    // Bind operation for monadic composition
    builtin_env.register(
        CoreBuiltin::CoreBind,
        BuiltinSpec {
            name: "bind",
            arity: 2,
            implementation: bind_implementation(),
        },
    );

    // Function composition
    builtin_env.register(
        CoreBuiltin::CoreCompose,
        BuiltinSpec {
            name: "compose",
            arity: 3,  // Takes two functions and a value
            implementation: compose_implementation(),
        },
    );

    // Conditional expression (cond)
    builtin_env.register(
        CoreBuiltin::CoreCond,
        BuiltinSpec {
            name: "cond",
            arity: 1,  // Takes a list of condition-expression pairs
            implementation: cond_implementation(),
        },
    );

    Ok(())
}

/// Identity function - returns its argument unchanged
fn identity_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("identity", &args, MilliGas(1))
            .bind(move |_| {
                if args.len() != 1 {
                    return args_error(info, "identity", &args);
                }
                
                // Simply return the argument as-is
                return_cek_value(cont, handler, args[0].clone())
            })
    })
}

/// Bind operation - applies a function to a value
/// bind :: a -> (a -> b) -> b
fn bind_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, env, args| {
        charge_gas_with_args("bind", &args, MilliGas(2))
            .bind(move |_| {
                if args.len() != 2 {
                    return args_error(info, "bind", &args);
                }
                
                match (&args[0], &args[1]) {
                    (value, CEKValue::VClosure(clo)) => {
                        // Apply the closure to the value
                        // Use apply_lambda which handles CanApply
                        crate::eval::apply_lambda(clo.clone(), env, vec![value.clone()], cont, handler)
                    }
                    _ => {
                        args_error(info, "bind", &args)
                    }
                }
            })
    })
}

/// Function composition - composes two functions
/// compose :: (b -> c) -> (a -> b) -> a -> c
/// From Haskell: coreCompose takes 3 args: [VClosure clo1, VClosure clo2, v]
fn compose_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, env, args| {
        charge_gas_with_args("compose", &args, MilliGas(2))
            .bind(move |_| {
                if args.len() != 3 {
                    return args_error(info, "compose", &args);
                }
                
                match (&args[0], &args[1], &args[2]) {
                    (CEKValue::VClosure(f), CEKValue::VClosure(g), value) => {
                        // Create continuation to apply f after g
                        // This matches Haskell: let cont' = Fn clo2 env [] [] cont
                        let fn_cont = crate::cont::Cont::Fn {
                            function: f.clone(),
                            env: env.clone(),
                            args: vec![],
                            values: vec![],
                            cont: Box::new(cont),
                        };
                        
                        // Apply g to value with continuation that will apply f
                        // This matches Haskell: applyLam clo1 [v] cont' handler
                        crate::eval::apply_lambda(g.clone(), env, vec![value.clone()], fn_cont, handler)
                    }
                    _ => {
                        args_error(info, "compose", &args)
                    }
                }
            })
    })
}

/// Conditional expression - evaluates condition-expression pairs
/// cond :: [(Bool, a)] -> a
fn cond_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("cond", &args, MilliGas(2))
            .bind(move |_| {
                if args.len() != 1 {
                    return args_error(info, "cond", &args);
                }
                
                match &args[0] {
                    CEKValue::VPactValue(PactValue::List(pairs)) => {
                        // Process each condition-expression pair
                        for pair in pairs {
                            match pair {
                                PactValue::List(elems) if elems.len() == 2 => {
                                    match &elems[0] {
                                        PactValue::Bool(true) => {
                                            // Found a true condition, return its expression
                                            return return_cek_value(cont, handler, CEKValue::VPactValue(elems[1].clone()));
                                        }
                                        PactValue::Bool(false) => {
                                            // Continue to next pair
                                            continue;
                                        }
                                        _ => {
                                            return throw_execution_error(info, EvalError::InvalidArgument("cond: Condition must be boolean".to_string()));
                                        }
                                    }
                                }
                                _ => {
                                    return throw_execution_error(info, EvalError::InvalidArgument("cond: Each element must be a pair [condition, expression]".to_string()));
                                }
                            }
                        }
                        // No true condition found
                        throw_execution_error(info, EvalError::InvalidArgument("cond: No condition evaluated to true".to_string()))
                    }
                    _ => {
                        args_error(info, "cond", &args)
                    }
                }
            })
    })
}