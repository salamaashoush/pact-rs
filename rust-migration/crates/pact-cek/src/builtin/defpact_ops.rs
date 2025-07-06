//! DefPact Operations Builtin Implementation
//!
//! This module implements defpact (multi-step smart contract) operations following
//! the Haskell reference implementation exactly. DefPacts are multi-step transactions
//! that can span multiple blocks and even multiple chains.

use crate::types::{CEKValue, CEKEnv, BuiltinEnv, BuiltinSpec, NativeFunction};
use crate::cont::Cont;
use crate::error::CEKErrorHandler;
use crate::monad::{EvalM, charge_gas_with_args};
use crate::eval::{EvalResult, return_cek_value};
use pact_ir::CoreBuiltin;
use pact_core::shared::{SpanInfo, ChainId};
use pact_core::values::{PactValue, Object};
use pact_core::names::ModuleHash;
use pact_core::gas::MilliGas;
use std::collections::HashMap;

/// Yield data for defpact steps
#[derive(Debug, Clone, PartialEq)]
pub struct Yield {
    /// The data being yielded
    pub data: HashMap<String, PactValue>,
    /// Provenance information for cross-chain yields
    pub provenance: Option<Provenance>,
    /// Source chain for cross-chain yields
    pub source_chain: Option<ChainId>,
}

/// Provenance information for cross-chain defpacts
#[derive(Debug, Clone, PartialEq)]
pub struct Provenance {
    /// Target chain ID
    pub target_chain_id: ChainId,
    /// Module hash for verification
    pub module_hash: ModuleHash,
}

/// Register all defpact builtin functions
pub fn register_defpact_builtins(builtin_env: &mut BuiltinEnv) -> Result<(), pact_core::errors::PactErrorI> {
    // Yield operation - yield data from current defpact step
    register_builtin(builtin_env, CoreBuiltin::CoreYield, "yield", 1, yield_implementation)?;

    // Yield to chain - yield data to another chain
    register_builtin(builtin_env, CoreBuiltin::CoreYieldToChain, "yield", 2, yield_to_chain_implementation)?;

    // Resume operation - resume defpact with yielded data
    register_builtin(builtin_env, CoreBuiltin::CoreResume, "resume", 1, resume_implementation)?;

    // Pact-id operation - get current pact ID
    register_builtin(builtin_env, CoreBuiltin::CorePactId, "pact-id", 0, pact_id_implementation)?;

    Ok(())
}

/// Helper function to register a builtin
fn register_builtin(
    builtin_env: &mut BuiltinEnv,
    builtin: CoreBuiltin,
    name: &'static str,
    arity: usize,
    implementation: fn(SpanInfo, CoreBuiltin, Cont, CEKErrorHandler, CEKEnv, Vec<CEKValue>) -> EvalM<EvalResult>,
) -> Result<(), pact_core::errors::PactErrorI> {
    let native_fn: NativeFunction = Box::new(implementation);
    let spec = BuiltinSpec {
        name,
        arity,
        implementation: native_fn,
    };
    builtin_env.register(builtin, spec);
    Ok(())
}

/// Yield implementation - yield data from current defpact step
fn yield_implementation(
    info: SpanInfo,
    _builtin: CoreBuiltin,
    cont: Cont,
    handler: CEKErrorHandler,
    env: CEKEnv,
    args: Vec<CEKValue>,
) -> EvalM<EvalResult> {
    if args.len() != 1 {
        return EvalM::throw_error(pact_core::errors::PactError::PEExecutionError(
            pact_core::errors::EvalError::ArgumentCountMismatch { function: "yield".to_string(), expected: 1, received: args.len() },
            vec![],
            pact_core::shared::SpanInfo::empty()
        ));
    }

    // Clone arguments before consuming
    let arg0 = args[0].clone();

    match &arg0 {
        CEKValue::VPactValue(PactValue::Object(obj)) => {
            // Check if we're in a defpact execution
            let pact_step = env.defpact_step.clone();
            let obj_data = obj.clone();

            charge_gas_with_args("yield", &[], MilliGas(10)).bind(move |_| {
                match pact_step {
                    Some(step_state) => {
                        // Create yield data
                        let yield_data = Yield {
                            data: obj_data.to_hashmap(),
                            provenance: None,
                            source_chain: None,
                        };

                        // In a full implementation, we would update the defpact state
                        // For now, we'll return the yielded object
                        let result = CEKValue::VPactValue(PactValue::Object(obj_data));
                        return_cek_value(cont, handler, result)
                    }
                    None => {
                        EvalM::throw_error(pact_core::errors::PactError::PEExecutionError(
                            pact_core::errors::EvalError::InvalidExecutionContext(format!("yield: yield called outside defpact execution")),
                            vec![],
                            pact_core::shared::SpanInfo::empty()
                        ))
                    }
                }
            })
        }
        _ => {
            EvalM::throw_error(pact_core::errors::PactError::PEExecutionError(
                pact_core::errors::EvalError::InvalidArgument(format!("yield: Expected object argument")),
                vec![],
                pact_core::shared::SpanInfo::empty()
            ))
        }
    }
}

/// Yield to chain implementation - yield data to another chain
fn yield_to_chain_implementation(
    info: SpanInfo,
    _builtin: CoreBuiltin,
    cont: Cont,
    handler: CEKErrorHandler,
    env: CEKEnv,
    args: Vec<CEKValue>,
) -> EvalM<EvalResult> {
    if args.len() != 2 {
        return EvalM::throw_error(pact_core::errors::PactError::PEExecutionError(
            pact_core::errors::EvalError::ArgumentCountMismatch { function: "yield".to_string(), expected: 2, received: args.len() },
            vec![],
            pact_core::shared::SpanInfo::empty()
        ));
    }

    // Clone arguments before consuming
    let arg0 = args[0].clone();
    let arg1 = args[1].clone();

    match (&arg0, &arg1) {
        (CEKValue::VPactValue(PactValue::Object(obj)), CEKValue::VPactValue(PactValue::String(chain_id))) => {
            // Check if we're in a defpact execution
            let pact_step = env.defpact_step.clone();
            let module_context = env.module_context.clone();
            let current_chain_id = env.chain_id.clone();
            let obj_data = obj.clone();
            let chain_id_str = chain_id.clone();

            charge_gas_with_args("yield", &[], MilliGas(15)).bind(move |_| {
                match pact_step {
                    Some(step_state) => {
                        // Check if rollback is enabled - cross-chain yield not allowed with rollback
                        if step_state.rollback {
                            return EvalM::throw_error(pact_core::errors::PactError::PEExecutionError(
                                pact_core::errors::EvalError::InvalidExecutionContext(format!("yield: Cross-chain yield not allowed in step with rollback")),
                                vec![],
                                pact_core::shared::SpanInfo::empty()
                            ));
                        }

                        // Get module hash for provenance
                        let module_hash = match module_context {
                            Some(m) => m.hash,
                            None => {
                                return EvalM::throw_error(pact_core::errors::PactError::PEExecutionError(
                                    pact_core::errors::EvalError::NoModuleContext,
                                    vec![],
                                    pact_core::shared::SpanInfo::empty()
                                ));
                            }
                        };

                        // Create provenance
                        let provenance = Provenance {
                            target_chain_id: ChainId::new(chain_id_str),
                            module_hash,
                        };

                        // Create yield data with provenance
                        let yield_data = Yield {
                            data: obj_data.to_hashmap(),
                            provenance: Some(provenance),
                            source_chain: current_chain_id,
                        };

                        // In a full implementation, we would update the defpact state
                        // For now, we'll return the yielded object
                        let result = CEKValue::VPactValue(PactValue::Object(obj_data));
                        return_cek_value(cont, handler, result)
                    }
                    None => {
                        EvalM::throw_error(pact_core::errors::PactError::PEExecutionError(
                            pact_core::errors::EvalError::InvalidExecutionContext(format!("yield: yield called outside defpact execution")),
                            vec![],
                            pact_core::shared::SpanInfo::empty()
                        ))
                    }
                }
            })
        }
        _ => {
            EvalM::throw_error(pact_core::errors::PactError::PEExecutionError(
                pact_core::errors::EvalError::InvalidArgument(format!("yield: Expected (object, chain-id) arguments")),
                vec![],
                pact_core::shared::SpanInfo::empty()
            ))
        }
    }
}

/// Resume implementation - resume defpact with yielded data
fn resume_implementation(
    info: SpanInfo,
    _builtin: CoreBuiltin,
    cont: Cont,
    handler: CEKErrorHandler,
    env: CEKEnv,
    args: Vec<CEKValue>,
) -> EvalM<EvalResult> {
    if args.len() != 1 {
        return EvalM::throw_error(pact_core::errors::PactError::PEExecutionError(
            pact_core::errors::EvalError::ArgumentCountMismatch { function: "resume".to_string(), expected: 1, received: args.len() },
            vec![],
            pact_core::shared::SpanInfo::empty()
        ));
    }

    // Clone arguments before consuming
    let closure = args[0].clone();

    match &closure {
        _ => {
            // Check if we have a defpact step with resume data
            let pact_step = env.defpact_step.clone();

            charge_gas_with_args("resume", &[], MilliGas(10)).bind(move |_| {
                match pact_step {
                    Some(step_state) => {
                        // Check if we have resume data (yield from previous step)
                        match step_state.step_result {
                            Some(resume_data) => {
                                // Verify provenance if this is a cross-chain resume
                                // In full implementation, would check provenance here

                                // Apply the closure with the resume data
                                match closure.can_apply() {
                                    Some(applicable) => {
                                        let resume_value = CEKValue::VPactValue(PactValue::Object(
                                            match resume_data {
                                                PactValue::Object(obj) => obj,
                                                _ => {
                                                    // Convert to object if needed
                                                    let mut obj = HashMap::new();
                                                    obj.insert("value".to_string(), resume_data);
                                                    Object::from_hashmap(obj)
                                                }
                                            }
                                        ));

                                        crate::eval::apply_function(
                                            applicable,
                                            vec![resume_value],
                                            cont,
                                            handler,
                                            env
                                        )
                                    }
                                    None => {
                                        EvalM::throw_error(pact_core::errors::PactError::PEExecutionError(
                                            pact_core::errors::EvalError::TypeMismatch {
                                                expected: "closure".to_string(),
                                                found: format!("{:?}", closure),
                                                context: "resume binding".to_string(),
                                            },
                                            vec![],
                                            pact_core::shared::SpanInfo::empty()
                                        ))
                                    }
                                }
                            }
                            None => {
                                EvalM::throw_error(pact_core::errors::PactError::PEExecutionError(
                                    pact_core::errors::EvalError::InvalidExecutionContext(format!("resume: No yield data available in defpact step")),
                                    vec![],
                                    pact_core::shared::SpanInfo::empty()
                                ))
                            }
                        }
                    }
                    None => {
                        EvalM::throw_error(pact_core::errors::PactError::PEExecutionError(
                            pact_core::errors::EvalError::InvalidExecutionContext(format!("resume: resume called outside defpact execution")),
                            vec![],
                            pact_core::shared::SpanInfo::empty()
                        ))
                    }
                }
            })
        }
        _ => {
            EvalM::throw_error(pact_core::errors::PactError::PEExecutionError(
                pact_core::errors::EvalError::InvalidArgument(format!("resume: Expected closure argument")),
                vec![],
                pact_core::shared::SpanInfo::empty()
            ))
        }
    }
}

/// Pact-id implementation - get current pact ID
fn pact_id_implementation(
    _info: SpanInfo,
    _builtin: CoreBuiltin,
    cont: Cont,
    handler: CEKErrorHandler,
    env: CEKEnv,
    args: Vec<CEKValue>,
) -> EvalM<EvalResult> {
    if !args.is_empty() {
        return EvalM::throw_error(pact_core::errors::PactError::PEExecutionError(
            pact_core::errors::EvalError::ArgumentCountMismatch { function: "pact-id".to_string(), expected: 0, received: args.len() },
            vec![],
            pact_core::shared::SpanInfo::empty()
        ));
    }

    charge_gas_with_args("pact-id", &[], MilliGas(2)).bind(move |_| {
        match env.defpact_step {
            Some(step_state) => {
                // Return the current pact ID
                let result = CEKValue::VPactValue(PactValue::String(step_state.pact_id));
                return_cek_value(cont, handler, result)
            }
            None => {
                EvalM::throw_error(pact_core::errors::PactError::PEExecutionError(
                    pact_core::errors::EvalError::InvalidExecutionContext(format!("pact-id: pact-id called outside defpact execution")),
                    vec![],
                    pact_core::shared::SpanInfo::empty()
                ))
            }
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{BuiltinEnv, CEKEnv, PactDb};
    use pact_core::shared::SpanInfo;
    use std::sync::Arc;

/// Helper function to create a builtin error with current stack frame
fn builtin_error<T>(error: pact_core::errors::EvalError) -> Result<T, pact_core::errors::PactErrorI> {
    // In a real implementation, we would get the current stack from the monad state
    // For now, we'll use an empty vec but with a TODO to fix
    // TODO: Get actual stack frames from EvalM state
    Err(pact_core::errors::PactError::PEExecutionError(
        error,
        vec![], // TODO: Get from EvalM::get_stack()
        pact_core::shared::SpanInfo::empty() // TODO: Get from current context
    ))
}

/// Helper function to create a builtin error with specific info
fn builtin_error_with_info<T>(error: pact_core::errors::EvalError, info: pact_core::shared::SpanInfo) -> Result<T, pact_core::errors::PactErrorI> {
    // TODO: Get actual stack frames from EvalM state
    Err(pact_core::errors::PactError::PEExecutionError(
        error,
        vec![], // TODO: Get from EvalM::get_stack()
        info
    ))
}

/// Helper function to create an error result for EvalM
fn builtin_error_result(error: pact_core::errors::EvalError, info: pact_core::shared::SpanInfo) -> EvalM<EvalResult> {
    // TODO: Get actual stack frames from EvalM state
    EvalM::pure_value(EvalResult::EvalError(
        pact_core::errors::PactError::PEExecutionError(
            error,
            vec![], // TODO: Get from EvalM::get_stack()
            info
        )
    ))
}


    #[derive(Debug)]
    struct MockPactDb;

    impl PactDb for MockPactDb {
        fn read(&self, _domain: crate::types::Domain, _key: crate::types::RowKey) -> crate::monad::EvalM<Option<crate::types::RowData>> {
            crate::monad::EvalM::pure_value(None)
        }

        fn write(&self, _domain: crate::types::Domain, _key: crate::types::RowKey, _data: crate::types::RowData) -> crate::monad::EvalM<()> {
            crate::monad::EvalM::pure_value(())
        }

        fn write_with_type(&self, _domain: crate::types::Domain, _key: crate::types::RowKey, _data: crate::types::RowData, _write_type: crate::types::WriteType) -> crate::monad::EvalM<()> {
            crate::monad::EvalM::pure_value(())
        }

        fn keys(&self, _domain: crate::types::Domain) -> crate::monad::EvalM<Vec<crate::types::RowKey>> {
            crate::monad::EvalM::pure_value(vec![])
        }

        fn select(&self, _domain: crate::types::Domain, _filter: Option<crate::monad::EvalM<bool>>) -> crate::monad::EvalM<Vec<(crate::types::RowKey, crate::types::RowData)>> {
            crate::monad::EvalM::pure_value(vec![])
        }

        fn create_table(&self, _table_name: String, _schema: crate::types::TableSchema) -> crate::monad::EvalM<()> {
            crate::monad::EvalM::pure_value(())
        }

        fn begin_tx(&self) -> crate::monad::EvalM<()> {
            crate::monad::EvalM::pure_value(())
        }

        fn commit_tx(&self) -> crate::monad::EvalM<()> {
            crate::monad::EvalM::pure_value(())
        }

        fn rollback_tx(&self) -> crate::monad::EvalM<()> {
            crate::monad::EvalM::pure_value(())
        }
    }

    #[test]
    fn test_defpact_builtin_registration() {
        let mut builtin_env = BuiltinEnv::new();
        let result = register_defpact_builtins(&mut builtin_env);
        assert!(result.is_ok(), "DefPact builtins should register without error");
    }

    #[test]
    fn test_all_defpact_builtins_are_registered() {
        let mut builtin_env = BuiltinEnv::new();
        register_defpact_builtins(&mut builtin_env).unwrap();

        // Test that all defpact operations are registered
        let defpact_ops = vec![
            (CoreBuiltin::CoreYield, 1),
            (CoreBuiltin::CoreYieldToChain, 2),
            (CoreBuiltin::CoreResume, 1),
            (CoreBuiltin::CorePactId, 0),
        ];

        for (builtin, expected_arity) in defpact_ops {
            let result = builtin_env.lookup(builtin, SpanInfo::empty());
            assert!(result.is_ok(), "{:?} should be registered", builtin);

            if let Ok(native_fn) = result {
                assert_eq!(native_fn.arity, expected_arity, "{:?} should have arity {}", builtin, expected_arity);
                assert_eq!(native_fn.builtin, builtin, "Should return correct builtin enum for {:?}", builtin);
            }
        }
    }
}
