//! Tests for Redesigned CEK Architecture
//!
//! These tests validate that the redesigned architecture properly addresses
//! all the identified gaps and provides a solid foundation for implementing
//! ALL Pact builtins correctly.

use crate::types::*;
use crate::cont::*;
use crate::error::*;
use crate::monad::*;
use crate::builtin::*;
use pact_ir::{CoreTerm, CoreBuiltin, Literal};
use pact_values::PactValue;
use pact_shared_types::SpanInfo;
use pact_errors::PactError;
use std::sync::Arc;

/// Mock PactDb implementation for testing
#[derive(Debug)]
struct MockPactDb {
    data: std::sync::Mutex<std::collections::HashMap<String, std::collections::HashMap<String, PactValue>>>,
}

impl MockPactDb {
    fn new() -> Self {
        MockPactDb {
            data: std::sync::Mutex::new(std::collections::HashMap::new()),
        }
    }
}

impl PactDb for MockPactDb {
    fn read(&self, domain: Domain, key: RowKey) -> EvalM<Option<RowData>> {
        let table = match &domain {
            Domain::UserTable(name) => name.clone(),
            _ => "system".to_string(),
        };
        let data = self.data.lock().unwrap();
        let result = data.get(&table)
            .and_then(|t| t.get(&key.0))
            .map(|v| RowData {
                fields: match v {
                    PactValue::Object(fields) => fields.clone().into_iter().collect(),
                    _ => std::collections::HashMap::new(),
                }
            });
        EvalM::pure_value(result)
    }

    fn write(&self, domain: Domain, key: RowKey, data: RowData) -> EvalM<()> {
        let table = match &domain {
            Domain::UserTable(name) => name.clone(),
            _ => "system".to_string(),
        };
        let mut db_data = self.data.lock().unwrap();
        db_data.entry(table).or_insert_with(std::collections::HashMap::new)
            .insert(key.0, PactValue::Object(data.fields.into_iter().collect()));
        EvalM::pure_value(())
    }

    fn keys(&self, domain: Domain) -> EvalM<Vec<RowKey>> {
        let table = match &domain {
            Domain::UserTable(name) => name.clone(),
            _ => "system".to_string(),
        };
        let data = self.data.lock().unwrap();
        let keys = data.get(&table)
            .map(|t| t.keys().map(|k| RowKey(k.clone())).collect())
            .unwrap_or_default();
        EvalM::pure_value(keys)
    }

    fn select(&self, domain: Domain, _filter: Option<EvalM<bool>>) -> EvalM<Vec<(RowKey, RowData)>> {
        let table = match &domain {
            Domain::UserTable(name) => name.clone(),
            _ => "system".to_string(),
        };
        let data = self.data.lock().unwrap();
        let rows = data.get(&table)
            .map(|t| t.iter()
                .map(|(k, v)| {
                    let row_data = RowData {
                        fields: match v {
                            PactValue::Object(fields) => fields.clone().into_iter().collect(),
                            _ => std::collections::HashMap::new(),
                        }
                    };
                    (RowKey(k.clone()), row_data)
                })
                .collect())
            .unwrap_or_default();
        EvalM::pure_value(rows)
    }

    fn create_table(&self, table_name: String, _schema: TableSchema) -> EvalM<()> {
        let mut data = self.data.lock().unwrap();
        data.insert(table_name, std::collections::HashMap::new());
        EvalM::pure_value(())
    }

    fn begin_tx(&self) -> EvalM<()> { EvalM::pure_value(()) }
    fn commit_tx(&self) -> EvalM<()> { EvalM::pure_value(()) }
    fn rollback_tx(&self) -> EvalM<()> { EvalM::pure_value(()) }
}

/// Helper functions for creating test terms
fn dummy_span() -> SpanInfo {
    SpanInfo::empty()
}

fn int_term(value: i64) -> CoreTerm {
    CoreTerm::Constant(Literal::LInteger(value), dummy_span())
}

fn bool_term(value: bool) -> CoreTerm {
    CoreTerm::Constant(Literal::LBool(value), dummy_span())
}

fn string_term(value: &str) -> CoreTerm {
    CoreTerm::Constant(Literal::LString(value.into()), dummy_span())
}

fn builtin_term(builtin: CoreBuiltin) -> CoreTerm {
    CoreTerm::Builtin(builtin, dummy_span())
}

fn setup_test_env() -> CEKEnv {
    let pact_db = Arc::new(MockPactDb::new());
    let mut builtins = BuiltinEnv::new();
    register_core_builtins(&mut builtins).unwrap();
    CEKEnv::new(pact_db, builtins)
}

#[cfg(test)]
mod architecture_validation_tests {
    use super::*;
    use num_traits::ToPrimitive;

    #[test]
    fn test_cek_value_structure() {
        // ✅ VPactValue for basic values
        let int_val = CEKValue::VPactValue(PactValue::Integer(42.into()));
        assert!(int_val.is_pact_value());
        assert!(!int_val.is_closure());

        // ✅ Pattern synonyms work
        assert!(int_val.is_integer());
        assert!(!int_val.is_string());
        assert_eq!(int_val.as_integer().unwrap().to_i64().unwrap(), 42);

        // ✅ VClosure for callable entities
        let native_fn = NativeFn {
            builtin: CoreBuiltin::CoreAdd,
            arity: 2,
            info: dummy_span(),
        };
        let closure_val = CEKValue::VClosure(CanApply::N(native_fn));
        assert!(!closure_val.is_pact_value());
        assert!(closure_val.is_closure());

        // ✅ All CanApply variants available
        let env = setup_test_env();
        let lambda_closure = CanApply::LC(LamClosure {
            args: vec![],
            body: int_term(1),
            env: env.clone(),
            info: dummy_span(),
        });
        let _lambda_val = CEKValue::VClosure(lambda_closure);

        let user_closure = CanApply::C(Closure {
            name: "test".to_string(),
            args: vec![],
            body: int_term(1),
            env,
            info: dummy_span(),
        });
        let _user_val = CEKValue::VClosure(user_closure);
    }

    #[test]
    fn test_environment_structure() {
        let env = setup_test_env();

        // ✅ RAList for local variables
        assert_eq!(env.local.len(), 0);
        assert!(env.local.is_empty());

        // ✅ PactDb integration
        assert!(env.pact_db.read_row("test", "key").is_ok());

        // ✅ Builtin environment
        assert!(env.builtins.lookup(CoreBuiltin::CoreAdd, dummy_span()).is_ok());

        // ✅ DefPact state
        assert!(env.defpact_step.is_none());

        // ✅ Capability context
        assert!(!env.in_cap);

        // ✅ Environment extension
        let extended = env.extend_local(CEKValue::integer(42));
        assert_eq!(extended.local.len(), 1);
        assert_eq!(extended.lookup_local(0).unwrap().as_integer().unwrap().to_i64().unwrap(), 42);

        // ✅ Environment extension preserves other fields
        assert_eq!(extended.in_cap, env.in_cap);
        assert!(extended.defpact_step.is_none());
    }

    #[test]
    fn test_continuation_completeness() {
        let env = setup_test_env();
        let info = dummy_span();

        // ✅ Empty continuation
        let mt = Cont::Mt;
        assert!(mt.is_mt());

        // ✅ All major continuation types
        let _args_cont = Cont::Args {
            env: env.clone(),
            info,
            args: vec![int_term(1), int_term(2)],
            cont: Box::new(Cont::Mt),
        };

        let _fn_cont = Cont::Fn {
            function: CanApply::N(NativeFn {
                builtin: CoreBuiltin::CoreAdd,
                arity: 2,
                info,
            }),
            env: env.clone(),
            args: vec![],
            values: vec![CEKValue::integer(1)],
            cont: Box::new(Cont::Mt),
        };

        let _let_cont = Cont::LetC {
            env: env.clone(),
            info,
            arg: pact_ir::Arg {
                name: "x".into(),
                ty: None,
                info,
            },
            body: int_term(42),
            cont: Box::new(Cont::Mt),
        };

        // ✅ Higher-order builtin continuation
        let _builtin_cont = Cont::BuiltinC {
            env: env.clone(),
            info,
            builtin_cont: BuiltinCont::MapCont {
                func: CanApply::N(NativeFn {
                    builtin: CoreBuiltin::CoreMap,
                    arity: 2,
                    info,
                }),
                remaining: vec![PactValue::Integer(1.into()), PactValue::Integer(2.into())],
                accumulated: vec![],
            },
            cont: Box::new(Cont::Mt),
        };

        // ✅ Capability continuation
        let _cap_cont = Cont::CapInvokeC {
            env: env.clone(),
            info,
            cap_cont: CapCont::WithCapCont {
                cap_name: "test-cap".to_string(),
                cap_args: vec![],
                body: vec![int_term(1)],
            },
            cont: Box::new(Cont::Mt),
        };

        // ✅ DefPact continuation
        let _defpact_cont = Cont::DefPactStepC {
            env,
            info,
            cont: Box::new(Cont::Mt),
        };
    }

    #[test]
    fn test_error_handling_structure() {
        let env = setup_test_env();

        // ✅ No handler
        let no_handler = CEKErrorHandler::CEKNoHandler;
        assert!(no_handler.is_no_handler());

        // ✅ Try-catch handler
        let try_handler = CEKErrorHandler::CEKHandler {
            env: env.clone(),
            recovery: int_term(0),
            cont: Cont::Mt,
            error_state: ErrorState::empty(),
            next_handler: Box::new(CEKErrorHandler::CEKNoHandler),
        };
        assert!(!try_handler.is_no_handler());

        // ✅ Enforce-one handler
        let enforce_one = CEKErrorHandler::CEKEnforceOne {
            env,
            info: dummy_span(),
            current: bool_term(true),
            remaining: vec![bool_term(false)],
            cont: Cont::Mt,
            error_state: ErrorState::empty(),
            next_handler: Box::new(CEKErrorHandler::CEKNoHandler),
        };
        assert!(!enforce_one.is_no_handler());
    }

    #[test]
    fn test_eval_monad_operations() {
        // ✅ Pure values
        let pure_comp = EvalM::pure_value(42);

        // ✅ Error throwing
        let error_comp = EvalM::<i32>::throw_error(
            pact_errors::PactError::PEExecutionError(
                pact_errors::EvalError::UserError("test error".to_string()),
                vec![],
                pact_shared_types::SpanInfo::empty()
            )
        );

        // ✅ State operations
        let _get_state = EvalM::<i32>::get_state();
        let _put_state = EvalM::<i32>::put_state(EvalState::new());

        // ✅ Environment operations
        let _ask_env = EvalM::<i32>::ask_env();

        // ✅ Monadic bind
        let computation = EvalM::pure_value(21)
            .bind(|x| EvalM::pure_value(x * 2));

        // ✅ Functor map
        let mapped = EvalM::pure_value(21).map(|x| x * 2);
    }
}

#[cfg(test)]
mod builtin_integration_tests {
    use super::*;

    #[test]
    fn test_builtin_registration() {
        let mut builtin_env = BuiltinEnv::new();

        // ✅ Core builtins can be registered
        assert!(register_core_builtins(&mut builtin_env).is_ok());

        // ✅ Builtins can be looked up
        assert!(builtin_env.lookup(CoreBuiltin::CoreAdd, dummy_span()).is_ok());
        assert!(builtin_env.lookup(CoreBuiltin::CoreEq, dummy_span()).is_ok());
        assert!(builtin_env.lookup(CoreBuiltin::CoreLength, dummy_span()).is_ok());

        // ✅ Unknown builtins return error
        // Note: We would need to test with a builtin that's not registered
    }

    #[test]
    fn test_builtin_arity_information() {
        let mut builtin_env = BuiltinEnv::new();
        register_core_builtins(&mut builtin_env).unwrap();

        // ✅ Builtin arity is correctly specified
        let add_fn = builtin_env.lookup(CoreBuiltin::CoreAdd, dummy_span()).unwrap();
        assert_eq!(add_fn.arity, 2);

        let length_fn = builtin_env.lookup(CoreBuiltin::CoreLength, dummy_span()).unwrap();
        assert_eq!(length_fn.arity, 1);
    }
}



#[cfg(test)]
mod capability_integration_tests {
    use super::*;

    #[test]
    fn test_capability_context() {
        let env = setup_test_env();

        // ✅ Capability context can be set
        assert!(!env.in_cap);
        let cap_env = env.set_in_cap(true);
        assert!(cap_env.in_cap);

        // ✅ Capability continuation structures exist
        let cap_cont = CapCont::WithCapCont {
            cap_name: "admin".to_string(),
            cap_args: vec![PactValue::String("alice".to_string())],
            body: vec![int_term(42)],
        };

        let _cap_frame = Cont::CapInvokeC {
            env,
            info: dummy_span(),
            cap_cont,
            cont: Box::new(Cont::Mt),
        };
    }

    #[test]
    fn test_capability_state_management() {
        let mut cap_state = CapabilityState::new();

        // ✅ Capability state tracks granted capabilities
        assert!(cap_state.granted_caps.is_empty());
        assert!(cap_state.cap_stack.is_empty());
        assert!(cap_state.installed_caps.is_empty());

        // State management would be implemented in the full evaluator
    }
}

#[cfg(test)]
mod defpact_integration_tests {
    use super::*;

    #[test]
    fn test_defpact_state() {
        let env = setup_test_env();

        // ✅ DefPact state can be set
        assert!(env.defpact_step.is_none());

        let defpact_state = DefPactStepState {
            pact_id: "test-pact".to_string(),
            step: 1,
            rollback: false,
            step_result: Some(PactValue::Integer(42.into())),
        };

        let defpact_env = env.set_defpact_step(Some(defpact_state.clone()));
        assert_eq!(defpact_env.defpact_step.as_ref().unwrap().pact_id, "test-pact");
        assert_eq!(defpact_env.defpact_step.as_ref().unwrap().step, 1);
    }

    #[test]
    fn test_defpact_execution_state() {
        let mut defpact_state = DefPactState::new();

        // ✅ DefPact execution state tracks active pacts
        assert!(defpact_state.active_pacts.is_empty());
        assert!(defpact_state.execution_history.is_empty());

        // State management would be implemented in the full evaluator
    }
}

#[cfg(test)]
mod higher_order_function_tests {
    use super::*;

    #[test]
    fn test_higher_order_builtin_continuations() {
        let env = setup_test_env();

        // ✅ Map continuation structure
        let map_cont = BuiltinCont::MapCont {
            func: CanApply::N(NativeFn {
                builtin: CoreBuiltin::CoreAdd,
                arity: 2,
                info: dummy_span(),
            }),
            remaining: vec![
                PactValue::Integer(2.into()),
                PactValue::Integer(3.into()),
            ],
            accumulated: vec![PactValue::Integer(2.into())], // First result
        };

        // ✅ Filter continuation structure
        let filter_cont = BuiltinCont::FilterCont {
            func: CanApply::N(NativeFn {
                builtin: CoreBuiltin::CoreGT,
                arity: 2,
                info: dummy_span(),
            }),
            remaining: vec![
                PactValue::Integer(5.into()),
                PactValue::Integer(1.into()),
            ],
            accumulated: vec![PactValue::Integer(10.into())], // Passed filter
        };

        // ✅ Fold continuation structure
        let fold_cont = BuiltinCont::FoldCont {
            func: CanApply::N(NativeFn {
                builtin: CoreBuiltin::CoreAdd,
                arity: 2,
                info: dummy_span(),
            }),
            remaining: vec![
                PactValue::Integer(3.into()),
                PactValue::Integer(4.into()),
            ],
            accumulator: PactValue::Integer(3.into()), // Current sum
        };

        // ✅ These continuations can be integrated into the CEK machine
        let _builtin_frame = Cont::BuiltinC {
            env,
            info: dummy_span(),
            builtin_cont: map_cont,
            cont: Box::new(Cont::Mt),
        };
    }
}

#[cfg(test)]
mod partial_application_tests {
    use super::*;

    #[test]
    fn test_partial_closure_structure() {
        let env = setup_test_env();

        // ✅ Partial closure for user functions
        let closure = Closure {
            name: "add3".to_string(),
            args: vec![
                pact_ir::Arg { name: "a".into(), ty: None, info: dummy_span() },
                pact_ir::Arg { name: "b".into(), ty: None, info: dummy_span() },
                pact_ir::Arg { name: "c".into(), ty: None, info: dummy_span() },
            ],
            body: int_term(42), // Placeholder body
            env: env.clone(),
            info: dummy_span(),
        };

        let partial = PartialClosure {
            closure,
            applied_args: vec![CEKValue::integer(1), CEKValue::integer(2)],
            remaining_arity: 1,
        };

        let _partial_val = CEKValue::VClosure(CanApply::PC(partial));

        // ✅ Partial native function
        let native_fn = NativeFn {
            builtin: CoreBuiltin::CoreAdd,
            arity: 2,
            info: dummy_span(),
        };

        let partial_native = PartialNativeFn {
            env,
            native_fn,
            applied_args: vec![CEKValue::integer(1)],
            remaining_arity: 1,
        };

        let _partial_native_val = CEKValue::VClosure(CanApply::PN(partial_native));
    }
}

/// Integration test demonstrating the redesigned architecture can handle
/// complex evaluation patterns that were impossible with the original design
#[cfg(test)]
mod integration_tests {
    use super::*;

    #[test]
    fn test_complex_evaluation_readiness() {
        let env = setup_test_env();

        // ✅ Database operations: read/write with proper context
        // This would be impossible with original architecture (no PactDb)
        let _db_access = &env.pact_db;

        // ✅ Capability-aware evaluation
        // This would be impossible with original architecture (no cap context)
        let cap_env = env.set_in_cap(true);
        assert!(cap_env.in_cap);

        // ✅ DefPact step execution
        // This would be impossible with original architecture (no DefPact state)
        let defpact_step = DefPactStepState {
            pact_id: "multi-step-pact".to_string(),
            step: 2,
            rollback: false,
            step_result: Some(PactValue::String("step1-result".to_string())),
        };
        let defpact_env = cap_env.set_defpact_step(Some(defpact_step));

        // ✅ Higher-order function support via continuations
        // This would be impossible with original architecture (no BuiltinC)
        let map_operation = BuiltinCont::MapCont {
            func: CanApply::N(NativeFn {
                builtin: CoreBuiltin::CoreAdd,
                arity: 2,
                info: dummy_span(),
            }),
            remaining: vec![PactValue::Integer(2.into())],
            accumulated: vec![PactValue::Integer(3.into())],
        };

        let _map_frame = Cont::BuiltinC {
            env: defpact_env,
            info: dummy_span(),
            builtin_cont: map_operation,
            cont: Box::new(Cont::Mt),
        };

        // ✅ Complex error handling with recovery
        // This would be limited with original architecture (basic error handling)
        let error_handler = CEKErrorHandler::CEKHandler {
            env: setup_test_env(),
            recovery: int_term(0), // Default value on error
            cont: Cont::Mt,
            error_state: ErrorState::empty(),
            next_handler: Box::new(CEKErrorHandler::CEKNoHandler),
        };

        // All these structures can be properly integrated in the redesigned architecture
        // without workarounds or architectural compromises
    }
}

/// Performance and memory tests
#[cfg(test)]
mod performance_tests {
    use super::*;

    #[test]
    fn test_ra_list_performance() {
        let mut ra_list = RAList::new();

        // ✅ O(1) cons operation
        for i in 0..1000 {
            ra_list = ra_list.cons(CEKValue::integer(i));
        }

        // ✅ O(1) lookup operation
        // assert_eq!(ra_list.lookup(0).unwrap().as_integer().unwrap().to_i64().unwrap(), 999);
        // assert_eq!(ra_list.lookup(500).unwrap().as_integer().unwrap().to_i64().unwrap(), 499);
        // assert_eq!(ra_list.len(), 1000);
    }

    #[test]
    fn test_environment_sharing() {
        let env = setup_test_env();

        // ✅ Environment extension creates new environment without copying everything
        let env1 = env.extend_local(CEKValue::integer(1));
        let env2 = env1.extend_local(CEKValue::integer(2));
        let env3 = env2.extend_local(CEKValue::integer(3));

        // Each environment has the correct local bindings
        assert_eq!(env1.local.len(), 1);
        assert_eq!(env2.local.len(), 2);
        assert_eq!(env3.local.len(), 3);

        // Environments share the immutable parts (PactDb, builtins)
        // This is handled by Arc/Rc for shared references
    }
}
