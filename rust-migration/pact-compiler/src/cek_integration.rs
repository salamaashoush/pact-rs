//! CEK Machine Integration
//!
//! This module provides the integration between the compiler pipeline and the CEK evaluator,
//! following the Haskell implementation pattern with proper module storage.

use pact_values::PactValue;
use pact_errors::PactError;
use pact_ir::{TopLevel, CoreTerm};
use pact_cek::CEKValue;
use crate::orchestration::CompilationContext;
use crate::module_storage::ModuleStorageManager;
use pact_db::MockDb;
use std::sync::Arc;

/// Evaluate a compiled top-level expression using the CEK machine with proper module storage
pub fn evaluate_with_cek(
    top_level: &TopLevel<pact_ir::Name, pact_ir::Type, pact_ir::CoreBuiltin, pact_parser::SpanInfo>,
    ctx: &mut CompilationContext,
) -> Result<PactValue, PactError> {
    evaluate_with_cek_and_storage(top_level, ctx, None)
}

/// Evaluate with explicit module storage (for advanced use cases)
pub fn evaluate_with_cek_and_storage(
    top_level: &TopLevel<pact_ir::Name, pact_ir::Type, pact_ir::CoreBuiltin, pact_parser::SpanInfo>,
    _ctx: &mut CompilationContext,
    storage_manager: Option<ModuleStorageManager>,
) -> Result<PactValue, PactError> {
    use pact_cek::{CEKEnv, BuiltinEnv, CEKValue, EvalResult, eval_cek, Cont, CEKErrorHandler};
    use pact_cek::{EvalM, EvalMEnv, EvalState};
    
    // Create or use provided storage manager
    let storage = storage_manager.unwrap_or_else(|| {
        let db = Arc::new(MockDb::new());
        ModuleStorageManager::new(db)
    });
    
    match top_level {
        TopLevel::TLModule(_module) => {
            // Module loading with proper storage
            let result = storage.process_compilation_result(top_level, "<source>")?;
            Ok(PactValue::String(result))
        }
        TopLevel::TLInterface(_interface) => {
            // Interface loading with proper storage
            let result = storage.process_compilation_result(top_level, "<source>")?;
            Ok(PactValue::String(result))
        }
        TopLevel::TLTerm(term) => {
            // Convert IR term to CEK term and evaluate with module context
            evaluate_core_term_with_storage(term, &storage)
        }
        TopLevel::TLUse(_import) => {
            // Import handling with proper dependency resolution
            let result = storage.process_compilation_result(top_level, "<source>")?;
            Ok(PactValue::String(result))
        }
    }
}

/// Evaluate a Core IR term using the CEK machine with module storage context
fn evaluate_core_term_with_storage(term: &CoreTerm, storage: &ModuleStorageManager) -> Result<PactValue, PactError> {
    use pact_cek::{CEKEnv, BuiltinEnv, CEKValue, EvalResult, eval_cek, Cont, CEKErrorHandler};
    use pact_cek::{EvalM, EvalMEnv, EvalState, register_core_builtins};
    use std::sync::Arc;
    
    // Use the storage manager's database for module loading
    let db = storage.db.clone();
    
    // Create builtin environment and register core builtins
    let mut builtins = BuiltinEnv::new();
    register_core_builtins(&mut builtins)?;
    
    // Create CEK environment
    let cek_env = CEKEnv::new(db, builtins);
    
    // Create empty continuation and error handler
    let cont = Cont::Mt;
    let handler = CEKErrorHandler::CEKNoHandler;
    
    // Create evaluation environment and state
    let eval_env = EvalMEnv::default();
    let eval_state = EvalState::new();
    
    // Run CEK evaluation
    let computation = eval_cek(cont, handler, cek_env, term.clone());
    
    match computation.run(eval_env, eval_state) {
        Ok((EvalResult::EvalValue(cek_value), _state)) => {
            // Convert CEK value back to PactValue
            cek_value_to_pact_value(cek_value)
        }
        Ok((EvalResult::EvalError(error), _state)) => {
            Err(error)
        }
        Err(error) => {
            Err(error)
        }
    }
}

/// Simple mock database for basic CEK integration
#[derive(Debug)]
struct SimpleMockDb;

impl pact_cek::PactDb for SimpleMockDb {
    fn read(&self, _domain: pact_cek::Domain, _key: pact_cek::RowKey) -> pact_cek::EvalM<Option<pact_cek::RowData>> {
        pact_cek::EvalM::pure_value(None)
    }

    fn write(&self, _domain: pact_cek::Domain, _key: pact_cek::RowKey, _data: pact_cek::RowData) -> pact_cek::EvalM<()> {
        pact_cek::EvalM::pure_value(())
    }

    fn keys(&self, _domain: pact_cek::Domain) -> pact_cek::EvalM<Vec<pact_cek::RowKey>> {
        pact_cek::EvalM::pure_value(vec![])
    }

    fn select(&self, _domain: pact_cek::Domain, _filter: Option<pact_cek::EvalM<bool>>) -> pact_cek::EvalM<Vec<(pact_cek::RowKey, pact_cek::RowData)>> {
        pact_cek::EvalM::pure_value(vec![])
    }

    fn create_table(&self, _table_name: String, _schema: pact_cek::TableSchema) -> pact_cek::EvalM<()> {
        pact_cek::EvalM::pure_value(())
    }

    fn begin_tx(&self) -> pact_cek::EvalM<()> {
        pact_cek::EvalM::pure_value(())
    }

    fn commit_tx(&self) -> pact_cek::EvalM<()> {
        pact_cek::EvalM::pure_value(())
    }

    fn rollback_tx(&self) -> pact_cek::EvalM<()> {
        pact_cek::EvalM::pure_value(())
    }
}

/// Convert CEK value to PactValue
fn cek_value_to_pact_value(cek_value: CEKValue) -> Result<PactValue, PactError> {
    match cek_value {
        CEKValue::VPactValue(pact_value) => Ok(pact_value),
        CEKValue::VClosure(_) => {
            Ok(PactValue::String("<function>".to_string()))
        }
        CEKValue::VTable { name, .. } => {
            Ok(PactValue::String(format!("<table:{}>", name)))
        }
    }
}