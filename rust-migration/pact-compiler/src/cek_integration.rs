//! CEK Machine Integration
//!
//! This module provides the integration between the compiler pipeline and the CEK evaluator,
//! following the Haskell implementation pattern with proper module storage.

use pact_values::PactValue;
use pact_errors::PactError;
use pact_ir::{TopLevel, CoreTerm};
use pact_cek::CEKValue;
use pact_parser::SpanInfo;
use crate::orchestration::CompilationContext;
use crate::module_storage::ModuleStorageManager;
use pact_db::MockDb;
use std::sync::Arc;

/// Adapter to make pact_db::PactDb work with pact_cek::PactDb
#[derive(Debug, Clone)]
struct CekDbAdapter {
    db: Arc<dyn pact_db::PactDb>,
}

impl pact_cek::PactDb for CekDbAdapter {
    fn read(&self, domain: pact_cek::Domain, key: pact_cek::RowKey) -> pact_cek::EvalM<Option<pact_cek::RowData>> {
        use pact_cek::EvalM;
        // Convert from pact_cek types to pact_db types
        let db_domain = match domain {
            pact_cek::Domain::User(table) => pact_db::Domain::User(pact_db::TableName::new(table.name)),
            pact_cek::Domain::Modules => pact_db::Domain::Modules,
            pact_cek::Domain::KeySets => pact_db::Domain::KeySets,
            pact_cek::Domain::Namespaces => pact_db::Domain::Namespaces,
            pact_cek::Domain::Pacts => pact_db::Domain::Pacts,
        };
        
        let db_key = pact_db::RowKey::from(key.0);
        
        EvalM::try_from_io(|| {
            self.db.read_row(&pact_db::TableName::new(match &domain {
                pact_cek::Domain::User(t) => t.name.clone(),
                _ => "".to_string()
            }), &db_key)
                .map(|opt| opt.map(|row_data| {
                    // Convert pact_db::RowData to pact_cek::RowData
                    let mut cek_row = std::collections::HashMap::new();
                    for (k, v) in row_data.fields() {
                        cek_row.insert(k.clone(), v.clone());
                    }
                    pact_cek::RowData::new(cek_row)
                }))
                .map_err(|e| format!("Database read error: {}", e))
        })
    }
    
    fn write(&self, domain: pact_cek::Domain, key: pact_cek::RowKey, data: pact_cek::RowData) -> pact_cek::EvalM<()> {
        self.write_with_type(domain, key, data, pact_cek::WriteType::Write)
    }
    
    fn write_with_type(&self, domain: pact_cek::Domain, key: pact_cek::RowKey, data: pact_cek::RowData, write_type: pact_cek::WriteType) -> pact_cek::EvalM<()> {
        use pact_cek::EvalM;
        // Convert types
        let db_domain = match domain {
            pact_cek::Domain::User(table) => pact_db::Domain::User(pact_db::TableName::new(table.name)),
            pact_cek::Domain::Modules => pact_db::Domain::Modules,
            pact_cek::Domain::KeySets => pact_db::Domain::KeySets,
            pact_cek::Domain::Namespaces => pact_db::Domain::Namespaces,
            pact_cek::Domain::Pacts => pact_db::Domain::Pacts,
        };
        
        let db_key = pact_db::RowKey::from(key.0);
        let db_write_type = match write_type {
            pact_cek::WriteType::Write => pact_db::WriteType::Write,
            pact_cek::WriteType::Insert => pact_db::WriteType::Insert,
            pact_cek::WriteType::Update => pact_db::WriteType::Update,
        };
        
        // Convert RowData
        let db_row_data = pact_db::RowData::from_map(data.fields().clone());
        
        EvalM::try_from_io(|| {
            self.db.write_row(db_write_type, &pact_db::TableName::new(match &domain {
                pact_cek::Domain::User(t) => t.name.clone(),
                _ => "".to_string()
            }), &db_key, &db_row_data)
                .map_err(|e| format!("Database write error: {}", e))
        })
    }
    
    fn keys(&self, domain: pact_cek::Domain) -> pact_cek::EvalM<Vec<pact_cek::RowKey>> {
        use pact_cek::EvalM;
        let table_name = match &domain {
            pact_cek::Domain::User(t) => pact_db::TableName::new(t.name.clone()),
            _ => pact_db::TableName::new("".to_string()),
        };
        
        EvalM::try_from_io(|| {
            self.db.table_keys(&table_name)
                .map(|keys| keys.into_iter().map(|k| pact_cek::RowKey(k.to_string())).collect())
                .map_err(|e| format!("Database keys error: {}", e))
        })
    }
    
    fn select(&self, domain: pact_cek::Domain, _filter: Option<pact_cek::EvalM<bool>>) -> pact_cek::EvalM<Vec<(pact_cek::RowKey, pact_cek::RowData)>> {
        use pact_cek::EvalM;
        let table_name = match &domain {
            pact_cek::Domain::User(t) => pact_db::TableName::new(t.name.clone()),
            _ => pact_db::TableName::new("".to_string()),
        };
        
        EvalM::try_from_io(|| {
            self.db.select_rows(&table_name, &pact_db::SelectCriteria::new())
                .map(|rows| {
                    rows.into_iter().map(|(k, v)| {
                        let mut cek_row = std::collections::HashMap::new();
                        for (field_k, field_v) in v.fields() {
                            cek_row.insert(field_k.clone(), field_v.clone());
                        }
                        (pact_cek::RowKey(k.to_string()), pact_cek::RowData::new(cek_row))
                    }).collect()
                })
                .map_err(|e| format!("Database select error: {}", e))
        })
    }
    
    fn create_table(&self, table: pact_cek::TableName, _schema: pact_schema::Schema) -> pact_cek::EvalM<()> {
        use pact_cek::EvalM;
        EvalM::try_from_io(|| {
            self.db.create_user_table(&pact_db::TableName::new(table.name), &pact_schema::Schema::default())
                .map_err(|e| format!("Create table error: {}", e))
        })
    }
    
    fn describe_table(&self, table: pact_cek::TableName) -> pact_cek::EvalM<Option<pact_schema::Schema>> {
        use pact_cek::EvalM;
        EvalM::try_from_io(|| {
            self.db.get_table_schema(&pact_db::TableName::new(table.name))
                .map_err(|e| format!("Describe table error: {}", e))
        })
    }
    
    fn table_exists(&self, table: pact_cek::TableName) -> pact_cek::EvalM<bool> {
        use pact_cek::EvalM;
        EvalM::try_from_io(|| {
            self.db.table_exists(&pact_db::TableName::new(table.name))
                .map_err(|e| format!("Table exists error: {}", e))
        })
    }
    
    fn tx_log(&self, _domain: pact_cek::Domain, _tx_id: pact_cek::TxId) -> pact_cek::EvalM<Vec<pact_cek::TxLog>> {
        use pact_cek::EvalM;
        // For now, return empty log
        EvalM::pure(vec![])
    }
    
    fn tx_ids(&self, _domain: pact_cek::Domain, _tx_id: pact_cek::TxId) -> pact_cek::EvalM<Vec<pact_cek::TxId>> {
        use pact_cek::EvalM;
        // For now, return empty list
        EvalM::pure(vec![])
    }
    
    fn rollback_tx(&self) -> pact_cek::EvalM<()> {
        use pact_cek::EvalM;
        EvalM::try_from_io(|| {
            self.db.rollback_tx()
                .map_err(|e| format!("Rollback error: {}", e))
        })
    }
    
    fn commit_tx(&self) -> pact_cek::EvalM<()> {
        use pact_cek::EvalM;
        EvalM::try_from_io(|| {
            self.db.commit_tx()
                .map(|_| ())
                .map_err(|e| format!("Commit error: {}", e))
        })
    }
    
    fn begin_tx(&self, mode: pact_cek::ExecutionMode) -> pact_cek::EvalM<Option<pact_cek::TxId>> {
        use pact_cek::EvalM;
        let db_mode = match mode {
            pact_cek::ExecutionMode::Local => pact_db::ExecutionMode::Local,
            pact_cek::ExecutionMode::Transactional => pact_db::ExecutionMode::Transactional,
        };
        
        EvalM::try_from_io(|| {
            self.db.begin_tx(db_mode)
                .map(|opt| opt.map(|id| pact_cek::TxId(id.0)))
                .map_err(|e| format!("Begin tx error: {}", e))
        })
    }
}

/// Evaluate a compiled top-level expression using the CEK machine with proper module storage
pub fn evaluate_with_cek(
    top_level: &TopLevel<pact_ir::Name, pact_ir::Type, pact_ir::CoreBuiltin, pact_parser::SpanInfo>,
    ctx: &mut CompilationContext,
) -> Result<PactValue, PactError<SpanInfo>> {
    evaluate_with_cek_and_storage(top_level, ctx, None)
}

/// Evaluate with explicit module storage (for advanced use cases)
pub fn evaluate_with_cek_and_storage(
    top_level: &TopLevel<pact_ir::Name, pact_ir::Type, pact_ir::CoreBuiltin, pact_parser::SpanInfo>,
    _ctx: &mut CompilationContext,
    storage_manager: Option<ModuleStorageManager>,
) -> Result<PactValue, PactError<SpanInfo>> {
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
fn evaluate_core_term_with_storage(term: &CoreTerm, storage: &ModuleStorageManager) -> Result<PactValue, PactError<SpanInfo>> {
    use pact_cek::{CEKEnv, BuiltinEnv, CEKValue, EvalResult, eval_cek, Cont, CEKErrorHandler};
    use pact_cek::{EvalM, EvalMEnv, EvalState, register_core_builtins};
    use std::sync::Arc;
    
    // Use the storage manager's database for module loading
    let db = storage.db.clone();
    
    // Create builtin environment and register core builtins
    let mut builtins = BuiltinEnv::new();
    register_core_builtins(&mut builtins)?;
    
    // Create CEK environment with adapter
    let adapter = CekDbAdapter { db };
    let cek_env = CEKEnv::new(Arc::new(adapter), builtins);
    
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
fn cek_value_to_pact_value(cek_value: CEKValue) -> Result<PactValue, PactError<SpanInfo>> {
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