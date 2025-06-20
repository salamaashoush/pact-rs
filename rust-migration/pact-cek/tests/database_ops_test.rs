/// Integration tests for database operations
/// Tests the newly implemented database functions to ensure they match Haskell behavior
use pact_cek::{
    database_ops, BuiltinEnv, CEKEnv, CEKValue, PactDb, Domain, RowKey, RowData,
    EvalM, EvalResult, Cont, CEKErrorHandler,
};
use pact_values::PactValue;
use pact_shared_types::SpanInfo;
use pact_ir::CoreBuiltin;
use std::sync::Arc;
use std::collections::HashMap;

/// Mock database for testing
#[derive(Debug, Clone)]
struct MockDb {
    data: Arc<std::sync::Mutex<HashMap<(Domain, RowKey), RowData>>>,
}

impl MockDb {
    fn new() -> Self {
        MockDb {
            data: Arc::new(std::sync::Mutex::new(HashMap::new())),
        }
    }

    fn with_keyset(keyset_name: &str, keyset_data: PactValue) -> Self {
        let db = Self::new();
        let mut data = db.data.lock().unwrap();
        let mut row = RowData::new();
        row.insert("keyset".to_string(), keyset_data);
        data.insert((Domain::KeysetTable, RowKey(keyset_name.to_string())), row);
        drop(data);
        db
    }

    fn with_module(module_name: &str, module_data: HashMap<String, PactValue>) -> Self {
        let db = Self::new();
        let mut data = db.data.lock().unwrap();
        let mut row = RowData::new();
        for (k, v) in module_data {
            row.insert(k, v);
        }
        data.insert((Domain::ModuleTable, RowKey(module_name.to_string())), row);
        drop(data);
        db
    }

    fn with_table_row(table_name: &str, row_key: &str, row_data: HashMap<String, PactValue>) -> Self {
        let db = Self::new();
        let mut data = db.data.lock().unwrap();
        let mut row = RowData::new();
        for (k, v) in row_data {
            row.insert(k, v);
        }
        data.insert((Domain::UserTable(table_name.to_string()), RowKey(row_key.to_string())), row);
        drop(data);
        db
    }
}

impl PactDb for MockDb {
    fn read(&self, domain: Domain, key: RowKey) -> EvalM<Option<RowData>> {
        let data = self.data.lock().unwrap();
        let result = data.get(&(domain, key)).cloned();
        EvalM::pure_value(result)
    }

    fn write(&self, domain: Domain, key: RowKey, data: RowData) -> EvalM<()> {
        let mut store = self.data.lock().unwrap();
        store.insert((domain, key), data);
        EvalM::pure_value(())
    }

    fn keys(&self, _domain: Domain) -> EvalM<Vec<RowKey>> {
        EvalM::pure_value(vec![])
    }

    fn select(&self, _domain: Domain, _filter: Option<EvalM<bool>>) -> EvalM<Vec<(RowKey, RowData)>> {
        EvalM::pure_value(vec![])
    }

    fn create_table(&self, _table_name: String, _schema: pact_cek::TableSchema) -> EvalM<()> {
        EvalM::pure_value(())
    }

    fn begin_tx(&self) -> EvalM<()> {
        EvalM::pure_value(())
    }

    fn commit_tx(&self) -> EvalM<()> {
        EvalM::pure_value(())
    }

    fn rollback_tx(&self) -> EvalM<()> {
        EvalM::pure_value(())
    }
}

fn setup_test_env(db: MockDb) -> (Arc<BuiltinEnv>, CEKEnv) {
    let mut builtin_env = BuiltinEnv::new();
    database_ops::register_database_builtins(&mut builtin_env).unwrap();
    
    // Create CEKEnv first (it takes ownership of builtin_env)
    let env = CEKEnv::new(Arc::new(db), builtin_env);
    
    // Get the builtin env reference from the CEKEnv
    let builtin_env_ref = env.builtins.clone();
    (builtin_env_ref, env)
}

fn dummy_span() -> SpanInfo {
    SpanInfo::empty()
}

fn dummy_cont() -> Cont {
    Cont::Mt
}

fn dummy_handler() -> CEKErrorHandler {
    CEKErrorHandler::CEKNoHandler
}

/// Helper to run EvalM computation for tests
fn run_evalm<T>(computation: EvalM<T>) -> Result<T, pact_errors::PactErrorI> {
    let eval_env = pact_cek::EvalMEnv::default();
    let eval_state = pact_cek::EvalState::default();
    
    computation.run(eval_env, eval_state).map(|(result, _state)| result)
}

#[cfg(test)]
mod database_ops_tests {
    use super::*;

    #[test]
    fn test_describe_keyset_success() {
        // Setup mock database with a keyset
        let keyset_data = PactValue::String("test-keyset-data".to_string());
        let db = MockDb::with_keyset("my-keyset", keyset_data);
        let (builtin_env, env) = setup_test_env(db);

        // Get the describe-keyset implementation
        let implementation = builtin_env.get_implementation(CoreBuiltin::CoreDescribeKeyset).unwrap();

        // Call describe-keyset with the keyset name
        let args = vec![CEKValue::VPactValue(PactValue::String("my-keyset".to_string()))];
        
        let result = implementation(
            dummy_span(),
            CoreBuiltin::CoreDescribeKeyset,
            dummy_cont(),
            dummy_handler(),
            env,
            args,
        );

        // Run the computation
        match run_evalm(result) {
            Ok(eval_result) => {
                match eval_result {
                    EvalResult::EvalValue(CEKValue::VPactValue(PactValue::String(s))) => {
                        assert!(s.contains("Keyset"), "Result should contain keyset data");
                    }
                    _ => panic!("Expected string result, got: {:?}", eval_result),
                }
            }
            Err(e) => panic!("Unexpected error: {:?}", e),
        }
    }

    #[test]
    fn test_describe_keyset_not_found() {
        // Setup empty mock database
        let db = MockDb::new();
        let (builtin_env, env) = setup_test_env(db);

        // Get the describe-keyset implementation
        let implementation = builtin_env.get_implementation(CoreBuiltin::CoreDescribeKeyset).unwrap();

        // Call describe-keyset with non-existent keyset
        let args = vec![CEKValue::VPactValue(PactValue::String("non-existent".to_string()))];
        
        let result = implementation(
            dummy_span(),
            CoreBuiltin::CoreDescribeKeyset,
            dummy_cont(),
            dummy_handler(),
            env,
            args,
        );

        // Run the computation
        match run_evalm(result) {
            Ok(eval_result) => {
                match eval_result {
                    EvalResult::EvalError(err) => {
                        // Should be NoSuchKeySet error
                        let err_str = format!("{:?}", err);
                        assert!(err_str.contains("NoSuchKeySet"), "Should be NoSuchKeySet error");
                    }
                    _ => panic!("Expected error, got: {:?}", eval_result),
                }
            }
            Err(e) => panic!("Unexpected computation error: {:?}", e),
        }
    }

    #[test]
    fn test_describe_module_success() {
        // Setup mock database with a module
        let mut module_data = HashMap::new();
        module_data.insert("hash".to_string(), PactValue::String("module-hash-123".to_string()));
        module_data.insert("blessed".to_string(), PactValue::Bool(false));
        
        let db = MockDb::with_module("my-module", module_data);
        let (builtin_env, env) = setup_test_env(db);

        // Get the describe-module implementation
        let implementation = builtin_env.get_implementation(CoreBuiltin::CoreDescribeModule).unwrap();

        // Call describe-module with the module name
        let args = vec![CEKValue::VPactValue(PactValue::String("my-module".to_string()))];
        
        let result = implementation(
            dummy_span(),
            CoreBuiltin::CoreDescribeModule,
            dummy_cont(),
            dummy_handler(),
            env,
            args,
        );

        // Run the computation
        match run_evalm(result) {
            Ok(eval_result) => {
                match eval_result {
                    EvalResult::EvalValue(CEKValue::VPactValue(PactValue::Object(obj))) => {
                        // Check that result contains module name
                        assert_eq!(
                            obj.get("name"),
                            Some(&PactValue::String("my-module".to_string())),
                            "Module object should contain name"
                        );
                    }
                    _ => panic!("Expected object result, got: {:?}", eval_result),
                }
            }
            Err(e) => panic!("Unexpected error: {:?}", e),
        }
    }

    #[test]
    fn test_describe_module_not_found() {
        // Setup empty mock database
        let db = MockDb::new();
        let (builtin_env, env) = setup_test_env(db);

        // Get the describe-module implementation
        let implementation = builtin_env.get_implementation(CoreBuiltin::CoreDescribeModule).unwrap();

        // Call describe-module with non-existent module
        let args = vec![CEKValue::VPactValue(PactValue::String("non-existent".to_string()))];
        
        let result = implementation(
            dummy_span(),
            CoreBuiltin::CoreDescribeModule,
            dummy_cont(),
            dummy_handler(),
            env,
            args,
        );

        // Run the computation
        match run_evalm(result) {
            Ok(eval_result) => {
                match eval_result {
                    EvalResult::EvalError(err) => {
                        // Should be NoSuchObjectInDb error
                        let err_str = format!("{:?}", err);
                        assert!(err_str.contains("NoSuchObjectInDb"), "Should be NoSuchObjectInDb error");
                    }
                    _ => panic!("Expected error, got: {:?}", eval_result),
                }
            }
            Err(e) => panic!("Unexpected computation error: {:?}", e),
        }
    }

    #[test]
    fn test_define_keyset_insert() {
        // Setup empty mock database
        let db = MockDb::new();
        let (builtin_env, env) = setup_test_env(db);

        // Get the define-keyset implementation
        let implementation = builtin_env.get_implementation(CoreBuiltin::CoreDefineKeySet).unwrap();

        // Call define-keyset to insert new keyset
        let keyset_data = PactValue::String("new-keyset-data".to_string());
        let args = vec![
            CEKValue::VPactValue(PactValue::String("new-keyset".to_string())),
            CEKValue::VPactValue(keyset_data),
        ];
        
        let result = implementation(
            dummy_span(),
            CoreBuiltin::CoreDefineKeySet,
            dummy_cont(),
            dummy_handler(),
            env,
            args,
        );

        // Run the computation
        match run_evalm(result) {
            Ok(eval_result) => {
                match eval_result {
                    EvalResult::EvalValue(CEKValue::VPactValue(PactValue::String(s))) => {
                        assert!(s.contains("success"), "Should indicate success");
                    }
                    _ => panic!("Expected string result, got: {:?}", eval_result),
                }
            }
            Err(e) => panic!("Unexpected error: {:?}", e),
        }
    }

    #[test]
    fn test_define_keyset_single_arg() {
        // Setup empty mock database
        let db = MockDb::new();
        let (builtin_env, env) = setup_test_env(db);

        // Get the define-keyset implementation (single arg form)
        let implementation = builtin_env.get_implementation(CoreBuiltin::CoreDefineKeysetData).unwrap();

        // Call define-keyset with single argument (reads from environment)
        let args = vec![
            CEKValue::VPactValue(PactValue::String("env-keyset".to_string())),
        ];
        
        let result = implementation(
            dummy_span(),
            CoreBuiltin::CoreDefineKeysetData,
            dummy_cont(),
            dummy_handler(),
            env,
            args,
        );

        // Run the computation
        match run_evalm(result) {
            Ok(eval_result) => {
                match eval_result {
                    EvalResult::EvalValue(CEKValue::VPactValue(PactValue::String(s))) => {
                        assert!(s.contains("success"), "Should indicate success");
                    }
                    _ => panic!("Expected string result, got: {:?}", eval_result),
                }
            }
            Err(e) => panic!("Unexpected error: {:?}", e),
        }
    }

    #[test]
    fn test_read_with_fields() {
        // Setup mock database with a table row
        let mut row_data = HashMap::new();
        row_data.insert("name".to_string(), PactValue::String("Alice".to_string()));
        row_data.insert("age".to_string(), PactValue::Integer(30.into()));
        row_data.insert("city".to_string(), PactValue::String("NYC".to_string()));
        
        let db = MockDb::with_table_row("users", "alice-key", row_data);
        let (builtin_env, mut env) = setup_test_env(db);

        // Create a table value for testing
        let table_schema = pact_cek::TableSchema {
            columns: vec![
                pact_cek::ColumnDef {
                    name: "name".to_string(),
                    column_type: pact_cek::ColumnType::String,
                    nullable: false,
                },
                pact_cek::ColumnDef {
                    name: "age".to_string(),
                    column_type: pact_cek::ColumnType::Integer,
                    nullable: false,
                },
                pact_cek::ColumnDef {
                    name: "city".to_string(),
                    column_type: pact_cek::ColumnType::String,
                    nullable: false,
                },
            ],
        };

        // Note: We're testing the read-with-fields implementation directly
        // In real usage, this would be handled as a read overload
        let read_impl = builtin_env.get_implementation(CoreBuiltin::CoreRead).unwrap();

        // Call read with field list (3-arg form)
        let args = vec![
            CEKValue::VTable {
                name: "users".to_string(),
                schema: table_schema,
                module_hash: None,
            },
            CEKValue::VPactValue(PactValue::String("alice-key".to_string())),
            CEKValue::VPactValue(PactValue::List(vec![
                PactValue::String("name".to_string()),
                PactValue::String("city".to_string()),
            ])),
        ];
        
        // Set up environment to allow reads
        env.flags.allow_read_in_local = true;
        
        let result = read_impl(
            dummy_span(),
            CoreBuiltin::CoreRead,
            dummy_cont(),
            dummy_handler(),
            env,
            args,
        );

        // Run the computation
        match run_evalm(result) {
            Ok(eval_result) => {
                match eval_result {
                    EvalResult::EvalValue(CEKValue::VPactValue(PactValue::Object(obj))) => {
                        // Should only contain requested fields
                        assert_eq!(obj.len(), 2, "Should have exactly 2 fields");
                        assert_eq!(
                            obj.get("name"),
                            Some(&PactValue::String("Alice".to_string())),
                            "Should have name field"
                        );
                        assert_eq!(
                            obj.get("city"),
                            Some(&PactValue::String("NYC".to_string())),
                            "Should have city field"
                        );
                        assert_eq!(
                            obj.get("age"),
                            None,
                            "Should not have age field"
                        );
                    }
                    _ => panic!("Expected object result, got: {:?}", eval_result),
                }
            }
            Err(e) => panic!("Unexpected error: {:?}", e),
        }
    }

    #[test]
    fn test_top_level_enforcement() {
        // Test that describe-keyset and describe-module enforce top-level only execution
        let db = MockDb::new();
        let (builtin_env, mut env) = setup_test_env(db);

        // Simulate being inside a function call (not top-level)
        env = env.push_call_frame();

        // Test describe-keyset
        let describe_keyset_impl = builtin_env.get_implementation(CoreBuiltin::CoreDescribeKeyset).unwrap();
        let args = vec![CEKValue::VPactValue(PactValue::String("some-keyset".to_string()))];
        
        let result = describe_keyset_impl(
            dummy_span(),
            CoreBuiltin::CoreDescribeKeyset,
            dummy_cont(),
            dummy_handler(),
            env.clone(),
            args,
        );

        match run_evalm(result) {
            Ok(eval_result) => {
                match eval_result {
                    EvalResult::EvalError(err) => {
                        let err_str = format!("{:?}", err);
                        assert!(err_str.contains("top-level"), "Should enforce top-level only");
                    }
                    _ => panic!("Expected error for non-top-level execution"),
                }
            }
            Err(e) => panic!("Unexpected computation error: {:?}", e),
        }

        // Test describe-module
        let describe_module_impl = builtin_env.get_implementation(CoreBuiltin::CoreDescribeModule).unwrap();
        let args = vec![CEKValue::VPactValue(PactValue::String("some-module".to_string()))];
        
        let result = describe_module_impl(
            dummy_span(),
            CoreBuiltin::CoreDescribeModule,
            dummy_cont(),
            dummy_handler(),
            env,
            args,
        );

        match run_evalm(result) {
            Ok(eval_result) => {
                match eval_result {
                    EvalResult::EvalError(err) => {
                        let err_str = format!("{:?}", err);
                        assert!(err_str.contains("top-level"), "Should enforce top-level only");
                    }
                    _ => panic!("Expected error for non-top-level execution"),
                }
            }
            Err(e) => panic!("Unexpected computation error: {:?}", e),
        }
    }

    #[test]
    fn test_newly_implemented_functions_registered() {
        let mut builtin_env = BuiltinEnv::new();
        database_ops::register_database_builtins(&mut builtin_env).unwrap();

        // Test all newly implemented operations are registered
        let new_ops = vec![
            (CoreBuiltin::CoreDescribeKeyset, "describe-keyset", 1),
            (CoreBuiltin::CoreDescribeModule, "describe-module", 1),
            (CoreBuiltin::CoreDefineKeySet, "define-keyset", 2),
            (CoreBuiltin::CoreDefineKeysetData, "define-keyset", 1),
        ];

        for (op, name, expected_arity) in new_ops {
            match builtin_env.lookup(op, dummy_span()) {
                Ok(native_fn) => {
                    assert_eq!(native_fn.arity, expected_arity,
                              "Operation {} ({:?}) should have arity {}", name, op, expected_arity);
                    assert_eq!(native_fn.builtin, op,
                              "NativeFn should have correct builtin variant");
                }
                Err(_) => panic!("Operation {} ({:?}) not found", name, op),
            }
        }
    }
}