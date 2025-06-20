/// Tests for list operations to ensure they match Haskell behavior exactly
use pact_cek::{CEKEnv, CEKValue, EvalM, EvalResult};
use pact_cek::{BuiltinEnv, PactDb, Domain, RowKey, RowData};
use pact_cek::{register_core_builtins};
use pact_values::{PactValue, Object};
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
}

impl PactDb for MockDb {
    fn read(&self, _domain: Domain, _key: RowKey) -> EvalM<Option<RowData>> {
        EvalM::pure_value(None)
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

fn setup_test_env() -> CEKEnv {
    let mut builtin_env = BuiltinEnv::new();
    register_core_builtins(&mut builtin_env).expect("Failed to register builtins");
    let db = Arc::new(MockDb::new());
    CEKEnv::new(db, builtin_env)
}

fn run_evalm<T>(computation: EvalM<T>) -> Result<T, pact_errors::PactErrorI> {
    let eval_env = pact_cek::EvalMEnv::default();
    let eval_state = pact_cek::EvalState::default();
    
    computation.run(eval_env, eval_state).map(|(result, _state)| result)
}

#[test]
fn test_sort_list() {
    let env = setup_test_env();
    
    // Test sorting numbers
    let numbers = vec![
        PactValue::Integer(3.into()),
        PactValue::Integer(1.into()),
        PactValue::Integer(4.into()),
        PactValue::Integer(1.into()),
        PactValue::Integer(5.into()),
    ];
    
    let args = vec![CEKValue::VPactValue(PactValue::List(numbers))];
    
    let sort_impl = env.builtins.get_implementation(CoreBuiltin::CoreSort)
        .expect("sort builtin not found");
    
    let cont = pact_cek::Cont::Mt;
    let handler = pact_cek::CEKErrorHandler::CEKNoHandler;
    
    let result = run_evalm(
        sort_impl(
            pact_shared_types::SpanInfo::empty(),
            CoreBuiltin::CoreSort,
            cont,
            handler,
            env.clone(),
            args
        )
    );
    
    match result {
        Ok(EvalResult::EvalValue(CEKValue::VPactValue(PactValue::List(sorted)))) => {
            assert_eq!(sorted.len(), 5);
            assert_eq!(sorted[0], PactValue::Integer(1.into()));
            assert_eq!(sorted[1], PactValue::Integer(1.into()));
            assert_eq!(sorted[2], PactValue::Integer(3.into()));
            assert_eq!(sorted[3], PactValue::Integer(4.into()));
            assert_eq!(sorted[4], PactValue::Integer(5.into()));
        }
        _ => panic!("Expected sorted list, got: {:?}", result),
    }
}

#[test]
fn test_sort_strings() {
    let env = setup_test_env();
    
    // Test sorting strings
    let strings = vec![
        PactValue::String("zebra".to_string()),
        PactValue::String("apple".to_string()),
        PactValue::String("banana".to_string()),
    ];
    
    let args = vec![CEKValue::VPactValue(PactValue::List(strings))];
    
    let sort_impl = env.builtins.get_implementation(CoreBuiltin::CoreSort)
        .expect("sort builtin not found");
    
    let cont = pact_cek::Cont::Mt;
    let handler = pact_cek::CEKErrorHandler::CEKNoHandler;
    
    let result = run_evalm(
        sort_impl(
            pact_shared_types::SpanInfo::empty(),
            CoreBuiltin::CoreSort,
            cont,
            handler,
            env.clone(),
            args
        )
    );
    
    match result {
        Ok(EvalResult::EvalValue(CEKValue::VPactValue(PactValue::List(sorted)))) => {
            assert_eq!(sorted.len(), 3);
            assert_eq!(sorted[0], PactValue::String("apple".to_string()));
            assert_eq!(sorted[1], PactValue::String("banana".to_string()));
            assert_eq!(sorted[2], PactValue::String("zebra".to_string()));
        }
        _ => panic!("Expected sorted list, got: {:?}", result),
    }
}

#[test]
fn test_reverse_list() {
    let env = setup_test_env();
    
    // Test reversing a list
    let list = vec![
        PactValue::Integer(1.into()),
        PactValue::Integer(2.into()),
        PactValue::Integer(3.into()),
    ];
    
    let args = vec![CEKValue::VPactValue(PactValue::List(list))];
    
    let reverse_impl = env.builtins.get_implementation(CoreBuiltin::CoreReverse)
        .expect("reverse builtin not found");
    
    let cont = pact_cek::Cont::Mt;
    let handler = pact_cek::CEKErrorHandler::CEKNoHandler;
    
    let result = run_evalm(
        reverse_impl(
            pact_shared_types::SpanInfo::empty(),
            CoreBuiltin::CoreReverse,
            cont,
            handler,
            env.clone(),
            args
        )
    );
    
    match result {
        Ok(EvalResult::EvalValue(CEKValue::VPactValue(PactValue::List(reversed)))) => {
            assert_eq!(reversed.len(), 3);
            assert_eq!(reversed[0], PactValue::Integer(3.into()));
            assert_eq!(reversed[1], PactValue::Integer(2.into()));
            assert_eq!(reversed[2], PactValue::Integer(1.into()));
        }
        _ => panic!("Expected reversed list, got: {:?}", result),
    }
}

#[test]
fn test_reverse_string() {
    let env = setup_test_env();
    
    // Test reversing a string
    let args = vec![CEKValue::VPactValue(PactValue::String("hello".to_string()))];
    
    let reverse_impl = env.builtins.get_implementation(CoreBuiltin::CoreReverse)
        .expect("reverse builtin not found");
    
    let cont = pact_cek::Cont::Mt;
    let handler = pact_cek::CEKErrorHandler::CEKNoHandler;
    
    let result = run_evalm(
        reverse_impl(
            pact_shared_types::SpanInfo::empty(),
            CoreBuiltin::CoreReverse,
            cont,
            handler,
            env.clone(),
            args
        )
    );
    
    match result {
        Ok(EvalResult::EvalValue(CEKValue::VPactValue(PactValue::String(reversed)))) => {
            assert_eq!(reversed, "olleh");
        }
        _ => panic!("Expected reversed string, got: {:?}", result),
    }
}

#[test]
fn test_contains_element_in_list() {
    let env = setup_test_env();
    
    // Test contains element in list
    let list = vec![
        PactValue::Integer(1.into()),
        PactValue::Integer(2.into()),
        PactValue::Integer(3.into()),
    ];
    
    let args = vec![
        CEKValue::VPactValue(PactValue::Integer(2.into())),
        CEKValue::VPactValue(PactValue::List(list))
    ];
    
    let contains_impl = env.builtins.get_implementation(CoreBuiltin::CoreContains)
        .expect("contains builtin not found");
    
    let cont = pact_cek::Cont::Mt;
    let handler = pact_cek::CEKErrorHandler::CEKNoHandler;
    
    let result = run_evalm(
        contains_impl(
            pact_shared_types::SpanInfo::empty(),
            CoreBuiltin::CoreContains,
            cont,
            handler,
            env.clone(),
            args
        )
    );
    
    match result {
        Ok(EvalResult::EvalValue(CEKValue::VPactValue(PactValue::Bool(found)))) => {
            assert!(found, "Expected to find element 2 in list");
        }
        _ => panic!("Expected bool result, got: {:?}", result),
    }
}

#[test]
fn test_contains_substring_in_string() {
    let env = setup_test_env();
    
    // Test contains substring in string
    let args = vec![
        CEKValue::VPactValue(PactValue::String("world".to_string())),
        CEKValue::VPactValue(PactValue::String("hello world".to_string()))
    ];
    
    let contains_impl = env.builtins.get_implementation(CoreBuiltin::CoreContains)
        .expect("contains builtin not found");
    
    let cont = pact_cek::Cont::Mt;
    let handler = pact_cek::CEKErrorHandler::CEKNoHandler;
    
    let result = run_evalm(
        contains_impl(
            pact_shared_types::SpanInfo::empty(),
            CoreBuiltin::CoreContains,
            cont,
            handler,
            env.clone(),
            args
        )
    );
    
    match result {
        Ok(EvalResult::EvalValue(CEKValue::VPactValue(PactValue::Bool(found)))) => {
            assert!(found, "Expected to find 'world' in 'hello world'");
        }
        _ => panic!("Expected bool result, got: {:?}", result),
    }
}

#[test]
fn test_contains_field_in_object() {
    let env = setup_test_env();
    
    // Test contains field in object
    let mut obj = Object::new();
    obj.insert("name", PactValue::String("Alice".to_string()));
    obj.insert("age", PactValue::Integer(30.into()));
    
    let args = vec![
        CEKValue::VPactValue(PactValue::String("name".to_string())),
        CEKValue::VPactValue(PactValue::Object(obj))
    ];
    
    let contains_impl = env.builtins.get_implementation(CoreBuiltin::CoreContains)
        .expect("contains builtin not found");
    
    let cont = pact_cek::Cont::Mt;
    let handler = pact_cek::CEKErrorHandler::CEKNoHandler;
    
    let result = run_evalm(
        contains_impl(
            pact_shared_types::SpanInfo::empty(),
            CoreBuiltin::CoreContains,
            cont,
            handler,
            env.clone(),
            args
        )
    );
    
    match result {
        Ok(EvalResult::EvalValue(CEKValue::VPactValue(PactValue::Bool(found)))) => {
            assert!(found, "Expected to find field 'name' in object");
        }
        _ => panic!("Expected bool result, got: {:?}", result),
    }
}

#[test]
fn test_sort_mixed_types_follows_haskell_ordering() {
    let env = setup_test_env();
    
    // According to Haskell: bool < string < number < time
    let mixed = vec![
        PactValue::Integer(42.into()),
        PactValue::String("hello".to_string()),
        PactValue::Bool(true),
        PactValue::Time(pact_values::time::PactTime::from_micros(1000000)),
        PactValue::Bool(false),
    ];
    
    let args = vec![CEKValue::VPactValue(PactValue::List(mixed))];
    
    let sort_impl = env.builtins.get_implementation(CoreBuiltin::CoreSort)
        .expect("sort builtin not found");
    
    let cont = pact_cek::Cont::Mt;
    let handler = pact_cek::CEKErrorHandler::CEKNoHandler;
    
    let result = run_evalm(
        sort_impl(
            pact_shared_types::SpanInfo::empty(),
            CoreBuiltin::CoreSort,
            cont,
            handler,
            env.clone(),
            args
        )
    );
    
    match result {
        Ok(EvalResult::EvalValue(CEKValue::VPactValue(PactValue::List(sorted)))) => {
            assert_eq!(sorted.len(), 5);
            // Bools come first (false < true)
            assert_eq!(sorted[0], PactValue::Bool(false));
            assert_eq!(sorted[1], PactValue::Bool(true));
            // Then strings
            assert_eq!(sorted[2], PactValue::String("hello".to_string()));
            // Then numbers
            assert_eq!(sorted[3], PactValue::Integer(42.into()));
            // Finally time
            assert!(matches!(sorted[4], PactValue::Time(_)));
        }
        _ => panic!("Expected sorted list, got: {:?}", result),
    }
}