//! Comprehensive tests for pact-values
//!
//! Tests cover all value types, conversions, guards, and collection operations.

use crate::*;
use num_bigint::BigInt;
use num_rational::BigRational;

#[cfg(test)]
mod value_tests {
    use super::*;

    #[test]
    fn test_string_value() {
        let val = PactValue::string("hello");
        assert_eq!(val.as_string().unwrap(), "hello");
        assert_eq!(val.type_of(), "string");

        // Test cloning
        let s: String = val.as_string().unwrap().clone();
        assert_eq!(s, "hello");
    }

    #[test]
    fn test_integer_value() {
        let val = PactValue::integer(42);
        assert_eq!(val.as_integer().unwrap(), &BigInt::from(42));
        assert_eq!(val.type_of(), "integer");

        // Test large integers
        let big = BigInt::parse_bytes(b"123456789012345678901234567890", 10).unwrap();
        let val2 = PactValue::Integer(big.clone());
        assert_eq!(val2.as_integer().unwrap(), &big);
    }

    #[test]
    fn test_decimal_value() {
        let val = PactValue::decimal(3.14);
        assert_eq!(val.type_of(), "decimal");

        // Test rational representation
        let rational = BigRational::new(BigInt::from(22), BigInt::from(7));
        let val2 = PactValue::Decimal(rational.clone());
        assert_eq!(val2.as_decimal().unwrap(), &Decimal::from(rational));
    }

    #[test]
    fn test_bool_value() {
        let val_true = PactValue::Bool(true);
        let val_false = PactValue::Bool(false);

        assert_eq!(val_true.as_bool().unwrap(), true);
        assert_eq!(val_false.as_bool().unwrap(), false);
        assert_eq!(val_true.type_of(), "bool");
    }

    #[test]
    fn test_list_value() {
        let items = vec![
            PactValue::integer(1),
            PactValue::integer(2),
            PactValue::integer(3),
        ];
        let val = PactValue::list(items.clone());

        assert_eq!(val.as_list().unwrap(), &items);
        assert_eq!(val.type_of(), "list");

        // Test empty list
        let empty = PactValue::empty_list();
        assert!(empty.as_list().unwrap().is_empty());
    }

    #[test]
    fn test_unit_value() {
        let val = PactValue::Unit;
        assert_eq!(val.type_of(), "unit");
        assert!(val.is_unit());
    }

    #[test]
    fn test_type_checking_methods() {
        assert!(PactValue::string("test").is_string());
        assert!(PactValue::integer(1).is_integer());
        assert!(PactValue::decimal(1.0).is_decimal());
        assert!(PactValue::Bool(true).is_bool());
        assert!(PactValue::empty_list().is_list());
        assert!(PactValue::empty_object().is_object());
        assert!(PactValue::Unit.is_unit());
    }

    #[test]
    fn test_error_handling() {
        let val = PactValue::string("test");

        // These should return errors
        assert!(val.as_integer().is_err());
        assert!(val.as_decimal().is_err());
        assert!(val.as_bool().is_err());
        assert!(val.as_list().is_err());
        assert!(val.as_object().is_err());
    }
}

#[cfg(test)]
mod object_tests {
    use super::*;

    #[test]
    fn test_object_creation() {
        let mut obj = Object::new();
        assert!(obj.is_empty());
        assert_eq!(obj.len(), 0);

        // Insert fields
        obj.insert("name", PactValue::string("Alice"));
        obj.insert("age", PactValue::integer(30));

        assert_eq!(obj.len(), 2);
        assert!(!obj.is_empty());
    }

    #[test]
    fn test_object_field_access() {
        let obj = Object::from_pairs(vec![
            ("name".to_string(), PactValue::string("Bob")),
            ("score".to_string(), PactValue::integer(100)),
        ]);

        // Get fields
        assert_eq!(obj.get("name").unwrap(), &PactValue::string("Bob"));
        assert_eq!(obj.get("score").unwrap(), &PactValue::integer(100));
        assert!(obj.get("missing").is_none());

        // Check field existence
        assert!(obj.contains_field("name"));
        assert!(!obj.contains_field("missing"));
    }

    #[test]
    fn test_object_mutation() {
        let mut obj = Object::new();

        // Insert and update
        obj.insert("x", PactValue::integer(1));
        assert_eq!(obj.get("x").unwrap(), &PactValue::integer(1));

        obj.insert("x", PactValue::integer(2));
        assert_eq!(obj.get("x").unwrap(), &PactValue::integer(2));

        // Remove
        let removed = obj.remove("x");
        assert_eq!(removed, Some(PactValue::integer(2)));
        assert!(obj.get("x").is_none());
    }

    #[test]
    fn test_object_builder_pattern() {
        let obj = Object::new()
            .with_field("name", PactValue::string("Charlie"))
            .with_field("level", PactValue::integer(5))
            .with_field("active", PactValue::Bool(true));

        assert_eq!(obj.len(), 3);
        assert_eq!(obj.get("name").unwrap(), &PactValue::string("Charlie"));
    }

    #[test]
    fn test_object_iteration() {
        let obj = Object::from_pairs(vec![
            ("a".to_string(), PactValue::integer(1)),
            ("b".to_string(), PactValue::integer(2)),
        ]);

        // Test field names
        let names: Vec<&String> = obj.field_names().collect();
        assert_eq!(names.len(), 2);
        assert!(names.contains(&&"a".to_string()));
        assert!(names.contains(&&"b".to_string()));

        // Test values
        let values: Vec<&PactValue> = obj.values().collect();
        assert_eq!(values.len(), 2);

        // Test entries
        let entries: Vec<(&String, &PactValue)> = obj.entries().collect();
        assert_eq!(entries.len(), 2);
    }
}

#[cfg(test)]
mod guard_tests {
    use super::*;

    #[test]
    fn test_keyset_guard() {
        let guard = Guard::KeySet {
            name: "admin-keyset".to_string(),
            keys: vec!["key1".to_string(), "key2".to_string()],
            pred: "keys-all".to_string(),
        };

        assert_eq!(guard.name(), "admin-keyset");

        match &guard {
            Guard::KeySet { keys, .. } => assert_eq!(keys.len(), 2),
            _ => panic!("Expected KeySet guard"),
        }
    }

    #[test]
    fn test_user_guard() {
        let guard = Guard::User {
            fun: "my-guard-fun".to_string(),
            args: vec![PactValue::string("arg1"), PactValue::integer(42)],
        };

        assert_eq!(guard.name(), "my-guard-fun");
    }

    #[test]
    fn test_capability_guard() {
        let guard = Guard::Capability {
            name: "TRANSFER".to_string(),
            args: vec![
                PactValue::string("alice"),
                PactValue::string("bob"),
                PactValue::decimal(100.0),
            ],
        };

        assert_eq!(guard.name(), "TRANSFER");
    }

    #[test]
    fn test_module_guard() {
        let guard = Guard::Module {
            name: "my-module".to_string(),
            governance: Some("governance-cap".to_string()),
        };

        assert_eq!(guard.name(), "my-module");
    }
}

#[cfg(test)]
mod keyset_tests {
    use super::*;

    #[test]
    fn test_keyset_creation() {
        let keyset = Keyset::new(
            vec!["key1".to_string(), "key2".to_string()],
            KeySetPredicate::KeysAll,
        );

        assert_eq!(keyset.keys().len(), 2);
        assert_eq!(keyset.pred(), &KeySetPredicate::KeysAll);
    }

    #[test]
    fn test_keyset_predicates() {
        let pred1 = KeySetPredicate::KeysAll;
        let pred2 = KeySetPredicate::KeysAny;
        let pred3 = KeySetPredicate::Keys2;
        let pred4 = KeySetPredicate::Custom("my-pred".to_string());

        assert_eq!(pred1.name(), "keys-all");
        assert_eq!(pred2.name(), "keys-any");
        assert_eq!(pred3.name(), "keys-2");
        assert_eq!(pred4.name(), "my-pred");
    }

    #[test]
    fn test_keyset_predicate_parsing() {
        assert_eq!(
            KeySetPredicate::from_name("keys-all"),
            KeySetPredicate::KeysAll
        );
        assert_eq!(
            KeySetPredicate::from_name("keys-any"),
            KeySetPredicate::KeysAny
        );
        assert_eq!(KeySetPredicate::from_name("keys-2"), KeySetPredicate::Keys2);
        assert_eq!(
            KeySetPredicate::from_name("custom-pred"),
            KeySetPredicate::Custom("custom-pred".to_string())
        );
    }
}

#[cfg(test)]
mod time_tests {
    use super::*;
    use std::time::SystemTime;

    #[test]
    fn test_time_creation() {
        let now = SystemTime::now();
        let pact_time = PactTime::from(now);

        let back = pact_time.as_system_time();
        let diff = now
            .duration_since(back)
            .or_else(|_| back.duration_since(now))
            .unwrap();
        assert!(diff.as_millis() < 1);
    }

    #[test]
    fn test_time_from_seconds() {
        let pact_time = PactTime::from_seconds(1705316400); // 2024-01-15 10:00:00 UTC
        assert_eq!(pact_time.as_seconds(), 1705316400);
    }

    #[test]
    fn test_time_formatting() {
        let pact_time = PactTime::from_seconds(0);
        let formatted = pact_time.to_iso_string();
        assert!(formatted.contains("1970"));
        assert!(formatted.contains("T"));
        assert!(formatted.contains("Z"));
    }

    #[test]
    fn test_time_arithmetic() {
        let time1 = PactTime::from_seconds(100);
        let time2 = time1.add_seconds(50);

        assert_eq!(time2.as_seconds(), 150);
        assert_eq!(time2.diff_seconds(&time1), 50);
    }
}

#[cfg(test)]
mod json_conversion_tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_json_to_pact_value() {
        // String
        let json_str = json!("hello");
        let pact_str = PactValue::from_json(json_str).unwrap();
        assert_eq!(pact_str, PactValue::string("hello"));

        // Number (integer)
        let json_int = json!(42);
        let pact_int = PactValue::from_json(json_int).unwrap();
        assert_eq!(pact_int.as_integer().unwrap(), &BigInt::from(42));

        // Number (decimal)
        let json_dec = json!(3.14);
        let pact_dec = PactValue::from_json(json_dec).unwrap();
        assert!(pact_dec.is_decimal());

        // Boolean
        let json_bool = json!(true);
        let pact_bool = PactValue::from_json(json_bool).unwrap();
        assert_eq!(pact_bool, PactValue::Bool(true));

        // Array
        let json_arr = json!([1, 2, 3]);
        let pact_arr = PactValue::from_json(json_arr).unwrap();
        assert!(pact_arr.is_list());
        assert_eq!(pact_arr.as_list().unwrap().len(), 3);

        // Object
        let json_obj = json!({"name": "Alice", "age": 30});
        let pact_obj = PactValue::from_json(json_obj).unwrap();
        assert!(pact_obj.is_object());
        let obj = pact_obj.as_object().unwrap();
        assert_eq!(obj.get("name").unwrap(), &PactValue::string("Alice"));

        // Null
        let json_null = json!(null);
        let pact_null = PactValue::from_json(json_null).unwrap();
        assert_eq!(pact_null, PactValue::Unit);
    }

    #[test]
    fn test_pact_value_to_json() {
        // Test string
        let pact_str = PactValue::string("test");
        let json_str = pact_str.to_json();
        assert_eq!(json_str, json!("test"));

        // Test integer
        let pact_int = PactValue::integer(123);
        let json_int = pact_int.to_json();
        assert_eq!(json_int, json!(123));

        // Test list
        let pact_list = PactValue::list(vec![PactValue::integer(1), PactValue::integer(2)]);
        let json_list = pact_list.to_json();
        assert_eq!(json_list, json!([1, 2]));

        // Test object
        let mut obj = Object::new();
        obj.insert("x", PactValue::integer(10));
        obj.insert("y", PactValue::string("test"));
        let pact_obj = PactValue::Object(obj);
        let json_obj = pact_obj.to_json();
        assert_eq!(json_obj["x"], json!(10));
        assert_eq!(json_obj["y"], json!("test"));
    }
}

#[cfg(test)]
mod list_operations_tests {
    use super::*;

    #[test]
    fn test_list_as_vec() {
        // PactList is just Vec<PactValue>
        let list: PactList = vec![
            PactValue::integer(1),
            PactValue::integer(2),
            PactValue::integer(3),
        ];

        assert_eq!(list.len(), 3);
        assert!(!list.is_empty());
        assert_eq!(list.get(0), Some(&PactValue::integer(1)));
        assert_eq!(list.get(3), None);
    }

    #[test]
    fn test_list_iteration() {
        let list: PactList = vec![
            PactValue::string("a"),
            PactValue::string("b"),
            PactValue::string("c"),
        ];

        let collected: Vec<&PactValue> = list.iter().collect();
        assert_eq!(collected.len(), 3);

        let strings: Vec<String> = list
            .iter()
            .filter_map(|v| v.as_string().ok().map(|s| s.to_string()))
            .collect();
        assert_eq!(strings, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_list_mutations() {
        let mut list: PactList = Vec::new();
        assert!(list.is_empty());

        list.push(PactValue::integer(1));
        list.push(PactValue::integer(2));
        assert_eq!(list.len(), 2);

        let popped = list.pop();
        assert_eq!(popped, Some(PactValue::integer(2)));
        assert_eq!(list.len(), 1);
    }
}

#[cfg(test)]
mod serialization_tests {
    use super::*;

    #[test]
    fn test_value_serialization() {
        // Test various value types
        let values = vec![
            PactValue::string("test"),
            PactValue::integer(42),
            PactValue::Bool(true),
            PactValue::Unit,
            PactValue::list(vec![PactValue::integer(1), PactValue::integer(2)]),
        ];

        for val in values {
            let serialized = serde_json::to_string(&val).unwrap();
            let deserialized: PactValue = serde_json::from_str(&serialized).unwrap();
            assert_eq!(val, deserialized);
        }
    }

    #[test]
    fn test_guard_serialization() {
        let guard = Guard::KeySet {
            name: "test-keyset".to_string(),
            keys: vec!["key1".to_string()],
            pred: "keys-all".to_string(),
        };

        let serialized = serde_json::to_string(&guard).unwrap();
        let deserialized: Guard = serde_json::from_str(&serialized).unwrap();
        assert_eq!(guard, deserialized);
    }

    #[test]
    fn test_object_serialization() {
        let mut obj = Object::new();
        obj.insert("field1", PactValue::string("value1"));
        obj.insert("field2", PactValue::integer(123));

        let serialized = serde_json::to_string(&obj).unwrap();
        let deserialized: Object = serde_json::from_str(&serialized).unwrap();

        assert_eq!(obj.get("field1"), deserialized.get("field1"));
        assert_eq!(obj.get("field2"), deserialized.get("field2"));
    }
}
