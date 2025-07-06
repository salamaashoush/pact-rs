//! Utility functions for the public API

use pact_core::values::{PactValue, Integer, Decimal};
use num_rational::BigRational;

/// Convert JSON value to PactValue
pub fn json_to_pact_value(val: serde_json::Value) -> PactValue {
    match val {
        serde_json::Value::Null => PactValue::Unit,
        serde_json::Value::Bool(b) => PactValue::Bool(b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                PactValue::Integer(Integer::from(i))
            } else if let Some(f) = n.as_f64() {
                // Convert to rational
                let rational = BigRational::from_float(f)
                    .unwrap_or_else(|| BigRational::from(Integer::from(0)));
                PactValue::Decimal(Decimal::from_rational(rational))
            } else {
                PactValue::Integer(Integer::from(0))
            }
        }
        serde_json::Value::String(s) => PactValue::String(s),
        serde_json::Value::Array(arr) => {
            PactValue::List(arr.into_iter().map(json_to_pact_value).collect())
        }
        serde_json::Value::Object(obj) => {
            let mut pact_obj = pact_core::values::Object::new();
            for (k, v) in obj {
                pact_obj.insert(k, json_to_pact_value(v));
            }
            PactValue::Object(pact_obj)
        }
    }
}

/// Convert PactValue to JSON
pub fn pact_value_to_json(value: &PactValue) -> serde_json::Value {
    match value {
        PactValue::Unit => serde_json::Value::Null,
        PactValue::Bool(b) => serde_json::Value::Bool(*b),
        PactValue::Integer(i) => serde_json::json!(i.to_string()),
        PactValue::Decimal(d) => serde_json::json!(d.to_string()),
        PactValue::String(s) => serde_json::Value::String(s.clone()),
        PactValue::Time(t) => serde_json::json!(t.to_string()),
        PactValue::List(items) => {
            serde_json::Value::Array(items.iter().map(pact_value_to_json).collect())
        }
        PactValue::Object(obj) => {
            let map: serde_json::Map<String, serde_json::Value> = obj.entries()
                .map(|(k, v)| (k.clone(), pact_value_to_json(v)))
                .collect();
            serde_json::Value::Object(map)
        }
        PactValue::Guard(g) => serde_json::json!({
            "type": "guard",
            "value": format!("{:?}", g)
        }),
        PactValue::Keyset(ks) => serde_json::json!({
            "type": "keyset",
            "value": format!("{:?}", ks)
        }),
        PactValue::ModRef(m) => serde_json::json!({
            "type": "module",
            "value": format!("{:?}", m)
        }),
        PactValue::Closure(c) => serde_json::json!({
            "type": "closure",
            "value": format!("{:?}", c)
        }),
        PactValue::CapToken(t) => serde_json::json!({
            "type": "capability",
            "value": format!("{:?}", t)
        }),
    }
}
