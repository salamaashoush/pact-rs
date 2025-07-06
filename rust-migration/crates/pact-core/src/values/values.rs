//! Core value types for Pact runtime
//!
//! This module provides the runtime representation of Pact values,
//! exactly matching the Haskell implementation.

use super::{Decimal, Guard, Integer, Keyset, Object, PactTime, ValueError, ValueResult};
use crate::names::ModuleName;
use serde::{Deserialize, Serialize};
use num_traits::ToPrimitive;
use std::collections::BTreeMap;
use std::fmt;
use std::sync::Arc;

/// Module reference
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ModRef {
    /// Module name
    pub name: ModuleName,
    /// Whether this is an interface reference
    pub is_interface: bool,
}

/// Closure for lambdas and partial applications
#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    /// Unique identifier
    pub id: String,
    /// Captured environment (placeholder - will be expanded with CEK integration)
    pub env: Arc<BTreeMap<String, PactValue>>,
}

// Custom serialization for Closure
impl Serialize for Closure {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeStruct;
        let mut state = serializer.serialize_struct("Closure", 2)?;
        state.serialize_field("id", &self.id)?;
        state.serialize_field("env", self.env.as_ref())?;
        state.end()
    }
}

impl<'de> Deserialize<'de> for Closure {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct ClosureData {
            id: String,
            env: BTreeMap<String, PactValue>,
        }

        let data = ClosureData::deserialize(deserializer)?;
        Ok(Closure {
            id: data.id,
            env: Arc::new(data.env),
        })
    }
}

/// Capability token
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CapToken {
    /// Capability name
    pub name: String,
    /// Capability arguments
    pub args: Vec<PactValue>,
}

/// Runtime value representation for Pact - matches Haskell PactValue exactly
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum PactValue {
    /// Literal string value
    String(String),

    /// Arbitrary precision integer
    Integer(Integer),

    /// Arbitrary precision decimal
    Decimal(Decimal),

    /// Boolean value
    Bool(bool),

    /// UTC timestamp with microsecond precision
    Time(PactTime),

    /// List of values (homogeneous in type)
    List(Vec<PactValue>),

    /// Object (key-value mapping)
    Object(Object),

    /// Authorization guard
    Guard(Guard),

    /// Key set for multi-signature authorization
    Keyset(Keyset),

    /// Module reference
    ModRef(ModRef),

    /// Closure (lambda/partial application)
    Closure(Closure),

    /// Capability token
    CapToken(CapToken),

    /// Unit value (for side effects)
    Unit,
}

impl PactValue {
    /// Get the type name of this value
    pub fn type_name(&self) -> &'static str {
        match self {
            PactValue::String(_) => "string",
            PactValue::Integer(_) => "integer",
            PactValue::Decimal(_) => "decimal",
            PactValue::Bool(_) => "bool",
            PactValue::Time(_) => "time",
            PactValue::List(_) => "list",
            PactValue::Object(_) => "object",
            PactValue::Guard(_) => "guard",
            PactValue::Keyset(_) => "keyset",
            PactValue::ModRef(_) => "module",
            PactValue::Closure(_) => "closure",
            PactValue::CapToken(_) => "capability",
            PactValue::Unit => "unit",
        }
    }

    /// Check if value is truthy (for conditional evaluation)
    pub fn is_truthy(&self) -> bool {
        match self {
            PactValue::Bool(b) => *b,
            PactValue::Unit => false,
            _ => true,
        }
    }

    /// Convert to string representation
    pub fn to_string_repr(&self) -> String {
        match self {
            PactValue::String(s) => s.clone(),
            PactValue::Integer(i) => i.to_string(),
            PactValue::Decimal(d) => d.to_string(),
            PactValue::Bool(b) => b.to_string(),
            PactValue::Time(t) => format!("time({})", t.as_micros()),
            PactValue::List(items) => {
                let items_str: Vec<String> = items.iter().map(|v| v.to_string_repr()).collect();
                format!("[{}]", items_str.join(", "))
            }
            PactValue::Object(obj) => obj.to_string(),
            PactValue::Guard(guard) => format!("Guard({:?})", guard),
            PactValue::Keyset(ks) => format!("KeySet({:?})", ks),
            PactValue::ModRef(m) => format!("ModRef({:?})", m),
            PactValue::Closure(c) => format!("Closure({:?})", c),
            PactValue::CapToken(c) => format!("CapToken({:?})", c),
            PactValue::Unit => "()".to_string(),
        }
    }

    /// Extract string value
    pub fn as_string(&self) -> ValueResult<&String> {
        match self {
            PactValue::String(s) => Ok(s),
            _ => Err(ValueError::TypeMismatch {
                expected: "string".to_string(),
                actual: self.type_name().to_string(),
            }),
        }
    }

    /// Extract integer value
    pub fn as_integer(&self) -> ValueResult<&Integer> {
        match self {
            PactValue::Integer(i) => Ok(i),
            _ => Err(ValueError::TypeMismatch {
                expected: "integer".to_string(),
                actual: self.type_name().to_string(),
            }),
        }
    }

    /// Extract decimal value
    pub fn as_decimal(&self) -> ValueResult<&Decimal> {
        match self {
            PactValue::Decimal(d) => Ok(d),
            _ => Err(ValueError::TypeMismatch {
                expected: "decimal".to_string(),
                actual: self.type_name().to_string(),
            }),
        }
    }

    /// Extract boolean value
    pub fn as_bool(&self) -> ValueResult<bool> {
        match self {
            PactValue::Bool(b) => Ok(*b),
            _ => Err(ValueError::TypeMismatch {
                expected: "bool".to_string(),
                actual: self.type_name().to_string(),
            }),
        }
    }

    /// Extract time value
    pub fn as_time(&self) -> ValueResult<&PactTime> {
        match self {
            PactValue::Time(t) => Ok(t),
            _ => Err(ValueError::TypeMismatch {
                expected: "time".to_string(),
                actual: self.type_name().to_string(),
            }),
        }
    }

    /// Extract list value
    pub fn as_list(&self) -> ValueResult<&Vec<PactValue>> {
        match self {
            PactValue::List(items) => Ok(items),
            _ => Err(ValueError::TypeMismatch {
                expected: "list".to_string(),
                actual: self.type_name().to_string(),
            }),
        }
    }

    /// Extract object value
    pub fn as_object(&self) -> ValueResult<&Object> {
        match self {
            PactValue::Object(obj) => Ok(obj),
            _ => Err(ValueError::TypeMismatch {
                expected: "object".to_string(),
                actual: self.type_name().to_string(),
            }),
        }
    }

    /// Extract guard value
    pub fn as_guard(&self) -> ValueResult<&Guard> {
        match self {
            PactValue::Guard(guard) => Ok(guard),
            _ => Err(ValueError::TypeMismatch {
                expected: "guard".to_string(),
                actual: self.type_name().to_string(),
            }),
        }
    }

    /// Extract keyset value
    pub fn as_keyset(&self) -> ValueResult<&Keyset> {
        match self {
            PactValue::Keyset(ks) => Ok(ks),
            _ => Err(ValueError::TypeMismatch {
                expected: "keyset".to_string(),
                actual: self.type_name().to_string(),
            }),
        }
    }

    /// Check if this is the unit value
    pub fn is_unit(&self) -> bool {
        matches!(self, PactValue::Unit)
    }

    /// Check if value is a string
    pub fn is_string(&self) -> bool {
        matches!(self, PactValue::String(_))
    }

    /// Check if value is an integer
    pub fn is_integer(&self) -> bool {
        matches!(self, PactValue::Integer(_))
    }

    /// Check if value is a decimal
    pub fn is_decimal(&self) -> bool {
        matches!(self, PactValue::Decimal(_))
    }

    /// Check if value is a boolean
    pub fn is_bool(&self) -> bool {
        matches!(self, PactValue::Bool(_))
    }

    /// Check if value is a time
    pub fn is_time(&self) -> bool {
        matches!(self, PactValue::Time(_))
    }

    /// Check if value is a list
    pub fn is_list(&self) -> bool {
        matches!(self, PactValue::List(_))
    }

    /// Check if value is an object
    pub fn is_object(&self) -> bool {
        matches!(self, PactValue::Object(_))
    }

    /// Check if value is a guard
    pub fn is_guard(&self) -> bool {
        matches!(self, PactValue::Guard(_))
    }

    /// Check if value is a keyset
    pub fn is_keyset(&self) -> bool {
        matches!(self, PactValue::Keyset(_))
    }

    /// Convert to JSON value for serialization
    pub fn to_json(&self) -> serde_json::Value {
        match self {
            PactValue::String(s) => serde_json::Value::String(s.clone()),
            PactValue::Integer(i) => {
                // Try to fit in i64/u64, otherwise use string
                if let Some(n) = i.to_i64() {
                    serde_json::Value::Number(serde_json::Number::from(n))
                } else if let Some(n) = i.to_u64() {
                    serde_json::Value::Number(serde_json::Number::from(n))
                } else {
                    serde_json::Value::String(i.to_string())
                }
            }
            PactValue::Decimal(d) => {
                let f = d.to_f64();
                serde_json::Number::from_f64(f)
                    .map(serde_json::Value::Number)
                    .unwrap_or_else(|| serde_json::Value::String(d.to_string()))
            }
            PactValue::Bool(b) => serde_json::Value::Bool(*b),
            PactValue::Time(t) => {
                serde_json::Value::Number(serde_json::Number::from(t.as_micros()))
            }
            PactValue::List(items) => {
                serde_json::Value::Array(items.iter().map(|v| v.to_json()).collect())
            }
            PactValue::Object(obj) => obj.to_json().unwrap_or(serde_json::Value::Null),
            PactValue::Guard(g) => {
                // For now, serialize guards as objects
                serde_json::to_value(g).unwrap_or(serde_json::Value::Null)
            }
            PactValue::Keyset(ks) => {
                // For now, serialize keysets as objects
                serde_json::to_value(ks).unwrap_or(serde_json::Value::Null)
            }
            PactValue::ModRef(m) => {
                // Serialize module ref as object
                serde_json::to_value(m).unwrap_or(serde_json::Value::Null)
            }
            PactValue::Closure(c) => {
                // Serialize closure as object
                serde_json::to_value(c).unwrap_or(serde_json::Value::Null)
            }
            PactValue::CapToken(c) => {
                // Serialize cap token as object
                serde_json::to_value(c).unwrap_or(serde_json::Value::Null)
            }
            PactValue::Unit => serde_json::Value::Null,
        }
    }

    /// Parse from JSON value - converts JSON types to PactValue types
    pub fn from_json(json: serde_json::Value) -> ValueResult<Self> {
        match json {
            serde_json::Value::Null => Ok(PactValue::Unit),
            serde_json::Value::Bool(b) => Ok(PactValue::Bool(b)),
            serde_json::Value::Number(n) => {
                if let Some(i) = n.as_i64() {
                    Ok(PactValue::Integer(Integer::from(i)))
                } else if let Some(u) = n.as_u64() {
                    Ok(PactValue::Integer(Integer::from(u)))
                } else if let Some(f) = n.as_f64() {
                    Ok(PactValue::Decimal(Decimal::from_f64(f)))
                } else {
                    Err(ValueError::Serialization {
                        message: "Invalid number format".to_string(),
                    })
                }
            }
            serde_json::Value::String(s) => Ok(PactValue::String(s)),
            serde_json::Value::Array(arr) => {
                let values: Result<Vec<PactValue>, ValueError> =
                    arr.into_iter().map(PactValue::from_json).collect();
                Ok(PactValue::List(values?))
            }
            serde_json::Value::Object(obj) => {
                let mut pact_obj = Object::new();
                for (key, value) in obj {
                    let pact_value = PactValue::from_json(value)?;
                    pact_obj.insert(key, pact_value);
                }
                Ok(PactValue::Object(pact_obj))
            }
        }
    }
}

impl fmt::Display for PactValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string_repr())
    }
}

impl From<String> for PactValue {
    fn from(s: String) -> Self {
        PactValue::String(s)
    }
}

impl From<&str> for PactValue {
    fn from(s: &str) -> Self {
        PactValue::String(s.to_string())
    }
}

impl From<Integer> for PactValue {
    fn from(i: Integer) -> Self {
        PactValue::Integer(i)
    }
}

impl From<i64> for PactValue {
    fn from(i: i64) -> Self {
        PactValue::Integer(Integer::from(i))
    }
}

impl From<i32> for PactValue {
    fn from(i: i32) -> Self {
        PactValue::Integer(Integer::from(i))
    }
}

impl From<Decimal> for PactValue {
    fn from(d: Decimal) -> Self {
        PactValue::Decimal(d)
    }
}

impl From<bool> for PactValue {
    fn from(b: bool) -> Self {
        PactValue::Bool(b)
    }
}

impl From<PactTime> for PactValue {
    fn from(t: PactTime) -> Self {
        PactValue::Time(t)
    }
}

impl From<Vec<PactValue>> for PactValue {
    fn from(items: Vec<PactValue>) -> Self {
        PactValue::List(items)
    }
}

impl From<Object> for PactValue {
    fn from(obj: Object) -> Self {
        PactValue::Object(obj)
    }
}

impl From<Guard> for PactValue {
    fn from(guard: Guard) -> Self {
        PactValue::Guard(guard)
    }
}

impl From<Keyset> for PactValue {
    fn from(ks: Keyset) -> Self {
        PactValue::Keyset(ks)
    }
}

/// Convenience constructors
impl PactValue {
    /// Create an empty list
    pub fn empty_list() -> Self {
        PactValue::List(Vec::new())
    }

    /// Create an empty object
    pub fn empty_object() -> Self {
        PactValue::Object(Object::new())
    }

    /// Create a list from an iterator
    pub fn list_from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = PactValue>,
    {
        PactValue::List(iter.into_iter().collect())
    }

    /// Create a string value
    pub fn string<S: Into<String>>(s: S) -> Self {
        PactValue::String(s.into())
    }

    /// Create an integer value
    pub fn integer(i: impl Into<Integer>) -> Self {
        PactValue::Integer(i.into())
    }

    /// Create a decimal value from f64
    pub fn decimal(d: f64) -> Self {
        PactValue::Decimal(Decimal::from_f64(d))
    }

    /// Create a list value
    pub fn list(items: Vec<PactValue>) -> Self {
        PactValue::List(items)
    }

    /// Get the type name of this value
    pub fn type_of(&self) -> &'static str {
        self.type_name()
    }

    /// Create a decimal value from components
    pub fn decimal_from_parts(num: Integer, denom: Integer) -> Self {
        PactValue::Decimal(Decimal::new(num, denom))
    }

    /// Create a boolean value
    pub fn bool(b: bool) -> Self {
        PactValue::Bool(b)
    }

    /// Create a time value from microseconds
    pub fn time_from_micros(micros: i64) -> Self {
        PactValue::Time(PactTime::from_micros(micros))
    }

    /// Create the unit value
    pub fn unit() -> Self {
        PactValue::Unit
    }
}

// TryFrom implementations for type conversions used by builtins

impl TryFrom<PactValue> for String {
    type Error = String;

    fn try_from(value: PactValue) -> Result<Self, Self::Error> {
        match value {
            PactValue::String(s) => Ok(s),
            _ => Err(format!("Expected string, got {}", value.type_name())),
        }
    }
}

impl TryFrom<PactValue> for Integer {
    type Error = String;

    fn try_from(value: PactValue) -> Result<Self, Self::Error> {
        match value {
            PactValue::Integer(i) => Ok(i),
            _ => Err(format!("Expected integer, got {}", value.type_name())),
        }
    }
}

impl TryFrom<PactValue> for Decimal {
    type Error = String;

    fn try_from(value: PactValue) -> Result<Self, Self::Error> {
        match value {
            PactValue::Decimal(d) => Ok(d),
            _ => Err(format!("Expected decimal, got {}", value.type_name())),
        }
    }
}

impl TryFrom<PactValue> for bool {
    type Error = String;

    fn try_from(value: PactValue) -> Result<Self, Self::Error> {
        match value {
            PactValue::Bool(b) => Ok(b),
            _ => Err(format!("Expected bool, got {}", value.type_name())),
        }
    }
}

impl TryFrom<PactValue> for Vec<PactValue> {
    type Error = String;

    fn try_from(value: PactValue) -> Result<Self, Self::Error> {
        match value {
            PactValue::List(l) => Ok(l),
            _ => Err(format!("Expected list, got {}", value.type_name())),
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_value_type_names() {
        assert_eq!(PactValue::String("test".to_string()).type_name(), "string");
        assert_eq!(PactValue::Integer(Integer::from(42)).type_name(), "integer");
        assert_eq!(PactValue::Bool(true).type_name(), "bool");
        assert_eq!(PactValue::Unit.type_name(), "unit");
    }

    #[test]
    fn test_value_truthiness() {
        assert!(PactValue::Bool(true).is_truthy());
        assert!(!PactValue::Bool(false).is_truthy());
        assert!(!PactValue::Unit.is_truthy());
        assert!(PactValue::String("".to_string()).is_truthy());
        assert!(PactValue::Integer(Integer::from(0)).is_truthy());
    }

    #[test]
    fn test_value_extraction() {
        let str_val = PactValue::String("test".to_string());
        assert_eq!(str_val.as_string().unwrap(), "test");
        assert!(str_val.as_integer().is_err());

        let int_val = PactValue::Integer(Integer::from(42));
        assert_eq!(*int_val.as_integer().unwrap(), Integer::from(42));
        assert!(int_val.as_string().is_err());
    }

    #[test]
    fn test_value_conversions() {
        let str_val: PactValue = "test".into();
        assert_eq!(str_val, PactValue::String("test".to_string()));

        let int_val: PactValue = 42i64.into();
        assert_eq!(int_val, PactValue::Integer(Integer::from(42)));

        let bool_val: PactValue = true.into();
        assert_eq!(bool_val, PactValue::Bool(true));
    }

    #[test]
    fn test_constructors() {
        assert_eq!(PactValue::empty_list(), PactValue::List(vec![]));
        assert_eq!(
            PactValue::string("test"),
            PactValue::String("test".to_string())
        );
        assert_eq!(PactValue::integer(42), PactValue::Integer(Integer::from(42)));
        assert_eq!(PactValue::bool(true), PactValue::Bool(true));
        assert_eq!(PactValue::unit(), PactValue::Unit);
    }

    #[test]
    fn test_json_roundtrip() {
        let values = vec![
            PactValue::String("test".to_string()),
            PactValue::Integer(Integer::from(42)),
            PactValue::Bool(true),
            PactValue::Unit,
            PactValue::List(vec![
                PactValue::Integer(Integer::from(1)),
                PactValue::Integer(Integer::from(2)),
            ]),
        ];

        for value in values {
            let json = value.to_json();
            let recovered = PactValue::from_json(json).unwrap();
            assert_eq!(value, recovered);
        }
    }
}
