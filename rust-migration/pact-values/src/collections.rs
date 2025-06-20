//! Collection types for Pact values
//!
//! Provides Object and List types with proper indexing and field access.

use crate::{PactValue, ValueError, ValueResult};
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::fmt;

/// Object type for key-value mappings - matches Haskell Object
/// Uses IndexMap to preserve insertion order
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Object {
    /// Fields stored in insertion order
    fields: IndexMap<String, PactValue>,
}

impl Object {
    /// Create a new empty object
    pub fn new() -> Self {
        Object {
            fields: IndexMap::new(),
        }
    }

    /// Create object from key-value pairs
    pub fn from_pairs<I>(pairs: I) -> Self
    where
        I: IntoIterator<Item = (String, PactValue)>,
    {
        Object {
            fields: pairs.into_iter().collect(),
        }
    }

    /// Get a field value by name
    pub fn get(&self, field: &str) -> Option<&PactValue> {
        self.fields.get(field)
    }

    /// Get a field value by name (mutable)
    pub fn get_mut(&mut self, field: &str) -> Option<&mut PactValue> {
        self.fields.get_mut(field)
    }

    /// Insert or update a field
    pub fn insert<S: Into<String>>(&mut self, field: S, value: PactValue) -> Option<PactValue> {
        self.fields.insert(field.into(), value)
    }

    /// Remove a field
    pub fn remove(&mut self, field: &str) -> Option<PactValue> {
        self.fields.shift_remove(field)
    }

    /// Check if field exists
    pub fn contains_field(&self, field: &str) -> bool {
        self.fields.contains_key(field)
    }

    /// Get all field names
    pub fn field_names(&self) -> impl Iterator<Item = &String> {
        self.fields.keys()
    }

    /// Get all values
    pub fn values(&self) -> impl Iterator<Item = &PactValue> {
        self.fields.values()
    }

    /// Get all field-value pairs
    pub fn entries(&self) -> impl Iterator<Item = (&String, &PactValue)> {
        self.fields.iter()
    }

    /// Get reference to the underlying fields map
    pub fn fields(&self) -> &IndexMap<String, PactValue> {
        &self.fields
    }

    /// Number of fields
    pub fn len(&self) -> usize {
        self.fields.len()
    }

    /// Check if object is empty
    pub fn is_empty(&self) -> bool {
        self.fields.is_empty()
    }

    /// Clear all fields
    pub fn clear(&mut self) {
        self.fields.clear();
    }

    /// Create a new object with an additional field
    pub fn with_field<S: Into<String>>(mut self, field: S, value: PactValue) -> Self {
        self.insert(field, value);
        self
    }

    /// Access field with error handling
    pub fn get_field(&self, field: &str) -> ValueResult<&PactValue> {
        self.get(field).ok_or_else(|| ValueError::InvalidField {
            field: field.to_string(),
        })
    }

    /// Try to get a field as a specific type
    pub fn get_string(&self, field: &str) -> ValueResult<&String> {
        self.get_field(field)?.as_string()
    }

    pub fn get_integer(&self, field: &str) -> ValueResult<&num_bigint::BigInt> {
        self.get_field(field)?.as_integer()
    }

    pub fn get_bool(&self, field: &str) -> ValueResult<bool> {
        self.get_field(field)?.as_bool()
    }

    /// Convert to HashMap for compatibility
    pub fn to_hashmap(&self) -> std::collections::HashMap<String, PactValue> {
        self.fields
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect()
    }

    /// Create from HashMap
    pub fn from_hashmap(map: std::collections::HashMap<String, PactValue>) -> Self {
        Object::from_pairs(map.into_iter())
    }
}

impl Default for Object {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        let mut first = true;
        for (key, value) in &self.fields {
            if !first {
                write!(f, ", ")?;
            }
            write!(f, "\"{}\": {}", key, value)?;
            first = false;
        }
        write!(f, "}}")
    }
}

impl FromIterator<(String, PactValue)> for Object {
    fn from_iter<I: IntoIterator<Item = (String, PactValue)>>(iter: I) -> Self {
        Object::from_pairs(iter)
    }
}

impl IntoIterator for Object {
    type Item = (String, PactValue);
    type IntoIter = indexmap::map::IntoIter<String, PactValue>;

    fn into_iter(self) -> Self::IntoIter {
        self.fields.into_iter()
    }
}

impl<'a> IntoIterator for &'a Object {
    type Item = (&'a String, &'a PactValue);
    type IntoIter = indexmap::map::Iter<'a, String, PactValue>;

    fn into_iter(self) -> Self::IntoIter {
        self.fields.iter()
    }
}

/// List type alias for clarity
pub type PactList = Vec<PactValue>;

/// Extension trait for PactList operations
pub trait PactListExt {
    /// Safe indexing with bounds checking
    fn get_at(&self, index: usize) -> ValueResult<&PactValue>;

    /// Get list length
    fn pact_length(&self) -> usize;

    /// Check if list is empty
    fn is_pact_empty(&self) -> bool;

    /// Get first element
    fn first_element(&self) -> ValueResult<&PactValue>;

    /// Get last element  
    fn last_element(&self) -> ValueResult<&PactValue>;

    /// Take first n elements
    fn take(&self, n: usize) -> PactList;

    /// Drop first n elements
    fn drop(&self, n: usize) -> PactList;

    /// Reverse the list
    fn pact_reverse(&self) -> PactList;

    /// Map over elements
    fn pact_map<F>(&self, f: F) -> ValueResult<PactList>
    where
        F: Fn(&PactValue) -> ValueResult<PactValue>;

    /// Filter elements
    fn pact_filter<F>(&self, f: F) -> ValueResult<PactList>
    where
        F: Fn(&PactValue) -> ValueResult<bool>;
}

impl PactListExt for PactList {
    fn get_at(&self, index: usize) -> ValueResult<&PactValue> {
        self.get(index)
            .ok_or_else(|| ValueError::IndexOutOfBounds { index })
    }

    fn pact_length(&self) -> usize {
        self.len()
    }

    fn is_pact_empty(&self) -> bool {
        self.is_empty()
    }

    fn first_element(&self) -> ValueResult<&PactValue> {
        self.get_at(0)
    }

    fn last_element(&self) -> ValueResult<&PactValue> {
        if self.is_empty() {
            Err(ValueError::IndexOutOfBounds { index: 0 })
        } else {
            Ok(&self[self.len() - 1])
        }
    }

    fn take(&self, n: usize) -> PactList {
        self.iter().take(n).cloned().collect()
    }

    fn drop(&self, n: usize) -> PactList {
        self.iter().skip(n).cloned().collect()
    }

    fn pact_reverse(&self) -> PactList {
        let mut reversed = self.clone();
        reversed.reverse();
        reversed
    }

    fn pact_map<F>(&self, f: F) -> ValueResult<PactList>
    where
        F: Fn(&PactValue) -> ValueResult<PactValue>,
    {
        self.iter().map(f).collect::<ValueResult<Vec<_>>>()
    }

    fn pact_filter<F>(&self, f: F) -> ValueResult<PactList>
    where
        F: Fn(&PactValue) -> ValueResult<bool>,
    {
        let mut result = Vec::new();
        for item in self {
            if f(item)? {
                result.push(item.clone());
            }
        }
        Ok(result)
    }
}

/// Convenience constructors
impl Object {
    /// Create object with one field
    pub fn singleton<S: Into<String>>(field: S, value: PactValue) -> Self {
        let mut obj = Object::new();
        obj.insert(field, value);
        obj
    }

    /// Create object from literal field definitions
    pub fn literal(fields: &[(&str, PactValue)]) -> Self {
        let pairs = fields.iter().map(|(k, v)| (k.to_string(), v.clone()));
        Object::from_pairs(pairs)
    }
}

/// JSON conversion support
impl Object {
    /// Convert to JSON object
    pub fn to_json(&self) -> ValueResult<serde_json::Value> {
        let mut map = serde_json::Map::new();
        for (key, value) in &self.fields {
            let json_value = value.to_json();
            map.insert(key.clone(), json_value);
        }
        Ok(serde_json::Value::Object(map))
    }

    /// Parse from JSON object
    pub fn from_json(json: serde_json::Value) -> ValueResult<Self> {
        match json {
            serde_json::Value::Object(map) => {
                let mut object = Object::new();
                for (key, value) in map {
                    let pact_value = PactValue::from_json(value)?;
                    object.insert(key, pact_value);
                }
                Ok(object)
            }
            _ => Err(ValueError::TypeMismatch {
                expected: "JSON object".to_string(),
                actual: "other JSON type".to_string(),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::PactValue;

    #[test]
    fn test_object_creation() {
        let obj = Object::new();
        assert!(obj.is_empty());
        assert_eq!(obj.len(), 0);

        let obj2 = Object::singleton("name", PactValue::string("test"));
        assert_eq!(obj2.len(), 1);
        assert_eq!(obj2.get("name").unwrap().as_string().unwrap(), "test");
    }

    #[test]
    fn test_object_operations() {
        let mut obj = Object::new();

        // Insert fields
        obj.insert("name", PactValue::string("Alice"));
        obj.insert("age", PactValue::integer(30));

        assert_eq!(obj.len(), 2);
        assert!(obj.contains_field("name"));
        assert!(obj.contains_field("age"));
        assert!(!obj.contains_field("email"));

        // Access fields
        assert_eq!(obj.get_string("name").unwrap(), "Alice");
        assert_eq!(
            *obj.get_integer("age").unwrap(),
            num_bigint::BigInt::from(30)
        );

        // Remove field
        let removed = obj.remove("age");
        assert!(removed.is_some());
        assert_eq!(obj.len(), 1);
        assert!(!obj.contains_field("age"));
    }

    #[test]
    fn test_object_iteration() {
        let obj = Object::literal(&[
            ("a", PactValue::integer(1)),
            ("b", PactValue::integer(2)),
            ("c", PactValue::integer(3)),
        ]);

        let field_names: Vec<&String> = obj.field_names().collect();
        assert_eq!(
            field_names,
            vec![&"a".to_string(), &"b".to_string(), &"c".to_string()]
        );

        let mut sum = num_bigint::BigInt::from(0);
        for (_, value) in &obj {
            if let Ok(int_val) = value.as_integer() {
                sum += int_val;
            }
        }
        assert_eq!(sum, num_bigint::BigInt::from(6));
    }

    #[test]
    fn test_list_extensions() {
        let list: PactList = vec![
            PactValue::integer(1),
            PactValue::integer(2),
            PactValue::integer(3),
            PactValue::integer(4),
        ];

        assert_eq!(list.pact_length(), 4);
        assert!(!list.is_pact_empty());

        assert_eq!(
            *list.first_element().unwrap().as_integer().unwrap(),
            num_bigint::BigInt::from(1)
        );
        assert_eq!(
            *list.last_element().unwrap().as_integer().unwrap(),
            num_bigint::BigInt::from(4)
        );

        let taken = list.take(2);
        assert_eq!(taken.len(), 2);

        let dropped = list.drop(2);
        assert_eq!(dropped.len(), 2);

        let reversed = list.pact_reverse();
        assert_eq!(
            *reversed.first_element().unwrap().as_integer().unwrap(),
            num_bigint::BigInt::from(4)
        );
    }

    #[test]
    fn test_list_functional_operations() {
        let list: PactList = vec![
            PactValue::integer(1),
            PactValue::integer(2),
            PactValue::integer(3),
        ];

        // Map: double each number
        let doubled = list
            .pact_map(|v| {
                let int_val = v.as_integer()?;
                Ok(PactValue::integer(int_val * 2))
            })
            .unwrap();

        assert_eq!(
            *doubled[0].as_integer().unwrap(),
            num_bigint::BigInt::from(2)
        );
        assert_eq!(
            *doubled[1].as_integer().unwrap(),
            num_bigint::BigInt::from(4)
        );
        assert_eq!(
            *doubled[2].as_integer().unwrap(),
            num_bigint::BigInt::from(6)
        );

        // Filter: only even numbers (after doubling)
        let evens = doubled
            .pact_filter(|v| {
                let int_val = v.as_integer()?;
                Ok(int_val % 2 == num_bigint::BigInt::from(0))
            })
            .unwrap();

        assert_eq!(evens.len(), 3); // All doubled numbers are even
    }

    #[test]
    fn test_object_json_conversion() {
        let obj = Object::literal(&[
            ("name", PactValue::string("test")),
            ("count", PactValue::integer(42)),
        ]);

        let json = obj.to_json().unwrap();
        let recovered = Object::from_json(json).unwrap();

        assert_eq!(obj, recovered);
    }
}
