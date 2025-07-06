//! Serialization for database operations
//!
//! This module provides CBOR-based serialization for PactValues and other
//! database types, maintaining compatibility with the Haskell implementation.

use crate::types::{DbError, DbResult, RowData};
use pact_core::values::PactValue;
use std::io::Cursor;

/// CBOR-based serializer for database values
#[derive(Debug, Clone)]
pub struct CborSerializer {
    /// Whether to use compact encoding
    compact: bool,
}

impl CborSerializer {
    /// Create a new CBOR serializer
    pub fn new() -> Self {
        CborSerializer { compact: true }
    }

    /// Create a new CBOR serializer with custom settings
    pub fn with_compact(compact: bool) -> Self {
        CborSerializer { compact }
    }

    /// Serialize a PactValue to bytes
    pub fn serialize(&self, value: &PactValue) -> DbResult<Vec<u8>> {
        let mut buffer = Vec::new();
        ciborium::ser::into_writer(value, &mut buffer).map_err(|e| DbError::Serialization {
            reason: format!("CBOR serialization failed: {}", e),
        })?;
        Ok(buffer)
    }

    /// Deserialize bytes to a PactValue
    pub fn deserialize(&self, bytes: &[u8]) -> DbResult<PactValue> {
        let cursor = Cursor::new(bytes);
        ciborium::de::from_reader(cursor).map_err(|e| DbError::Serialization {
            reason: format!("CBOR deserialization failed: {}", e),
        })
    }

    /// Serialize row data to bytes
    pub fn serialize_row(&self, row: &RowData) -> DbResult<Vec<u8>> {
        let mut buffer = Vec::new();
        ciborium::ser::into_writer(&row.data, &mut buffer).map_err(|e| DbError::Serialization {
            reason: format!("CBOR row serialization failed: {}", e),
        })?;
        Ok(buffer)
    }

    /// Deserialize bytes to row data
    pub fn deserialize_row(&self, bytes: &[u8]) -> DbResult<RowData> {
        let cursor = Cursor::new(bytes);
        let data = ciborium::de::from_reader(cursor).map_err(|e| DbError::Serialization {
            reason: format!("CBOR row deserialization failed: {}", e),
        })?;
        Ok(RowData::from_map(data))
    }
}

impl Default for CborSerializer {
    fn default() -> Self {
        Self::new()
    }
}

/// JSON serializer for debugging and compatibility
#[derive(Debug, Clone)]
pub struct JsonSerializer {
    /// Pretty print JSON
    pretty: bool,
}

impl JsonSerializer {
    /// Create a new JSON serializer
    pub fn new() -> Self {
        JsonSerializer { pretty: false }
    }

    /// Create a pretty-printing JSON serializer
    pub fn pretty() -> Self {
        JsonSerializer { pretty: true }
    }

    /// Serialize a PactValue to bytes
    pub fn serialize(&self, value: &PactValue) -> DbResult<Vec<u8>> {
        let result = if self.pretty {
            serde_json::to_vec_pretty(value)
        } else {
            serde_json::to_vec(value)
        };

        result.map_err(|e| DbError::Serialization {
            reason: format!("JSON serialization failed: {}", e),
        })
    }

    /// Deserialize bytes to a PactValue
    pub fn deserialize(&self, bytes: &[u8]) -> DbResult<PactValue> {
        serde_json::from_slice(bytes).map_err(|e| DbError::Serialization {
            reason: format!("JSON deserialization failed: {}", e),
        })
    }

    /// Serialize row data to bytes
    pub fn serialize_row(&self, row: &RowData) -> DbResult<Vec<u8>> {
        let result = if self.pretty {
            serde_json::to_vec_pretty(&row.data)
        } else {
            serde_json::to_vec(&row.data)
        };

        result.map_err(|e| DbError::Serialization {
            reason: format!("JSON row serialization failed: {}", e),
        })
    }

    /// Deserialize bytes to row data
    pub fn deserialize_row(&self, bytes: &[u8]) -> DbResult<RowData> {
        let data = serde_json::from_slice(bytes).map_err(|e| DbError::Serialization {
            reason: format!("JSON row deserialization failed: {}", e),
        })?;
        Ok(RowData::from_map(data))
    }
}

impl Default for JsonSerializer {
    fn default() -> Self {
        Self::new()
    }
}

/// Get the default serializer (CBOR)
pub fn default_serializer() -> CborSerializer {
    CborSerializer::new()
}

#[cfg(test)]
mod tests {
    use super::*;
    use pact_core::values::PactValue;
    use std::collections::HashMap;

    #[test]
    fn test_cbor_value_serialization() {
        let serializer = CborSerializer::new();

        // Test integer
        let int_val = PactValue::integer(42);
        let bytes = serializer.serialize(&int_val).unwrap();
        let deserialized = serializer.deserialize(&bytes).unwrap();
        assert_eq!(int_val, deserialized);

        // Test string
        let str_val = PactValue::string("hello world");
        let bytes = serializer.serialize(&str_val).unwrap();
        let deserialized = serializer.deserialize(&bytes).unwrap();
        assert_eq!(str_val, deserialized);

        // Test boolean
        let bool_val = PactValue::bool(true);
        let bytes = serializer.serialize(&bool_val).unwrap();
        let deserialized = serializer.deserialize(&bytes).unwrap();
        assert_eq!(bool_val, deserialized);
    }

    #[test]
    fn test_cbor_row_serialization() {
        let serializer = CborSerializer::new();

        let mut data = HashMap::new();
        data.insert("name".to_string(), PactValue::string("Alice"));
        data.insert("age".to_string(), PactValue::integer(30));
        data.insert("active".to_string(), PactValue::bool(true));

        let row = RowData::from_map(data);

        let bytes = serializer.serialize_row(&row).unwrap();
        let deserialized = serializer.deserialize_row(&bytes).unwrap();

        assert_eq!(row, deserialized);
    }

    #[test]
    fn test_json_value_serialization() {
        let serializer = JsonSerializer::new();

        // Test integer
        let int_val = PactValue::integer(42);
        let bytes = serializer.serialize(&int_val).unwrap();
        let deserialized = serializer.deserialize(&bytes).unwrap();
        assert_eq!(int_val, deserialized);

        // Test string
        let str_val = PactValue::string("hello world");
        let bytes = serializer.serialize(&str_val).unwrap();
        let deserialized = serializer.deserialize(&bytes).unwrap();
        assert_eq!(str_val, deserialized);
    }

    #[test]
    fn test_serialization_roundtrip() {
        let cbor = CborSerializer::new();
        let json = JsonSerializer::new();

        let original = PactValue::list(vec![
            PactValue::integer(1),
            PactValue::string("test"),
            PactValue::bool(false),
        ]);

        // Test CBOR roundtrip
        let cbor_bytes = cbor.serialize(&original).unwrap();
        let cbor_result = cbor.deserialize(&cbor_bytes).unwrap();
        assert_eq!(original, cbor_result);

        // Test JSON roundtrip
        let json_bytes = json.serialize(&original).unwrap();
        let json_result = json.deserialize(&json_bytes).unwrap();
        assert_eq!(original, json_result);
    }
}