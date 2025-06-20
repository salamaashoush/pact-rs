//! Pact Database Operations and Persistence Layer
//!
//! This crate provides the database abstraction and persistence layer for the Pact
//! smart contract language. It includes:
//!
//! - Core database traits and types
//! - Schema validation
//! - Serialization (CBOR and JSON)
//! - Mock in-memory database for testing
//! - SQLite backend for production (optional)
//! - Transaction support with rollback capability
//!
//! ## Basic Usage
//!
//! ```rust
//! use pact_db::{MockDb, PactDb, PactDbExt, TableName, RowKey, RowData};
//! use pact_names::{ModuleName, QualifiedName};
//! use pact_schema::{Schema, Field, Type, PrimType};
//! use pact_values::PactValue;
//! use std::collections::HashMap;
//!
//! // Create a database
//! let db = MockDb::new();
//!
//! // Create a table
//! let table = TableName::new("users".to_string(), ModuleName::simple("test".to_string()));
//! let mut fields = HashMap::new();
//! fields.insert(Field("name".to_string()), Type::Prim(PrimType::String));
//! fields.insert(Field("age".to_string()), Type::Prim(PrimType::Integer));
//! let schema = Schema {
//!     name: QualifiedName::new(ModuleName::simple("test".to_string()), "User".to_string()),
//!     fields,
//! };
//!
//! db.create_user_table(&table, &schema).unwrap();
//!
//! // Insert a row
//! let key = RowKey::from("user1");
//! let mut row_data = HashMap::new();
//! row_data.insert("name".to_string(), PactValue::string("Alice"));
//! row_data.insert("age".to_string(), PactValue::integer(30));
//! let row = RowData::from_map(row_data);
//!
//! db.insert_row(&table, &key, &row).unwrap();
//!
//! // Read the row back
//! let retrieved = db.read_row(&table, &key).unwrap().unwrap();
//! assert_eq!(row, retrieved);
//! ```

pub mod mock;
pub mod operations;
pub mod serialization;
pub mod traits;
pub mod types;
pub mod validation;

#[cfg(feature = "sqlite")]
pub mod sqlite;

// Re-export core types and traits
pub use mock::MockDb;
pub use operations::{DatabaseInterface, DatabaseOperation};
pub use serialization::{default_serializer, CborSerializer, JsonSerializer};
pub use traits::{DbFactory, DbSerializer, PactDb, PactDbExt, SchemaValidator};
pub use types::*;
pub use validation::{validate_row_with_context, DefaultValidator, PermissiveValidator};

#[cfg(feature = "sqlite")]
pub use sqlite::SqliteDb;
