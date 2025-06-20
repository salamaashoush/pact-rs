//! Core database types for Pact persistence layer
//!
//! This module defines the fundamental types and abstractions for Pact's
//! database operations, matching the Haskell implementation patterns.

use pact_names::ModuleName;
use pact_schema::Schema;
use pact_values::PactValue;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use thiserror::Error;

/// Table name with module qualification
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TableName {
    /// Table name
    pub name: String,
    /// Module that owns this table
    pub module: ModuleName,
}

impl TableName {
    /// Create a new table name
    pub fn new(name: String, module: ModuleName) -> Self {
        TableName { name, module }
    }

    /// Render table name for storage (moduleName_tableName)
    pub fn render(&self) -> String {
        format!("{}_{}", self.module.render(), self.name)
    }
}

impl fmt::Display for TableName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.render())
    }
}

/// Row key for database operations
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RowKey(pub String);

impl fmt::Display for RowKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for RowKey {
    fn from(s: String) -> Self {
        RowKey(s)
    }
}

impl From<&str> for RowKey {
    fn from(s: &str) -> Self {
        RowKey(s.to_string())
    }
}

/// Row data as key-value mapping
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RowData {
    /// Field values
    pub data: HashMap<String, PactValue>,
}

impl RowData {
    /// Create new empty row data
    pub fn new() -> Self {
        RowData {
            data: HashMap::new(),
        }
    }

    /// Create row data from field mappings
    pub fn from_map(data: HashMap<String, PactValue>) -> Self {
        RowData { data }
    }

    /// Get field value
    pub fn get_field(&self, field: &str) -> Option<&PactValue> {
        self.data.get(field)
    }

    /// Set field value
    pub fn set_field(&mut self, field: String, value: PactValue) {
        self.data.insert(field, value);
    }

    /// Get all field names
    pub fn field_names(&self) -> Vec<&String> {
        self.data.keys().collect()
    }

    /// Merge with another row (this takes precedence)
    pub fn merge(mut self, other: RowData) -> Self {
        for (field, value) in other.data {
            self.data.entry(field).or_insert(value);
        }
        self
    }

    /// Update with another row (other takes precedence)
    pub fn update(mut self, other: RowData) -> Self {
        self.data.extend(other.data);
        self
    }

    /// Convert to HashMap
    pub fn into_map(self) -> HashMap<String, PactValue> {
        self.data
    }

    /// Project to specific fields
    pub fn project(&self, fields: &[String]) -> RowData {
        let mut projected = HashMap::new();
        for field in fields {
            if let Some(value) = self.data.get(field) {
                projected.insert(field.clone(), value.clone());
            }
        }
        RowData::from_map(projected)
    }
}

impl Default for RowData {
    fn default() -> Self {
        Self::new()
    }
}

/// Write operation type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum WriteType {
    /// Insert new row (fail if key exists)
    Insert,
    /// Update existing row (fail if key doesn't exist)
    Update,
    /// Write/upsert row (always succeeds)
    Write,
}

impl fmt::Display for WriteType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WriteType::Insert => write!(f, "insert"),
            WriteType::Update => write!(f, "update"),
            WriteType::Write => write!(f, "write"),
        }
    }
}

/// Database domain types for different storage namespaces
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Domain {
    /// User-defined tables
    UserTables(TableName),
    /// System keysets
    KeySets,
    /// Modules and interfaces
    Modules,
    /// Namespaces
    Namespaces,
    /// DefPact execution state
    DefPacts,
    /// Module source code
    ModuleSource,
}

impl Domain {
    /// Render domain for storage key prefix
    pub fn render(&self) -> String {
        match self {
            Domain::UserTables(table) => format!("USER_TABLE_{}", table.render()),
            Domain::KeySets => "SYS_KEYSETS".to_string(),
            Domain::Modules => "SYS_MODULES".to_string(),
            Domain::Namespaces => "SYS_NAMESPACES".to_string(),
            Domain::DefPacts => "SYS_DEFPACTS".to_string(),
            Domain::ModuleSource => "SYS_MODULE_SOURCE".to_string(),
        }
    }
}

impl fmt::Display for Domain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.render())
    }
}

/// Execution mode for transactions
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ExecutionMode {
    /// Changes persist on commit
    Transactional,
    /// Changes rollback on commit (for local/read-only execution)
    Local,
}

impl fmt::Display for ExecutionMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExecutionMode::Transactional => write!(f, "transactional"),
            ExecutionMode::Local => write!(f, "local"),
        }
    }
}

/// Database purity level
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Purity {
    /// Pure (no side effects)
    Pure,
    /// Impure (has side effects)
    Impure,
}

/// Transaction ID
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TxId(pub u64);

impl fmt::Display for TxId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "tx:{}", self.0)
    }
}

/// Transaction log entry
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TxLog {
    /// Domain identifier
    pub domain: String,
    /// Key within domain
    pub key: String,
    /// Serialized value
    pub value: Vec<u8>,
}

impl TxLog {
    /// Create new transaction log entry
    pub fn new(domain: String, key: String, value: Vec<u8>) -> Self {
        TxLog { domain, key, value }
    }
}

/// Table definition value
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TableValue {
    /// Table name
    pub name: TableName,
    /// Table schema
    pub schema: Schema,
}

impl TableValue {
    /// Create new table definition
    pub fn new(name: TableName, schema: Schema) -> Self {
        TableValue { name, schema }
    }
}

/// Database errors
#[derive(Debug, Clone, Error, PartialEq, Serialize, Deserialize)]
pub enum DbError {
    /// Row not found
    #[error("Row not found: {table}.{key}")]
    RowNotFound { table: String, key: String },

    /// Row already exists
    #[error("Row already exists: {table}.{key}")]
    RowExists { table: String, key: String },

    /// Table not found
    #[error("Table not found: {table}")]
    TableNotFound { table: String },

    /// Table already exists
    #[error("Table already exists: {table}")]
    TableExists { table: String },

    /// Schema validation error
    #[error("Schema validation failed for {table}.{key}: {reason}")]
    SchemaValidation {
        table: String,
        key: String,
        reason: String,
    },

    /// Missing required field
    #[error("Missing required field '{field}' in {table}.{key}")]
    MissingField {
        table: String,
        key: String,
        field: String,
    },

    /// Type mismatch
    #[error(
        "Type mismatch for field '{field}' in {table}.{key}: expected {expected}, got {actual}"
    )]
    TypeMismatch {
        table: String,
        key: String,
        field: String,
        expected: String,
        actual: String,
    },

    /// No transaction active
    #[error("No transaction active")]
    NoTransaction,

    /// Transaction already active
    #[error("Transaction already active: {tx_id}")]
    TransactionActive { tx_id: TxId },

    /// Serialization error
    #[error("Serialization error: {reason}")]
    Serialization { reason: String },

    /// IO error
    #[error("IO error: {reason}")]
    Io { reason: String },

    /// Backend error
    #[error("Backend error: {reason}")]
    Backend { reason: String },
}

/// Database result type
pub type DbResult<T> = Result<T, DbError>;

/// Query filter function type
pub type QueryFilter = Box<dyn Fn(&RowData) -> bool + Send + Sync>;

/// Selection criteria for database queries
pub struct SelectCriteria {
    /// Optional filter predicate
    pub filter: Option<QueryFilter>,
    /// Optional field projection
    pub fields: Option<Vec<String>>,
    /// Optional limit on results
    pub limit: Option<usize>,
    /// Optional offset for pagination
    pub offset: Option<usize>,
}

impl std::fmt::Debug for SelectCriteria {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SelectCriteria")
            .field("filter", &"<function>")
            .field("fields", &self.fields)
            .field("limit", &self.limit)
            .field("offset", &self.offset)
            .finish()
    }
}

impl SelectCriteria {
    /// Create new empty selection criteria
    pub fn new() -> Self {
        SelectCriteria {
            filter: None,
            fields: None,
            limit: None,
            offset: None,
        }
    }

    /// Add filter predicate
    pub fn with_filter<F>(mut self, filter: F) -> Self
    where
        F: Fn(&RowData) -> bool + Send + Sync + 'static,
    {
        self.filter = Some(Box::new(filter));
        self
    }

    /// Add field projection
    pub fn with_fields(mut self, fields: Vec<String>) -> Self {
        self.fields = Some(fields);
        self
    }

    /// Add limit
    pub fn with_limit(mut self, limit: usize) -> Self {
        self.limit = Some(limit);
        self
    }

    /// Add offset
    pub fn with_offset(mut self, offset: usize) -> Self {
        self.offset = Some(offset);
        self
    }
}

impl Default for SelectCriteria {
    fn default() -> Self {
        Self::new()
    }
}

/// Database statistics
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DbStats {
    /// Number of tables
    pub table_count: usize,
    /// Total number of rows across all tables
    pub total_rows: usize,
    /// Number of active transactions
    pub active_transactions: usize,
    /// Backend-specific statistics
    pub backend_stats: HashMap<String, String>,
}

impl DbStats {
    /// Create new empty statistics
    pub fn new() -> Self {
        DbStats {
            table_count: 0,
            total_rows: 0,
            active_transactions: 0,
            backend_stats: HashMap::new(),
        }
    }
}

impl Default for DbStats {
    fn default() -> Self {
        Self::new()
    }
}
