//! Core database traits for Pact persistence layer
//!
//! This module defines the main database abstraction trait that all
//! Pact database backends must implement.

use crate::types::{
    DbResult, DbStats, Domain, ExecutionMode, Purity, RowData, RowKey, SelectCriteria, TableName,
    TxId, TxLog, WriteType,
};
use pact_schema::Schema;
use pact_values::PactValue;
use pact_names::ModuleName;
use pact_ir::{CoreModuleData, ModuleHash, Hash as TxHash};

/// Core database abstraction trait
///
/// This trait defines the interface that all Pact database backends must implement.
/// It supports both user tables and system domains, transactions, and various
/// query operations.
pub trait PactDb: Send + Sync {
    /// Get the purity level of this database
    fn purity(&self) -> Purity;

    // === Basic Key-Value Operations ===

    /// Read a value from a domain
    fn read_raw(&self, domain: &Domain, key: &str) -> DbResult<Option<Vec<u8>>>;

    /// Write a value to a domain
    fn write_raw(
        &self,
        write_type: WriteType,
        domain: &Domain,
        key: &str,
        value: &[u8],
    ) -> DbResult<()>;

    /// Get all keys in a domain
    fn keys_raw(&self, domain: &Domain) -> DbResult<Vec<String>>;

    // === Table Operations ===

    /// Create a user table
    fn create_user_table(&self, table: &TableName, schema: &Schema) -> DbResult<()>;

    /// Drop a user table
    fn drop_user_table(&self, table: &TableName) -> DbResult<()>;

    /// Check if a table exists
    fn table_exists(&self, table: &TableName) -> DbResult<bool>;

    /// Get table schema
    fn get_table_schema(&self, table: &TableName) -> DbResult<Option<Schema>>;

    /// List all user tables
    fn list_tables(&self) -> DbResult<Vec<TableName>>;

    // === Row Operations ===

    /// Read a row from a table
    fn read_row(&self, table: &TableName, key: &RowKey) -> DbResult<Option<RowData>>;

    /// Write a row to a table
    fn write_row(
        &self,
        write_type: WriteType,
        table: &TableName,
        key: &RowKey,
        row: &RowData,
    ) -> DbResult<()>;

    /// Get all keys in a table
    fn table_keys(&self, table: &TableName) -> DbResult<Vec<RowKey>>;

    /// Select rows from a table with criteria
    fn select_rows(
        &self,
        table: &TableName,
        criteria: &SelectCriteria,
    ) -> DbResult<Vec<(RowKey, RowData)>>;

    /// Count rows in a table
    fn count_rows(&self, table: &TableName) -> DbResult<usize>;

    // === Transaction Operations ===

    /// Begin a new transaction
    fn begin_tx(&self, mode: ExecutionMode) -> DbResult<Option<TxId>>;

    /// Commit the current transaction
    fn commit_tx(&self) -> DbResult<Vec<TxLog>>;

    /// Rollback the current transaction
    fn rollback_tx(&self) -> DbResult<()>;

    /// Get current transaction ID
    fn current_tx(&self) -> Option<TxId>;

    // === Module Operations ===

    /// Store a module in the database
    fn write_module(&self, module_data: &CoreModuleData) -> DbResult<()>;

    /// Read a module from the database
    fn read_module(&self, module_name: &ModuleName) -> DbResult<Option<CoreModuleData>>;

    /// Check if a module exists
    fn module_exists(&self, module_name: &ModuleName) -> DbResult<bool>;

    /// List all modules
    fn list_modules(&self) -> DbResult<Vec<ModuleName>>;

    /// Store module source code
    fn write_module_source(&self, module_name: &ModuleName, hash: &ModuleHash, source: &str) -> DbResult<()>;

    /// Read module source code by hash
    fn read_module_source(&self, module_name: &ModuleName, hash: &ModuleHash) -> DbResult<Option<String>>;

    // === Utility Operations ===

    /// Get database statistics
    fn get_stats(&self) -> DbResult<DbStats>;

    /// Compact/optimize the database
    fn compact(&self) -> DbResult<()>;

    /// Backup the database
    fn backup(&self, path: &str) -> DbResult<()>;

    /// Restore from backup
    fn restore(&self, path: &str) -> DbResult<()>;
}

/// High-level database operations that build on the core trait
pub trait PactDbExt: PactDb {
    /// Read with default value
    fn read_row_with_default(
        &self,
        table: &TableName,
        key: &RowKey,
        default: RowData,
    ) -> DbResult<RowData> {
        match self.read_row(table, key)? {
            Some(row) => Ok(row),
            None => Ok(default),
        }
    }

    /// Read specific fields from a row
    fn read_row_fields(
        &self,
        table: &TableName,
        key: &RowKey,
        fields: &[String],
    ) -> DbResult<Option<RowData>> {
        match self.read_row(table, key)? {
            Some(row) => Ok(Some(row.project(fields))),
            None => Ok(None),
        }
    }

    /// Insert row (convenience method)
    fn insert_row(&self, table: &TableName, key: &RowKey, row: &RowData) -> DbResult<()> {
        self.write_row(WriteType::Insert, table, key, row)
    }

    /// Update row (convenience method)
    fn update_row(&self, table: &TableName, key: &RowKey, row: &RowData) -> DbResult<()> {
        self.write_row(WriteType::Update, table, key, row)
    }

    /// Upsert row (convenience method)  
    fn upsert_row(&self, table: &TableName, key: &RowKey, row: &RowData) -> DbResult<()> {
        self.write_row(WriteType::Write, table, key, row)
    }

    /// Select all rows from a table
    fn select_all_rows(&self, table: &TableName) -> DbResult<Vec<(RowKey, RowData)>> {
        self.select_rows(table, &SelectCriteria::new())
    }

    /// Select rows with a simple filter
    fn select_rows_where<F>(&self, table: &TableName, filter: F) -> DbResult<Vec<(RowKey, RowData)>>
    where
        F: Fn(&RowData) -> bool + Send + Sync + 'static,
    {
        let criteria = SelectCriteria::new().with_filter(filter);
        self.select_rows(table, &criteria)
    }

    /// Select specific fields from all rows
    fn select_fields(
        &self,
        table: &TableName,
        fields: Vec<String>,
    ) -> DbResult<Vec<(RowKey, RowData)>> {
        let criteria = SelectCriteria::new().with_fields(fields);
        self.select_rows(table, &criteria)
    }

    /// Fold over all rows in a table
    fn fold_table<T, F>(&self, table: &TableName, init: T, mut f: F) -> DbResult<T>
    where
        F: FnMut(T, &RowKey, &RowData) -> T,
    {
        let rows = self.select_all_rows(table)?;
        let mut acc = init;
        for (key, row) in &rows {
            acc = f(acc, key, row);
        }
        Ok(acc)
    }

    /// Check if a row exists
    fn row_exists(&self, table: &TableName, key: &RowKey) -> DbResult<bool> {
        Ok(self.read_row(table, key)?.is_some())
    }

    /// Get the number of tables
    fn table_count(&self) -> DbResult<usize> {
        Ok(self.list_tables()?.len())
    }
}

// Automatically implement PactDbExt for any type that implements PactDb
impl<T: PactDb> PactDbExt for T {}

/// Database factory trait for creating database instances
pub trait DbFactory {
    type Db: PactDb;
    type Config;

    /// Create a new database instance
    fn create(config: Self::Config) -> DbResult<Self::Db>;

    /// Create an in-memory database for testing
    fn create_memory() -> DbResult<Self::Db>;
}

/// Schema validation trait
pub trait SchemaValidator {
    /// Validate a row against a schema
    fn validate_row(&self, schema: &Schema, row: &RowData) -> DbResult<()>;

    /// Check if a row has all required fields
    fn check_required_fields(&self, schema: &Schema, row: &RowData) -> DbResult<()>;

    /// Validate field types
    fn validate_field_types(&self, schema: &Schema, row: &RowData) -> DbResult<()>;
}

/// Serialization trait for database values
pub trait DbSerializer {
    /// Serialize a PactValue to bytes
    fn serialize(&self, value: &PactValue) -> DbResult<Vec<u8>>;

    /// Deserialize bytes to a PactValue
    fn deserialize(&self, bytes: &[u8]) -> DbResult<PactValue>;

    /// Serialize row data to bytes
    fn serialize_row(&self, row: &RowData) -> DbResult<Vec<u8>>;

    /// Deserialize bytes to row data
    fn deserialize_row(&self, bytes: &[u8]) -> DbResult<RowData>;
}

/// Database event listener for monitoring operations
pub trait DbEventListener {
    /// Called before a read operation
    fn on_read(&self, domain: &Domain, key: &str);

    /// Called after a successful read operation
    fn on_read_success(&self, domain: &Domain, key: &str, found: bool);

    /// Called before a write operation
    fn on_write(&self, write_type: WriteType, domain: &Domain, key: &str);

    /// Called after a successful write operation
    fn on_write_success(&self, write_type: WriteType, domain: &Domain, key: &str);

    /// Called before a transaction operation
    fn on_transaction(&self, mode: ExecutionMode);

    /// Called after a successful transaction commit
    fn on_commit(&self, logs: &[TxLog]);

    /// Called after a transaction rollback
    fn on_rollback(&self);

    /// Called on any database error
    fn on_error(&self, error: &crate::types::DbError);
}
