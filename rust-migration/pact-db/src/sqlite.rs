//! SQLite database backend for production use
//!
//! This module provides a SQLite-based implementation of the PactDb trait,
//! suitable for production use with persistent storage.

use crate::serialization::CborSerializer;
use crate::traits::{DbFactory, DbSerializer, PactDb};
use crate::types::{
    DbError, DbResult, DbStats, Domain, ExecutionMode, Purity, RowData, RowKey, SelectCriteria,
    TableName, TxId, TxLog, WriteType,
};
use crate::validation::DefaultValidator;
use pact_names;
use pact_schema::Schema;
use rusqlite::{params, Connection, OptionalExtension};
use std::collections::HashMap;
use std::path::Path;
use std::sync::{Arc, Mutex};

/// SQLite database configuration
#[derive(Debug, Clone)]
pub struct SqliteConfig {
    /// Path to the database file (":memory:" for in-memory)
    pub path: String,
    /// Enable WAL mode for better concurrency
    pub enable_wal: bool,
    /// Journal mode
    pub journal_mode: String,
    /// Synchronous mode
    pub synchronous: String,
    /// Cache size in KB
    pub cache_size: i32,
    /// Timeout for busy connections (ms)
    pub busy_timeout: u32,
}

impl SqliteConfig {
    /// Create a new configuration for a file database
    pub fn file<P: AsRef<Path>>(path: P) -> Self {
        SqliteConfig {
            path: path.as_ref().to_string_lossy().to_string(),
            enable_wal: true,
            journal_mode: "WAL".to_string(),
            synchronous: "NORMAL".to_string(),
            cache_size: 10000,  // 10MB
            busy_timeout: 5000, // 5 seconds
        }
    }

    /// Create a new configuration for an in-memory database
    pub fn memory() -> Self {
        SqliteConfig {
            path: ":memory:".to_string(),
            enable_wal: false, // WAL doesn't work with in-memory
            journal_mode: "MEMORY".to_string(),
            synchronous: "OFF".to_string(),
            cache_size: 10000,
            busy_timeout: 1000,
        }
    }
}

impl Default for SqliteConfig {
    fn default() -> Self {
        Self::memory()
    }
}

/// SQLite database implementation
pub struct SqliteDb {
    /// Database connection
    conn: Arc<Mutex<Connection>>,
    /// Current transaction state
    current_tx: Arc<Mutex<Option<TxState>>>,
    /// Schema validator
    validator: DefaultValidator,
    /// Database configuration
    config: SqliteConfig,
}

/// Transaction state for SQLite
#[derive(Debug)]
struct TxState {
    /// Transaction ID
    id: TxId,
    /// Execution mode
    mode: ExecutionMode,
    /// Transaction logs
    logs: Vec<TxLog>,
}

impl SqliteDb {
    /// Create a new SQLite database with the given configuration
    pub fn new(config: SqliteConfig) -> DbResult<Self> {
        let conn = Connection::open(&config.path).map_err(|e| DbError::Backend {
            reason: format!("Failed to open SQLite database: {}", e),
        })?;

        let db = SqliteDb {
            conn: Arc::new(Mutex::new(conn)),
            current_tx: Arc::new(Mutex::new(None)),
            validator: DefaultValidator::new(),
            config,
        };

        db.initialize()?;
        Ok(db)
    }

    /// Initialize the database schema
    fn initialize(&self) -> DbResult<()> {
        let conn = self.conn.lock().unwrap();

        // Configure SQLite settings
        conn.execute_batch(&format!(
            "
            PRAGMA journal_mode = {};
            PRAGMA synchronous = {};
            PRAGMA cache_size = {};
            PRAGMA busy_timeout = {};
            PRAGMA foreign_keys = ON;
            ",
            self.config.journal_mode,
            self.config.synchronous,
            self.config.cache_size,
            self.config.busy_timeout
        ))
        .map_err(|e| DbError::Backend {
            reason: format!("Failed to configure SQLite: {}", e),
        })?;

        // Create system tables
        conn.execute_batch(
            "
            -- Key-value storage for all domains
            CREATE TABLE IF NOT EXISTS kv_store (
                domain TEXT NOT NULL,
                key TEXT NOT NULL,
                value BLOB NOT NULL,
                created_at INTEGER DEFAULT (strftime('%s', 'now')),
                updated_at INTEGER DEFAULT (strftime('%s', 'now')),
                PRIMARY KEY (domain, key)
            );

            -- Table schemas storage
            CREATE TABLE IF NOT EXISTS table_schemas (
                table_name TEXT PRIMARY KEY,
                schema_data BLOB NOT NULL,
                created_at INTEGER DEFAULT (strftime('%s', 'now'))
            );

            -- Transaction logs
            CREATE TABLE IF NOT EXISTS tx_logs (
                tx_id INTEGER NOT NULL,
                domain TEXT NOT NULL,
                key TEXT NOT NULL,
                value BLOB NOT NULL,
                timestamp INTEGER DEFAULT (strftime('%s', 'now')),
                PRIMARY KEY (tx_id, domain, key)
            );

            -- Indexes for performance
            CREATE INDEX IF NOT EXISTS idx_kv_domain ON kv_store(domain);
            CREATE INDEX IF NOT EXISTS idx_kv_key ON kv_store(key);
            CREATE INDEX IF NOT EXISTS idx_tx_logs_tx_id ON tx_logs(tx_id);

            -- Trigger to update updated_at
            CREATE TRIGGER IF NOT EXISTS update_kv_timestamp
            AFTER UPDATE ON kv_store
            BEGIN
                UPDATE kv_store SET updated_at = strftime('%s', 'now')
                WHERE domain = NEW.domain AND key = NEW.key;
            END;
            ",
        )
        .map_err(|e| DbError::Backend {
            reason: format!("Failed to create schema: {}", e),
        })?;

        Ok(())
    }

    /// Execute with connection
    fn with_connection<F, R>(&self, f: F) -> DbResult<R>
    where
        F: FnOnce(&Connection) -> DbResult<R>,
    {
        let conn = self.conn.lock().unwrap();
        f(&*conn)
    }

    /// Add transaction log entry
    fn add_tx_log(&self, domain: &Domain, key: &str, value: &[u8]) {
        if let Ok(mut tx_opt) = self.current_tx.lock() {
            if let Some(ref mut tx) = *tx_opt {
                tx.logs
                    .push(TxLog::new(domain.render(), key.to_string(), value.to_vec()));
            }
        }
    }
}

impl PactDb for SqliteDb {
    fn purity(&self) -> Purity {
        Purity::Impure
    }

    fn read_raw(&self, domain: &Domain, key: &str) -> DbResult<Option<Vec<u8>>> {
        self.with_connection(|conn| {
            let mut stmt = conn
                .prepare_cached("SELECT value FROM kv_store WHERE domain = ? AND key = ?")
                .map_err(|e| DbError::Backend {
                    reason: format!("Failed to prepare statement: {}", e),
                })?;

            let result = stmt
                .query_row(params![domain.render(), key], |row| {
                    row.get::<_, Vec<u8>>(0)
                })
                .optional()
                .map_err(|e| DbError::Backend {
                    reason: format!("Failed to read: {}", e),
                })?;

            Ok(result)
        })
    }

    fn write_raw(
        &self,
        write_type: WriteType,
        domain: &Domain,
        key: &str,
        value: &[u8],
    ) -> DbResult<()> {
        self.with_connection(|conn| {
            let domain_str = domain.render();

            match write_type {
                WriteType::Insert => {
                    // Check if key exists
                    let exists: bool = conn
                        .prepare_cached("SELECT 1 FROM kv_store WHERE domain = ? AND key = ?")
                        .map_err(|e| DbError::Backend {
                            reason: format!("Failed to prepare statement: {}", e),
                        })?
                        .query_row(params![domain_str, key], |_| Ok(true))
                        .optional()
                        .map_err(|e| DbError::Backend {
                            reason: format!("Failed to check existence: {}", e),
                        })?
                        .is_some();

                    if exists {
                        return Err(DbError::RowExists {
                            table: domain_str,
                            key: key.to_string(),
                        });
                    }

                    conn.execute(
                        "INSERT INTO kv_store (domain, key, value) VALUES (?, ?, ?)",
                        params![domain_str, key, value],
                    )
                    .map_err(|e| DbError::Backend {
                        reason: format!("Failed to insert: {}", e),
                    })?;
                }
                WriteType::Update => {
                    let rows_affected = conn
                        .execute(
                            "UPDATE kv_store SET value = ? WHERE domain = ? AND key = ?",
                            params![value, domain_str, key],
                        )
                        .map_err(|e| DbError::Backend {
                            reason: format!("Failed to update: {}", e),
                        })?;

                    if rows_affected == 0 {
                        return Err(DbError::RowNotFound {
                            table: domain_str,
                            key: key.to_string(),
                        });
                    }
                }
                WriteType::Write => {
                    // Upsert
                    conn.execute(
                        "INSERT OR REPLACE INTO kv_store (domain, key, value) VALUES (?, ?, ?)",
                        params![domain_str, key, value],
                    )
                    .map_err(|e| DbError::Backend {
                        reason: format!("Failed to upsert: {}", e),
                    })?;
                }
            }

            self.add_tx_log(domain, key, value);
            Ok(())
        })
    }

    fn keys_raw(&self, domain: &Domain) -> DbResult<Vec<String>> {
        self.with_connection(|conn| {
            let mut stmt = conn
                .prepare_cached("SELECT key FROM kv_store WHERE domain = ? ORDER BY key")
                .map_err(|e| DbError::Backend {
                    reason: format!("Failed to prepare statement: {}", e),
                })?;

            let rows = stmt
                .query_map(params![domain.render()], |row| row.get::<_, String>(0))
                .map_err(|e| DbError::Backend {
                    reason: format!("Failed to query keys: {}", e),
                })?;

            let mut keys = Vec::new();
            for row in rows {
                keys.push(row.map_err(|e| DbError::Backend {
                    reason: format!("Failed to read key: {}", e),
                })?);
            }

            Ok(keys)
        })
    }

    fn create_user_table(&self, table: &TableName, schema: &Schema) -> DbResult<()> {
        self.with_connection(|conn| {
            let table_key = table.render();

            // Check if table already exists
            let exists: bool = conn
                .prepare_cached("SELECT 1 FROM table_schemas WHERE table_name = ?")
                .map_err(|e| DbError::Backend {
                    reason: format!("Failed to prepare statement: {}", e),
                })?
                .query_row(params![table_key], |_| Ok(true))
                .optional()
                .map_err(|e| DbError::Backend {
                    reason: format!("Failed to check table existence: {}", e),
                })?
                .is_some();

            if exists {
                return Err(DbError::TableExists { table: table_key });
            }

            // Serialize schema as JSON bytes directly
            let schema_bytes = serde_json::to_vec(schema).map_err(|e| DbError::Serialization {
                reason: format!("Failed to serialize schema: {}", e),
            })?;

            // Insert schema
            conn.execute(
                "INSERT INTO table_schemas (table_name, schema_data) VALUES (?, ?)",
                params![table_key, schema_bytes],
            )
            .map_err(|e| DbError::Backend {
                reason: format!("Failed to insert schema: {}", e),
            })?;

            Ok(())
        })
    }

    fn drop_user_table(&self, table: &TableName) -> DbResult<()> {
        self.with_connection(|conn| {
            let table_key = table.render();

            let rows_affected = conn
                .execute(
                    "DELETE FROM table_schemas WHERE table_name = ?",
                    params![table_key],
                )
                .map_err(|e| DbError::Backend {
                    reason: format!("Failed to delete schema: {}", e),
                })?;

            if rows_affected == 0 {
                return Err(DbError::TableNotFound { table: table_key });
            }

            // Remove table data
            let domain = Domain::UserTables(table.clone());
            conn.execute(
                "DELETE FROM kv_store WHERE domain = ?",
                params![domain.render()],
            )
            .map_err(|e| DbError::Backend {
                reason: format!("Failed to delete table data: {}", e),
            })?;

            Ok(())
        })
    }

    fn table_exists(&self, table: &TableName) -> DbResult<bool> {
        self.with_connection(|conn| {
            let exists = conn
                .prepare_cached("SELECT 1 FROM table_schemas WHERE table_name = ?")
                .map_err(|e| DbError::Backend {
                    reason: format!("Failed to prepare statement: {}", e),
                })?
                .query_row(params![table.render()], |_| Ok(true))
                .optional()
                .map_err(|e| DbError::Backend {
                    reason: format!("Failed to check table existence: {}", e),
                })?
                .is_some();

            Ok(exists)
        })
    }

    fn get_table_schema(&self, table: &TableName) -> DbResult<Option<Schema>> {
        self.with_connection(|conn| {
            let schema_bytes: Option<Vec<u8>> = conn
                .prepare_cached("SELECT schema_data FROM table_schemas WHERE table_name = ?")
                .map_err(|e| DbError::Backend {
                    reason: format!("Failed to prepare statement: {}", e),
                })?
                .query_row(params![table.render()], |row| row.get(0))
                .optional()
                .map_err(|e| DbError::Backend {
                    reason: format!("Failed to read schema: {}", e),
                })?;

            if let Some(bytes) = schema_bytes {
                let schema: Schema =
                    serde_json::from_slice(&bytes).map_err(|e| DbError::Serialization {
                        reason: format!("Failed to deserialize schema: {}", e),
                    })?;
                Ok(Some(schema))
            } else {
                Ok(None)
            }
        })
    }

    fn list_tables(&self) -> DbResult<Vec<TableName>> {
        self.with_connection(|conn| {
            let mut stmt = conn
                .prepare_cached("SELECT table_name FROM table_schemas ORDER BY table_name")
                .map_err(|e| DbError::Backend {
                    reason: format!("Failed to prepare statement: {}", e),
                })?;

            let rows = stmt
                .query_map(params![], |row| row.get::<_, String>(0))
                .map_err(|e| DbError::Backend {
                    reason: format!("Failed to query tables: {}", e),
                })?;

            let mut tables = Vec::new();
            for row in rows {
                let table_key = row.map_err(|e| DbError::Backend {
                    reason: format!("Failed to read table name: {}", e),
                })?;

                // Parse table key back to TableName
                if let Some(underscore_pos) = table_key.find('_') {
                    let module_part = &table_key[..underscore_pos];
                    let table_part = &table_key[underscore_pos + 1..];

                    let module = pact_names::ModuleName::simple(module_part.to_string());
                    let table = TableName::new(table_part.to_string(), module);
                    tables.push(table);
                }
            }

            Ok(tables)
        })
    }

    fn read_row(&self, table: &TableName, key: &RowKey) -> DbResult<Option<RowData>> {
        let domain = Domain::UserTables(table.clone());

        if let Some(bytes) = self.read_raw(&domain, &key.0)? {
            let serializer = CborSerializer::new();
            let row = serializer.deserialize_row(&bytes)?;
            Ok(Some(row))
        } else {
            Ok(None)
        }
    }

    fn write_row(
        &self,
        write_type: WriteType,
        table: &TableName,
        key: &RowKey,
        row: &RowData,
    ) -> DbResult<()> {
        // Validate against schema
        if let Some(schema) = self.get_table_schema(table)? {
            crate::validation::validate_row_with_context(
                &self.validator,
                table,
                &key.0,
                &schema,
                row,
            )?;
        }

        let domain = Domain::UserTables(table.clone());
        let serializer = CborSerializer::new();
        let bytes = serializer.serialize_row(row)?;

        self.write_raw(write_type, &domain, &key.0, &bytes)
    }

    fn table_keys(&self, table: &TableName) -> DbResult<Vec<RowKey>> {
        let domain = Domain::UserTables(table.clone());
        let keys = self.keys_raw(&domain)?;
        Ok(keys.into_iter().map(RowKey).collect())
    }

    fn select_rows(
        &self,
        table: &TableName,
        criteria: &SelectCriteria,
    ) -> DbResult<Vec<(RowKey, RowData)>> {
        let keys = self.table_keys(table)?;
        let mut results = Vec::new();

        for key in keys {
            if let Some(row) = self.read_row(table, &key)? {
                // Apply filter if present
                if let Some(ref filter) = criteria.filter {
                    if !filter(&row) {
                        continue;
                    }
                }

                // Apply field projection if present
                let projected_row = if let Some(ref fields) = criteria.fields {
                    row.project(fields)
                } else {
                    row
                };

                results.push((key, projected_row));
            }
        }

        // Apply offset and limit
        let start = criteria.offset.unwrap_or(0);
        let end = if let Some(limit) = criteria.limit {
            (start + limit).min(results.len())
        } else {
            results.len()
        };

        if start < results.len() {
            Ok(results[start..end].to_vec())
        } else {
            Ok(Vec::new())
        }
    }

    fn count_rows(&self, table: &TableName) -> DbResult<usize> {
        let domain = Domain::UserTables(table.clone());
        self.with_connection(|conn| {
            let count: i64 = conn
                .prepare_cached("SELECT COUNT(*) FROM kv_store WHERE domain = ?")
                .map_err(|e| DbError::Backend {
                    reason: format!("Failed to prepare statement: {}", e),
                })?
                .query_row(params![domain.render()], |row| row.get(0))
                .map_err(|e| DbError::Backend {
                    reason: format!("Failed to count rows: {}", e),
                })?;

            Ok(count as usize)
        })
    }

    fn begin_tx(&self, mode: ExecutionMode) -> DbResult<Option<TxId>> {
        let mut tx_opt = self.current_tx.lock().unwrap();

        if tx_opt.is_some() {
            return Err(DbError::TransactionActive {
                tx_id: tx_opt.as_ref().unwrap().id,
            });
        }

        // Generate transaction ID
        let tx_id = TxId(chrono::Utc::now().timestamp_millis() as u64);

        self.with_connection(|conn| {
            conn.execute("BEGIN IMMEDIATE", params![])
                .map_err(|e| DbError::Backend {
                    reason: format!("Failed to begin transaction: {}", e),
                })?;
            Ok(())
        })?;

        let tx_state = TxState {
            id: tx_id,
            mode,
            logs: Vec::new(),
        };

        *tx_opt = Some(tx_state);
        Ok(Some(tx_id))
    }

    fn commit_tx(&self) -> DbResult<Vec<TxLog>> {
        let mut tx_opt = self.current_tx.lock().unwrap();

        if let Some(tx_state) = tx_opt.take() {
            let logs = tx_state.logs.clone();

            match tx_state.mode {
                ExecutionMode::Transactional => {
                    // Commit the transaction
                    self.with_connection(|conn| {
                        conn.execute("COMMIT", params![])
                            .map_err(|e| DbError::Backend {
                                reason: format!("Failed to commit transaction: {}", e),
                            })?;
                        Ok(())
                    })?;

                    Ok(logs)
                }
                ExecutionMode::Local => {
                    // Rollback for local execution
                    self.with_connection(|conn| {
                        conn.execute("ROLLBACK", params![])
                            .map_err(|e| DbError::Backend {
                                reason: format!("Failed to rollback transaction: {}", e),
                            })?;
                        Ok(())
                    })?;

                    Ok(logs)
                }
            }
        } else {
            Err(DbError::NoTransaction)
        }
    }

    fn rollback_tx(&self) -> DbResult<()> {
        let mut tx_opt = self.current_tx.lock().unwrap();

        if tx_opt.take().is_some() {
            self.with_connection(|conn| {
                conn.execute("ROLLBACK", params![])
                    .map_err(|e| DbError::Backend {
                        reason: format!("Failed to rollback transaction: {}", e),
                    })?;
                Ok(())
            })
        } else {
            Err(DbError::NoTransaction)
        }
    }

    fn current_tx(&self) -> Option<TxId> {
        self.current_tx.lock().unwrap().as_ref().map(|tx| tx.id)
    }

    fn get_stats(&self) -> DbResult<DbStats> {
        self.with_connection(|conn| {
            let table_count: i64 = conn
                .prepare_cached("SELECT COUNT(*) FROM table_schemas")
                .map_err(|e| DbError::Backend {
                    reason: format!("Failed to prepare statement: {}", e),
                })?
                .query_row(params![], |row| row.get(0))
                .map_err(|e| DbError::Backend {
                    reason: format!("Failed to count tables: {}", e),
                })?;

            let total_rows: i64 = conn
                .prepare_cached("SELECT COUNT(*) FROM kv_store")
                .map_err(|e| DbError::Backend {
                    reason: format!("Failed to prepare statement: {}", e),
                })?
                .query_row(params![], |row| row.get(0))
                .map_err(|e| DbError::Backend {
                    reason: format!("Failed to count rows: {}", e),
                })?;

            let current_tx = self.current_tx.lock().unwrap();

            let mut backend_stats = HashMap::new();
            backend_stats.insert("type".to_string(), "sqlite".to_string());
            backend_stats.insert("path".to_string(), self.config.path.clone());
            backend_stats.insert("journal_mode".to_string(), self.config.journal_mode.clone());

            Ok(DbStats {
                table_count: table_count as usize,
                total_rows: total_rows as usize,
                active_transactions: if current_tx.is_some() { 1 } else { 0 },
                backend_stats,
            })
        })
    }

    // === Module Operations ===
    
    fn write_module(&self, module_data: &pact_ir::CoreModuleData) -> DbResult<()> {
        // TODO: Implement proper SQLite module storage
        // For now, use the raw storage API
        let module_name = module_data.name();
        
        // Serialize module data to CBOR directly
        let mut serialized = Vec::new();
        ciborium::ser::into_writer(module_data, &mut serialized)
            .map_err(|e| DbError::Serialization { reason: e.to_string() })?;
        
        self.write_raw(WriteType::Write, &Domain::Modules, &module_name.render(), &serialized)
    }
    
    fn read_module(&self, module_name: &pact_names::ModuleName) -> DbResult<Option<pact_ir::CoreModuleData>> {
        if let Some(data) = self.read_raw(&Domain::Modules, &module_name.render())? {
            // Deserialize module data from CBOR
            let module_data = ciborium::de::from_reader(data.as_slice())
                .map_err(|e| DbError::Serialization { reason: e.to_string() })?;
            Ok(Some(module_data))
        } else {
            Ok(None)
        }
    }
    
    fn module_exists(&self, module_name: &pact_names::ModuleName) -> DbResult<bool> {
        Ok(self.read_raw(&Domain::Modules, &module_name.render())?.is_some())
    }
    
    fn list_modules(&self) -> DbResult<Vec<pact_names::ModuleName>> {
        let keys = self.keys_raw(&Domain::Modules)?;
        let mut modules = Vec::new();
        
        for key in keys {
            modules.push(pact_names::ModuleName::parse(&key));
        }
        
        Ok(modules)
    }
    
    fn write_module_source(&self, module_name: &pact_names::ModuleName, hash: &pact_ir::ModuleHash, source: &str) -> DbResult<()> {
        let key = format!("{}#{}", module_name.render(), hash.0);
        self.write_raw(WriteType::Write, &Domain::ModuleSource, &key, source.as_bytes())
    }
    
    fn read_module_source(&self, module_name: &pact_names::ModuleName, hash: &pact_ir::ModuleHash) -> DbResult<Option<String>> {
        let key = format!("{}#{}", module_name.render(), hash.0);
        if let Some(data) = self.read_raw(&Domain::ModuleSource, &key)? {
            let source = String::from_utf8(data)
                .map_err(|e| DbError::Serialization { reason: e.to_string() })?;
            Ok(Some(source))
        } else {
            Ok(None)
        }
    }

    fn compact(&self) -> DbResult<()> {
        self.with_connection(|conn| {
            conn.execute("VACUUM", params![])
                .map_err(|e| DbError::Backend {
                    reason: format!("Failed to vacuum database: {}", e),
                })?;
            Ok(())
        })
    }

    fn backup(&self, _path: &str) -> DbResult<()> {
        // TODO: Implement SQLite backup functionality
        // This would require additional rusqlite features or custom implementation
        Ok(())
    }

    fn restore(&self, _path: &str) -> DbResult<()> {
        // TODO: Implement SQLite restore functionality
        // This would require additional rusqlite features or custom implementation
        Ok(())
    }
}

impl DbFactory for SqliteDb {
    type Db = SqliteDb;
    type Config = SqliteConfig;

    fn create(config: Self::Config) -> DbResult<Self::Db> {
        SqliteDb::new(config)
    }

    fn create_memory() -> DbResult<Self::Db> {
        SqliteDb::new(SqliteConfig::memory())
    }
}
