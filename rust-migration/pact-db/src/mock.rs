//! Mock in-memory database implementation for testing
//!
//! This module provides a simple in-memory database backend that implements
//! all PactDb operations. It's primarily intended for testing and development.

use crate::serialization::CborSerializer;
use crate::traits::{DbSerializer, PactDb};
use crate::types::{
    DbError, DbResult, DbStats, Domain, ExecutionMode, Purity, RowData, RowKey, SelectCriteria,
    TableName, TxId, TxLog, WriteType,
};
use crate::validation::DefaultValidator;
use pact_names::ModuleName;
use pact_schema::Schema;
use std::collections::HashMap;
use std::sync::{Arc, Mutex, RwLock};

/// Transaction state for rollback capability
#[derive(Debug, Clone)]
struct TxState {
    /// Transaction ID
    id: TxId,
    /// Execution mode
    mode: ExecutionMode,
    /// Snapshot of data at transaction start
    snapshot: HashMap<String, HashMap<String, Vec<u8>>>,
    /// Transaction logs
    logs: Vec<TxLog>,
}

/// Mock database implementation
#[derive(Debug)]
pub struct MockDb {
    /// Data storage organized by domain -> key -> value
    data: Arc<RwLock<HashMap<String, HashMap<String, Vec<u8>>>>>,
    /// Table schemas
    schemas: Arc<RwLock<HashMap<String, Schema>>>,
    /// Current transaction state
    current_tx: Arc<Mutex<Option<TxState>>>,
    /// Transaction ID counter
    tx_counter: Arc<Mutex<u64>>,
    /// Schema validator
    validator: DefaultValidator,
    /// Purity level
    purity: Purity,
}

impl MockDb {
    /// Create a new mock database
    pub fn new() -> Self {
        MockDb {
            data: Arc::new(RwLock::new(HashMap::new())),
            schemas: Arc::new(RwLock::new(HashMap::new())),
            current_tx: Arc::new(Mutex::new(None)),
            tx_counter: Arc::new(Mutex::new(0)),
            validator: DefaultValidator::new(),
            purity: Purity::Impure,
        }
    }

    /// Create a pure mock database (for read-only operations)
    pub fn pure() -> Self {
        let mut db = Self::new();
        db.purity = Purity::Pure;
        db
    }

    /// Get domain key for storage
    fn domain_key(&self, domain: &Domain) -> String {
        domain.render()
    }

    /// Create snapshot of current data for transaction rollback
    fn create_snapshot(&self) -> HashMap<String, HashMap<String, Vec<u8>>> {
        self.data.read().unwrap().clone()
    }

    /// Restore data from snapshot
    fn restore_snapshot(&self, snapshot: HashMap<String, HashMap<String, Vec<u8>>>) {
        let mut data = self.data.write().unwrap();
        *data = snapshot;
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

    /// Get table schema if it exists
    fn get_schema(&self, table: &TableName) -> DbResult<Option<Schema>> {
        let table_key = table.render();
        let schemas = self.schemas.read().unwrap();
        Ok(schemas.get(&table_key).cloned())
    }

    /// Validate row against table schema
    fn validate_row_data(&self, table: &TableName, key: &RowKey, row: &RowData) -> DbResult<()> {
        if let Some(schema) = self.get_schema(table)? {
            crate::validation::validate_row_with_context(
                &self.validator,
                table,
                &key.0,
                &schema,
                row,
            )?;
        }
        Ok(())
    }
}

impl Default for MockDb {
    fn default() -> Self {
        Self::new()
    }
}

impl PactDb for MockDb {
    fn purity(&self) -> Purity {
        self.purity
    }

    fn read_raw(&self, domain: &Domain, key: &str) -> DbResult<Option<Vec<u8>>> {
        let data = self.data.read().unwrap();
        let domain_key = self.domain_key(domain);

        if let Some(domain_data) = data.get(&domain_key) {
            Ok(domain_data.get(key).cloned())
        } else {
            Ok(None)
        }
    }

    fn write_raw(
        &self,
        write_type: WriteType,
        domain: &Domain,
        key: &str,
        value: &[u8],
    ) -> DbResult<()> {
        let mut data = self.data.write().unwrap();
        let domain_key = self.domain_key(domain);

        let domain_data = data.entry(domain_key).or_insert_with(HashMap::new);

        match write_type {
            WriteType::Insert => {
                if domain_data.contains_key(key) {
                    return Err(DbError::RowExists {
                        table: domain.render(),
                        key: key.to_string(),
                    });
                }
            }
            WriteType::Update => {
                if !domain_data.contains_key(key) {
                    return Err(DbError::RowNotFound {
                        table: domain.render(),
                        key: key.to_string(),
                    });
                }
            }
            WriteType::Write => {
                // Always succeeds (upsert)
            }
        }

        domain_data.insert(key.to_string(), value.to_vec());
        self.add_tx_log(domain, key, value);

        Ok(())
    }

    fn keys_raw(&self, domain: &Domain) -> DbResult<Vec<String>> {
        let data = self.data.read().unwrap();
        let domain_key = self.domain_key(domain);

        if let Some(domain_data) = data.get(&domain_key) {
            Ok(domain_data.keys().cloned().collect())
        } else {
            Ok(Vec::new())
        }
    }

    fn create_user_table(&self, table: &TableName, schema: &Schema) -> DbResult<()> {
        let table_key = table.render();
        let mut schemas = self.schemas.write().unwrap();

        if schemas.contains_key(&table_key) {
            return Err(DbError::TableExists { table: table_key });
        }

        schemas.insert(table_key, schema.clone());

        // Create empty domain for this table
        let domain = Domain::UserTables(table.clone());
        let mut data = self.data.write().unwrap();
        data.entry(self.domain_key(&domain))
            .or_insert_with(HashMap::new);

        Ok(())
    }

    fn drop_user_table(&self, table: &TableName) -> DbResult<()> {
        let table_key = table.render();
        let mut schemas = self.schemas.write().unwrap();

        if !schemas.contains_key(&table_key) {
            return Err(DbError::TableNotFound { table: table_key });
        }

        schemas.remove(&table_key);

        // Remove table data
        let domain = Domain::UserTables(table.clone());
        let mut data = self.data.write().unwrap();
        data.remove(&self.domain_key(&domain));

        Ok(())
    }

    fn table_exists(&self, table: &TableName) -> DbResult<bool> {
        let table_key = table.render();
        let schemas = self.schemas.read().unwrap();
        Ok(schemas.contains_key(&table_key))
    }

    fn get_table_schema(&self, table: &TableName) -> DbResult<Option<Schema>> {
        self.get_schema(table)
    }

    fn list_tables(&self) -> DbResult<Vec<TableName>> {
        let schemas = self.schemas.read().unwrap();
        let mut tables = Vec::new();

        for table_key in schemas.keys() {
            // Parse table key back to TableName
            // This is a simplification - in practice we'd store more metadata
            if let Some(underscore_pos) = table_key.find('_') {
                let module_part = &table_key[..underscore_pos];
                let table_part = &table_key[underscore_pos + 1..];

                let module = ModuleName::simple(module_part.to_string());
                let table = TableName::new(table_part.to_string(), module);
                tables.push(table);
            }
        }

        Ok(tables)
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
        self.validate_row_data(table, key, row)?;

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
        let keys = self.table_keys(table)?;
        Ok(keys.len())
    }

    fn begin_tx(&self, mode: ExecutionMode) -> DbResult<Option<TxId>> {
        let mut tx_opt = self.current_tx.lock().unwrap();

        if tx_opt.is_some() {
            return Err(DbError::TransactionActive {
                tx_id: tx_opt.as_ref().unwrap().id,
            });
        }

        let mut counter = self.tx_counter.lock().unwrap();
        *counter += 1;
        let tx_id = TxId(*counter);

        let tx_state = TxState {
            id: tx_id,
            mode,
            snapshot: self.create_snapshot(),
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
                    // Keep changes, return logs
                    Ok(logs)
                }
                ExecutionMode::Local => {
                    // Rollback changes for local execution
                    self.restore_snapshot(tx_state.snapshot);
                    Ok(logs)
                }
            }
        } else {
            Err(DbError::NoTransaction)
        }
    }

    fn rollback_tx(&self) -> DbResult<()> {
        let mut tx_opt = self.current_tx.lock().unwrap();

        if let Some(tx_state) = tx_opt.take() {
            self.restore_snapshot(tx_state.snapshot);
            Ok(())
        } else {
            Err(DbError::NoTransaction)
        }
    }

    fn current_tx(&self) -> Option<TxId> {
        self.current_tx.lock().unwrap().as_ref().map(|tx| tx.id)
    }

    // === Module Operations ===
    
    fn write_module(&self, module_data: &pact_ir::CoreModuleData) -> DbResult<()> {
        let module_name = module_data.name();
        
        // Serialize module data to CBOR directly
        let mut serialized = Vec::new();
        ciborium::ser::into_writer(module_data, &mut serialized)
            .map_err(|e| DbError::Serialization { reason: e.to_string() })?;
        
        self.write_raw(WriteType::Write, &Domain::Modules, &module_name.render(), &serialized)
    }
    
    fn read_module(&self, module_name: &ModuleName) -> DbResult<Option<pact_ir::CoreModuleData>> {
        if let Some(data) = self.read_raw(&Domain::Modules, &module_name.render())? {
            // Deserialize module data from CBOR
            let module_data = ciborium::de::from_reader(data.as_slice())
                .map_err(|e| DbError::Serialization { reason: e.to_string() })?;
            Ok(Some(module_data))
        } else {
            Ok(None)
        }
    }
    
    fn module_exists(&self, module_name: &ModuleName) -> DbResult<bool> {
        Ok(self.read_raw(&Domain::Modules, &module_name.render())?.is_some())
    }
    
    fn list_modules(&self) -> DbResult<Vec<ModuleName>> {
        let keys = self.keys_raw(&Domain::Modules)?;
        let mut modules = Vec::new();
        
        for key in keys {
            // Parse module name from key format "modulename" or "namespace.modulename"
            modules.push(ModuleName::parse(&key));
        }
        
        Ok(modules)
    }
    
    fn write_module_source(&self, module_name: &ModuleName, hash: &pact_ir::ModuleHash, source: &str) -> DbResult<()> {
        let key = format!("{}#{}", module_name.render(), hash.0);
        self.write_raw(WriteType::Write, &Domain::ModuleSource, &key, source.as_bytes())
    }
    
    fn read_module_source(&self, module_name: &ModuleName, hash: &pact_ir::ModuleHash) -> DbResult<Option<String>> {
        let key = format!("{}#{}", module_name.render(), hash.0);
        if let Some(data) = self.read_raw(&Domain::ModuleSource, &key)? {
            let source = String::from_utf8(data)
                .map_err(|e| DbError::Serialization { reason: e.to_string() })?;
            Ok(Some(source))
        } else {
            Ok(None)
        }
    }

    fn get_stats(&self) -> DbResult<DbStats> {
        let data = self.data.read().unwrap();
        let schemas = self.schemas.read().unwrap();
        let current_tx = self.current_tx.lock().unwrap();

        let total_rows = data.values().map(|domain_data| domain_data.len()).sum();

        let mut backend_stats = HashMap::new();
        backend_stats.insert("type".to_string(), "mock".to_string());
        backend_stats.insert("domains".to_string(), data.len().to_string());

        Ok(DbStats {
            table_count: schemas.len(),
            total_rows,
            active_transactions: if current_tx.is_some() { 1 } else { 0 },
            backend_stats,
        })
    }

    fn compact(&self) -> DbResult<()> {
        // Mock implementation - no-op for in-memory
        Ok(())
    }

    fn backup(&self, _path: &str) -> DbResult<()> {
        // Mock implementation - could serialize to file
        Ok(())
    }

    fn restore(&self, _path: &str) -> DbResult<()> {
        // Mock implementation - could deserialize from file
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::traits::PactDbExt;
    use pact_names::{ModuleName, QualifiedName};
    use pact_schema::{Field, PrimType, Type};
    use pact_values::PactValue;
    use std::collections::HashMap;

    fn create_test_table_and_schema() -> (TableName, Schema) {
        let table = TableName::new("users".to_string(), ModuleName::simple("test".to_string()));

        let mut fields = HashMap::new();
        fields.insert(Field("name".to_string()), Type::Prim(PrimType::String));
        fields.insert(Field("age".to_string()), Type::Prim(PrimType::Integer));

        let schema = Schema {
            name: QualifiedName::new(ModuleName::simple("test".to_string()), "User".to_string()),
            fields,
        };

        (table, schema)
    }

    #[test]
    fn test_table_creation() {
        let db = MockDb::new();
        let (table, schema) = create_test_table_and_schema();

        assert!(!db.table_exists(&table).unwrap());

        db.create_user_table(&table, &schema).unwrap();

        assert!(db.table_exists(&table).unwrap());

        let retrieved_schema = db.get_table_schema(&table).unwrap().unwrap();
        assert_eq!(schema.name, retrieved_schema.name);
        assert_eq!(schema.fields.len(), retrieved_schema.fields.len());
    }

    #[test]
    fn test_row_operations() {
        let db = MockDb::new();
        let (table, schema) = create_test_table_and_schema();

        db.create_user_table(&table, &schema).unwrap();

        let key = RowKey("user1".to_string());
        let mut row_data = HashMap::new();
        row_data.insert("name".to_string(), PactValue::string("Alice"));
        row_data.insert("age".to_string(), PactValue::integer(30));
        let row = RowData::from_map(row_data);

        // Insert row
        db.insert_row(&table, &key, &row).unwrap();

        // Read row
        let read_row = db.read_row(&table, &key).unwrap().unwrap();
        assert_eq!(row, read_row);

        // Update row
        let mut updated_data = HashMap::new();
        updated_data.insert("name".to_string(), PactValue::string("Alice"));
        updated_data.insert("age".to_string(), PactValue::integer(31));
        let updated_row = RowData::from_map(updated_data);

        db.update_row(&table, &key, &updated_row).unwrap();

        let read_updated = db.read_row(&table, &key).unwrap().unwrap();
        assert_eq!(updated_row, read_updated);
    }

    #[test]
    fn test_write_type_constraints() {
        let db = MockDb::new();
        let (table, schema) = create_test_table_and_schema();

        db.create_user_table(&table, &schema).unwrap();

        let key = RowKey("user1".to_string());
        let mut row_data = HashMap::new();
        row_data.insert("name".to_string(), PactValue::string("Alice"));
        row_data.insert("age".to_string(), PactValue::integer(30));
        let row = RowData::from_map(row_data);

        // Insert should succeed
        db.write_row(WriteType::Insert, &table, &key, &row).unwrap();

        // Insert again should fail
        let result = db.write_row(WriteType::Insert, &table, &key, &row);
        assert!(matches!(result, Err(DbError::RowExists { .. })));

        // Update should succeed
        db.write_row(WriteType::Update, &table, &key, &row).unwrap();

        // Update non-existent row should fail
        let key2 = RowKey("user2".to_string());
        let result = db.write_row(WriteType::Update, &table, &key2, &row);
        assert!(matches!(result, Err(DbError::RowNotFound { .. })));

        // Write (upsert) should always succeed
        db.write_row(WriteType::Write, &table, &key2, &row).unwrap();
    }

    #[test]
    fn test_transactions() {
        let db = MockDb::new();
        let (table, schema) = create_test_table_and_schema();

        db.create_user_table(&table, &schema).unwrap();

        let key = RowKey("user1".to_string());
        let mut row_data = HashMap::new();
        row_data.insert("name".to_string(), PactValue::string("Alice"));
        row_data.insert("age".to_string(), PactValue::integer(30));
        let row = RowData::from_map(row_data);

        // Begin transaction
        let tx_id = db.begin_tx(ExecutionMode::Transactional).unwrap().unwrap();
        assert_eq!(db.current_tx(), Some(tx_id));

        // Insert row in transaction
        db.insert_row(&table, &key, &row).unwrap();

        // Commit transaction
        let logs = db.commit_tx().unwrap();
        assert!(!logs.is_empty());
        assert_eq!(db.current_tx(), None);

        // Row should still exist
        assert!(db.read_row(&table, &key).unwrap().is_some());
    }

    #[test]
    fn test_local_transaction_rollback() {
        let db = MockDb::new();
        let (table, schema) = create_test_table_and_schema();

        db.create_user_table(&table, &schema).unwrap();

        let key = RowKey("user1".to_string());
        let mut row_data = HashMap::new();
        row_data.insert("name".to_string(), PactValue::string("Alice"));
        row_data.insert("age".to_string(), PactValue::integer(30));
        let row = RowData::from_map(row_data);

        // Begin local transaction
        db.begin_tx(ExecutionMode::Local).unwrap();

        // Insert row in transaction
        db.insert_row(&table, &key, &row).unwrap();
        assert!(db.read_row(&table, &key).unwrap().is_some());

        // Commit local transaction (should rollback)
        db.commit_tx().unwrap();

        // Row should not exist
        assert!(db.read_row(&table, &key).unwrap().is_none());
    }

    #[test]
    fn test_schema_validation() {
        let db = MockDb::new();
        let (table, schema) = create_test_table_and_schema();

        db.create_user_table(&table, &schema).unwrap();

        let key = RowKey("user1".to_string());

        // Valid row
        let mut valid_data = HashMap::new();
        valid_data.insert("name".to_string(), PactValue::string("Alice"));
        valid_data.insert("age".to_string(), PactValue::integer(30));
        let valid_row = RowData::from_map(valid_data);

        assert!(db.insert_row(&table, &key, &valid_row).is_ok());

        // Invalid row (missing field)
        let mut invalid_data = HashMap::new();
        invalid_data.insert("name".to_string(), PactValue::string("Bob"));
        // Missing "age" field
        let invalid_row = RowData::from_map(invalid_data);

        let key2 = RowKey("user2".to_string());
        let result = db.insert_row(&table, &key2, &invalid_row);
        assert!(matches!(result, Err(DbError::MissingField { .. })));

        // Invalid row (wrong type)
        let mut wrong_type_data = HashMap::new();
        wrong_type_data.insert("name".to_string(), PactValue::string("Charlie"));
        wrong_type_data.insert("age".to_string(), PactValue::string("thirty"));
        let wrong_type_row = RowData::from_map(wrong_type_data);

        let key3 = RowKey("user3".to_string());
        let result = db.insert_row(&table, &key3, &wrong_type_row);
        assert!(matches!(result, Err(DbError::TypeMismatch { .. })));
    }
}
