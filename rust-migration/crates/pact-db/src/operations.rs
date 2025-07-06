//! Database operations for the CEK machine
//!
//! This module defines database operation types and execution logic
//! for use in the Pact evaluator.

use crate::{PactDb, RowData, RowKey, SelectCriteria, TableName, WriteType};
use compact_str::CompactString;
use pact_core::errors::{PactError, EvalError};
use pact_core::shared::SpanInfo;
use pact_core::names::ModuleName;
use pact_core::values::PactValue;
use std::collections::HashMap;

/// Database operations for the CEK machine
#[derive(Debug, Clone)]
pub enum DatabaseOperation {
    /// Create table operation
    CreateTable {
        table_name: CompactString,
        schema: Option<pact_schema::Schema>,
    },

    /// Read a row from a table
    Read {
        table_name: CompactString,
        key: CompactString,
        fields: Option<Vec<CompactString>>,
    },

    /// Write a row to a table
    Write {
        table_name: CompactString,
        key: CompactString,
        row_data: HashMap<String, PactValue>,
    },

    /// Insert a new row (fails if exists)
    Insert {
        table_name: CompactString,
        key: CompactString,
        row_data: HashMap<String, PactValue>,
    },

    /// Update an existing row (fails if not exists)
    Update {
        table_name: CompactString,
        key: CompactString,
        row_data: HashMap<String, PactValue>,
    },

    /// Get all keys from a table
    Keys {
        table_name: CompactString,
    },

    /// Select rows with optional filtering
    Select {
        table_name: CompactString,
        where_clause: Option<CompactString>,
        fields: Option<Vec<CompactString>>,
    },

    /// Describe table schema
    DescribeTable {
        table_name: CompactString,
    },
}

/// Database interface for the CEK machine
#[derive(Clone)]
pub struct DatabaseInterface {
    /// Database instance
    pub db: std::sync::Arc<dyn PactDb>,
    /// Current module context for table namespacing
    pub current_module: ModuleName,
    /// Transaction state
    pub transaction_mode: crate::ExecutionMode,
}

impl std::fmt::Debug for DatabaseInterface {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DatabaseInterface")
            .field("current_module", &self.current_module)
            .field("transaction_mode", &self.transaction_mode)
            .field("db", &"<PactDb instance>")
            .finish()
    }
}

impl DatabaseInterface {
    /// Create a new database interface
    pub fn new(
        db: std::sync::Arc<dyn PactDb>,
        current_module: ModuleName,
        transaction_mode: crate::ExecutionMode,
    ) -> Self {
        Self {
            db,
            current_module,
            transaction_mode,
        }
    }

    /// Create table name with module qualification
    pub fn qualified_table_name(&self, table_name: &str) -> TableName {
        TableName::new(table_name.to_string(), self.current_module.clone())
    }

    /// Convert to database row data
    pub fn to_row_data(&self, data: HashMap<String, PactValue>) -> RowData {
        RowData::from_map(data)
    }

    /// Convert from database row data
    pub fn from_row_data(&self, row: RowData) -> HashMap<String, PactValue> {
        row.into_map()
    }

    /// Execute a database operation
    pub fn execute_operation(&self, operation: &DatabaseOperation) -> Result<PactValue, PactError<SpanInfo>> {
        match operation {
            DatabaseOperation::Read { table_name, key, fields } => {
                let table = self.qualified_table_name(table_name);
                let row_key = RowKey::from(key.as_str());

                match self.db.read_row(&table, &row_key) {
                    Ok(Some(row)) => {
                        let data = if let Some(field_list) = fields {
                            let field_names: Vec<String> = field_list.iter().map(|s| s.to_string()).collect();
                            row.project(&field_names)
                        } else {
                            row
                        };

                        let obj = pact_core::values::Object::from_pairs(data.into_map().into_iter());
                        Ok(PactValue::Object(obj))
                    }
                    Ok(None) => {
                        Err(PactError::PEExecutionError(
                            EvalError::RowNotFound { 
                                table: table_name.to_string(), 
                                key: key.to_string() 
                            },
                            vec![], 
                            SpanInfo::empty()
                        ))
                    }
                    Err(e) => {
                        Err(PactError::PEExecutionError(EvalError::RuntimeError(e.to_string()),
                         vec![], 
                            SpanInfo::empty()
                        ))
                    }
                }
            }

            DatabaseOperation::Write { table_name, key, row_data } => {
                let table = self.qualified_table_name(table_name);
                let row_key = RowKey::from(key.as_str());
                let row = self.to_row_data(row_data.clone());

                match self.db.write_row(WriteType::Write, &table, &row_key, &row) {
                    Ok(_) => Ok(PactValue::string("Write succeeded")),
                    Err(e) => {
                        Err(PactError::PEExecutionError(EvalError::RuntimeError(e.to_string()),
                         vec![], 
                            SpanInfo::empty()
                        ))
                    }
                }
            }

            DatabaseOperation::Insert { table_name, key, row_data } => {
                let table = self.qualified_table_name(table_name);
                let row_key = RowKey::from(key.as_str());
                let row = self.to_row_data(row_data.clone());

                match self.db.write_row(WriteType::Insert, &table, &row_key, &row) {
                    Ok(_) => Ok(PactValue::string("Insert succeeded")),
                    Err(e) => {
                        Err(PactError::PEExecutionError(EvalError::RuntimeError(e.to_string()),
                         vec![], 
                            SpanInfo::empty()
                        ))
                    }
                }
            }

            DatabaseOperation::Update { table_name, key, row_data } => {
                let table = self.qualified_table_name(table_name);
                let row_key = RowKey::from(key.as_str());
                let row = self.to_row_data(row_data.clone());

                match self.db.write_row(WriteType::Update, &table, &row_key, &row) {
                    Ok(_) => Ok(PactValue::string("Update succeeded")),
                    Err(e) => {
                        Err(PactError::PEExecutionError(EvalError::RuntimeError(e.to_string()),
                         vec![], 
                            SpanInfo::empty()
                        ))
                    }
                }
            }

            DatabaseOperation::Keys { table_name } => {
                let table = self.qualified_table_name(table_name);

                match self.db.table_keys(&table) {
                    Ok(keys) => {
                        let key_values: Vec<PactValue> = keys
                            .into_iter()
                            .map(|key| PactValue::string(key.0))
                            .collect();

                        Ok(PactValue::list(key_values))
                    }
                    Err(e) => {
                        Err(PactError::PEExecutionError(EvalError::RuntimeError(e.to_string()),
                         vec![], 
                            SpanInfo::empty()
                        ))
                    }
                }
            }

            DatabaseOperation::Select { table_name, where_clause: _, fields } => {
                let table = self.qualified_table_name(table_name);

                // For now, select all rows (where clause evaluation requires more integration)
                let criteria = SelectCriteria::new();

                match self.db.select_rows(&table, &criteria) {
                    Ok(rows) => {
                        let objects: Vec<PactValue> = rows
                            .into_iter()
                            .map(|(_, row_data)| {
                                let data = if let Some(field_list) = fields {
                                    let field_names: Vec<String> = field_list.iter().map(|s| s.to_string()).collect();
                                    row_data.project(&field_names)
                                } else {
                                    row_data
                                };
                                let obj = pact_core::values::Object::from_pairs(data.into_map().into_iter());
                                PactValue::Object(obj)
                            })
                            .collect();

                        Ok(PactValue::list(objects))
                    }
                    Err(e) => {
                        Err(PactError::PEExecutionError(EvalError::RuntimeError(e.to_string()),
                         vec![], 
                            SpanInfo::empty()
                        ))
                    }
                }
            }

            DatabaseOperation::CreateTable { table_name, schema } => {
                let table = self.qualified_table_name(table_name);

                // Use provided schema or create empty one
                let schema = schema.clone().unwrap_or_else(|| {
                    pact_schema::Schema {
                        name: pact_core::names::QualifiedName::new(
                            self.current_module.clone(),
                            table_name.to_string(),
                        ),
                        fields: std::collections::HashMap::new(),
                    }
                });

                match self.db.create_user_table(&table, &schema) {
                    Ok(_) => Ok(PactValue::string("Table created")),
                    Err(e) => {
                        Err(PactError::PEExecutionError(EvalError::RuntimeError(e.to_string()),
                         vec![], 
                            SpanInfo::empty()
                        ))
                    }
                }
            }

            DatabaseOperation::DescribeTable { table_name } => {
                let table = self.qualified_table_name(table_name);

                match self.db.get_table_schema(&table) {
                    Ok(Some(schema)) => {
                        // Convert schema to PactValue representation
                        let schema_obj = pact_core::values::Object::from_pairs([
                            ("name".to_string(), PactValue::string(schema.name.render())),
                            ("fields".to_string(), PactValue::list(
                                schema.fields.iter().map(|(field, ty)| {
                                    PactValue::Object(pact_core::values::Object::from_pairs([
                                        ("name".to_string(), PactValue::string(field.0.clone())),
                                        ("type".to_string(), PactValue::string(format!("{:?}", ty))),
                                    ]))
                                }).collect()
                            )),
                        ]);
                        Ok(PactValue::Object(schema_obj))
                    }
                    Ok(None) => {
                        Err(PactError::PEExecutionError(
                            EvalError::TableNotFound(table_name.to_string()),
                            vec![], 
                            SpanInfo::empty()
                        ))
                    }
                    Err(e) => {
                        Err(PactError::PEExecutionError(EvalError::RuntimeError(e.to_string()),
                         vec![], 
                            SpanInfo::empty()
                        ))
                    }
                }
            }
        }
    }

    /// Begin a database transaction
    pub fn begin_transaction(&self) -> Result<(), PactError<SpanInfo>> {
        self.db.begin_tx(self.transaction_mode).map(|_| ()).map_err(|e| {
            PactError::PEExecutionError(
                EvalError::RuntimeError(e.to_string()),
                vec![], 
                SpanInfo::empty()
            )
        })
    }

    /// Commit database transaction
    pub fn commit_transaction(&self) -> Result<Vec<crate::TxLog>, PactError<SpanInfo>> {
        self.db.commit_tx().map_err(|e| {
            PactError::PEExecutionError(
                EvalError::RuntimeError(e.to_string()),
                vec![], 
                SpanInfo::empty()
            )
        })
    }

    /// Rollback database transaction
    pub fn rollback_transaction(&self) -> Result<(), PactError<SpanInfo>> {
        self.db.rollback_tx().map_err(|e| {
            PactError::PEExecutionError(
                EvalError::RuntimeError(e.to_string()),
                vec![], 
                SpanInfo::empty()
            )
        })
    }
}