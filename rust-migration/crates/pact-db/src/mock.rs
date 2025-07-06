//! Mock in-memory database implementation for testing
//!
//! This module provides a simple in-memory database backend that implements
//! the gas-metered PactDb trait.

use crate::gas::{GasM, charge_gas_m, gas_costs};
use crate::traits::PactDb;
use crate::types::{
    Domain, ExecutionMode, Purity, RowData, RowKey,
    TableName, TxId, TxLog, WriteType,
};
use pact_core::errors::{PactError, EvalError};
use pact_core::gas::{GasArgs, MilliGas};
use pact_core::shared::SpanInfo;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

/// Mock database implementation with gas metering
#[derive(Debug, Clone)]
pub struct MockDb {
    /// Data storage
    data: Arc<Mutex<HashMap<String, HashMap<String, RowData>>>>,
    /// Current transaction ID
    tx_id: Arc<Mutex<Option<TxId>>>,
    /// Transaction counter
    tx_counter: Arc<Mutex<u64>>,
    /// Purity level
    purity: Purity,
}

impl MockDb {
    /// Create a new mock database
    pub fn new() -> Self {
        MockDb {
            data: Arc::new(Mutex::new(HashMap::new())),
            tx_id: Arc::new(Mutex::new(None)),
            tx_counter: Arc::new(Mutex::new(0)),
            purity: Purity::PImpure,
        }
    }
    
    /// Create a new read-only mock database
    pub fn new_read_only() -> Self {
        MockDb {
            data: Arc::new(Mutex::new(HashMap::new())),
            tx_id: Arc::new(Mutex::new(None)),
            tx_counter: Arc::new(Mutex::new(0)),
            purity: Purity::PReadOnly,
        }
    }
    
    fn domain_key(&self, domain: &Domain) -> String {
        match domain {
            Domain::UserTables(table) => format!("user:{}", table.render()),
            Domain::KeySets => "sys:keysets".to_string(),
            Domain::Modules => "sys:modules".to_string(),
            Domain::Namespaces => "sys:namespaces".to_string(),
            Domain::DefPacts => "sys:defpacts".to_string(),
            Domain::ModuleSource => "sys:module_source".to_string(),
        }
    }
}

impl PactDb for MockDb {
    fn purity(&self) -> Purity {
        self.purity
    }
    
    fn read(&self, domain: &Domain, key: &RowKey) -> GasM<Option<RowData>> {
        let data = self.data.clone();
        let domain_key = self.domain_key(domain);
        let key = key.clone();
        
        GasM::new(move |ctx| {
            let data_guard = data.lock().unwrap();
            let result = data_guard
                .get(&domain_key)
                .and_then(|domain_data| domain_data.get(&key.0))
                .cloned();
            
            // Charge gas based on data size
            let gas_cost = match &result {
                Some(row) => {
                    let size = estimate_row_size(row);
                    gas_costs::calculate_gas_cost(&GasArgs::Read(size))
                }
                None => MilliGas(100), // Minimal cost for miss
            };
            
            Ok((result, crate::gas::GasUsed(gas_cost)))
        })
    }
    
    fn write(&self, write_type: WriteType, domain: &Domain, key: &RowKey, value: &RowData) -> GasM<()> {
        let data = self.data.clone();
        let domain_key = self.domain_key(domain);
        let key = key.clone();
        let value = value.clone();
        
        // Calculate gas cost based on data size
        let size = estimate_row_size(&value);
        let gas_args = GasArgs::Write(size);
        
        charge_gas_m(gas_args)
            .bind(move |_| {
                GasM::new(move |ctx| {
                    let mut data_guard = data.lock().unwrap();
                    let domain_data = data_guard.entry(domain_key).or_insert_with(HashMap::new);
                    
                    match write_type {
                        WriteType::Insert => {
                            if domain_data.contains_key(&key.0) {
                                return Err(PactError::PEExecutionError(
                                    EvalError::RuntimeError(format!("Key already exists: {}", key.0)),
                                    vec![],
                                    ctx.info,
                                ));
                            }
                            domain_data.insert(key.0, value);
                        }
                        WriteType::Update => {
                            if !domain_data.contains_key(&key.0) {
                                return Err(PactError::PEExecutionError(
                                    EvalError::RuntimeError(format!("Key not found: {}", key.0)),
                                    vec![],
                                    ctx.info,
                                ));
                            }
                            domain_data.insert(key.0, value);
                        }
                        WriteType::Write => {
                            domain_data.insert(key.0, value);
                        }
                    }
                    
                    Ok(((), crate::gas::GasUsed::zero()))
                })
            })
    }
    
    fn keys(&self, domain: &Domain) -> GasM<Vec<RowKey>> {
        let data = self.data.clone();
        let domain_key = self.domain_key(domain);
        
        charge_gas_m(GasArgs::Constant(gas_costs::KEYS_COST))
            .bind(move |_| {
                GasM::new(move |_ctx| {
                    let data_guard = data.lock().unwrap();
                    let keys = data_guard
                        .get(&domain_key)
                        .map(|domain_data| {
                            domain_data.keys()
                                .map(|k| RowKey(k.clone()))
                                .collect()
                        })
                        .unwrap_or_else(Vec::new);
                    
                    // Additional gas per key
                    let per_key_gas = MilliGas(keys.len() as u64 * 10);
                    Ok((keys, crate::gas::GasUsed(per_key_gas)))
                })
            })
    }
    
    fn create_user_table(&self, table: &TableName) -> GasM<()> {
        charge_gas_m(GasArgs::Constant(gas_costs::CREATE_TABLE_COST))
            .map(|_| ())
    }
    
    fn begin_tx(&self, mode: ExecutionMode) -> GasM<Option<TxId>> {
        let tx_id_ref = self.tx_id.clone();
        let tx_counter = self.tx_counter.clone();
        
        charge_gas_m(GasArgs::Constant(gas_costs::BEGIN_TX_COST))
            .bind(move |_| {
                GasM::new(move |_ctx| {
                    let mut counter = tx_counter.lock().unwrap();
                    *counter += 1;
                    let tx_id = TxId(*counter);
                    
                    let mut current_tx = tx_id_ref.lock().unwrap();
                    *current_tx = Some(tx_id.clone());
                    
                    Ok((Some(tx_id), crate::gas::GasUsed::zero()))
                })
            })
    }
    
    fn commit_tx(&self) -> GasM<Vec<TxLog>> {
        let tx_id_ref = self.tx_id.clone();
        
        charge_gas_m(GasArgs::Constant(gas_costs::COMMIT_TX_COST))
            .bind(move |_| {
                GasM::new(move |_ctx| {
                    let mut current_tx = tx_id_ref.lock().unwrap();
                    *current_tx = None;
                    
                    // Return empty logs for now
                    Ok((vec![], crate::gas::GasUsed::zero()))
                })
            })
    }
    
    fn rollback_tx(&self) -> GasM<()> {
        let tx_id_ref = self.tx_id.clone();
        
        charge_gas_m(GasArgs::Constant(gas_costs::ROLLBACK_TX_COST))
            .bind(move |_| {
                GasM::new(move |_ctx| {
                    let mut current_tx = tx_id_ref.lock().unwrap();
                    *current_tx = None;
                    
                    Ok(((), crate::gas::GasUsed::zero()))
                })
            })
    }
}

impl Default for MockDb {
    fn default() -> Self {
        Self::new()
    }
}

/// Estimate the size of a row for gas calculation
fn estimate_row_size(row: &RowData) -> u64 {
    let mut size = 0u64;
    for (key, value) in row.fields() {
        size += key.len() as u64;
        size += estimate_value_size(value);
    }
    size
}

/// Estimate the size of a PactValue for gas calculation
fn estimate_value_size(value: &pact_core::values::PactValue) -> u64 {
    use pact_core::values::PactValue;
    match value {
        PactValue::String(s) => s.len() as u64,
        PactValue::Integer(_) => 8,
        PactValue::Decimal(_) => 16,
        PactValue::Bool(_) => 1,
        PactValue::Time(_) => 8,
        PactValue::List(items) => {
            let mut size = 8;
            for item in items {
                size += estimate_value_size(item);
            }
            size
        }
        PactValue::Object(obj) => {
            let mut size = 8;
            for (k, v) in obj.entries() {
                size += k.len() as u64;
                size += estimate_value_size(v);
            }
            size
        }
        _ => 8,
    }
}