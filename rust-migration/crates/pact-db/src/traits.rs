//! Core database trait for Pact persistence layer
//!
//! This module defines the PactDb trait that all database backends must implement.
//! All operations run in GasM for proper gas metering, matching the Haskell implementation.

use crate::types::{
    Domain, ExecutionMode, Purity, RowData, RowKey, TableName,
    TxId, TxLog, WriteType,
};
use crate::gas::GasM;

/// Core database abstraction trait matching Haskell's PactDb
/// 
/// All operations are gas-metered through the GasM monad.
/// This is the primary interface used by the CEK evaluator.
pub trait PactDb: Send + Sync {
    /// Get the purity level of this database
    fn purity(&self) -> Purity;
    
    /// Read a value from a domain with gas charging
    fn read(&self, domain: &Domain, key: &RowKey) -> GasM<Option<RowData>>;
    
    /// Write a value to a domain with gas charging
    fn write(&self, write_type: WriteType, domain: &Domain, key: &RowKey, value: &RowData) -> GasM<()>;
    
    /// Get all keys in a domain with gas charging
    fn keys(&self, domain: &Domain) -> GasM<Vec<RowKey>>;
    
    /// Create a user table with gas charging
    fn create_user_table(&self, table: &TableName) -> GasM<()>;
    
    /// Begin a transaction with gas charging
    fn begin_tx(&self, mode: ExecutionMode) -> GasM<Option<TxId>>;
    
    /// Commit transaction with gas charging
    fn commit_tx(&self) -> GasM<Vec<TxLog>>;
    
    /// Rollback transaction with gas charging
    fn rollback_tx(&self) -> GasM<()>;
}