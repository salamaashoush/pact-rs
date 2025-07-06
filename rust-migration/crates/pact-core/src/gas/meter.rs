//! Gas metering for blockchain execution costs
//!
//! This module provides gas metering functionality for tracking
//! execution costs in the Pact runtime.

use crate::errors::{PactError, EvalError};
use crate::shared::SpanInfo;

/// Gas meter for blockchain execution costs
#[derive(Debug, Clone)]
pub struct GasMeter {
    /// Current gas used
    pub gas_used: u64,
    /// Maximum gas allowed
    pub gas_limit: u64,
    /// Gas cost per operation type
    pub gas_costs: GasCosts,
}

/// Gas costs for different operation types
#[derive(Debug, Clone)]
pub struct GasCosts {
    pub function_call: u64,
    pub variable_lookup: u64,
    pub arithmetic_op: u64,
    pub list_construction: u64,
    pub object_construction: u64,
    pub database_read: u64,
    pub database_write: u64,
    pub capability_invocation: u64,
    pub lambda_creation: u64,
}

impl Default for GasCosts {
    fn default() -> Self {
        Self {
            function_call: 10,
            variable_lookup: 1,
            arithmetic_op: 5,
            list_construction: 3,
            object_construction: 5,
            database_read: 20,
            database_write: 30,
            capability_invocation: 15,
            lambda_creation: 8,
        }
    }
}

impl GasMeter {
    /// Create a new gas meter with default gas limit
    pub fn new() -> Self {
        Self::with_limit(1_000_000)
    }

    /// Create a new gas meter with specified gas limit
    pub fn with_limit(gas_limit: u64) -> Self {
        Self {
            gas_used: 0,
            gas_limit,
            gas_costs: GasCosts::default(),
        }
    }

    /// Charge gas for an operation
    pub fn charge(&mut self, cost: u64) -> Result<(), PactError<SpanInfo>> {
        self.gas_used = self.gas_used.saturating_add(cost);
        if self.gas_used > self.gas_limit {
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError(format!(
                    "Gas limit exceeded: used {} > limit {}",
                    self.gas_used,
                    self.gas_limit
                )),
                vec![], // No stack frames for gas errors
                SpanInfo::empty(), // Default span
            ))
        } else {
            Ok(())
        }
    }

    /// Charge gas for a specific operation type
    pub fn charge_operation(&mut self, op: GasOperation) -> Result<(), PactError<SpanInfo>> {
        let cost = match op {
            GasOperation::FunctionCall => self.gas_costs.function_call,
            GasOperation::VariableLookup => self.gas_costs.variable_lookup,
            GasOperation::ArithmeticOp => self.gas_costs.arithmetic_op,
            GasOperation::ListConstruction => self.gas_costs.list_construction,
            GasOperation::ObjectConstruction => self.gas_costs.object_construction,
            GasOperation::DatabaseRead => self.gas_costs.database_read,
            GasOperation::DatabaseWrite => self.gas_costs.database_write,
            GasOperation::CapabilityInvocation => self.gas_costs.capability_invocation,
            GasOperation::LambdaCreation => self.gas_costs.lambda_creation,
        };
        self.charge(cost)
    }

    /// Get remaining gas
    pub fn remaining_gas(&self) -> u64 {
        self.gas_limit.saturating_sub(self.gas_used)
    }

    /// Check if gas limit has been exceeded
    pub fn is_exhausted(&self) -> bool {
        self.gas_used > self.gas_limit
    }

    /// Reset gas meter
    pub fn reset(&mut self) {
        self.gas_used = 0;
    }
    
    // TODO: Re-enable when circular dependency issue is resolved
    /*
    /// Charge gas for expression evaluation
    pub fn charge_expr(&mut self, expr: &pact_syntax::ast::ParsedExpr<crate::shared::SpanInfo>) -> Result<(), PactError<SpanInfo>> {
        use pact_syntax::ast::ParsedExpr;
        
        let cost = match expr {
            ParsedExpr::Var(..) => self.gas_costs.variable_lookup,
            ParsedExpr::Constant(..) => 1, // Minimal cost for constants
            ParsedExpr::App { .. } => self.gas_costs.function_call,
            ParsedExpr::Lam { .. } => self.gas_costs.lambda_creation,
            ParsedExpr::List(..) => self.gas_costs.list_construction,
            ParsedExpr::Object(..) => self.gas_costs.object_construction,
            ParsedExpr::Let { .. } => 2, // Small cost for bindings
            ParsedExpr::If { .. } => 3, // Conditional evaluation cost
            ParsedExpr::WithCapability { .. } => self.gas_costs.capability_invocation,
            ParsedExpr::TypeAnn { .. } => 1, // Minimal cost for type annotations
            ParsedExpr::Suspend { .. } => 5, // Cost for suspend operation
            ParsedExpr::Binding { .. } => 2, // Cost for binding patterns
            ParsedExpr::And { .. } => 2, // Cost for and expressions
            ParsedExpr::Or { .. } => 2, // Cost for or expressions
            ParsedExpr::Cond { .. } => 3, // Cost for cond expressions
            ParsedExpr::Enforce { .. } => 4, // Cost for enforce
            ParsedExpr::EnforceOne { .. } => 5, // Cost for enforce-one
            ParsedExpr::Try { .. } => 6, // Cost for try expressions
            ParsedExpr::CreateUserGuard { .. } => 7, // Cost for creating guards
            ParsedExpr::RequireCapability { .. } => 5, // Cost for require-capability
            ParsedExpr::ComposeCapability { .. } => 6, // Cost for compose-capability
            ParsedExpr::InstallCapability { .. } => 7, // Cost for install-capability
            ParsedExpr::EmitEvent { .. } => 4, // Cost for emit-event
            ParsedExpr::Yield { .. } => 8, // Cost for yield operations
            ParsedExpr::Resume { .. } => 9, // Cost for resume operations
            
            // Definition expressions (used as top-level terms)
            ParsedExpr::DefConst(..) => 10, // Cost for defining constants
            ParsedExpr::DefFun(..) => 15, // Cost for defining functions
            ParsedExpr::DefCap(..) => 12, // Cost for defining capabilities
            ParsedExpr::DefSchema(..) => 8, // Cost for defining schemas
            ParsedExpr::DefTable(..) => 10, // Cost for defining tables
            ParsedExpr::DefPact(..) => 20, // Cost for defining pacts
        };
        
        self.charge(cost)
    }
    */
}

/// Gas operation types
#[derive(Debug, Clone, Copy)]
pub enum GasOperation {
    FunctionCall,
    VariableLookup,
    ArithmeticOp,
    ListConstruction,
    ObjectConstruction,
    DatabaseRead,
    DatabaseWrite,
    CapabilityInvocation,
    LambdaCreation,
}

impl Default for GasMeter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gas_charging() {
        let mut meter = GasMeter::with_limit(100);
        
        // Charge some gas
        assert!(meter.charge(50).is_ok());
        assert_eq!(meter.gas_used, 50);
        assert_eq!(meter.remaining_gas(), 50);
        
        // Charge more gas
        assert!(meter.charge(40).is_ok());
        assert_eq!(meter.gas_used, 90);
        assert_eq!(meter.remaining_gas(), 10);
        
        // Exceed limit
        assert!(meter.charge(20).is_err());
        assert!(meter.is_exhausted());
    }

    #[test]
    fn test_gas_operations() {
        let mut meter = GasMeter::with_limit(100);
        
        // Test different operations
        assert!(meter.charge_operation(GasOperation::VariableLookup).is_ok());
        assert_eq!(meter.gas_used, 1);
        
        assert!(meter.charge_operation(GasOperation::FunctionCall).is_ok());
        assert_eq!(meter.gas_used, 11);
        
        assert!(meter.charge_operation(GasOperation::DatabaseWrite).is_ok());
        assert_eq!(meter.gas_used, 41);
    }

    #[test]
    fn test_gas_reset() {
        let mut meter = GasMeter::with_limit(100);
        meter.charge(50).unwrap();
        assert_eq!(meter.gas_used, 50);
        
        meter.reset();
        assert_eq!(meter.gas_used, 0);
        assert_eq!(meter.remaining_gas(), 100);
    }
}