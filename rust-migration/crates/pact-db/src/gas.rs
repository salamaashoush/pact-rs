//! Gas metering integration for database operations
//!
//! This module provides the GasM monad for tracking gas usage during
//! database operations, matching the Haskell implementation exactly.

use pact_core::errors::{PactError, EvalError};
use pact_core::gas::{GasArgs, MilliGas, MilliGasLimit};
use pact_core::shared::SpanInfo;

/// GasM monad for gas-aware database operations
/// 
/// This type wraps database operations with gas metering, ensuring
/// that all database access properly charges gas for consensus.
/// 
/// This is designed to integrate with the CEK evaluator's EvalM monad.
pub struct GasM<T> {
    // Function that performs the computation with gas tracking
    inner: Box<dyn FnOnce(GasContext) -> Result<(T, GasUsed), PactError<SpanInfo>> + Send>,
}

/// Context for gas operations
#[derive(Clone)]
pub struct GasContext {
    pub info: SpanInfo,
    pub limit: Option<MilliGasLimit>,
}

/// Gas used during an operation
#[derive(Clone, Copy, Debug)]
pub struct GasUsed(pub MilliGas);

impl GasUsed {
    pub fn zero() -> Self {
        GasUsed(MilliGas(0))
    }
    
    pub fn add(&self, other: GasUsed) -> Self {
        GasUsed(MilliGas(self.0.0 + other.0.0))
    }
}

impl<T> GasM<T> {
    /// Create a new GasM computation
    pub fn new<F>(f: F) -> Self 
    where 
        F: FnOnce(GasContext) -> Result<(T, GasUsed), PactError<SpanInfo>> + Send + 'static,
    {
        GasM { inner: Box::new(f) }
    }
    
    /// Create a pure GasM that returns a value without gas charges
    pub fn pure(value: T) -> Self 
    where 
        T: Send + 'static,
    {
        GasM::new(move |_ctx| Ok((value, GasUsed::zero())))
    }
    
    /// Create a GasM that returns an error
    pub fn error(error: PactError<SpanInfo>) -> Self 
    where 
        T: Send + 'static,
    {
        GasM::new(move |_ctx| Err(error))
    }
    
    /// Run the GasM computation with a gas context
    pub fn run(self, ctx: GasContext) -> Result<(T, GasUsed), PactError<SpanInfo>> {
        (self.inner)(ctx)
    }
    
    /// Map over the result of a GasM computation
    pub fn map<U, F>(self, f: F) -> GasM<U>
    where
        F: FnOnce(T) -> U + Send + 'static,
        U: Send + 'static,
        T: Send + 'static,
    {
        GasM::new(move |ctx| {
            let (result, gas) = self.run(ctx)?;
            Ok((f(result), gas))
        })
    }
    
    /// FlatMap/Bind operation for GasM
    pub fn bind<U, F>(self, f: F) -> GasM<U>
    where
        F: FnOnce(T) -> GasM<U> + Send + 'static,
        T: Send + 'static,
        U: Send + 'static,
    {
        GasM::new(move |ctx| {
            let (result, gas1) = self.run(ctx.clone())?;
            let next = f(result);
            let (final_result, gas2) = next.run(ctx)?;
            Ok((final_result, gas1.add(gas2)))
        })
    }
    
    /// Sequential composition (like Haskell's >>)
    pub fn then<U, F>(self, next: F) -> GasM<U>
    where
        F: FnOnce() -> GasM<U> + Send + 'static,
        T: Send + 'static,
        U: Send + 'static,
    {
        self.bind(move |_| next())
    }
}

/// Helper function to charge gas within a GasM computation
pub fn charge_gas_m(args: GasArgs) -> GasM<()> {
    GasM::new(move |ctx| {
        let gas_cost = calculate_gas_cost(&args);
        
        // Check against limit if present
        if let Some(limit) = ctx.limit {
            let limit_milligas = MilliGas(limit.0);
            if gas_cost.0 > limit_milligas.0 {
                return Err(PactError::PEExecutionError(
                    EvalError::GasExceeded(format!(
                        "Gas limit exceeded: used {}, limit {}",
                        gas_cost.0, limit_milligas.0
                    )),
                    vec![],
                    ctx.info,
                ));
            }
        }
        
        Ok(((), GasUsed(gas_cost)))
    })
}

/// Calculate gas cost from gas arguments
fn calculate_gas_cost(args: &GasArgs) -> MilliGas {
    match args {
        GasArgs::Constant(gas) => *gas,
        GasArgs::Read(bytes) => {
            // Base cost + per-byte cost
            MilliGas(gas_costs::READ_PENALTY.0 + (bytes * gas_costs::PER_BYTE_READ_COST.0 as u64))
        }
        GasArgs::Write(bytes) => {
            // Base cost + per-byte cost
            MilliGas(gas_costs::WRITE_PENALTY.0 + (bytes * gas_costs::PER_BYTE_WRITE_COST.0 as u64))
        }
        _ => MilliGas(1), // Default minimal cost
    }
}

/// Helper function to lift a non-gas computation into GasM
pub fn lift_gas_m<T, F>(f: F) -> GasM<T>
where
    F: FnOnce() -> Result<T, PactError<SpanInfo>> + Send + 'static,
    T: Send + 'static,
{
    GasM::new(move |_ctx| {
        let result = f()?;
        Ok((result, GasUsed::zero()))
    })
}

/// Gas costs for database operations (matching Haskell)
pub mod gas_costs {
    use pact_core::gas::{MilliGas, GasArgs};
    
    /// Cost for reading a single row (2,500 milligas base + 100 per byte)
    pub const READ_PENALTY: MilliGas = MilliGas(2500);
    pub const PER_BYTE_READ_COST: MilliGas = MilliGas(100);
    
    /// Cost for writing a single row (25,000 milligas base + 200 per byte)
    pub const WRITE_PENALTY: MilliGas = MilliGas(25000);
    pub const PER_BYTE_WRITE_COST: MilliGas = MilliGas(200);
    
    /// Cost for listing keys
    pub const KEYS_COST: MilliGas = MilliGas(40_000_000);
    
    /// Cost for creating a table
    pub const CREATE_TABLE_COST: MilliGas = MilliGas(500_000);
    
    /// Cost for beginning a transaction
    pub const BEGIN_TX_COST: MilliGas = MilliGas(100);
    
    /// Cost for committing a transaction
    pub const COMMIT_TX_COST: MilliGas = MilliGas(200);
    
    /// Cost for rolling back a transaction
    pub const ROLLBACK_TX_COST: MilliGas = MilliGas(100);
    
    /// Create gas args for a database read
    pub fn read_gas(bytes: u64) -> GasArgs {
        GasArgs::Read(bytes)
    }
    
    /// Create gas args for a database write
    pub fn write_gas(bytes: u64) -> GasArgs {
        GasArgs::Write(bytes)
    }
    
    /// Create gas args for listing keys
    pub fn keys_gas() -> GasArgs {
        GasArgs::Constant(KEYS_COST)
    }
    
    /// Create gas args for creating a table
    pub fn create_table_gas() -> GasArgs {
        GasArgs::Constant(CREATE_TABLE_COST)
    }
    
    /// Calculate gas cost from gas arguments
    pub fn calculate_gas_cost(args: &GasArgs) -> MilliGas {
        match args {
            GasArgs::Constant(gas) => *gas,
            GasArgs::Read(bytes) => {
                // Base cost + per-byte cost
                MilliGas(READ_PENALTY.0 + (bytes * PER_BYTE_READ_COST.0 / 100))
            }
            GasArgs::Write(bytes) => {
                // Base cost + per-byte cost
                MilliGas(WRITE_PENALTY.0 + (bytes * PER_BYTE_WRITE_COST.0 / 100))
            }
            _ => MilliGas(1), // Default minimal cost
        }
    }
}

/// Applicative-style operations for GasM
impl<T> GasM<T> {
    /// Apply a function in GasM to a value in GasM
    pub fn ap<U>(self, func: GasM<impl FnOnce(T) -> U + Send + 'static>) -> GasM<U>
    where
        T: Send + 'static,
        U: Send + 'static,
    {
        func.bind(move |f| self.map(f))
    }
}

/// Sequence a list of GasM computations
pub fn sequence<T>(ms: Vec<GasM<T>>) -> GasM<Vec<T>>
where
    T: Send + 'static,
{
    ms.into_iter().fold(
        GasM::pure(Vec::new()),
        |acc, m| acc.bind(move |mut xs| {
            m.map(move |x| {
                xs.push(x);
                xs
            })
        })
    )
}