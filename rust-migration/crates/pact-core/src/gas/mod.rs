//! Gas metering for Pact operations - complete implementation matching Haskell Pact
//!
//! This module provides comprehensive gas tracking and limiting functionality
//! with exact compatibility to Haskell Pact's gas model.

pub mod meter;

use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::sync::Arc;
use std::time::Instant;

/// Milli-gas unit - base unit of gas measurement (1000 milligas = 1 gas, ~2ns per milligas)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct MilliGas(pub u64);

impl MilliGas {
    pub const ZERO: MilliGas = MilliGas(0);

    pub fn new(value: u64) -> Self {
        MilliGas(value)
    }

    pub fn from_gas(gas: Gas) -> Self {
        MilliGas(gas.0 * 1000)
    }

    pub fn value(&self) -> u64 {
        self.0
    }

    /// Add milligas amounts, checking for overflow
    pub fn checked_add(self, other: MilliGas) -> Option<MilliGas> {
        self.0.checked_add(other.0).map(MilliGas)
    }

    /// Convert to user-facing gas units
    pub fn to_gas(self) -> Gas {
        Gas(self.0 / 1000)
    }
}

impl std::ops::Add for MilliGas {
    type Output = MilliGas;

    fn add(self, other: MilliGas) -> MilliGas {
        MilliGas(self.0 + other.0)
    }
}

/// Gas unit for user-facing operations
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Gas(pub u64);

impl Gas {
    pub const ZERO: Gas = Gas(0);

    pub fn new(value: u64) -> Self {
        Gas(value)
    }

    pub fn value(&self) -> u64 {
        self.0
    }

    /// Add gas amounts, checking for overflow
    pub fn checked_add(self, other: Gas) -> Option<Gas> {
        self.0.checked_add(other.0).map(Gas)
    }

    /// Convert to precise milligas
    pub fn to_milligas(self) -> MilliGas {
        MilliGas(self.0 * 1000)
    }
}

impl Default for Gas {
    fn default() -> Self {
        Gas::ZERO
    }
}

/// Gas limit for execution
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct GasLimit(pub Gas);

/// Milli-gas limit for precise tracking
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct MilliGasLimit(pub MilliGas);

impl GasLimit {
    pub fn new(gas: Gas) -> Self {
        GasLimit(gas)
    }

    pub fn unlimited() -> Self {
        GasLimit(Gas(u64::MAX))
    }

    pub fn to_milligas_limit(self) -> MilliGasLimit {
        MilliGasLimit(self.0.to_milligas())
    }
}

/// Gas price for calculating transaction costs
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct GasPrice(pub u64);

/// Integer primitive operations for gas costing
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum IntegerPrimOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Abs,
    Negate,
    Signum,
}

/// Transcendental operation costs
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TranscendentalCost {
    Exp,
    Ln,
    Sqrt,
    Log,
    Round,
    Ceiling,
    Floor,
}

/// String operation types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum StrOp {
    Length,
    Reverse,
    Concat,
    Take,
    Drop,
    Format,
}

/// Object operation types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ObjOp {
    At,
    Length,
    Keys,
    Values,
    Merge,
    Drop,
}

/// Capability operation types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum CapOp {
    WithCapability,
    InstallCapability,
    RequireCapability,
    ComposeCapability,
}

/// Hash operation types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum HashOp {
    Keccak256,
    Sha256,
    Sha512,
    Blake2b256,
    Blake2b512,
}

/// Module operation types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ModuleOp {
    LoadModule,
    InstallModule,
    DescribeModule,
}

/// Comparison operation types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ComparisonType {
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
}

/// Concatenation types for gas costing
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ConcatType {
    ListConcat(u64),
    ObjConcat(u64),
    StrConcat(u64),
}

/// Search operation types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum SearchType {
    Keys,
    TxIds,
    TxLogs,
    Select,
    Fold,
    Filter,
    Map,
}

/// ZK cryptographic argument types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ZKArg {
    Pairing,
    PointAdd,
    ScalarMult,
    PointCompress,
}

/// Gas arguments for different operation types - matches Haskell exactly
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum GasArgs {
    /// Constant gas cost
    Constant(MilliGas),
    /// Native function base cost
    Native(String),
    /// Integer operation with operand-dependent costing
    IntegerOpCost(IntegerPrimOp, i64, i64),
    /// Function application cost
    ApplyLam(Option<String>, usize),
    /// Concatenation operations
    Concat(ConcatType),
    /// List creation
    MakeList(u64, u64),
    /// ZK cryptographic operations
    ZKArgs(ZKArg),
    /// Database write (per byte)
    Write(u64),
    /// Database read (per byte)
    Read(u64),
    /// Comparison operations
    Comparison(ComparisonType),
    /// Search operations
    Search(SearchType),
    /// Module operations
    ModuleOp(ModuleOp),
    /// Transcendental functions
    Transcendental(TranscendentalCost),
    /// String operations
    StrOp(StrOp),
    /// Object operations
    ObjOp(ObjOp),
    /// Capability operations
    CapOp(CapOp),
    /// Hash operations
    HashOp(HashOp),
}

/// Gas cost configuration - matches Haskell GasCostConfig exactly
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct GasCostConfig {
    /// Native function basic work cost (100 mg)
    pub native_basic_work: u64,
    /// Function argument cost (25 mg per arg)
    pub function_argument_cost: u64,
    /// Machine tick cost (25 mg per state transition)
    pub machine_tick_cost: u64,
    /// Uncons work cost (100 mg)
    pub uncons_work: u64,
    /// Database read penalty (2,500 mg)
    pub read_penalty: u64,
    /// Database write penalty (25,000 mg)
    pub write_penalty: u64,
    /// Per-byte write cost (200 mg per byte)
    pub per_byte_write_cost: u64,
    /// Per-byte read cost (100 mg per byte)
    pub per_byte_read_cost: u64,
    /// Integer operation base cost (100 mg)
    pub integer_op_cost: u64,
    /// Log base cost (500 mg)
    pub log_base_cost: u64,
    /// Hash base cost (500 mg)
    pub hash_base_cost: u64,
    /// Signature verification base cost (1000 mg)
    pub sig_verify_cost: u64,
    /// Point addition cost (10000 mg)
    pub point_add_cost: u64,
    /// Scalar multiplication cost (100000 mg)
    pub scalar_mult_cost: u64,
    /// Pairing cost (1000000 mg)
    pub pairing_cost: u64,
    /// Per-byte string operation cost (10 mg)
    pub string_op_cost: u64,
    /// Object operation base cost (100 mg)
    pub obj_op_cost: u64,
    /// Capability operation cost (200 mg)
    pub cap_op_cost: u64,
}

impl Default for GasCostConfig {
    fn default() -> Self {
        GasCostConfig {
            native_basic_work: 100,
            function_argument_cost: 25,
            machine_tick_cost: 25,
            uncons_work: 100,
            read_penalty: 2500,
            write_penalty: 25000,
            per_byte_write_cost: 200,
            per_byte_read_cost: 100,
            integer_op_cost: 100,
            log_base_cost: 500,
            hash_base_cost: 500,
            sig_verify_cost: 1000,
            point_add_cost: 10000,
            scalar_mult_cost: 100000,
            pairing_cost: 1000000,
            string_op_cost: 10,
            obj_op_cost: 100,
            cap_op_cost: 200,
        }
    }
}

/// Gas model with configuration and limits
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct GasModel {
    pub config: GasCostConfig,
    pub limit: Option<MilliGasLimit>,
}

impl Default for GasModel {
    fn default() -> Self {
        GasModel {
            config: GasCostConfig::default(),
            limit: None,
        }
    }
}

/// Trait for gas cost calculation
pub trait GasCalculator {
    /// Calculate gas cost for an operation
    fn compute_gas(&self, args: &GasArgs) -> Gas;
}

impl GasCalculator for GasModel {
    fn compute_gas(&self, args: &GasArgs) -> Gas {
        self.calculate_cost(args).to_gas()
    }
}

impl GasModel {
    /// Calculate gas cost for an operation
    pub fn calculate_cost(&self, args: &GasArgs) -> MilliGas {
        match args {
            GasArgs::Constant(cost) => *cost,
            GasArgs::Native(_) => MilliGas(self.config.native_basic_work),
            GasArgs::IntegerOpCost(op, a, b) => self.integer_op_cost(op, *a, *b),
            GasArgs::ApplyLam(_, arg_count) => {
                MilliGas(self.config.function_argument_cost * (*arg_count as u64))
            }
            GasArgs::Concat(concat_type) => self.concat_cost(concat_type),
            GasArgs::MakeList(len, _) => MilliGas(self.config.uncons_work * len),
            GasArgs::ZKArgs(zk_arg) => self.zk_cost(zk_arg),
            GasArgs::Write(bytes) => {
                MilliGas(self.config.write_penalty + self.config.per_byte_write_cost * bytes)
            }
            GasArgs::Read(bytes) => {
                MilliGas(self.config.read_penalty + self.config.per_byte_read_cost * bytes)
            }
            GasArgs::Comparison(_) => MilliGas(self.config.native_basic_work),
            GasArgs::Search(_) => MilliGas(self.config.read_penalty),
            GasArgs::ModuleOp(_) => MilliGas(self.config.write_penalty),
            GasArgs::Transcendental(tc) => self.transcendental_cost(tc),
            GasArgs::StrOp(_) => MilliGas(self.config.string_op_cost),
            GasArgs::ObjOp(_) => MilliGas(self.config.obj_op_cost),
            GasArgs::CapOp(_) => MilliGas(self.config.cap_op_cost),
            GasArgs::HashOp(_) => MilliGas(self.config.hash_base_cost),
        }
    }

    /// Calculate integer operation cost based on operand sizes
    fn integer_op_cost(&self, op: &IntegerPrimOp, a: i64, b: i64) -> MilliGas {
        let base_cost = self.config.integer_op_cost;

        // Calculate bit lengths
        let bits_a = if a == 0 {
            1
        } else {
            (a.abs() as u64).ilog2() + 1
        } as u64;
        let bits_b = if b == 0 {
            1
        } else {
            (b.abs() as u64).ilog2() + 1
        } as u64;

        let complexity_cost = match op {
            IntegerPrimOp::Add | IntegerPrimOp::Sub => {
                // O(n) where n = max bits
                bits_a.max(bits_b)
            }
            IntegerPrimOp::Mul => {
                // O(n^1.465) Karatsuba algorithm approximation
                let max_bits = bits_a.max(bits_b) as f64;
                (max_bits.powf(1.465)) as u64
            }
            IntegerPrimOp::Div | IntegerPrimOp::Mod => {
                // O(M(n)log n) complexity approximation
                let max_bits = bits_a.max(bits_b) as f64;
                (max_bits * max_bits.log2()) as u64
            }
            IntegerPrimOp::Pow => {
                // Exponential cost based on exponent
                if b < 0 {
                    base_cost * 10
                } else {
                    base_cost * (1 + b as u64)
                }
            }
            IntegerPrimOp::Abs | IntegerPrimOp::Negate | IntegerPrimOp::Signum => {
                // O(1) operations
                base_cost
            }
        };

        MilliGas(base_cost + complexity_cost)
    }

    /// Calculate concatenation cost
    fn concat_cost(&self, concat_type: &ConcatType) -> MilliGas {
        match concat_type {
            ConcatType::ListConcat(len) => MilliGas(self.config.uncons_work * len),
            ConcatType::ObjConcat(size) => MilliGas(self.config.obj_op_cost * size),
            ConcatType::StrConcat(len) => MilliGas(self.config.string_op_cost * len),
        }
    }

    /// Calculate ZK operation cost
    fn zk_cost(&self, zk_arg: &ZKArg) -> MilliGas {
        match zk_arg {
            ZKArg::Pairing => MilliGas(self.config.pairing_cost),
            ZKArg::PointAdd => MilliGas(self.config.point_add_cost),
            ZKArg::ScalarMult => MilliGas(self.config.scalar_mult_cost),
            ZKArg::PointCompress => MilliGas(self.config.point_add_cost / 2),
        }
    }

    /// Calculate transcendental function cost
    fn transcendental_cost(&self, tc: &TranscendentalCost) -> MilliGas {
        match tc {
            TranscendentalCost::Exp => MilliGas(self.config.log_base_cost * 5),
            TranscendentalCost::Ln | TranscendentalCost::Log => MilliGas(self.config.log_base_cost),
            TranscendentalCost::Sqrt => MilliGas(self.config.log_base_cost * 2),
            TranscendentalCost::Round | TranscendentalCost::Ceiling | TranscendentalCost::Floor => {
                MilliGas(self.config.native_basic_work)
            }
        }
    }
}

/// Gas log entry for debugging and analysis
#[derive(Debug, Clone, PartialEq)]
pub struct GasLogEntry {
    pub operation: String,
    pub args: GasArgs,
    pub cost: MilliGas,
    pub total: MilliGas,
    pub timestamp: Instant,
}

/// Gas environment for tracking consumption - matches Haskell GasEnv
#[derive(Debug)]
pub struct GasEnv {
    /// Current gas consumed
    pub consumed: RefCell<MilliGas>,
    /// Optional gas logging
    pub log: RefCell<Option<Vec<GasLogEntry>>>,
    /// Gas model configuration
    pub model: GasModel,
}

impl GasEnv {
    /// Create a new gas environment
    pub fn new(model: GasModel) -> Self {
        GasEnv {
            consumed: RefCell::new(MilliGas::ZERO),
            log: RefCell::new(None),
            model,
        }
    }

    /// Create unlimited gas environment
    pub fn unlimited() -> Self {
        Self::new(GasModel::default())
    }

    /// Enable gas logging
    pub fn enable_logging(&self) {
        *self.log.borrow_mut() = Some(Vec::new());
    }

    /// Charge gas for an operation - exact match to Haskell chargeGasArgsM
    pub fn charge_gas_args(&self, operation: &str, args: GasArgs) -> Result<(), GasError> {
        let cost = self.model.calculate_cost(&args);
        let mut consumed = self.consumed.borrow_mut();
        let new_total = consumed.checked_add(cost).ok_or(GasError::Overflow)?;

        // Check limit
        if let Some(limit) = self.model.limit {
            if new_total.0 > limit.0 .0 {
                return Err(GasError::Exceeded {
                    limit: limit.0.to_gas(),
                    used: new_total.to_gas(),
                });
            }
        }

        // Update consumption
        *consumed = new_total;

        // Log the operation if enabled
        if let Some(ref mut log) = *self.log.borrow_mut() {
            log.push(GasLogEntry {
                operation: operation.to_string(),
                args,
                cost,
                total: new_total,
                timestamp: Instant::now(),
            });
        }

        Ok(())
    }

    /// Charge flat native gas - convenience method
    pub fn charge_flat_native_gas(&self, builtin_name: &str) -> Result<(), GasError> {
        self.charge_gas_args(builtin_name, GasArgs::Native(builtin_name.to_string()))
    }

    /// Get current gas consumption
    pub fn consumed(&self) -> MilliGas {
        *self.consumed.borrow()
    }

    /// Get gas log if enabled
    pub fn get_log(&self) -> Option<Vec<GasLogEntry>> {
        self.log.borrow().clone()
    }

    /// Reset gas consumption
    pub fn reset(&self) {
        *self.consumed.borrow_mut() = MilliGas::ZERO;
        if let Some(ref mut log) = *self.log.borrow_mut() {
            log.clear();
        }
    }
}

/// Gas-related errors
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum GasError {
    #[error("Gas limit exceeded: used {used:?}, limit {limit:?}")]
    Exceeded { limit: Gas, used: Gas },

    #[error("Gas calculation overflow")]
    Overflow,

    #[error("Invalid gas operation: {message}")]
    InvalidOperation { message: String },
}

thread_local! {
    static GAS_ENV: RefCell<Option<Arc<GasEnv>>> = RefCell::new(None);
}

/// Set the thread-local gas environment
pub fn set_gas_env(env: Arc<GasEnv>) {
    GAS_ENV.with(|g| {
        *g.borrow_mut() = Some(env);
    });
}

/// Clear the thread-local gas environment
pub fn clear_gas_env() {
    GAS_ENV.with(|g| {
        *g.borrow_mut() = None;
    });
}

/// Execute with gas environment
pub fn with_gas_env<F, R>(f: F) -> Option<R>
where
    F: FnOnce(&GasEnv) -> R,
{
    GAS_ENV.with(|g| g.borrow().as_ref().map(|env| f(env)))
}

/// Charge gas for an operation using thread-local environment - main API
pub fn charge_gas_args(operation: &str, args: GasArgs) -> Result<(), GasError> {
    with_gas_env(|env| env.charge_gas_args(operation, args)).unwrap_or(Ok(())) // No gas env means unlimited gas
}

/// Charge flat native gas - convenience function
pub fn charge_flat_native_gas(builtin_name: &str) -> Result<(), GasError> {
    charge_gas_args(builtin_name, GasArgs::Native(builtin_name.to_string()))
}

/// Get current gas consumption
pub fn get_gas_consumed() -> MilliGas {
    with_gas_env(|env| env.consumed()).unwrap_or(MilliGas::ZERO)
}

/// Macro for easy gas charging with error propagation
#[macro_export]
macro_rules! charge_gas {
    ($op:expr, $args:expr) => {
        if let Err(e) = $crate::charge_gas_args($op, $args) {
            return Err(pact_errors::PactError::PEExecutionError(
                pact_errors::EvalError::RuntimeError(e.to_string()),
                vec![], // No stack frames for gas errors
                crate::shared::SpanInfo::empty(), // Default span
            ));
        }
    };
}

/// Macro for charging flat native gas
#[macro_export]
macro_rules! charge_native_gas {
    ($builtin:expr) => {
        if let Err(e) = $crate::charge_flat_native_gas($builtin) {
            return Err(pact_errors::PactError::PEExecutionError(
                pact_errors::EvalError::RuntimeError(e.to_string()),
                vec![], // No stack frames for gas errors
                crate::shared::SpanInfo::empty(), // Default span
            ));
        }
    };
}

// Convenience functions for CEK evaluator

/// Charge gas for CEK machine state transitions
pub fn charge_machine_tick() -> Result<(), GasError> {
    charge_gas_args("machine-tick", GasArgs::Constant(MilliGas(25)))
}

/// Charge gas for function application
pub fn charge_apply_lam(function_name: Option<String>, arg_count: usize) -> Result<(), GasError> {
    charge_gas_args("apply-lam", GasArgs::ApplyLam(function_name, arg_count))
}

/// Charge gas for list unconsing operations
pub fn charge_uncons_work() -> Result<(), GasError> {
    charge_gas_args("uncons-work", GasArgs::Constant(MilliGas(100)))
}

/// Charge gas for variable lookup operations
pub fn charge_var_lookup() -> Result<(), GasError> {
    charge_gas_args("var-lookup", GasArgs::Constant(MilliGas(25)))
}

/// Charge gas for lambda evaluation
pub fn charge_lambda_eval() -> Result<(), GasError> {
    charge_gas_args("lambda-eval", GasArgs::Constant(MilliGas(50)))
}

/// Charge gas for conditional evaluation
pub fn charge_conditional() -> Result<(), GasError> {
    charge_gas_args("conditional", GasArgs::Constant(MilliGas(25)))
}

/// Charge gas for sequence evaluation
pub fn charge_sequence(count: usize) -> Result<(), GasError> {
    charge_gas_args("sequence", GasArgs::Constant(MilliGas((count * 25) as u64)))
}

/// Charge gas for object construction
pub fn charge_obj_construction(field_count: usize) -> Result<(), GasError> {
    charge_gas_args("obj-construction", GasArgs::Constant(MilliGas((field_count * 25) as u64)))
}

/// Charge gas for list construction
pub fn charge_list_construction(element_count: usize) -> Result<(), GasError> {
    charge_gas_args("list-construction", GasArgs::Constant(MilliGas((element_count * 25) as u64)))
}

/// Charge gas for database read operations
pub fn charge_db_read() -> Result<(), GasError> {
    charge_gas_args("db-read", GasArgs::Constant(MilliGas(2_500)))
}

/// Charge gas for database write operations
pub fn charge_db_write(byte_count: usize) -> Result<(), GasError> {
    let base_cost = 25_000u64;
    let byte_cost = (byte_count * 200) as u64;
    charge_gas_args("db-write", GasArgs::Constant(MilliGas(base_cost + byte_cost)))
}

/// Charge gas for select query operations
pub fn charge_select_query() -> Result<(), GasError> {
    charge_gas_args("select-query", GasArgs::Constant(MilliGas(40_000_000)))
}

/// Charge gas for comparison operations
pub fn charge_comparison_op(size_hint: usize) -> Result<(), GasError> {
    let base_cost = match size_hint {
        0..=10 => 25,
        11..=100 => 50,
        101..=1000 => 100,
        _ => 200,
    };
    charge_gas_args("comparison", GasArgs::Constant(MilliGas(base_cost)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gas_cost_calculation() {
        let model = GasModel::default();

        // Test constant cost
        let cost = model.calculate_cost(&GasArgs::Constant(MilliGas(500)));
        assert_eq!(cost, MilliGas(500));

        // Test native function cost
        let cost = model.calculate_cost(&GasArgs::Native("test".to_string()));
        assert_eq!(cost, MilliGas(100));

        // Test integer operation cost
        let cost = model.calculate_cost(&GasArgs::IntegerOpCost(IntegerPrimOp::Add, 10, 20));
        assert!(cost.0 >= 100); // At least base cost

        // Test database read/write
        let read_cost = model.calculate_cost(&GasArgs::Read(100));
        assert_eq!(read_cost, MilliGas(2500 + 100 * 100)); // penalty + per-byte

        let write_cost = model.calculate_cost(&GasArgs::Write(100));
        assert_eq!(write_cost, MilliGas(25000 + 100 * 200)); // penalty + per-byte
    }

    #[test]
    fn test_gas_environment() {
        let model = GasModel {
            config: GasCostConfig::default(),
            limit: Some(MilliGasLimit(MilliGas(1000))),
        };

        let env = GasEnv::new(model);

        // Test charging gas
        assert!(env
            .charge_gas_args("test", GasArgs::Native("test".to_string()))
            .is_ok());
        assert_eq!(env.consumed(), MilliGas(100));

        // Test gas limit
        let large_cost = GasArgs::Write(10); // Will exceed limit
        assert!(env.charge_gas_args("write", large_cost).is_err());
    }

    #[test]
    fn test_integer_op_complexity() {
        let model = GasModel::default();

        // Small numbers should have lower cost than large numbers
        let small_cost = model.calculate_cost(&GasArgs::IntegerOpCost(IntegerPrimOp::Mul, 2, 3));
        let large_cost = model.calculate_cost(&GasArgs::IntegerOpCost(
            IntegerPrimOp::Mul,
            1000000,
            2000000,
        ));

        assert!(small_cost < large_cost);

        // Addition should be cheaper than multiplication
        let add_cost =
            model.calculate_cost(&GasArgs::IntegerOpCost(IntegerPrimOp::Add, 1000, 2000));
        let mul_cost =
            model.calculate_cost(&GasArgs::IntegerOpCost(IntegerPrimOp::Mul, 1000, 2000));

        assert!(add_cost < mul_cost);
    }

    #[test]
    fn test_thread_local_gas() {
        let model = GasModel::default();
        let env = Arc::new(GasEnv::new(model));

        set_gas_env(env);

        // Test charging through global API
        assert!(charge_flat_native_gas("test").is_ok());
        assert_eq!(get_gas_consumed(), MilliGas(100));

        // Test clearing environment
        clear_gas_env();
        assert_eq!(get_gas_consumed(), MilliGas::ZERO);
    }
}
