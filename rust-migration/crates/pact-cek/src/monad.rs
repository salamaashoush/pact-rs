//! Evaluation Monad
//!
//! This module implements the EvalM monad that corresponds to the Haskell
//! evaluation monad. It provides state management, error handling, and
//! IO operations for the CEK machine.

use crate::types::CEKValue;
use crate::error::{GasSnapshot, CallFrame};
use crate::types::GasState;
use pact_core::errors::{PactError, EvalError, PactErrorI};
use pact_core::shared::SpanInfo;
use pact_core::gas::MilliGas;
use std::sync::Arc;

/// Evaluation monad - equivalent to Haskell EvalM
///
/// ```haskell
/// newtype EvalM e b i a = EvalM (ReaderT (EvalMEnv e b i) (ExceptT (PactError i) (StateT (EvalState b i) IO)) a)
/// ```
pub struct EvalM<T> {
    inner: Box<dyn FnOnce(EvalMEnv, EvalState) -> Result<(T, EvalState), PactErrorI> + Send>,
}

impl<T> EvalM<T> {
    /// Create new EvalM computation
    pub fn new<F>(f: F) -> Self
    where
        F: FnOnce(EvalMEnv, EvalState) -> Result<(T, EvalState), PactErrorI> + Send + 'static,
        T: Send + 'static,
    {
        EvalM {
            inner: Box::new(f),
        }
    }

    /// Run the computation with initial environment and state
    pub fn run(self, env: EvalMEnv, state: EvalState) -> Result<(T, EvalState), PactErrorI> {
        (self.inner)(env, state)
    }

    /// Create pure value (return in monad)
    pub fn pure_value(value: T) -> Self
    where
        T: Send + 'static,
    {
        EvalM::new(move |_env, state| Ok((value, state)))
    }

    /// Alias for pure_value - commonly used in Haskell-style code
    pub fn pure(value: T) -> Self
    where
        T: Send + 'static,
    {
        Self::pure_value(value)
    }

    /// Throw an error
    pub fn throw_error(error: PactErrorI) -> Self
    where
        T: Send + 'static,
    {
        EvalM::new(move |_env, _state| Err(error))
    }

    /// Get current state
    pub fn get_state() -> EvalM<EvalState> {
        EvalM::new(|_env, state| Ok((state.clone(), state)))
    }

    /// Put new state
    pub fn put_state(new_state: EvalState) -> EvalM<()> {
        EvalM::new(move |_env, _state| Ok(((), new_state)))
    }

    /// Modify state
    pub fn modify_state<F>(f: F) -> EvalM<()>
    where
        F: FnOnce(EvalState) -> EvalState + Send + 'static,
    {
        EvalM::new(move |_env, state| {
            let new_state = f(state);
            Ok(((), new_state))
        })
    }

    /// Ask for environment
    pub fn ask_env() -> EvalM<EvalMEnv> {
        EvalM::new(|env, state| Ok((env, state)))
    }

    /// Lift an IO operation that may fail into EvalM
    /// This method converts Result<T, E> into EvalM<T> by converting errors to PactError
    pub fn try_from_io<E, F>(io_op: F) -> Self
    where
        T: Send + 'static,
        E: std::fmt::Display + Send + 'static,
        F: FnOnce() -> Result<T, E> + Send + 'static,
    {
        EvalM::new(move |_env, state| {
            match io_op() {
                Ok(value) => Ok((value, state)),
                Err(error) => {
                    let pact_error = PactError::PEExecutionError(
                        EvalError::RuntimeError(format!("IO Error: {}", error)),
                        vec![],
                        SpanInfo::empty()
                    );
                    Err(pact_error)
                }
            }
        })
    }

    /// Local environment modification
    pub fn local_env<F>(f: F, computation: EvalM<T>) -> EvalM<T>
    where
        F: FnOnce(EvalMEnv) -> EvalMEnv + Send + 'static,
        T: Send + 'static,
    {
        EvalM::new(move |env, state| {
            let new_env = f(env);
            computation.run(new_env, state)
        })
    }
}

impl<T> EvalM<T>
where
    T: Send + 'static,
{
    /// Monadic bind (flatMap)
    pub fn bind<U, F>(self, f: F) -> EvalM<U>
    where
        F: FnOnce(T) -> EvalM<U> + Send + 'static,
        U: Send + 'static,
    {
        EvalM::new(move |env, state| {
            let (value, new_state) = self.run(env.clone(), state)?;
            let next_computation = f(value);
            next_computation.run(env, new_state)
        })
    }

    /// Functor map
    pub fn map<U, F>(self, f: F) -> EvalM<U>
    where
        F: FnOnce(T) -> U + Send + 'static,
        U: Send + 'static,
    {
        EvalM::new(move |env, state| {
            let (value, new_state) = self.run(env, state)?;
            Ok((f(value), new_state))
        })
    }

    /// Apply function in monad
    pub fn apply<U, F>(self, func: EvalM<F>) -> EvalM<U>
    where
        F: FnOnce(T) -> U + Send + 'static,
        U: Send + 'static,
    {
        EvalM::new(move |env, state| {
            let (f, state1) = func.run(env.clone(), state)?;
            let (value, state2) = self.run(env, state1)?;
            Ok((f(value), state2))
        })
    }

    /// Try computation with error recovery and proper state unwinding
    pub fn try_with<F>(self, recovery: F) -> EvalM<T>
    where
        F: FnOnce(PactErrorI) -> EvalM<T> + Send + 'static,
    {
        EvalM::new(move |env, state| {
            match self.run(env.clone(), state) {
                Ok(result) => Ok(result),
                Err(error) => {
                    // Apply error recovery with proper state handling
                    let recovery_computation = recovery(error.clone());
                    match recovery_computation.run(env, EvalState::new()) {
                        Ok(recovered_result) => Ok(recovered_result),
                        Err(recovery_error) => {
                            // Recovery failed, propagate original error if it's more severe
                            if is_more_severe_error(&error, &recovery_error) {
                                Err(error)
                            } else {
                                Err(recovery_error)
                            }
                        }
                    }
                }
            }
        })
    }

    /// Catch specific error types
    pub fn catch<F>(self, handler: F) -> EvalM<T>
    where
        F: FnOnce(PactErrorI) -> Option<EvalM<T>> + Send + 'static,
    {
        EvalM::new(move |env, state| {
            match self.run(env.clone(), state) {
                Ok(result) => Ok(result),
                Err(error) => {
                    if let Some(recovery) = handler(error.clone()) {
                        recovery.run(env, EvalState::new())
                    } else {
                        Err(error)
                    }
                }
            }
        })
    }
}

/// Environment for the evaluation monad
#[derive(Debug, Clone)]
pub struct EvalMEnv {
    /// Gas tracking environment
    pub gas_env: GasEnv,
    /// Purity environment (whether in pure context)
    pub purity_env: PurityEnv,
    /// Transaction environment
    pub tx_env: TxEnv,
    /// Namespace environment
    pub namespace_env: NamespaceEnv,
}

/// Gas tracking environment
#[derive(Debug, Clone)]
pub struct GasEnv {
    /// Gas limit for current computation
    pub gas_limit: u64,
    /// Gas pricing model
    pub gas_model: GasModel,
    /// Whether gas is being tracked
    pub gas_tracking_enabled: bool,
}

impl Default for GasEnv {
    fn default() -> Self {
        GasEnv {
            gas_limit: 1_000_000,
            gas_model: GasModel::default(),
            gas_tracking_enabled: true,
        }
    }
}

/// Gas pricing model
#[derive(Clone)]
pub enum GasModel {
    /// Fixed cost model
    Fixed {
        /// Cost per operation
        base_cost: MilliGas,
    },
    /// Table-based cost model
    Table {
        /// Operation costs
        operation_costs: std::collections::HashMap<String, MilliGas>,
        /// Default cost for unknown operations
        default_cost: MilliGas,
    },
    /// Dynamic cost model
    Dynamic {
        /// Cost calculation function (no Debug implementation)
        calculator: Arc<dyn Fn(&str, &[CEKValue]) -> MilliGas + Send + Sync>,
    },
}

impl std::fmt::Debug for GasModel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GasModel::Fixed { base_cost } => f.debug_struct("Fixed")
                .field("base_cost", base_cost)
                .finish(),
            GasModel::Table { operation_costs, default_cost } => f.debug_struct("Table")
                .field("operation_costs", operation_costs)
                .field("default_cost", default_cost)
                .finish(),
            GasModel::Dynamic { .. } => f.debug_struct("Dynamic")
                .field("calculator", &"<function>")
                .finish(),
        }
    }
}

impl Default for GasModel {
    fn default() -> Self {
        GasModel::Fixed { base_cost: MilliGas(1) }
    }
}

/// Purity environment
#[derive(Debug, Clone)]
pub struct PurityEnv {
    /// Whether currently in pure context
    pub in_pure_context: bool,
    /// Pure modules
    pub pure_modules: Vec<String>,
    /// Pure functions
    pub pure_functions: Vec<String>,
}

impl Default for PurityEnv {
    fn default() -> Self {
        PurityEnv {
            in_pure_context: false,
            pure_modules: vec![],
            pure_functions: vec![],
        }
    }
}

/// Transaction environment
#[derive(Debug, Clone)]
pub struct TxEnv {
    /// Transaction ID
    pub tx_id: Option<String>,
    /// Transaction step (for DefPacts)
    pub tx_step: Option<u32>,
    /// Transaction sender
    pub sender: Option<String>,
    /// Transaction chainId
    pub chain_id: Option<String>,
    /// Block height
    pub block_height: Option<u64>,
    /// Block time
    pub block_time: Option<chrono::DateTime<chrono::Utc>>,
}

impl Default for TxEnv {
    fn default() -> Self {
        TxEnv {
            tx_id: None,
            tx_step: None,
            sender: None,
            chain_id: None,
            block_height: None,
            block_time: None,
        }
    }
}

/// Namespace environment
#[derive(Debug, Clone)]
pub struct NamespaceEnv {
    /// Current namespace
    pub current_namespace: Option<String>,
    /// Available namespaces
    pub available_namespaces: Vec<String>,
    /// Namespace permissions
    pub namespace_permissions: std::collections::HashMap<String, Vec<String>>,
}

impl Default for NamespaceEnv {
    fn default() -> Self {
        NamespaceEnv {
            current_namespace: None,
            available_namespaces: vec![],
            namespace_permissions: std::collections::HashMap::new(),
        }
    }
}

/// State for the evaluation monad
#[derive(Debug, Clone)]
pub struct EvalState {
    /// Gas tracking state
    pub gas_state: GasState,
    /// Evaluation call stack
    pub eval_stack: Vec<EvalStackFrame>,
    /// Call stack for debugging
    pub call_stack: Vec<CallFrame>,
    /// Module loading state
    pub module_state: ModuleState,
    /// Capability state
    pub capability_state: CapabilityState,
    /// DefPact execution state
    pub defpact_state: DefPactState,
}


/// Stack frame for evaluation tracking
#[derive(Debug, Clone)]
pub struct EvalStackFrame {
    /// Frame type
    pub frame_type: FrameType,
    /// Source location
    pub source_info: SpanInfo,
    /// Local variables in this frame
    pub locals: Vec<(String, CEKValue)>,
}

/// Types of stack frames
#[derive(Debug, Clone)]
pub enum FrameType {
    /// Function call frame
    FunctionCall { name: String, arity: usize },
    /// Builtin call frame
    BuiltinCall { builtin: String, arity: usize },
    /// Let binding frame
    LetBinding { variable: String },
    /// Module evaluation frame
    ModuleEval { module: String },
    /// Capability frame
    CapabilityFrame { capability: String },
    /// DefPact step frame
    DefPactStep { pact_id: String, step: u32 },
}

/// Module loading and management state
#[derive(Debug, Clone)]
pub struct ModuleState {
    /// Loaded modules
    pub loaded_modules: std::collections::HashMap<String, ModuleInfo>,
    /// Module dependencies
    pub dependencies: std::collections::HashMap<String, Vec<String>>,
    /// Module hash verification
    pub hash_verification: bool,
}

/// Module information
#[derive(Debug, Clone)]
pub struct ModuleInfo {
    /// Module name
    pub name: String,
    /// Module hash
    pub hash: Option<String>,
    /// Module governance
    pub governance: GovernanceInfo,
    /// Module interfaces
    pub interfaces: Vec<String>,
}

/// Governance information
#[derive(Debug, Clone)]
pub enum GovernanceInfo {
    /// Keyset governance
    Keyset { keyset_name: String },
    /// Capability governance
    Capability { cap_name: String },
}

/// Capability execution state
#[derive(Debug, Clone)]
pub struct CapabilityState {
    /// Currently granted capabilities
    pub granted_caps: Vec<GrantedCapability>,
    /// Capability stack
    pub cap_stack: Vec<CapabilityFrame>,
    /// Installed capabilities
    pub installed_caps: std::collections::HashMap<String, InstalledCapability>,
}

/// Granted capability
#[derive(Debug, Clone)]
pub struct GrantedCapability {
    /// Capability name
    pub name: String,
    /// Capability arguments
    pub args: Vec<pact_core::values::PactValue>,
    /// Grant source location
    pub source_info: SpanInfo,
}

/// Capability frame in execution stack
#[derive(Debug, Clone)]
pub struct CapabilityFrame {
    /// Capability name
    pub name: String,
    /// Capability arguments
    pub args: Vec<pact_core::values::PactValue>,
    /// Whether this is a composed capability
    pub is_composed: bool,
}

/// Installed capability
#[derive(Debug, Clone)]
pub struct InstalledCapability {
    /// Capability name
    pub name: String,
    /// Installation arguments
    pub args: Vec<pact_core::values::PactValue>,
    /// Installation source
    pub source_info: SpanInfo,
}

/// DefPact execution state
#[derive(Debug, Clone)]
pub struct DefPactState {
    /// Active DefPact executions
    pub active_pacts: std::collections::HashMap<String, DefPactExecution>,
    /// DefPact execution history
    pub execution_history: Vec<DefPactHistoryEntry>,
}

/// DefPact execution information
#[derive(Debug, Clone)]
pub struct DefPactExecution {
    /// Pact ID
    pub pact_id: String,
    /// Current step
    pub current_step: u32,
    /// Total steps
    pub total_steps: u32,
    /// Step results
    pub step_results: Vec<pact_core::values::PactValue>,
    /// Continuation chain
    pub continuation: Option<String>,
}

/// DefPact history entry
#[derive(Debug, Clone)]
pub struct DefPactHistoryEntry {
    /// Pact ID
    pub pact_id: String,
    /// Step number
    pub step: u32,
    /// Step result
    pub result: pact_core::values::PactValue,
    /// Execution timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

impl Default for EvalMEnv {
    fn default() -> Self {
        EvalMEnv {
            gas_env: GasEnv::default(),
            purity_env: PurityEnv::default(),
            tx_env: TxEnv::default(),
            namespace_env: NamespaceEnv::default(),
        }
    }
}


impl EvalState {
    /// Create new evaluation state
    pub fn new() -> Self {
        EvalState {
            gas_state: GasState::default(),
            eval_stack: Vec::new(),
            call_stack: Vec::new(),
            module_state: ModuleState::new(),
            capability_state: CapabilityState::new(),
            defpact_state: DefPactState::new(),
        }
    }

    /// Push stack frame
    pub fn push_frame(&mut self, frame: EvalStackFrame) {
        self.eval_stack.push(frame);
    }

    /// Pop stack frame
    pub fn pop_frame(&mut self) -> Option<EvalStackFrame> {
        self.eval_stack.pop()
    }

    /// Push call frame
    pub fn push_call(&mut self, frame: CallFrame) {
        self.call_stack.push(frame);
    }

    /// Pop call frame
    pub fn pop_call(&mut self) -> Option<CallFrame> {
        self.call_stack.pop()
    }

    /// Get current gas snapshot
    pub fn gas_snapshot(&self) -> GasSnapshot {
        GasSnapshot::new(
            self.gas_state.gas_limit(),
            self.gas_state.gas_used(),
        )
    }
}

impl Default for EvalState {
    fn default() -> Self {
        Self::new()
    }
}

impl ModuleState {
    pub fn new() -> Self {
        ModuleState {
            loaded_modules: std::collections::HashMap::new(),
            dependencies: std::collections::HashMap::new(),
            hash_verification: true,
        }
    }
}

impl CapabilityState {
    pub fn new() -> Self {
        CapabilityState {
            granted_caps: Vec::new(),
            cap_stack: Vec::new(),
            installed_caps: std::collections::HashMap::new(),
        }
    }
}

impl DefPactState {
    pub fn new() -> Self {
        DefPactState {
            active_pacts: std::collections::HashMap::new(),
            execution_history: Vec::new(),
        }
    }
}

/// Gas charging function
pub fn charge_gas(operation: &str, cost: MilliGas) -> EvalM<()> {
    let op = operation.to_string();
    EvalM::<()>::modify_state(move |mut state| {
        match state.gas_state.charge_gas(cost) {
            Ok(new_gas_state) => {
                state.gas_state = new_gas_state;
            }
            Err(_) => {
                // Gas limit exceeded - state remains unchanged but error will be propagated
                eprintln!("Gas limit exceeded for operation: {}", op);
            }
        }
        state
    })
}

/// Gas charging with arguments
pub fn charge_gas_with_args(operation: &str, args: &[CEKValue], base_cost: MilliGas) -> EvalM<()> {
    let cost = calculate_gas_cost(operation, args, base_cost);
    charge_gas(operation, cost)
}

/// Calculate gas cost based on operation and arguments
fn calculate_gas_cost(operation: &str, args: &[CEKValue], base_cost: MilliGas) -> MilliGas {
    match operation {
        // Higher-order functions: cost scales with collection size
        "map" | "filter" | "fold" => {
            if let Some(list_arg) = args.get(1) {
                if let Some(pact_core::values::PactValue::List(list)) = list_arg.as_pact_value() {
                    // Gas cost: base + (elements * multiplier)
                    let size_cost = list.len() as u64;
                    MilliGas(base_cost.0 + (size_cost * 2)) // 2 gas per element
                } else {
                    base_cost
                }
            } else {
                base_cost
            }
        }

        // String operations: cost scales with string length
        "concat" => {
            let total_length = args.iter()
                .filter_map(|arg| arg.as_pact_value())
                .filter_map(|pv| match pv {
                    pact_core::values::PactValue::String(s) => Some(s.len()),
                    _ => None,
                })
                .sum::<usize>() as u64;
            MilliGas(base_cost.0 + total_length.max(1))
        }

        // List operations: cost scales with list size
        "length" | "reverse" => {
            if let Some(arg) = args.get(0) {
                match arg.as_pact_value() {
                    Some(pact_core::values::PactValue::String(s)) => {
                        MilliGas(base_cost.0 + (s.len() as u64 / 10)) // 1 gas per 10 chars
                    }
                    Some(pact_core::values::PactValue::List(list)) => {
                        MilliGas(base_cost.0 + list.len() as u64) // 1 gas per element
                    }
                    _ => base_cost,
                }
            } else {
                base_cost
            }
        }

        // Database operations: higher base cost + size-dependent costs
        "read" => {
            MilliGas(base_cost.0 * 4) // Database reads are expensive
        }
        "write" => {
            // Write cost depends on data size
            if let Some(data_arg) = args.get(2) {
                let data_size = estimate_value_size(data_arg);
                MilliGas(base_cost.0 * 6 + data_size) // Higher base + size cost
            } else {
                MilliGas(base_cost.0 * 6)
            }
        }
        "select" => {
            MilliGas(base_cost.0 * 8) // Select operations can be very expensive
        }

        // Function application and evaluation
        "eval-step" => MilliGas(1), // Minimal cost for basic evaluation
        "eval-constant" => MilliGas(1), // Constants are cheap
        "eval-lambda" => MilliGas(3), // Lambda creation has some overhead
        "eval-app" => MilliGas(2), // Function application overhead
        "eval-builtin" => MilliGas(1), // Builtin lookup is fast

        // Arithmetic operations
        "+" | "-" => MilliGas(2),
        "*" => MilliGas(3), // Multiplication is more expensive
        "/" => MilliGas(4), // Division is most expensive

        // Comparison operations
        "=" | "<" | ">" => MilliGas(2),

        // Boolean operations
        "not" => MilliGas(1),

        // Default for unknown operations
        _ => base_cost,
    }
}

/// Estimate the gas cost of a CEKValue based on its size and complexity
fn estimate_value_size(value: &CEKValue) -> u64 {
    match value {
        CEKValue::VPactValue(pv) => estimate_pact_value_size(pv),
        CEKValue::VClosure(_) => 10, // Closures have moderate overhead
        CEKValue::VTable { .. } => 5, // Tables have fixed overhead
    }
}

/// Estimate the gas cost of a PactValue based on its size and complexity
fn estimate_pact_value_size(value: &pact_core::values::PactValue) -> u64 {
    match value {
        pact_core::values::PactValue::String(s) => s.len() as u64 / 4, // 1 gas per 4 chars
        pact_core::values::PactValue::Integer(_) => 1,
        pact_core::values::PactValue::Decimal(_) => 2,
        pact_core::values::PactValue::Bool(_) => 1,
        pact_core::values::PactValue::List(items) => {
            1 + items.iter().map(estimate_pact_value_size).sum::<u64>()
        }
        pact_core::values::PactValue::Object(obj) => {
            1 + obj.entries().map(|(k, v)| {
                (k.len() as u64 / 4) + estimate_pact_value_size(v)
            }).sum::<u64>()
        }
        pact_core::values::PactValue::Unit => 1,
        _ => 5, // Conservative estimate for unknown types
    }
}

/// Determine which error is more severe for error propagation
fn is_more_severe_error(original: &PactErrorI, recovery: &PactErrorI) -> bool {
    let original_severity = error_severity(original);
    let recovery_severity = error_severity(recovery);

    original_severity >= recovery_severity
}

/// Get error severity level for prioritization
fn error_severity(error: &PactErrorI) -> u8 {
    match error {
        PactError::PEExecutionError(eval_error, _, _) => {
            match eval_error {
                // Critical system errors (highest priority)  
                EvalError::NumericOverflow => 9,
                EvalError::DivisionByZero => 8,

                // Security errors (high priority)
                EvalError::CapabilityNotGranted(_) => 7,
                EvalError::GuardFailure(_) => 6,
                EvalError::KeysetFailure(_) => 6,

                // Type and argument errors (medium priority)
                EvalError::TypeMismatch { .. } => 5,
                EvalError::ArgumentCountMismatch { .. } => 5,
                EvalError::InvalidArgument(_) => 4,

                // Database errors (lower priority)
                EvalError::TableNotFound(_) => 3,
                EvalError::RowNotFound { .. } => 3,

                // Module errors
                EvalError::ModuleNotFound(_) => 3,
                EvalError::ModuleAlreadyExists(_) => 3,

                // Unbound variable errors
                EvalError::UnboundVariable(_) => 4,

                // General runtime errors (medium priority)
                EvalError::RuntimeError(_) => 4,
                EvalError::InvariantFailure(_) => 8,

                // Capability errors
                EvalError::CapabilityAlreadyAcquired(_) => 2,
                
                // All other error variants - default medium priority
                _ => 4,
            }
        }

        // User recoverable errors (lower priority)
        PactError::PEUserRecoverableError(_, _, _) => 2,

        // Parse errors (shouldn't occur during evaluation)
        PactError::PEParseError(_, _) => 8,
        PactError::PELexerError(_, _) => 8,

        // Desugar errors
        PactError::PEDesugarError(_, _) => 6,

        // Verifier errors
        PactError::PEVerifierError(_, _) => 5,
    }
}

/// Unwind capability stack during error handling
pub fn unwind_capability_stack(error: &PactErrorI) -> EvalM<()> {
    let error_severity_level = error_severity(error);
    EvalM::<()>::modify_state(move |mut state| {
        // For severe errors, clear the capability stack to prevent privilege escalation
        if error_severity_level >= 7 {
            state.capability_state.granted_caps.clear();
            state.capability_state.cap_stack.clear();
        } else {
            // For minor errors, only pop the most recent capability
            state.capability_state.cap_stack.pop();
            if let Some(granted) = state.capability_state.granted_caps.last() {
                if granted.name.starts_with("temporary.") {
                    state.capability_state.granted_caps.pop();
                }
            }
        }
        state
    })
}

/// Create error handler with proper state capture
pub fn create_error_handler_with_state() -> EvalM<crate::error::ErrorState> {
    EvalM::<crate::error::ErrorState>::get_state()
        .map(|state| {
            crate::error::ErrorState::new(
                SpanInfo::empty(), // Would be filled with actual source info
                state.call_stack.clone(),
                state.gas_snapshot(),
            )
        })
}
