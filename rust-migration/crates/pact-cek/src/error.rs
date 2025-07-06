//! CEK Error Handling
//!
//! This module implements the error handling system for the CEK machine,
//! following the Haskell reference implementation exactly.

use crate::types::CEKEnv;
use crate::cont::Cont;
use pact_ir::CoreTerm;
use pact_core::shared::SpanInfo;
use pact_core::errors::{PactErrorI};

/// CEK Error Handler - matches Haskell CEKErrorHandler exactly
///
/// ```haskell
/// data CEKErrorHandler e b i
///   = CEKNoHandler
///   | CEKHandler (CEKEnv e b i) (EvalTerm b i) (Cont e b i) (ErrorState i) (CEKErrorHandler e b i)
///   | CEKEnforceOne (CEKEnv e b i) i (EvalTerm b i) [EvalTerm b i] (Cont e b i) (ErrorState i) (CEKErrorHandler e b i)
/// ```
#[derive(Debug, Clone)]
pub enum CEKErrorHandler {
    /// No error handler - errors propagate up
    CEKNoHandler,

    /// Try-catch style error handler
    /// Captures errors and executes recovery expression
    CEKHandler {
        /// Environment snapshot for recovery
        env: CEKEnv,
        /// Recovery expression to evaluate on error
        recovery: CoreTerm,
        /// Continuation after recovery
        cont: Cont,
        /// Error state information
        error_state: ErrorState,
        /// Next error handler in chain
        next_handler: Box<CEKErrorHandler>,
    },

    /// Enforce-one error handler (special case)
    /// Tries multiple conditions, succeeds if any succeeds
    CEKEnforceOne {
        /// Environment for remaining attempts
        env: CEKEnv,
        /// Source information
        info: SpanInfo,
        /// Current expression being tried
        current: CoreTerm,
        /// Remaining expressions to try
        remaining: Vec<CoreTerm>,
        /// Continuation after success
        cont: Cont,
        /// Error state information
        error_state: ErrorState,
        /// Next error handler in chain
        next_handler: Box<CEKErrorHandler>,
    },
}

/// Error state information for debugging and recovery
#[derive(Debug, Clone)]
pub struct ErrorState {
    /// Source location where error handler was installed
    pub source_info: SpanInfo,
    /// Call stack snapshot
    pub call_stack: Vec<CallFrame>,
    /// Gas state at time of handler installation
    pub gas_snapshot: GasSnapshot,
}

/// Call frame for stack traces
#[derive(Debug, Clone)]
pub struct CallFrame {
    /// Fully qualified function name
    pub function_name: pact_core::errors::FullyQualifiedName,
    /// Function arguments as PactValues
    pub arguments: Vec<pact_core::values::PactValue>,
    /// Function type (defun, defcap, defpact)
    pub function_type: pact_core::errors::StackFunctionType,
    /// Source location of call
    pub source_info: SpanInfo,
}

/// Gas state snapshot for recovery
#[derive(Debug, Clone)]
pub struct GasSnapshot {
    /// Gas limit at snapshot time
    pub gas_limit: u64,
    /// Gas consumed at snapshot time
    pub gas_consumed: u64,
}

/// Error recovery result
#[derive(Debug, Clone)]
pub enum ErrorRecovery {
    /// Error was handled, continue with value
    Recovered(super::types::CEKValue),
    /// Error was not handled, propagate
    Propagate(PactErrorI),
    /// Try next handler in chain
    Continue(PactErrorI),
}

impl CEKErrorHandler {
    /// Create a no-handler
    pub fn no_handler() -> Self {
        CEKErrorHandler::CEKNoHandler
    }

    /// Create a try-catch handler
    pub fn try_handler(
        env: CEKEnv,
        recovery: CoreTerm,
        cont: Cont,
        error_state: ErrorState,
        next_handler: CEKErrorHandler,
    ) -> Self {
        CEKErrorHandler::CEKHandler {
            env,
            recovery,
            cont,
            error_state,
            next_handler: Box::new(next_handler),
        }
    }

    /// Create an enforce-one handler
    pub fn enforce_one_handler(
        env: CEKEnv,
        info: SpanInfo,
        current: CoreTerm,
        remaining: Vec<CoreTerm>,
        cont: Cont,
        error_state: ErrorState,
        next_handler: CEKErrorHandler,
    ) -> Self {
        CEKErrorHandler::CEKEnforceOne {
            env,
            info,
            current,
            remaining,
            cont,
            error_state,
            next_handler: Box::new(next_handler),
        }
    }

    /// Check if this is a no-handler
    pub fn is_no_handler(&self) -> bool {
        matches!(self, CEKErrorHandler::CEKNoHandler)
    }

    /// Get the next handler in the chain
    pub fn next_handler(&self) -> Option<&CEKErrorHandler> {
        match self {
            CEKErrorHandler::CEKNoHandler => None,
            CEKErrorHandler::CEKHandler { next_handler, .. } => Some(next_handler),
            CEKErrorHandler::CEKEnforceOne { next_handler, .. } => Some(next_handler),
        }
    }

    /// Handle an error with this handler
    pub fn handle_error(&self, error: PactErrorI) -> ErrorRecovery {
        match self {
            CEKErrorHandler::CEKNoHandler => {
                // No handler, propagate error
                ErrorRecovery::Propagate(error)
            }

            CEKErrorHandler::CEKHandler { .. } => {
                // Try-catch handler - check if error is recoverable
                if is_recoverable_error(&error) {
                    // Error can be recovered, evaluate recovery expression
                    ErrorRecovery::Continue(error)
                } else {
                    // Error cannot be recovered, propagate
                    ErrorRecovery::Propagate(error)
                }
            }

            CEKErrorHandler::CEKEnforceOne { remaining, .. } => {
                // Enforce-one handler
                if !remaining.is_empty() {
                    // More conditions to try
                    ErrorRecovery::Continue(error)
                } else {
                    // No more conditions, propagate original error
                    ErrorRecovery::Propagate(error)
                }
            }
        }
    }

    /// Get source information from error state
    pub fn source_info(&self) -> Option<&SpanInfo> {
        match self {
            CEKErrorHandler::CEKNoHandler => None,
            CEKErrorHandler::CEKHandler { error_state, .. } => Some(&error_state.source_info),
            CEKErrorHandler::CEKEnforceOne { info, .. } => Some(info),
        }
    }

    /// Get call stack from error state
    pub fn call_stack(&self) -> Option<&Vec<CallFrame>> {
        match self {
            CEKErrorHandler::CEKNoHandler => None,
            CEKErrorHandler::CEKHandler { error_state, .. } => Some(&error_state.call_stack),
            CEKErrorHandler::CEKEnforceOne { error_state, .. } => Some(&error_state.call_stack),
        }
    }
}

/// Check if an error is recoverable by a try-catch handler
fn is_recoverable_error(error: &PactErrorI) -> bool {
    use pact_core::errors::{PactError, EvalError};

    match error {
        // Execution errors that can be caught
        PactError::PEExecutionError(eval_error, _, _) => {
            match eval_error {
                // User errors can be caught
                EvalError::RuntimeError(_) => true,
                EvalError::GuardFailure(_) => true,
                EvalError::CapabilityNotGranted(_) => true,
                EvalError::KeysetFailure(_) => true,

                // Type errors can be caught
                EvalError::TypeMismatch { .. } => true,
                EvalError::InvalidArgument(_) => true,
                EvalError::ArgumentCountMismatch { .. } => true,

                // Database errors can be caught
                EvalError::TableNotFound(_) => true,
                EvalError::RowNotFound { .. } => true,

                // Module errors can be caught
                EvalError::ModuleNotFound(_) => true,
                EvalError::ModuleAlreadyExists(_) => true,

                // Variable errors can be caught
                EvalError::UnboundVariable(_) => true,

                // Capability errors can be caught
                EvalError::CapabilityAlreadyAcquired(_) => true,

                // Arithmetic errors - some can be caught
                EvalError::DivisionByZero => true,
                EvalError::NumericOverflow => false, // Generally too severe

                // Invariant failures cannot be caught
                EvalError::InvariantFailure(_) => false,

                // All other error types - default to catchable
                _ => true,
            }
        }

        // User recoverable errors can be caught
        PactError::PEUserRecoverableError(_, _, _) => true,

        // Parse errors cannot be caught (shouldn't occur during evaluation)
        PactError::PEParseError(_, _) => false,
        PactError::PELexerError(_, _) => false,

        // Desugar errors can be caught
        PactError::PEDesugarError(_, _) => true,

        // Verifier errors can be caught
        PactError::PEVerifierError(_, _) => true,
    }
}

impl ErrorState {
    /// Create new error state
    pub fn new(source_info: SpanInfo, call_stack: Vec<CallFrame>, gas_snapshot: GasSnapshot) -> Self {
        ErrorState {
            source_info,
            call_stack,
            gas_snapshot,
        }
    }

    /// Create empty error state for testing
    pub fn empty() -> Self {
        ErrorState {
            source_info: SpanInfo::empty(),
            call_stack: Vec::new(),
            gas_snapshot: GasSnapshot {
                gas_limit: 0,
                gas_consumed: 0,
            },
        }
    }
}


impl GasSnapshot {
    /// Create new gas snapshot
    pub fn new(gas_limit: u64, gas_consumed: u64) -> Self {
        GasSnapshot {
            gas_limit,
            gas_consumed,
        }
    }

    /// Get remaining gas at snapshot time
    pub fn remaining_gas(&self) -> u64 {
        self.gas_limit.saturating_sub(self.gas_consumed)
    }
}

/// Error context for improved error reporting
#[derive(Debug, Clone)]
pub struct ErrorContext {
    /// Primary error
    pub error: PactErrorI,
    /// Call stack trace
    pub call_stack: Vec<CallFrame>,
    /// Source code context
    pub source_context: Option<String>,
    /// Related error locations
    pub related_locations: Vec<(String, SpanInfo)>,
}

impl ErrorContext {
    /// Create new error context
    pub fn new(error: PactErrorI) -> Self {
        ErrorContext {
            error,
            call_stack: Vec::new(),
            source_context: None,
            related_locations: Vec::new(),
        }
    }

    /// Add call frame to context
    pub fn with_call_frame(mut self, frame: CallFrame) -> Self {
        self.call_stack.push(frame);
        self
    }

    /// Add call stack to context
    pub fn with_call_stack(mut self, stack: Vec<CallFrame>) -> Self {
        self.call_stack = stack;
        self
    }

    /// Add source context to error
    pub fn with_source_context(mut self, context: String) -> Self {
        self.source_context = Some(context);
        self
    }

    /// Add related location to error
    pub fn with_related_location(mut self, label: String, location: SpanInfo) -> Self {
        self.related_locations.push((label, location));
        self
    }

    /// Format error with full context
    pub fn format(&self) -> String {
        let mut result = format!("Error: {}\n", self.error);

        // Add call stack
        if !self.call_stack.is_empty() {
            result.push_str("\nCall stack:\n");
            for (i, frame) in self.call_stack.iter().enumerate() {
                let name = &frame.function_name.name;
                result.push_str(&format!("  {}: {} at {:?}\n", i, name, frame.source_info));

                if !frame.arguments.is_empty() {
                    let args_str: Vec<String> = frame.arguments.iter().map(|v| format!("{:?}", v)).collect();
                    result.push_str(&format!("      Arguments: {}\n", args_str.join(", ")));
                }
            }
        }

        // Add source context
        if let Some(context) = &self.source_context {
            result.push_str("\nSource context:\n");
            result.push_str(context);
            result.push('\n');
        }

        // Add related locations
        if !self.related_locations.is_empty() {
            result.push_str("\nRelated locations:\n");
            for (label, location) in &self.related_locations {
                result.push_str(&format!("  {}: {:?}\n", label, location));
            }
        }

        result
    }
}

/// User-recoverable error types from Haskell implementation
#[derive(Debug, Clone)]
pub enum UserRecoverableError {
    /// Enforce error with message
    EnforceError(String),

    /// One-shot capability already used
    OneShotCapAlreadyUsed,

    /// Capability not granted
    CapabilityNotGranted { capability: String },

    /// No such object in database
    NoSuchObjectInDb { table: String, key: String },

    /// Keyset predicate failure
    KeysetPredicateFailure { predicate: String },

    /// Capability pact guard invalid pact ID
    CapabilityPactGuardInvalidPactId { expected: String, actual: String },

    /// Environment read function failure
    EnvReadFunctionFailure { function: String },

    /// Verifier failure
    VerifierFailure { verifier: String, message: String },

    /// Capability guard not acquired
    CapabilityGuardNotAcquired { guard: String },
}

impl UserRecoverableError {
    /// Convert to PactError
    pub fn into_pact_error(self, span: SpanInfo) -> PactErrorI {
        use pact_core::errors::{EvalError, PactError};

        let eval_error = match self {
            UserRecoverableError::EnforceError(msg) => {
                EvalError::RuntimeError(msg)
            }
            UserRecoverableError::CapabilityNotGranted { capability } => {
                EvalError::CapabilityNotGranted(capability)
            }
            UserRecoverableError::NoSuchObjectInDb { table, key } => {
                EvalError::RowNotFound { table, key }
            }
            UserRecoverableError::KeysetPredicateFailure { predicate } => {
                EvalError::KeysetFailure(format!("Keyset predicate failure: {}", predicate))
            }
            UserRecoverableError::OneShotCapAlreadyUsed => {
                EvalError::CapabilityAlreadyAcquired("One-shot capability already used".to_string())
            }
            UserRecoverableError::CapabilityPactGuardInvalidPactId { expected, actual } => {
                EvalError::GuardFailure(format!(
                    "Capability pact guard invalid pact ID: expected {}, got {}",
                    expected, actual
                ))
            }
            UserRecoverableError::EnvReadFunctionFailure { function } => {
                EvalError::RuntimeError(format!("Environment context required for function: {}", function))
            }
            UserRecoverableError::VerifierFailure { verifier, message } => {
                EvalError::RuntimeError(format!("Verifier failure: {}: {}", verifier, message))
            }
            UserRecoverableError::CapabilityGuardNotAcquired { guard } => {
                EvalError::GuardFailure(format!("Capability guard not acquired: {}", guard))
            }
        };

        PactError::PEExecutionError(eval_error, vec![], span)
    }
}
