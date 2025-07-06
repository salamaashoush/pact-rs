//! Error helper functions for builtins with proper stack frame support
//!
//! This module provides helper functions for creating errors with proper
//! stack frames and span information, following the Haskell implementation.

use crate::monad::{EvalM, EvalState};
use crate::eval::EvalResult;
use pact_core::errors::{PactError, EvalError};
use pact_core::shared::SpanInfo;
use pact_core::errors::StackFrame;
use pact_ir::CoreBuiltin;

/// Create a builtin error with stack frames from current evaluation state
pub fn builtin_error_with_stack<T: Send + 'static>(
    builtin: CoreBuiltin,
    error: EvalError,
    info: SpanInfo,
) -> EvalM<T> {
    EvalM::<T>::get_state().bind(move |state: EvalState| {
        // Convert our call stack to Pact stack frames
        let stack_frames = state.call_stack.iter().map(|frame| {
            StackFrame {
                name: frame.function_name.clone(),
                args: frame.arguments.iter().map(|v| format!("{:?}", v)).collect(),
                fn_type: frame.function_type.clone(),
                info: frame.source_info.clone(),
            }
        }).collect();
        
        EvalM::throw_error(PactError::PEExecutionError(
            error,
            stack_frames,
            info
        ))
    })
}

/// Create an error result with stack frames
pub fn builtin_error_result_with_stack(
    builtin: CoreBuiltin,
    error: EvalError,
    info: SpanInfo,
) -> EvalM<EvalResult> {
    EvalM::<EvalResult>::get_state().bind(move |state: EvalState| {
        // Convert our call stack to Pact stack frames
        let stack_frames = state.call_stack.iter().map(|frame| {
            StackFrame {
                name: frame.function_name.clone(),
                args: frame.arguments.iter().map(|v| format!("{:?}", v)).collect(),
                fn_type: frame.function_type.clone(),
                info: frame.source_info.clone(),
            }
        }).collect();
        
        EvalM::pure_value(EvalResult::EvalError(
            PactError::PEExecutionError(
                error,
                stack_frames,
                info
            )
        ))
    })
}

/// Throw an argument count error with proper stack frame
pub fn throw_argument_count_error_with_stack(
    builtin: CoreBuiltin,
    info: SpanInfo,
    function: &str,
    expected: usize,
    received: usize,
) -> EvalM<EvalResult> {
    let error = EvalError::ArgumentCountMismatch {
        function: function.to_string(),
        expected,
        received
    };
    builtin_error_result_with_stack(builtin, error, info)
}

/// Throw a type mismatch error with proper stack frame
pub fn throw_type_mismatch_error_with_stack(
    builtin: CoreBuiltin,
    info: SpanInfo,
    expected: &str,
    found: &str,
    context: &str,
) -> EvalM<EvalResult> {
    let error = EvalError::TypeMismatch {
        expected: expected.to_string(),
        found: found.to_string(),
        context: context.to_string()
    };
    builtin_error_result_with_stack(builtin, error, info)
}

/// Push a builtin call frame onto the stack - simplified version
pub fn with_builtin_frame(
    _builtin: CoreBuiltin,
    _info: SpanInfo,
    _args: Vec<pact_core::values::PactValue>,
) -> () {
    // TODO: Implement actual frame tracking
    // For now, this is a placeholder
    ()
}

/// Macro to simplify builtin error creation with proper stack frames
#[macro_export]
macro_rules! builtin_error {
    ($builtin:expr, $error:expr, $info:expr) => {
        $crate::builtin::error_helpers::builtin_error_with_stack($builtin, $error, $info)
    };
}

/// Macro to simplify error result creation
#[macro_export]
macro_rules! builtin_error_result {
    ($builtin:expr, $error:expr, $info:expr) => {
        $crate::builtin::error_helpers::builtin_error_result_with_stack($builtin, $error, $info)
    };
}