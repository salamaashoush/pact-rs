use super::*;

/// Helper function to create a builtin error with current stack frame
fn builtin_error<T>(error: pact_errors::EvalError) -> Result<T, pact_errors::PactErrorI> {
    // In a real implementation, we would get the current stack from the monad state
    // For now, we'll use an empty vec but with a TODO to fix
    // TODO: Get actual stack frames from EvalM state
    Err(pact_errors::PactError::PEExecutionError(
        error,
        vec![], // TODO: Get from EvalM::get_stack()
        pact_shared_types::SpanInfo::empty() // TODO: Get from current context
    ))
}

/// Helper function to create a builtin error with specific info
fn builtin_error_with_info<T>(error: pact_errors::EvalError, info: pact_shared_types::SpanInfo) -> Result<T, pact_errors::PactErrorI> {
    // TODO: Get actual stack frames from EvalM state
    Err(pact_errors::PactError::PEExecutionError(
        error,
        vec![], // TODO: Get from EvalM::get_stack()
        info
    ))
}

/// Helper function to create an error result for EvalM
fn builtin_error_result(error: pact_errors::EvalError, info: pact_shared_types::SpanInfo) -> EvalM<EvalResult> {
    // TODO: Get actual stack frames from EvalM state
    EvalM::pure_value(EvalResult::EvalError(
        pact_errors::PactError::PEExecutionError(
            error,
            vec![], // TODO: Get from EvalM::get_stack()
            info
        )
    ))
}


pub fn register_string_builtins(builtin_env: &mut BuiltinEnv) -> Result<(), pact_errors::PactErrorI> {
    // String-specific operations would go here
    // Currently, most string operations are polymorphic and handled in polymorphic.rs
    Ok(())
}
