//! New Builtin System
//!
//! This module implements the builtin function system following the Haskell
//! reference implementation exactly. All builtins follow the proper CEK
//! integration pattern with environment and continuation access.

use crate::types::{CEKValue, CEKEnv, BuiltinEnv, BuiltinSpec, NativeFunction};
use crate::cont::Cont;
use crate::error::CEKErrorHandler;
use crate::monad::{EvalM, charge_gas_with_args};
use crate::eval::{EvalResult, return_cek_value};
use pact_ir::CoreBuiltin;
use pact_shared_types::SpanInfo;
use pact_gas::MilliGas;
use pact_errors::{PactError, EvalError, ArgTypeError, NativeName, PrimType};
use pact_values::PactValue;

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



pub mod arithmetic;
pub mod comparison;
/// Polymorphic operations that work across types
pub mod polymorphic;
/// List operations module (higher-order functions require continuation support)
pub mod list_ops;
pub mod string_ops;
pub mod boolean_ops;
pub mod error_ops;
pub mod database_ops;
pub mod capability_ops;
pub mod defpact_ops;
pub mod module_ops;
pub mod guard_ops;
pub mod time_ops;
pub mod error_helpers;

/// Convert CEKValue to ArgTypeError for error reporting
/// Matches the Haskell pattern of reporting actual argument types
impl CEKValue {
    pub fn to_arg_type_error(&self) -> ArgTypeError {
        match self {
            CEKValue::VPactValue(pv) => match pv {
                PactValue::String(_) => ArgTypeError::ATEPrim(PrimType::PrimString),
                PactValue::Integer(_) => ArgTypeError::ATEPrim(PrimType::PrimInt),
                PactValue::Decimal(_) => ArgTypeError::ATEPrim(PrimType::PrimDecimal),
                PactValue::Bool(_) => ArgTypeError::ATEPrim(PrimType::PrimBool),
                PactValue::Time(_) => ArgTypeError::ATEPrim(PrimType::PrimTime),
                PactValue::List(_) => ArgTypeError::ATEList,
                PactValue::Object(_) => ArgTypeError::ATEObject,
                // Note: Haskell doesn't have these variants in PactValue, so we map to closest
                _ => ArgTypeError::ATEPrim(PrimType::PrimString), // Default for unmapped types
            },
            CEKValue::VClosure(_) => ArgTypeError::ATEClosure,
            CEKValue::VTable { .. } => ArgTypeError::ATETable,
        }
    }
}

/// Arguments error helper - matches Haskell argsError exactly
/// 
/// From Haskell Utils.hs:
/// ```haskell
/// argsError info b args =
///   throwExecutionError info (NativeArgumentsError (builtinName b) (toArgTypeError <$> args))
/// ```
pub fn args_error(
    info: SpanInfo,
    builtin_name: &str,
    args: &[CEKValue],
) -> EvalM<EvalResult> {
    let arg_type_errors: Vec<ArgTypeError> = args.iter()
        .map(|arg| arg.to_arg_type_error())
        .collect();
    
    EvalM::pure_value(EvalResult::EvalError(
        PactError::PEExecutionError(
            EvalError::NativeArgumentsError {
                name: NativeName(builtin_name.into()),
                errors: arg_type_errors,
            },
            vec![], // TODO: Add proper stack frames
            info,
        )
    ))
}

/// Throw execution error helper - matches Haskell throwExecutionError
/// 
/// From Haskell: throwExecutionError :: i -> EvalError -> EvalM e b i a
pub fn throw_execution_error(
    info: SpanInfo,
    error: EvalError,
) -> EvalM<EvalResult> {
    EvalM::pure_value(EvalResult::EvalError(
        PactError::PEExecutionError(
            error,
            vec![], // TODO: Add proper stack frames
            info,
        )
    ))
}

/// Throw argument count mismatch error helper
pub fn throw_argument_count_error(
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
    // TODO: Get actual stack frames from EvalM state
    EvalM::pure_value(EvalResult::EvalError(
        PactError::PEExecutionError(
            error,
            vec![], // TODO: Get from EvalM::get_stack()
            info
        )
    ))
}

/// Throw type mismatch error helper
pub fn throw_type_mismatch_error(
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
    // TODO: Get actual stack frames from EvalM state
    EvalM::pure_value(EvalResult::EvalError(
        PactError::PEExecutionError(
            error,
            vec![], // TODO: Get from EvalM::get_stack()
            info
        )
    ))
}

/// Throw invalid argument error helper
pub fn throw_invalid_argument_error(
    info: SpanInfo,
    function: &str,
    message: &str,
) -> EvalM<EvalResult> {
    throw_execution_error(
        info,
        EvalError::InvalidArgument(format!("{}: {}", function, message))
    )
}



/// Register all core builtin functions
pub fn register_core_builtins(builtin_env: &mut BuiltinEnv) -> Result<(), pact_errors::PactErrorI> {
    // Arithmetic operations
    arithmetic::register_arithmetic_builtins(builtin_env)?;

    // Comparison operations
    comparison::register_comparison_builtins(builtin_env)?;

    // Polymorphic operations (length, take, drop, etc. that work across types)
    polymorphic::register_polymorphic_builtins(builtin_env)?;

    // List operations
    list_ops::register_list_builtins(builtin_env)?;

    // String operations
    string_ops::register_string_builtins(builtin_env)?;

    // Boolean operations
    boolean_ops::register_boolean_builtins(builtin_env)?;

    // Error handling operations
    error_ops::register_error_builtins(builtin_env)?;

    // Database operations
    database_ops::register_database_builtins(builtin_env)?;

    // Capability operations
    capability_ops::register_capability_builtins(builtin_env)?;

    // DefPact operations
    defpact_ops::register_defpact_builtins(builtin_env)?;

    // Module system operations
    module_ops::register_module_builtins(builtin_env)?;

    // Guard operations
    guard_ops::register_guard_builtins(builtin_env)?;

    // Time operations
    time_ops::register_time_builtins(builtin_env)?;

    Ok(())
}

/// Core builtin implementations trait
pub trait CoreBuiltinImpl {
    /// Get the builtin identifier
    fn builtin(&self) -> CoreBuiltin;

    /// Get the builtin name
    fn name(&self) -> &'static str;

    /// Get the builtin arity
    fn arity(&self) -> usize;

    /// Execute the builtin with CEK context
    fn execute(
        info: SpanInfo,
        builtin: CoreBuiltin,
        cont: Cont,
        handler: CEKErrorHandler,
        env: CEKEnv,
        args: Vec<CEKValue>,
    ) -> EvalM<EvalResult>;
}



