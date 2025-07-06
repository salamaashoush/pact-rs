//! Database builtin functions
//!
//! This module provides placeholder implementations for database builtins.
//! The actual implementation will use the PactDb from the CEK environment.

use pact_cek::types::{CEKValue, NativeFunction, BuiltinSpec};
use pact_cek::cont::Cont;
use pact_cek::error::CEKErrorHandler;
use pact_cek::eval::{EvalResult, return_cek_value};
use pact_cek::monad::EvalM;
use pact_core::shared::SpanInfo;
use pact_core::values::PactValue;
use pact_core::errors::{PactError, EvalError};
use pact_ir::CoreBuiltin;

/// Placeholder implementation of the `read` builtin function
/// 
/// (read table key)
/// 
/// In the real implementation, this will use the PactDb from CEKEnv
pub fn builtin_read() -> BuiltinSpec {
    BuiltinSpec {
        name: "read",
        arity: 2,
        implementation: Box::new(|info, _builtin, cont, handler, env, args| {
            // Placeholder - real implementation will:
            // 1. Extract table and key from args
            // 2. Get PactDb from env.pact_db
            // 3. Call db.read() which returns GasM<Option<RowData>>
            // 4. Lift GasM to EvalM using liftGasM
            // 5. Return the result
            
            EvalM::pure_value(EvalResult::EvalError(
                PactError::PEExecutionError(
                    EvalError::RuntimeError("Database operations not yet implemented".to_string()),
                    vec![],
                    info,
                )
            ))
        }),
    }
}

/// Placeholder implementation of the `write` builtin function
pub fn builtin_write() -> BuiltinSpec {
    BuiltinSpec {
        name: "write",
        arity: 3,
        implementation: Box::new(|info, _builtin, cont, handler, env, args| {
            EvalM::pure_value(EvalResult::EvalError(
                PactError::PEExecutionError(
                    EvalError::RuntimeError("Database operations not yet implemented".to_string()),
                    vec![],
                    info,
                )
            ))
        }),
    }
}

/// Placeholder implementation of the `keys` builtin function
pub fn builtin_keys() -> BuiltinSpec {
    BuiltinSpec {
        name: "keys",
        arity: 1,
        implementation: Box::new(|info, _builtin, cont, handler, env, args| {
            EvalM::pure_value(EvalResult::EvalError(
                PactError::PEExecutionError(
                    EvalError::RuntimeError("Database operations not yet implemented".to_string()),
                    vec![],
                    info,
                )
            ))
        }),
    }
}

/// Register all database builtins
pub fn register_database_builtins(env: &mut pact_cek::types::BuiltinEnv) {
    env.register(CoreBuiltin::CoreRead, builtin_read());
    env.register(CoreBuiltin::CoreWrite, builtin_write());
    env.register(CoreBuiltin::CoreKeys, builtin_keys());
    
    // Additional database builtins will be added here:
    // - insert
    // - update  
    // - select
    // - create-table
    // - with-read
    // - with-default-read
}