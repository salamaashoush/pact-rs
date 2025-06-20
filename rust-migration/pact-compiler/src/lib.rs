//! Pact Compilation Orchestration
//!
//! This crate orchestrates the complete Pact compilation pipeline,
//! exactly matching the Haskell Compile module functionality.
//!
//! Pipeline stages:
//! 1. Lexing (pact-lexer)
//! 2. Parsing (pact-parser)
//! 3. Desugaring (pact-ir)
//! 4. Name Resolution (pact-ir)
//! 5. Constant Evaluation (pact-eval)
//! 6. Module Hashing (this crate)
//! 7. Final compilation result

pub mod compile;
pub mod module_hash;
pub mod module_storage;
pub mod orchestration;
pub mod cek_integration;
pub mod transitive_deps;

pub use compile::*;
pub use module_hash::*;
pub use module_storage::*;
pub use orchestration::*;
pub use cek_integration::*;
pub use transitive_deps::*;

// Re-export the CompilationResult type for convenience
pub use orchestration::CompileResult as CompilationResult;

/// Compile and evaluate Pact source code
/// 
/// This function combines compilation and evaluation in one step,
/// returning both the compilation result and the evaluation result.
pub fn compile_and_evaluate(source: &str) -> Result<(CompileResult, pact_values::PactValue), pact_errors::PactError<pact_parser::SpanInfo>> {
    // First, compile the source
    let compile_result = compile_pact_source(source)?;
    
    // Then, evaluate using CEK integration
    let mut context = orchestration::CompilationContext::new();
    let eval_result = evaluate_with_cek(&compile_result.top_level, &mut context)?;
    
    Ok((compile_result, eval_result))
}

/// Compile and evaluate Pact source code with persistent module storage
/// 
/// This function provides full module storage and loading capabilities,
/// matching the Haskell implementation exactly.
pub fn compile_and_evaluate_with_storage(
    source: &str, 
    storage: ModuleStorageManager
) -> Result<(CompileResult, pact_values::PactValue), pact_errors::PactError<pact_parser::SpanInfo>> {
    // First, compile the source
    let compile_result = compile_pact_source(source)?;
    
    // Then, evaluate using CEK integration with storage
    let mut context = orchestration::CompilationContext::new();
    let eval_result = evaluate_with_cek_and_storage(&compile_result.top_level, &mut context, Some(storage))?;
    
    Ok((compile_result, eval_result))
}

/// Create a module storage manager with a mock database (for testing)
pub fn create_mock_storage() -> ModuleStorageManager {
    use std::sync::Arc;
    let db = Arc::new(pact_db::MockDb::new());
    ModuleStorageManager::new(db)
}
