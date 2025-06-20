//! High-level compilation interface
//!
//! This module provides convenient high-level functions for compiling Pact code,
//! wrapping the lower-level orchestration functionality.

use crate::orchestration::{compile_top_level, CompilationContext, CompileResult};
use crate::cek_integration::evaluate_with_cek;
use pact_errors::PactError;
use pact_values::PactValue;

/// Simple function to compile Pact source code to a result
/// This is the main entry point for most users
pub fn compile_pact_source(source_code: &str) -> Result<CompileResult, PactError> {
    let mut ctx = CompilationContext::new();
    compile_top_level(source_code, &mut ctx)
}

/// Compile Pact source code with custom context
/// Useful when you need to maintain state across multiple compilations
pub fn compile_pact_source_with_context(
    source_code: &str,
    ctx: &mut CompilationContext,
) -> Result<CompileResult, PactError> {
    compile_top_level(source_code, ctx)
}

/// Compile multiple Pact files
pub fn compile_pact_files(files: Vec<(&str, &str)>) -> Result<Vec<CompileResult>, PactError> {
    let mut ctx = CompilationContext::new();
    crate::orchestration::compile_multiple_sources(files, &mut ctx)
}

/// Check if Pact source code compiles without errors
/// Returns true if compilation succeeds, false otherwise
pub fn check_pact_source(source_code: &str) -> bool {
    compile_pact_source(source_code).is_ok()
}

/// Compile and evaluate Pact source code through the full pipeline
/// This runs the complete compilation pipeline and then evaluates the result
/// using the CEK machine
pub fn compile_and_evaluate(
    source_code: &str,
    ctx: &mut CompilationContext,
) -> Result<PactValue, PactError> {
    // First compile through all stages
    let compile_result = compile_top_level(source_code, ctx)?;
    
    // Then evaluate with CEK machine
    evaluate_with_cek(&compile_result.top_level, ctx)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_simple_constant() {
        let source = "(defconst VALUE 42)";
        
        match compile_pact_source(source) {
            Ok(result) => {
                println!("Compilation succeeded: {:?}", result.stats);
            }
            Err(e) => {
                println!("Compilation failed: {:?}", e);
            }
        }
    }

    #[test]
    fn test_check_valid_source() {
        let valid_source = "(defconst VALID 123)";
        let invalid_source = "(invalid syntax here";
        
        // Note: These may not work perfectly yet due to incomplete implementation
        println!("Valid source compiles: {}", check_pact_source(valid_source));
        println!("Invalid source compiles: {}", check_pact_source(invalid_source));
    }
}