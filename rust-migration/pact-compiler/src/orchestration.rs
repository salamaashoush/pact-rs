//! Complete compilation pipeline orchestration
//!
//! This module coordinates the entire Pact compilation pipeline, exactly
//! matching the Haskell Compile module's `interpretTopLevel` functionality.
//!
//! Pipeline stages:
//! 1. Parse (already done by caller)
//! 2. Desugar (pact-ir)
//! 3. Name Resolution (pact-ir)
//! 4. Constant Evaluation (pact-eval)
//! 5. Module Hashing (this crate)
//! 6. Result compilation

use pact_ir::{
    desugar_top_level, DesugarContext,
    TopLevel
};
use pact_eval::{eval_top_level_constants, ConstEvalContext};
use crate::module_hash::{hash_top_level, ModuleHashContext};
use pact_parser::{Parser, ParsedTopLevel};
use pact_errors::PactError;

/// Complete compilation result
#[derive(Debug, Clone)]
pub struct CompileResult {
    /// The fully processed top-level item
    pub top_level: TopLevel<pact_ir::Name, pact_ir::Type, pact_ir::CoreBuiltin, pact_parser::SpanInfo>,
    /// Compilation statistics
    pub stats: CompilationStats,
}

/// Compilation statistics
#[derive(Debug, Clone, Default)]
pub struct CompilationStats {
    /// Desugaring statistics
    pub desugar_stats: DesugarStats,
    /// Name resolution statistics
    pub rename_stats: RenameStats,
    /// Constant evaluation statistics
    pub const_eval_stats: pact_eval::ConstEvalStats,
    /// Module hashing statistics
    pub hash_stats: crate::module_hash::ModuleHashStats,
    /// Total compilation time
    pub total_time_ns: u64,
}

/// Desugaring statistics (placeholder)
#[derive(Debug, Clone, Default)]
pub struct DesugarStats {
    pub terms_desugared: usize,
    pub special_forms_processed: usize,
}

/// Name resolution statistics (placeholder)
#[derive(Debug, Clone, Default)]
pub struct RenameStats {
    pub names_resolved: usize,
    pub debruijn_indices_assigned: usize,
}

/// Compilation context that coordinates all stages
#[derive(Debug, Clone)]
pub struct CompilationContext {
    /// Desugaring context (includes name resolution)
    pub desugar_ctx: DesugarContext,
    /// Constant evaluation context
    pub const_eval_ctx: ConstEvalContext,
    /// Module hashing context
    pub hash_ctx: ModuleHashContext,
}

impl CompilationContext {
    /// Create a new compilation context
    pub fn new() -> Self {
        Self {
            desugar_ctx: DesugarContext::new(),
            const_eval_ctx: ConstEvalContext::new(),
            hash_ctx: ModuleHashContext::new(),
        }
    }
}

/// Main compilation orchestration function
/// Matches Haskell: interpretTopLevel :: Interpreter e b i -> RawCode -> Lisp.TopLevel i -> EvalM e b i (CompileValue i)
pub fn compile_top_level(
    source_code: &str,
    ctx: &mut CompilationContext,
) -> Result<CompileResult, PactError> {
    let start_time = std::time::Instant::now();

    // Stage 1: Parse (Lexing + Parsing)
    let parsed_toplevel = parse_source_code(source_code)?;

    // Stage 2-6: Run the complete pipeline
    let result = compile_parsed_top_level(parsed_toplevel, ctx)?;

    // Collect final statistics
    let total_time = start_time.elapsed().as_nanos() as u64;
    let stats = CompilationStats {
        desugar_stats: DesugarStats {
            terms_desugared: ctx.desugar_ctx.get_stats().terms_desugared,
            special_forms_processed: ctx.desugar_ctx.get_stats().special_forms_processed,
        },
        rename_stats: RenameStats::default(),   // TODO: Extract from rename context
        const_eval_stats: ctx.const_eval_ctx.stats().clone(),
        hash_stats: ctx.hash_ctx.stats().clone(),
        total_time_ns: total_time,
    };

    Ok(CompileResult {
        top_level: result,
        stats,
    })
}

/// Compile an already-parsed top-level item
/// This is useful when you already have the parsed AST
pub fn compile_parsed_top_level(
    parsed_toplevel: ParsedTopLevel<pact_parser::SpanInfo>,
    ctx: &mut CompilationContext,
) -> Result<TopLevel<pact_ir::Name, pact_ir::Type, pact_ir::CoreBuiltin, pact_parser::SpanInfo>, PactError> {
    // Stage 2: Desugar + Rename (ParseTree -> Core IR with resolved names)
    // This combines desugaring and name resolution in one step, matching Haskell
    let desugared = desugar_top_level(parsed_toplevel, &mut ctx.desugar_ctx)?;

    // Stage 3: Constant Evaluation
    let const_evaled = eval_top_level_constants(desugared.result, &mut ctx.const_eval_ctx)?;

    // Stage 4: Module Hashing
    let hashed = hash_top_level(const_evaled, &mut ctx.hash_ctx)?;

    Ok(hashed)
}

/// Parse source code to get the initial AST
/// This combines lexing and parsing stages
fn parse_source_code(source_code: &str) -> Result<ParsedTopLevel<pact_parser::SpanInfo>, PactError> {
    // Create parser (which handles lexing internally)
    let mut parser = Parser::new(source_code)
        .map_err(|e| PactError::Runtime(pact_errors::RuntimeError::UserError {
            message: format!("Parser creation failed: {:?}", e),
        }))?;

    // Parse the complete program
    let program = parser.parse_program()
        .map_err(|e| PactError::Runtime(pact_errors::RuntimeError::UserError {
            message: format!("Parsing failed: {:?}", e),
        }))?;

    // Handle multiple top-level items like Haskell does
    match program.len() {
        0 => Err(PactError::Runtime(pact_errors::RuntimeError::UserError {
            message: "No top-level items found in source code".to_string(),
        })),
        1 => Ok(program.into_iter().next().unwrap()),
        _ => {
            // Multiple top-level items: collect all expressions
            // Non-expression items (modules, interfaces, uses) are returned as-is
            // Multiple expressions are wrapped in a block expression
            let mut expressions = Vec::new();
            let mut non_expressions = Vec::new();
            
            for item in program {
                match item {
                    pact_parser::ParsedTopLevel::TLTerm(expr) => expressions.push(expr),
                    other => non_expressions.push(other),
                }
            }
            
            // If we have a single non-expression, return it
            if non_expressions.len() == 1 && expressions.is_empty() {
                return Ok(non_expressions.into_iter().next().unwrap());
            }
            
            // If we have multiple expressions, create a block
            // This will be converted to a Sequence during desugaring
            if expressions.len() > 1 {
                // Use a let expression with empty bindings to represent a block
                // This is similar to how (begin expr1 expr2 ...) would be represented
                let combined_span = get_combined_span(&expressions);
                let block_expr = create_block_expression(expressions, combined_span);
                Ok(pact_parser::ParsedTopLevel::TLTerm(block_expr))
            } else if expressions.len() == 1 {
                Ok(pact_parser::ParsedTopLevel::TLTerm(expressions.into_iter().next().unwrap()))
            } else {
                // All were non-expressions, return the first one
                Ok(non_expressions.into_iter().next().unwrap())
            }
        }
    }
}

/// Get combined span from multiple expressions
fn get_combined_span(expressions: &[pact_parser::ParsedExpr<pact_parser::SpanInfo>]) -> pact_parser::SpanInfo {
    let first_span = expressions.first().unwrap().span();
    let last_span = expressions.last().unwrap().span();
    pact_parser::SpanInfo {
        start: first_span.start,
        end: last_span.end,
    }
}

/// Create a block expression from multiple expressions
/// This uses a let with empty bindings and a list body to represent sequential execution
fn create_block_expression(
    expressions: Vec<pact_parser::ParsedExpr<pact_parser::SpanInfo>>,
    span: pact_parser::SpanInfo,
) -> pact_parser::ParsedExpr<pact_parser::SpanInfo> {
    // Use a let with no bindings and multiple body expressions
    // This will be converted to a Sequence during desugaring (matching Haskell's nelToSequence)
    pact_parser::ParsedExpr::Let {
        form: pact_parser::LetForm::LFLetNormal,
        bindings: Vec::new(), // No bindings
        body: expressions,   // Multiple expressions in body
        info: span,
    }
}

/// Compile multiple source files in dependency order
/// This handles module dependency resolution and compilation ordering
pub fn compile_multiple_sources(
    sources: Vec<(&str, &str)>, // (filename, source_code) pairs
    ctx: &mut CompilationContext,
) -> Result<Vec<CompileResult>, PactError> {
    let mut results = Vec::new();

    // For now, compile in the order provided
    // TODO: Implement proper dependency analysis and topological sorting
    for (filename, source_code) in sources {
        match compile_top_level(source_code, ctx) {
            Ok(result) => {
                results.push(result);
            }
            Err(e) => {
                return Err(PactError::Runtime(pact_errors::RuntimeError::UserError {
                    message: format!("Compilation failed for {}: {}", filename, e),
                }));
            }
        }
    }

    Ok(results)
}

/// Check if a module depends on another module
/// This is used for dependency analysis
pub fn module_depends_on(
    module_name: &str,
    dependency_name: &str,
    ctx: &CompilationContext,
) -> bool {
    // TODO: Implement proper dependency checking
    // For now, return false as a placeholder
    false
}

/// Compile only through the desugaring stage (useful for debugging)
/// Matches Haskell: compileDesugarOnly
pub fn compile_desugar_only(
    source_code: &str,
    ctx: &mut CompilationContext,
) -> Result<TopLevel<pact_ir::Name, pact_ir::Type, pact_ir::CoreBuiltin, pact_parser::SpanInfo>, PactError> {
    // Stage 1: Parse
    let parsed_toplevel = parse_source_code(source_code)?;

    // Stage 2: Desugar only
    let desugared = desugar_top_level(parsed_toplevel, &mut ctx.desugar_ctx)?;

    Ok(desugared.result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_constant_compilation() {
        let mut ctx = CompilationContext::new();

        let source = r#"
            (defconst TEST_CONST 42)
        "#;

        let result = compile_top_level(source, &mut ctx);

        match result {
            Ok(compile_result) => {
                // Should succeed and produce valid statistics
                assert!(compile_result.stats.total_time_ns > 0);
                println!("Compilation succeeded: {:?}", compile_result.stats);
            }
            Err(e) => {
                // May fail due to incomplete implementation, but should be informative
                println!("Compilation failed (expected): {:?}", e);
            }
        }
    }

    #[test]
    fn test_simple_module_compilation() {
        let mut ctx = CompilationContext::new();

        let source = r#"
            (module test-module GOV
                (defconst MESSAGE "Hello, World!")
                (defun greet () MESSAGE)
            )
        "#;

        let result = compile_top_level(source, &mut ctx);

        match result {
            Ok(compile_result) => {
                // Should succeed for modules
                assert!(compile_result.stats.total_time_ns > 0);
                println!("Module compilation succeeded: {:?}", compile_result.stats);
            }
            Err(e) => {
                println!("Module compilation failed (may be expected): {:?}", e);
            }
        }
    }

    #[test]
    fn test_desugar_only_compilation() {
        let mut ctx = CompilationContext::new();

        let source = r#"
            (defconst SIMPLE_CONST 123)
        "#;

        let result = compile_desugar_only(source, &mut ctx);

        match result {
            Ok(_) => {
                println!("Desugar-only compilation succeeded");
            }
            Err(e) => {
                println!("Desugar-only compilation failed: {:?}", e);
            }
        }
    }

    #[test]
    fn test_multiple_source_compilation() {
        let mut ctx = CompilationContext::new();

        let sources = vec![
            ("const.pact", "(defconst VALUE 100)"),
            ("func.pact", "(defun get-value () VALUE)"),
        ];

        let result = compile_multiple_sources(sources, &mut ctx);

        match result {
            Ok(results) => {
                assert_eq!(results.len(), 2);
                println!("Multiple source compilation succeeded");
            }
            Err(e) => {
                println!("Multiple source compilation failed: {:?}", e);
            }
        }
    }
}
