//! Complete pipeline integration test
//!
//! This module provides an end-to-end test of the lexer → parser → desugar → rename pipeline

use crate::*;
use pact_parser::Parser;
use pact_errors::{PactError, EvalError};
use pact_shared_types::SpanInfo;

/// Complete pipeline transformation from source to renamed Core IR
pub fn pipeline_transform(source: &str) -> Result<Vec<CoreTopLevel>, PactError<SpanInfo>> {
    // Step 1 & 2: Lexing and Parsing combined
    let mut parser = Parser::new(source).map_err(|_| PactError::PEExecutionError(
        EvalError::RuntimeError("Parser creation failed".to_string()),
        vec![],
        SpanInfo { start: 0, end: 0 }
    )))?;
    let program = parser.parse_program().map_err(|_| PactError::PEExecutionError(
        EvalError::RuntimeError("Parsing failed".to_string()),
        vec![],
        SpanInfo { start: 0, end: 0 }
    ))?;
    
    // Step 3 & 4: Process each top-level item
    let mut results = Vec::new();
    for parsed_toplevel in program {
        // Desugaring
        let mut desugar_ctx = DesugarContext::new();
        let desugared = desugar_top_level(parsed_toplevel, &mut desugar_ctx)?;
        
        // Name resolution and renaming
        let mut renamer_env = RenamerEnv::new();
        let renamed = rename_top_level(desugared.result, &mut renamer_env)?;
        
        results.push(renamed);
    }
    
    Ok(results)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_complete_pipeline_simple_function() {
        let source = r#"
            (defun add (x:integer y:integer)
                (+ x y))
        "#;
        
        let result = pipeline_transform(source);
        assert!(result.is_ok(), "Pipeline should succeed for simple function: {:?}", result);
    }

    #[test]
    fn test_complete_pipeline_lambda() {
        let source = r#"
            (lambda (x y) (+ x y))
        "#;
        
        let result = pipeline_transform(source);
        assert!(result.is_ok(), "Pipeline should succeed for lambda: {:?}", result);
    }

    #[test]
    fn test_complete_pipeline_nested_lambda() {
        let source = r#"
            (lambda (x) (lambda (y) (+ x y)))
        "#;
        
        let result = pipeline_transform(source);
        assert!(result.is_ok(), "Pipeline should succeed for nested lambda: {:?}", result);
    }

    #[test]
    fn test_complete_pipeline_let_binding() {
        let source = r#"
            (let ((x 5)) (+ x 10))
        "#;
        
        let result = pipeline_transform(source);
        assert!(result.is_ok(), "Pipeline should succeed for let binding: {:?}", result);
    }

    #[test]
    fn test_complete_pipeline_if_expression() {
        let source = r#"
            (if (> x 0) "positive" "non-positive")
        "#;
        
        let result = pipeline_transform(source);
        assert!(result.is_ok(), "Pipeline should succeed for if expression: {:?}", result);
    }
}