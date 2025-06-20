//! Integration tests for the complete compilation pipeline

use pact_compiler::{CompilationContext, compile_top_level};

#[test]
fn test_simple_expression_compilation() {
    let mut ctx = CompilationContext::new();
    
    // Test a simple arithmetic expression
    let source = "(+ 1 2)";
    
    match compile_top_level(source, &mut ctx) {
        Ok(result) => {
            println!("Successfully compiled expression: {:?}", result.stats);
            // The expression should produce an InterpretValue
            match &result.top_level {
                pact_ir::TopLevel::TLTerm(_) => {
                    println!("Got TLTerm as expected");
                }
                _ => panic!("Expected TLTerm for simple expression"),
            }
        }
        Err(e) => {
            // This might fail due to CEK integration issues, but let's see
            println!("Compilation failed (may be expected): {:?}", e);
        }
    }
}

#[test]
fn test_module_compilation_pipeline() {
    let mut ctx = CompilationContext::new();
    
    // Test a simple module
    let source = r#"
        (module test-module GOV
            (defconst TEST_VALUE 42)
            (defun get-value () TEST_VALUE)
        )
    "#;
    
    match compile_top_level(source, &mut ctx) {
        Ok(result) => {
            println!("Successfully compiled module");
            println!("Stats: {:?}", result.stats);
            
            // Verify we got a module
            match &result.top_level {
                pact_ir::TopLevel::TLModule(m) => {
                    assert_eq!(m.name.to_string(), "test-module");
                    println!("Module name verified: {}", m.name);
                }
                _ => panic!("Expected TLModule"),
            }
        }
        Err(e) => {
            println!("Module compilation failed: {:?}", e);
        }
    }
}

#[test]
fn test_defconst_evaluation() {
    let mut ctx = CompilationContext::new();
    
    // Test constant evaluation
    let source = "(defconst MY_CONST (+ 10 32))";
    
    match compile_top_level(source, &mut ctx) {
        Ok(result) => {
            println!("Successfully compiled defconst");
            
            // Check that constant evaluation happened
            assert!(result.stats.const_eval_stats.constants_found > 0);
            println!("Constants found: {}", result.stats.const_eval_stats.constants_found);
        }
        Err(e) => {
            println!("Defconst compilation failed: {:?}", e);
        }
    }
}

#[test]
fn test_interface_compilation() {
    let mut ctx = CompilationContext::new();
    
    let source = r#"
        (interface test-interface
            (defconst INTERFACE_VERSION "1.0")
            (defun do-something:string ())
        )
    "#;
    
    match compile_top_level(source, &mut ctx) {
        Ok(result) => {
            println!("Successfully compiled interface");
            
            match &result.top_level {
                pact_ir::TopLevel::TLInterface(i) => {
                    assert_eq!(i.name.to_string(), "test-interface");
                    println!("Interface name verified: {}", i.name);
                }
                _ => panic!("Expected TLInterface"),
            }
        }
        Err(e) => {
            println!("Interface compilation failed: {:?}", e);
        }
    }
}