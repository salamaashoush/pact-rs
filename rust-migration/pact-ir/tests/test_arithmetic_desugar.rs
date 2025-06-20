//! Test arithmetic expression desugaring

use pact_parser::{Parser, ParsedTopLevel};
use pact_ir::{desugar_top_level, DesugarContext, TopLevel, Term, CoreBuiltin, Literal};

#[test]
fn test_desugar_simple_addition() {
    let source = "(+ 1 2)";
    
    // Parse
    let mut parser = Parser::new(source).expect("Failed to create parser");
    let program = parser.parse_program().expect("Failed to parse");
    
    let parsed_top_level = program.into_iter().next().unwrap();
    
    // Desugar
    let mut ctx = DesugarContext::new();
    let desugared = desugar_top_level(parsed_top_level, &mut ctx)
        .expect("Failed to desugar");
    
    println!("✓ Desugaring succeeded");
    println!("Dependencies: {:?}", desugared.dependencies);
    println!("Stats: {:?}", ctx.get_stats());
    
    // Check the result
    match &desugared.result {
        TopLevel::TLTerm(term) => {
            println!("\nDesugared term structure:");
            match term {
                Term::App { func, args, .. } => {
                    println!("✓ Correctly desugared to App");
                    
                    // Check function is CoreAdd builtin
                    match &**func {
                        Term::Builtin(builtin, _) => {
                            assert_eq!(*builtin, CoreBuiltin::CoreAdd);
                            println!("✓ Function is CoreAdd builtin");
                        }
                        _ => panic!("Expected Builtin CoreAdd, got {:?}", func),
                    }
                    
                    // Check arguments
                    assert_eq!(args.len(), 2);
                    
                    match &args[0] {
                        Term::Constant(Literal::LInteger(n), _) => {
                            assert_eq!(*n, 1);
                            println!("✓ First arg: {}", n);
                        }
                        _ => panic!("Expected integer constant 1, got {:?}", args[0]),
                    }
                    
                    match &args[1] {
                        Term::Constant(Literal::LInteger(n), _) => {
                            assert_eq!(*n, 2);
                            println!("✓ Second arg: {}", n);
                        }
                        _ => panic!("Expected integer constant 2, got {:?}", args[1]),
                    }
                }
                Term::ListLit { .. } => {
                    panic!("ERROR: Desugared to ListLit instead of App!");
                }
                _ => panic!("Unexpected term type: {:?}", term),
            }
        }
        _ => panic!("Expected TLTerm"),
    }
}

#[test]
fn test_desugar_list_literal() {
    let source = "[1 2 3]";
    
    // Parse
    let mut parser = Parser::new(source).expect("Failed to create parser");
    let program = parser.parse_program().expect("Failed to parse");
    
    let parsed_top_level = program.into_iter().next().unwrap();
    
    // Desugar
    let mut ctx = DesugarContext::new();
    let desugared = desugar_top_level(parsed_top_level, &mut ctx)
        .expect("Failed to desugar");
    
    println!("✓ List desugaring succeeded");
    
    match &desugared.result {
        TopLevel::TLTerm(term) => {
            match term {
                Term::ListLit { elements, .. } => {
                    assert_eq!(elements.len(), 3);
                    println!("✓ Correctly desugared to ListLit with {} elements", elements.len());
                }
                _ => panic!("Expected ListLit, got {:?}", term),
            }
        }
        _ => panic!("Expected TLTerm"),
    }
}