//! Test arithmetic expression compilation through the full pipeline

use pact_compiler::{compile_pact_source, compile_desugar_only, CompilationContext};
use pact_ir::{TopLevel, Term, CoreBuiltin, Literal};

#[test]
fn test_compile_simple_addition() {
    let source = "(+ 1 2)";
    
    let result = compile_pact_source(source);
    
    match result {
        Ok(compile_result) => {
            println!("✓ Compilation succeeded");
            println!("Stats: {:?}", compile_result.stats);
            
            // Check the resulting IR
            match &compile_result.top_level {
                TopLevel::TLTerm(term) => {
                    println!("Top-level term: {:?}", term);
                    
                    // We expect an App with builtin + and two constant arguments
                    match term {
                        Term::App { func, args, .. } => {
                            println!("✓ Correctly compiled to App");
                            
                            // Check function is CoreAdd builtin
                            match &**func {
                                Term::Builtin(builtin, _) => {
                                    assert_eq!(*builtin, CoreBuiltin::CoreAdd);
                                    println!("✓ Function is CoreAdd builtin");
                                }
                                _ => panic!("Expected Builtin, got {:?}", func),
                            }
                            
                            // Check arguments
                            assert_eq!(args.len(), 2);
                            
                            match &args[0] {
                                Term::Constant(Literal::LInteger(n), _) => {
                                    assert_eq!(*n, 1);
                                    println!("✓ First arg: {}", n);
                                }
                                _ => panic!("Expected integer constant for first arg"),
                            }
                            
                            match &args[1] {
                                Term::Constant(Literal::LInteger(n), _) => {
                                    assert_eq!(*n, 2);
                                    println!("✓ Second arg: {}", n);
                                }
                                _ => panic!("Expected integer constant for second arg"),
                            }
                        }
                        Term::ListLit { .. } => {
                            panic!("ERROR: Compiled to ListLit instead of App!");
                        }
                        _ => panic!("Unexpected term type: {:?}", term),
                    }
                }
                _ => panic!("Expected TLTerm, got {:?}", compile_result.top_level),
            }
        }
        Err(e) => panic!("Compilation failed: {:?}", e),
    }
}

#[test]
fn test_desugar_only() {
    let source = "(+ 1 2)";
    
    let mut ctx = CompilationContext::new();
    let result = compile_desugar_only(source, &mut ctx);
    
    match result {
        Ok(top_level) => {
            println!("✓ Desugaring succeeded");
            
            match &top_level {
                TopLevel::TLTerm(term) => {
                    println!("Desugared term structure:");
                    print_term_structure(term, 0);
                }
                _ => println!("Top-level: {:?}", top_level),
            }
        }
        Err(e) => panic!("Desugaring failed: {:?}", e),
    }
}

fn print_term_structure<N, T, B, I>(term: &Term<N, T, B, I>, indent: usize)
where
    N: std::fmt::Debug,
    T: std::fmt::Debug,
    B: std::fmt::Debug,
    I: std::fmt::Debug,
{
    let prefix = "  ".repeat(indent);
    match term {
        Term::Var(name, _) => println!("{}Var({:?})", prefix, name),
        Term::Lam { args, body, .. } => {
            println!("{}Lam", prefix);
            println!("{}  args: {:?}", prefix, args);
            print_term_structure(body, indent + 1);
        }
        Term::Let { arg, expr, body, .. } => {
            println!("{}Let", prefix);
            println!("{}  {} : {:?} =", prefix, arg.name, arg.ty);
            print_term_structure(expr, indent + 2);
            println!("{}  in", prefix);
            print_term_structure(body, indent + 1);
        }
        Term::App { func, args, .. } => {
            println!("{}App", prefix);
            println!("{}  func:", prefix);
            print_term_structure(func, indent + 2);
            println!("{}  args:", prefix);
            for arg in args {
                print_term_structure(arg, indent + 2);
            }
        }
        Term::Sequence { first, second, .. } => {
            println!("{}Sequence", prefix);
            print_term_structure(first, indent + 1);
            print_term_structure(second, indent + 1);
        }
        Term::Builtin(builtin, _) => println!("{}Builtin({:?})", prefix, builtin),
        Term::Constant(lit, _) => println!("{}Constant({:?})", prefix, lit),
        Term::ListLit { elements, .. } => {
            println!("{}ListLit [{} elements]", prefix, elements.len());
            for elem in elements {
                print_term_structure(elem, indent + 1);
            }
        }
        Term::ObjectLit { fields, .. } => {
            println!("{}ObjectLit", prefix);
            for (field, value) in fields {
                println!("{}  {:?}: ", prefix, field);
                print_term_structure(value, indent + 2);
            }
        }
        Term::BuiltinForm { form, .. } => println!("{}BuiltinForm({:?})", prefix, form),
        Term::InlineValue { value, .. } => println!("{}InlineValue({:?})", prefix, value),
    }
}