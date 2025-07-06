use pact_parser::*;

#[test]
fn test_simple_arithmetic_parsing() {
    let source = "(+ 1 2)";
    
    let result = parse(source);
    
    match result {
        Ok(program) => {
            assert_eq!(program.len(), 1, "Should have one top-level item");
            
            match &program[0] {
                ParsedTopLevel::TLTerm(expr) => {
                    match expr {
                        ParsedExpr::App { func, args, .. } => {
                            println!("✓ Correctly parsed as function application");
                            
                            // Check function is "+"
                            match &**func {
                                ParsedExpr::Var(name, _) => {
                                    match name {
                                        ParsedName::BN(bare) => {
                                            assert_eq!(bare.0.as_str(), "+", "Function should be +");
                                            println!("✓ Function is: {}", bare.0);
                                        }
                                        _ => panic!("Expected bare name for +"),
                                    }
                                }
                                _ => panic!("Expected Var for function"),
                            }
                            
                            // Check arguments
                            assert_eq!(args.len(), 2, "Should have 2 arguments");
                            
                            // Check first arg is 1
                            match &args[0] {
                                ParsedExpr::Constant(Literal::LInteger(n), _) => {
                                    assert_eq!(*n, 1, "First arg should be 1");
                                    println!("✓ First arg: {}", n);
                                }
                                _ => panic!("Expected integer constant for first arg"),
                            }
                            
                            // Check second arg is 2
                            match &args[1] {
                                ParsedExpr::Constant(Literal::LInteger(n), _) => {
                                    assert_eq!(*n, 2, "Second arg should be 2");
                                    println!("✓ Second arg: {}", n);
                                }
                                _ => panic!("Expected integer constant for second arg"),
                            }
                        }
                        ParsedExpr::List(_, _) => {
                            panic!("ERROR: (+ 1 2) was parsed as a List instead of App!");
                        }
                        _ => panic!("Unexpected expression type: {:?}", expr),
                    }
                }
                _ => panic!("Expected TLTerm"),
            }
        }
        Err(e) => panic!("Parse failed: {:?}", e),
    }
}

#[test]
fn test_list_vs_app_distinction() {
    // Test list literal
    let list_source = "[1 2 3]";
    let list_result = parse(list_source);
    
    match list_result {
        Ok(program) => {
            match &program[0] {
                ParsedTopLevel::TLTerm(ParsedExpr::List(elements, _)) => {
                    assert_eq!(elements.len(), 3);
                    println!("✓ [1 2 3] correctly parsed as List");
                }
                _ => panic!("[1 2 3] should parse as List"),
            }
        }
        Err(e) => panic!("List parse failed: {:?}", e),
    }
    
    // Test function application
    let app_source = "(+ 1 2)";
    let app_result = parse(app_source);
    
    match app_result {
        Ok(program) => {
            match &program[0] {
                ParsedTopLevel::TLTerm(ParsedExpr::App { .. }) => {
                    println!("✓ (+ 1 2) correctly parsed as App");
                }
                _ => panic!("(+ 1 2) should parse as App"),
            }
        }
        Err(e) => panic!("App parse failed: {:?}", e),
    }
}