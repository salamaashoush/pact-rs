//! Specific tests for step parsing in defpact

use pact_parser::*;

#[test]
fn test_step_with_string_expression() {
    let code = r#"
        (module test-steps 'test-keyset
          (defpact simple-steps ()
            (step "First step")
            (step "Second step")
          )
        )
    "#;

    let result = parse(code);
    
    // This should parse successfully
    match &result {
        Err(e) => {
            println!("Parse error: {:?}", e);
            panic!("Should have parsed successfully");
        }
        Ok(_) => println!("Parsed successfully"),
    }
    
    let program = result.unwrap();
    match &program[0] {
        ParsedTopLevel::TLModule(module) => {
            match &module.definitions[0] {
                ParsedDef::DPact(defpact) => {
                    assert_eq!(defpact.name.name, "simple-steps");
                    assert_eq!(defpact.steps.len(), 2);
                    
                    // Both steps should be simple Step variants with string expressions
                    match &defpact.steps[0] {
                        PactStep::Step { entity, expr, .. } => {
                            // Should have no entity
                            assert!(entity.is_none());
                            
                            // Expression should be a string constant
                            match expr {
                                ParsedExpr::Constant(Literal::LString(s), _) => {
                                    assert_eq!(s, "First step");
                                }
                                _ => panic!("Expected string constant in step, got: {:?}", expr),
                            }
                        }
                        _ => panic!("Expected Step variant"),
                    }
                }
                _ => panic!("Expected defpact"),
            }
        }
        _ => panic!("Expected module"),
    }
}

#[test] 
fn test_step_with_entity_and_expression() {
    let code = r#"
        (module test-entity-steps 'test-keyset
          (defpact entity-steps ()
            (step "admin" "First step with entity")
            (step (let ((x 1)) (+ x 2)))
          )
        )
    "#;

    let result = parse(code);
    
    match &result {
        Err(e) => {
            println!("Parse error: {:?}", e);
            panic!("Should have parsed successfully");
        }
        Ok(_) => println!("Parsed successfully"),
    }
    
    let program = result.unwrap();
    match &program[0] {
        ParsedTopLevel::TLModule(module) => {
            match &module.definitions[0] {
                ParsedDef::DPact(defpact) => {
                    assert_eq!(defpact.steps.len(), 2);
                    
                    // First step should have entity
                    match &defpact.steps[0] {
                        PactStep::Step { entity, expr, .. } => {
                            // Should have entity
                            assert!(entity.is_some());
                            
                            match entity.as_ref().unwrap() {
                                ParsedExpr::Constant(Literal::LString(s), _) => {
                                    assert_eq!(s, "admin");
                                }
                                _ => panic!("Expected string constant for entity"),
                            }
                            
                            match expr {
                                ParsedExpr::Constant(Literal::LString(s), _) => {
                                    assert_eq!(s, "First step with entity");
                                }
                                _ => panic!("Expected string constant in step expression"),
                            }
                        }
                        _ => panic!("Expected Step variant"),
                    }
                    
                    // Second step should have no entity but complex expression
                    match &defpact.steps[1] {
                        PactStep::Step { entity, expr, .. } => {
                            assert!(entity.is_none());
                            
                            match expr {
                                ParsedExpr::Let { .. } => {
                                    // This is expected - a let expression
                                }
                                _ => panic!("Expected let expression in second step"),
                            }
                        }
                        _ => panic!("Expected Step variant"),
                    }
                }
                _ => panic!("Expected defpact"),
            }
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_step_with_rollback() {
    let code = r#"
        (module test-rollback 'test-keyset
          (defpact rollback-test ()
            (step-with-rollback 
              (create-account "alice" 100.0)
              (delete-account "alice"))
          )
        )
    "#;

    let result = parse(code);
    
    match &result {
        Err(e) => {
            println!("Parse error: {:?}", e);
            panic!("Should have parsed successfully");
        }
        Ok(_) => println!("Parsed successfully"),
    }
    
    let program = result.unwrap();
    match &program[0] {
        ParsedTopLevel::TLModule(module) => {
            match &module.definitions[0] {
                ParsedDef::DPact(defpact) => {
                    assert_eq!(defpact.steps.len(), 1);
                    
                    match &defpact.steps[0] {
                        PactStep::StepWithRollback { entity, expr, rollback, .. } => {
                            assert!(entity.is_none());
                            
                            // Check the step expression
                            match expr {
                                ParsedExpr::App { args, .. } => {
                                    // Should be function application
                                    assert_eq!(args.len(), 2);
                                }
                                _ => panic!("Expected app expression in step"),
                            }
                            
                            // Check the rollback expression
                            match rollback {
                                ParsedExpr::App { .. } => {
                                    // Should be function application for rollback
                                }
                                _ => panic!("Expected app expression in rollback"),
                            }
                        }
                        _ => panic!("Expected StepWithRollback variant"),
                    }
                }
                _ => panic!("Expected defpact"),
            }
        }
        _ => panic!("Expected module"),
    }
}