//! Integration test for step syntax validation based on the original issue

use pact_parser::*;

#[test]
fn test_step_string_expression_issue() {
    // This is the exact syntax that was mentioned in the issue description:
    // (step "expression") should parse correctly
    let code = r#"
        (module test-module 'test-keyset
          (defpact example-pact ()
            (step "First step expression")
            (step "Second step expression")
            (step (+ 1 2))
          )
        )
    "#;

    let result = parse(code);
    
    // This should parse successfully
    assert!(result.is_ok(), "Failed to parse step with string expressions: {:?}", result.err());
    
    let program = result.unwrap();
    match &program[0] {
        ParsedTopLevel::TLModule(module) => {
            match &module.definitions[0] {
                ParsedDef::DPact(defpact) => {
                    assert_eq!(defpact.steps.len(), 3);
                    
                    // First step: (step "First step expression")
                    match &defpact.steps[0] {
                        PactStep::Step { entity, expr, .. } => {
                            assert!(entity.is_none(), "First step should not have entity");
                            match expr {
                                ParsedExpr::Constant(Literal::LString(s), _) => {
                                    assert_eq!(s, "First step expression");
                                }
                                _ => panic!("Expected string literal in first step"),
                            }
                        }
                        _ => panic!("Expected Step variant for first step"),
                    }
                    
                    // Second step: (step "Second step expression")  
                    match &defpact.steps[1] {
                        PactStep::Step { entity, expr, .. } => {
                            assert!(entity.is_none(), "Second step should not have entity");
                            match expr {
                                ParsedExpr::Constant(Literal::LString(s), _) => {
                                    assert_eq!(s, "Second step expression");
                                }
                                _ => panic!("Expected string literal in second step"),
                            }
                        }
                        _ => panic!("Expected Step variant for second step"),
                    }
                    
                    // Third step: (step (+ 1 2))
                    match &defpact.steps[2] {
                        PactStep::Step { entity, expr, .. } => {
                            assert!(entity.is_none(), "Third step should not have entity");
                            match expr {
                                ParsedExpr::App { .. } => {
                                    // This is expected - an application expression
                                }
                                _ => panic!("Expected app expression in third step"),
                            }
                        }
                        _ => panic!("Expected Step variant for third step"),
                    }
                }
                _ => panic!("Expected defpact definition"),
            }
        }
        _ => panic!("Expected module"),
    }
}

#[test] 
fn test_step_with_entity_syntax() {
    // Test the two-argument form: (step entity expr)
    let code = r#"
        (module test-entity 'test-keyset
          (defpact entity-pact ()
            (step "admin" "Admin step")
            (step "user" (+ 5 10))
          )
        )
    "#;

    let result = parse(code);
    assert!(result.is_ok(), "Failed to parse step with entity: {:?}", result.err());
    
    let program = result.unwrap();
    match &program[0] {
        ParsedTopLevel::TLModule(module) => {
            match &module.definitions[0] {
                ParsedDef::DPact(defpact) => {
                    assert_eq!(defpact.steps.len(), 2);
                    
                    // First step: (step "admin" "Admin step")
                    match &defpact.steps[0] {
                        PactStep::Step { entity, expr, .. } => {
                            // Should have entity
                            assert!(entity.is_some(), "First step should have entity");
                            match entity.as_ref().unwrap() {
                                ParsedExpr::Constant(Literal::LString(s), _) => {
                                    assert_eq!(s, "admin");
                                }
                                _ => panic!("Expected string literal for entity"),
                            }
                            
                            match expr {
                                ParsedExpr::Constant(Literal::LString(s), _) => {
                                    assert_eq!(s, "Admin step");
                                }
                                _ => panic!("Expected string literal for step expression"),
                            }
                        }
                        _ => panic!("Expected Step variant"),
                    }
                    
                    // Second step: (step "user" (+ 5 10))
                    match &defpact.steps[1] {
                        PactStep::Step { entity, expr, .. } => {
                            assert!(entity.is_some(), "Second step should have entity");
                            match entity.as_ref().unwrap() {
                                ParsedExpr::Constant(Literal::LString(s), _) => {
                                    assert_eq!(s, "user");
                                }
                                _ => panic!("Expected string literal for entity"),
                            }
                            
                            match expr {
                                ParsedExpr::App { .. } => {
                                    // This is expected - an application expression
                                }
                                _ => panic!("Expected app expression"),
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
fn test_step_with_rollback_syntax() {
    // Test step-with-rollback forms
    let code = r#"
        (module test-rollback 'test-keyset
          (defpact rollback-pact ()
            (step-with-rollback 
              (create-account "alice")
              (delete-account "alice"))
            (step-with-rollback "admin"
              (create-account "bob")  
              (delete-account "bob"))
          )
        )
    "#;

    let result = parse(code);
    assert!(result.is_ok(), "Failed to parse step-with-rollback: {:?}", result.err());
    
    let program = result.unwrap();
    match &program[0] {
        ParsedTopLevel::TLModule(module) => {
            match &module.definitions[0] {
                ParsedDef::DPact(defpact) => {
                    assert_eq!(defpact.steps.len(), 2);
                    
                    // First step: (step-with-rollback expr rollback) - no entity
                    match &defpact.steps[0] {
                        PactStep::StepWithRollback { entity, expr, rollback, .. } => {
                            assert!(entity.is_none(), "First step should not have entity");
                            
                            match expr {
                                ParsedExpr::App { .. } => {
                                    // Expected - create-account application
                                }
                                _ => panic!("Expected app expression for step"),
                            }
                            
                            match rollback {
                                ParsedExpr::App { .. } => {
                                    // Expected - delete-account application  
                                }
                                _ => panic!("Expected app expression for rollback"),
                            }
                        }
                        _ => panic!("Expected StepWithRollback variant"),
                    }
                    
                    // Second step: (step-with-rollback entity expr rollback)
                    match &defpact.steps[1] {
                        PactStep::StepWithRollback { entity, expr, rollback, .. } => {
                            assert!(entity.is_some(), "Second step should have entity");
                            
                            match entity.as_ref().unwrap() {
                                ParsedExpr::Constant(Literal::LString(s), _) => {
                                    assert_eq!(s, "admin");
                                }
                                _ => panic!("Expected string literal for entity"),
                            }
                            
                            match expr {
                                ParsedExpr::App { .. } => {
                                    // Expected - create-account application
                                }
                                _ => panic!("Expected app expression for step"),
                            }
                            
                            match rollback {
                                ParsedExpr::App { .. } => {
                                    // Expected - delete-account application
                                }
                                _ => panic!("Expected app expression for rollback"),
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