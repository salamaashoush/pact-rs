//! Parser tests

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_empty_program() {
        let result = parse("");
        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.items.len(), 0);
    }

    #[test]
    fn test_parse_simple_expression() {
        let result = parse("(+ 1 2)");
        if let Err(e) = &result {
            eprintln!("Parse error: {:?}", e);
        }
        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.items.len(), 1);

        match &program.items[0] {
            TopLevel::Expression(expr) => match expr {
                Expr::Add(left, right, _) => match (left.as_ref(), right.as_ref()) {
                    (Expr::Integer(1, _), Expr::Integer(2, _)) => {}
                    _ => panic!("Expected (+ 1 2)"),
                },
                _ => panic!("Expected Add expression"),
            },
            _ => panic!("Expected expression"),
        }
    }

    #[test]
    fn test_parse_nested_arithmetic() {
        let result = parse("(* (+ 1 2) (- 5 3))");
        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.items.len(), 1);
    }

    #[test]
    fn test_parse_simple_module() {
        let code = r#"
            (module test 'test-keyset
              (defun add (x:integer y:integer)
                (+ x y))
            )
        "#;

        let result = parse(code);
        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.items.len(), 1);

        match &program.items[0] {
            TopLevel::Module(module) => {
                assert_eq!(module.name, "test");
                assert_eq!(module.declarations.len(), 1);

                match &module.declarations[0] {
                    Declaration::Defun(defun) => {
                        assert_eq!(defun.name, "add");
                        assert_eq!(defun.params.len(), 2);
                        assert_eq!(defun.body.len(), 1);
                    }
                    _ => panic!("Expected defun"),
                }
            }
            _ => panic!("Expected module"),
        }
    }

    #[test]
    fn test_parse_let_expression() {
        let result = parse("(let ((x 5) (y 3)) (+ x y))");
        assert!(result.is_ok());
        let program = result.unwrap();

        match &program.items[0] {
            TopLevel::Expression(Expr::Let { bindings, body, .. }) => {
                assert_eq!(bindings.len(), 2);
                assert_eq!(bindings[0].name, "x");
                assert_eq!(bindings[1].name, "y");
                assert_eq!(body.len(), 1);
            }
            _ => panic!("Expected let expression"),
        }
    }

    #[test]
    fn test_parse_if_expression() {
        let result = parse("(if (> x 0) x (- x))");
        if let Err(e) = &result {
            eprintln!("Parse error for if expression: {:?}", e);
        }
        assert!(result.is_ok());
        let program = result.unwrap();

        match &program.items[0] {
            TopLevel::Expression(Expr::If {
                cond,
                then_expr: _,
                else_expr,
                ..
            }) => {
                // Check condition is a comparison
                match cond.as_ref() {
                    Expr::Gt(_, _, _) => {}
                    _ => panic!("Expected > comparison"),
                }

                // Check else branch exists
                assert!(else_expr.is_some());
            }
            _ => panic!("Expected if expression"),
        }
    }

    #[test]
    fn test_parse_list_literal() {
        let result = parse("[1 2 3]");
        if let Err(e) = &result {
            eprintln!("Parse error for list literal: {:?}", e);
        }
        assert!(result.is_ok());
        let program = result.unwrap();

        match &program.items[0] {
            TopLevel::Expression(Expr::List(elements, _)) => {
                assert_eq!(elements.len(), 3);
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn test_parse_object_literal() {
        let result = parse(r#"{ "name": "alice", "balance": 100 }"#);
        assert!(result.is_ok());
        let program = result.unwrap();

        match &program.items[0] {
            TopLevel::Expression(Expr::Object(fields, _)) => {
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0].0, "name");
                assert_eq!(fields[1].0, "balance");
            }
            _ => panic!("Expected object"),
        }
    }

    #[test]
    fn test_parse_with_capability() {
        let result = parse(
            "(with-capability (TRANSFER sender receiver amount) (transfer sender receiver amount))",
        );
        assert!(result.is_ok());
        let program = result.unwrap();

        match &program.items[0] {
            TopLevel::Expression(Expr::WithCapability { cap, body, .. }) => {
                // Check capability is an application
                match cap.as_ref() {
                    Expr::App { .. } => {}
                    _ => panic!("Expected capability application"),
                }
                assert_eq!(body.len(), 1);
            }
            _ => panic!("Expected with-capability"),
        }
    }

    #[test]
    fn test_parse_database_operations() {
        let cases = vec![
            "(read accounts \"alice\")",
            "(write accounts \"alice\" { \"balance\": 100 })",
            "(insert accounts \"alice\" { \"balance\": 100 })",
            "(update accounts \"alice\" { \"balance\": 200 })",
        ];

        for code in cases {
            let result = parse(code);
            assert!(result.is_ok(), "Failed to parse: {}", code);
        }
    }

    #[test]
    fn test_parse_error_unterminated() {
        let result = parse("(+ 1 2");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_error_unexpected_token() {
        let result = parse("(+ 1 ])");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_complex_module() {
        let code = r#"
            (module accounts 'accounts-admin-keyset
              
              (defschema account
                balance:decimal
                name:string)
                
              (deftable accounts:{account})
              
              (defcap TRANSFER (sender:string receiver:string amount:decimal)
                (enforce-guard (at 'guard (read accounts sender))))
                
              (defun transfer (sender:string receiver:string amount:decimal)
                (with-capability (TRANSFER sender receiver amount)
                  (let ((sender-bal (at 'balance (read accounts sender)))
                        (receiver-bal (at 'balance (read accounts receiver))))
                    (enforce (>= sender-bal amount) "Insufficient balance")
                    (update accounts sender { "balance": (- sender-bal amount) })
                    (update accounts receiver { "balance": (+ receiver-bal amount) }))))
            )
        "#;

        let result = parse(code);
        // For now, just check it doesn't crash
        // Full parsing of all features will be implemented incrementally
        match result {
            Ok(program) => {
                assert_eq!(program.items.len(), 1);
                match &program.items[0] {
                    TopLevel::Module(module) => {
                        assert_eq!(module.name, "accounts");
                        assert!(module.declarations.len() >= 3);
                    }
                    _ => panic!("Expected module"),
                }
            }
            Err(e) => panic!("Parse error: {:?}", e),
        }
    }

    #[test]
    fn test_parse_standalone_defun() {
        // This is the failing case mentioned in the issue
        let result = parse("(defun add (x y) (+ x y))");
        if let Err(e) = &result {
            eprintln!("Parse error for standalone defun: {:?}", e);
        }
        // With the fix, this should now succeed
        assert!(result.is_ok());

        let program = result.unwrap();
        assert_eq!(program.items.len(), 1);

        match &program.items[0] {
            TopLevel::Declaration(Declaration::Defun(defun)) => {
                assert_eq!(defun.name, "add");
                assert_eq!(defun.params.len(), 2);
                assert_eq!(defun.params[0].name, "x");
                assert_eq!(defun.params[1].name, "y");
                assert_eq!(defun.body.len(), 1);

                // Check that the body is parsed correctly as (+ x y)
                match &defun.body[0] {
                    Expr::Add(left, right, _) => match (left.as_ref(), right.as_ref()) {
                        (Expr::Var(x, _), Expr::Var(y, _)) => {
                            assert_eq!(x, "x");
                            assert_eq!(y, "y");
                        }
                        _ => panic!("Expected (+ x y) in body"),
                    },
                    _ => panic!("Expected Add expression in body"),
                }
            }
            _ => panic!("Expected standalone defun declaration"),
        }
    }

    #[test]
    fn test_parse_standalone_defcap() {
        // Test that standalone defcap also works
        let result = parse("(defcap TRANSFER (sender:string receiver:string amount:decimal) (enforce (> amount 0.0) \"Positive amount\"))");
        assert!(result.is_ok());

        let program = result.unwrap();
        assert_eq!(program.items.len(), 1);

        match &program.items[0] {
            TopLevel::Declaration(Declaration::Defcap(defcap)) => {
                assert_eq!(defcap.name, "TRANSFER");
                assert_eq!(defcap.params.len(), 3);
                assert_eq!(defcap.body.len(), 1);
            }
            _ => panic!("Expected standalone defcap declaration"),
        }
    }

    #[test]
    fn test_parse_standalone_defconst() {
        // Test that standalone defconst also works
        let result = parse("(defconst MINIMUM_BALANCE 10.0)");
        assert!(result.is_ok());

        let program = result.unwrap();
        assert_eq!(program.items.len(), 1);

        match &program.items[0] {
            TopLevel::Declaration(Declaration::Defconst(defconst)) => {
                assert_eq!(defconst.name, "MINIMUM_BALANCE");
                match &defconst.value {
                    Expr::Decimal(val, _) => assert_eq!(val, "10.0"),
                    _ => panic!("Expected decimal value"),
                }
            }
            _ => panic!("Expected standalone defconst declaration"),
        }
    }

    #[test]
    fn test_parse_multiple_standalone_declarations() {
        // Test multiple standalone declarations
        let code = r#"
            (defconst PI 3.14159)
            (defun square (x) (* x x))
            (defun circle-area (radius) (* PI (square radius)))
        "#;

        let result = parse(code);
        assert!(result.is_ok());

        let program = result.unwrap();
        assert_eq!(program.items.len(), 3);

        // First item should be defconst
        match &program.items[0] {
            TopLevel::Declaration(Declaration::Defconst(defconst)) => {
                assert_eq!(defconst.name, "PI");
            }
            _ => panic!("Expected defconst"),
        }

        // Second item should be defun square
        match &program.items[1] {
            TopLevel::Declaration(Declaration::Defun(defun)) => {
                assert_eq!(defun.name, "square");
                assert_eq!(defun.params.len(), 1);
            }
            _ => panic!("Expected defun square"),
        }

        // Third item should be defun circle-area
        match &program.items[2] {
            TopLevel::Declaration(Declaration::Defun(defun)) => {
                assert_eq!(defun.name, "circle-area");
                assert_eq!(defun.params.len(), 1);
            }
            _ => panic!("Expected defun circle-area"),
        }
    }

    #[test]
    fn test_original_failing_case() {
        // This is the exact original failing case from the issue report
        println!("Testing original case: (defun add (x y) (+ x y))");

        let result = parse("(defun add (x y) (+ x y))");

        // Before the fix, this would fail with:
        // UnexpectedToken(Defun, Span { start: 1, end: 6 })
        // After the fix, it should successfully parse
        assert!(result.is_ok(), "Failed to parse standalone defun");

        let program = result.unwrap();
        println!("Successfully parsed program: {:#?}", program);

        assert_eq!(program.items.len(), 1);

        match &program.items[0] {
            TopLevel::Declaration(Declaration::Defun(defun)) => {
                assert_eq!(defun.name, "add");
                assert_eq!(defun.params.len(), 2);
                assert_eq!(defun.params[0].name, "x");
                assert_eq!(defun.params[1].name, "y");
                assert!(defun.params[0].ty.is_none()); // No type annotation
                assert!(defun.params[1].ty.is_none()); // No type annotation
                assert_eq!(defun.body.len(), 1);

                // The body should be parsed as (+ x y)
                match &defun.body[0] {
                    Expr::Add(left, right, _) => match (left.as_ref(), right.as_ref()) {
                        (Expr::Var(x_name, _), Expr::Var(y_name, _)) => {
                            assert_eq!(x_name, "x");
                            assert_eq!(y_name, "y");
                        }
                        _ => panic!("Expected (+ x y) in body, got {:?}", defun.body[0]),
                    },
                    _ => panic!("Expected Add expression in body, got {:?}", defun.body[0]),
                }

                println!("✓ Standalone defun parsed correctly!");
                println!("  Function name: {}", defun.name);
                println!(
                    "  Parameters: {:?}",
                    defun.params.iter().map(|p| &p.name).collect::<Vec<_>>()
                );
                println!("  Body length: {}", defun.body.len());
            }
            _ => panic!(
                "Expected TopLevel::Declaration(Declaration::Defun), got {:?}",
                program.items[0]
            ),
        }
    }
}
