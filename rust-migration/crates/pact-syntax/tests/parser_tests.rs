//! Comprehensive parser tests for Haskell-conformant Pact parser

use pact_parser::*;

#[test]
fn test_parse_empty_program() {
    let result = parse("");
    assert!(result.is_ok());
    let program = result.unwrap();
    assert_eq!(program.len(), 0);
}

#[test]
fn test_parse_literals() {
    // Integer literal
    let result = parse("42");
    assert!(result.is_ok());
    let program = result.unwrap();
    assert_eq!(program.len(), 1);
    match &program[0] {
        ParsedTopLevel::TLTerm(ParsedExpr::Constant(Literal::LInteger(42), _)) => {}
        _ => panic!("Expected integer literal"),
    }

    // String literal
    let result = parse("\"hello world\"");
    assert!(result.is_ok());

    // Boolean literals
    let result = parse("true");
    assert!(result.is_ok());
    let result = parse("false");
    assert!(result.is_ok());
}

#[test]
fn test_parse_if_expression() {
    let result = parse("(if (> x 0) x (- x))");
    assert!(result.is_ok());
    let program = result.unwrap();
    assert_eq!(program.len(), 1);

    match &program[0] {
        ParsedTopLevel::TLTerm(ParsedExpr::If {
            cond: _,
            then_expr: _,
            else_expr,
            ..
        }) => {
            assert!(else_expr.is_some());
            // Verify structure exists
        }
        _ => panic!("Expected if expression"),
    }
}

#[test]
fn test_parse_and_or_expressions() {
    let result = parse("(and true false)");
    assert!(result.is_ok());

    let result = parse("(or x y)");
    assert!(result.is_ok());
}

#[test]
fn test_parse_cond_expression() {
    let code = "(cond ((> x 0) \"positive\") ((< x 0) \"negative\") (true \"zero\"))";
    let result = parse(code);
    assert!(result.is_ok());

    let program = result.unwrap();
    match &program[0] {
        ParsedTopLevel::TLTerm(ParsedExpr::Cond { branches, .. }) => {
            assert_eq!(branches.len(), 3);
        }
        _ => panic!("Expected cond expression"),
    }
}

#[test]
fn test_parse_enforce_expressions() {
    let result = parse("(enforce (> balance 0) \"Insufficient balance\")");
    assert!(result.is_ok());

    let result = parse("(enforce-one \"Must pass one check\" [(> x 0) (< x 10)])");
    assert!(result.is_ok());
}

#[test]
fn test_parse_capability_expressions() {
    let result = parse("(with-capability (TRANSFER sender receiver amount) (do-transfer))");
    assert!(result.is_ok());

    let result = parse("(require-capability (ADMIN))");
    assert!(result.is_ok());

    let result = parse("(create-user-guard my-guard)");
    assert!(result.is_ok());
}

#[test]
fn test_parse_let_expressions() {
    let result = parse("(let ((x 5) (y 10)) (+ x y))");
    assert!(result.is_ok());

    let result = parse("(let* ((x 5) (y (* x 2))) y)");
    assert!(result.is_ok());
}

#[test]
fn test_parse_lambda() {
    let result = parse("(lambda (x y) (+ x y))");
    assert!(result.is_ok());

    let result = parse("(lambda (x:integer) (* x x))");
    assert!(result.is_ok());
}

#[test]
fn test_parse_list_literal() {
    let result = parse("[1 2 3 4 5]");
    assert!(result.is_ok());

    let result = parse("[\"a\" \"b\" \"c\"]");
    assert!(result.is_ok());
}

#[test]
fn test_parse_object_literal() {
    let result = parse("{\"name\": \"Alice\", \"balance\": 100}");
    assert!(result.is_ok());

    let program = result.unwrap();
    match &program[0] {
        ParsedTopLevel::TLTerm(ParsedExpr::Object(fields, _)) => {
            assert_eq!(fields.len(), 2);
        }
        _ => panic!("Expected object literal"),
    }
}

#[test]
fn test_parse_simple_module() {
    let code = r#"
        (module test 'test-keyset
          (defun add (x:integer y:integer)
            (+ x y))
            
          (defconst PI 3.14159)
        )
    "#;

    let result = parse(code);
    match &result {
        Err(e) => panic!("Parse error: {:?}", e),
        Ok(_) => {}
    }
    let program = result.unwrap();

    match &program[0] {
        ParsedTopLevel::TLModule(module) => {
            assert_eq!(module.name, "test");
            assert_eq!(module.definitions.len(), 2);
        }
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_parse_defcap() {
    let code = r#"
        (module test 'test-keyset
          (defcap TRANSFER (sender:string receiver:string amount:decimal)
            (enforce-guard (at 'guard (read accounts sender)))
            (enforce (> amount 0.0) "Amount must be positive"))
        )
    "#;

    let result = parse(code);
    assert!(result.is_ok());

    let program = result.unwrap();
    match &program[0] {
        ParsedTopLevel::TLModule(module) => match &module.definitions[0] {
            ParsedDef::DCap(defcap) => {
                assert_eq!(defcap.name.name, "TRANSFER");
                assert_eq!(defcap.args.len(), 3);
                assert_eq!(defcap.body.len(), 2);
            }
            _ => panic!("Expected defcap"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_parse_defschema() {
    let code = r#"
        (module test 'test-keyset
          (defschema account
            balance:decimal
            name:string
            active:bool)
        )
    "#;

    let result = parse(code);
    assert!(result.is_ok());

    let program = result.unwrap();
    match &program[0] {
        ParsedTopLevel::TLModule(module) => match &module.definitions[0] {
            ParsedDef::DSchema(schema) => {
                assert_eq!(schema.name, "account");
                assert_eq!(schema.fields.len(), 3);
            }
            _ => panic!("Expected defschema"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_parse_deftable() {
    let code = r#"
        (module test 'test-keyset
          (defschema account balance:decimal)
          (deftable accounts:{account})
        )
    "#;

    let result = parse(code);
    assert!(result.is_ok());
}

#[test]
fn test_parse_defpact() {
    let code = r#"
        (module test 'test-keyset
          (defpact two-party-escrow (payer payee amount)
            (step 
              (let ((payer-bal (at 'balance (read accounts payer))))
                (enforce (>= payer-bal amount) "Insufficient balance")))
            (step "finalize"
              (credit payee amount)))
        )
    "#;

    let result = parse(code);
    assert!(result.is_ok());

    let program = result.unwrap();
    match &program[0] {
        ParsedTopLevel::TLModule(module) => match &module.definitions[0] {
            ParsedDef::DPact(defpact) => {
                assert_eq!(defpact.name.name, "two-party-escrow");
                assert_eq!(defpact.args.len(), 3);
                assert_eq!(defpact.steps.len(), 2);
            }
            _ => panic!("Expected defpact"),
        },
        _ => panic!("Expected module"),
    }
}

#[test]
fn test_parse_interface() {
    let code = r#"
        (interface fungible-v2
          (defun transfer:string (from:string to:string amount:decimal))
          (defcap TRANSFER:bool (sender:string receiver:string amount:decimal))
          (defschema account balance:decimal)
        )
    "#;

    let result = parse(code);
    match &result {
        Err(e) => panic!("Parse error: {:?}", e),
        Ok(_) => {}
    }

    let program = result.unwrap();
    match &program[0] {
        ParsedTopLevel::TLInterface(interface) => {
            assert_eq!(interface.name, "fungible-v2");
            assert_eq!(interface.definitions.len(), 3);
        }
        _ => panic!("Expected interface"),
    }
}

#[test]
fn test_parse_import() {
    let result = parse("(use coin)");
    assert!(result.is_ok());

    let result = parse("(use coin [transfer create-account])");
    assert!(result.is_ok());

    let result = parse("(use coin \"hash123\")");
    assert!(result.is_ok());
}

#[test]
fn test_parse_qualified_names() {
    let code = "(coin.transfer \"alice\" \"bob\" 10.0)";
    let result = parse(code);
    assert!(result.is_ok());
}

#[test]
fn test_parse_try_expression() {
    let result = parse("(try (/ 1 0) \"Division by zero\")");
    assert!(result.is_ok());
}

#[test]
fn test_parse_yield_resume() {
    let result = parse("(yield {\"result\": 42})");
    assert!(result.is_ok());

    let result = parse("(resume ((result 0)) result)");
    assert!(result.is_ok());
}

#[test]
fn test_parse_complex_module() {
    let code = r#"
        (module bank 'bank-admin
          
          (defschema account
            balance:decimal
            owner:string)
            
          (deftable accounts:{account})
          
          (defcap TRANSFER (from:string to:string amount:decimal)
            @doc "Transfer capability"
            (enforce-guard (at 'guard (read accounts from)))
            (enforce (> amount 0.0) "Positive amount only"))
            
          (defun transfer (from:string to:string amount:decimal)
            @doc "Transfer funds between accounts"
            (with-capability (TRANSFER from to amount)
              (let ((from-bal (at 'balance (read accounts from)))
                    (to-bal (at 'balance (read accounts to))))
                (enforce (>= from-bal amount) "Insufficient funds")
                (update accounts from {"balance": (- from-bal amount)})
                (update accounts to {"balance": (+ to-bal amount)}))))
                
          (defpact escrow (payer payee arbiter amount)
            (step
              (with-capability (TRANSFER payer arbiter amount)
                (transfer payer arbiter amount)))
            (step-with-rollback
              (transfer arbiter payee amount)
              (transfer arbiter payer amount)))
        )
    "#;

    let result = parse(code);
    match &result {
        Err(e) => panic!("Parse error: {:?}", e),
        Ok(program) => {
            assert_eq!(program.len(), 1);
            match &program[0] {
                ParsedTopLevel::TLModule(module) => {
                    assert_eq!(module.name, "bank");
                    assert!(module.definitions.len() >= 4);
                }
                _ => panic!("Expected module"),
            }
        }
    }
}

#[test]
fn test_error_handling() {
    // Unclosed paren
    let result = parse("(+ 1 2");
    assert!(result.is_err());

    // Unexpected token
    let result = parse("(+ 1 ])");
    assert!(result.is_err());
}
