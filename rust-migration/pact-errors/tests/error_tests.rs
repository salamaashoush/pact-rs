//! Comprehensive tests for the error system

use pact_errors::*;
use pact_shared_types::SpanInfo;

#[test]
fn test_lexer_errors() {
    // Test unterminated string
    let error = PactError::PELexerError(
        LexerError::UnterminatedString,
        SpanInfo { start: 10, end: 25 }
    );
    
    let source = r#"(defun test () "hello world)"#;
    let config = DiagnosticConfig::default();
    let source_info = SourceInfo {
        filename: "test.pact",
        source,
    };
    
    let output = render_diagnostic(&error, Some(&source_info), &config);
    assert!(output.contains("Unterminated string"));
    assert!(output.contains("test.pact:1:11"));
}

#[test]
fn test_parse_errors() {
    // Test unexpected token
    let error = PactError::PEParseError(
        ParseError::UnexpectedToken {
            expected: "closing parenthesis ')'".to_string(),
            found: "identifier 'test'".to_string(),
        },
        SpanInfo { start: 20, end: 24 }
    );
    
    let source = "(defun add (x y) (+ x y test)";
    let config = DiagnosticConfig::default();
    let source_info = SourceInfo {
        filename: "test.pact",
        source,
    };
    
    let output = render_diagnostic(&error, Some(&source_info), &config);
    assert!(output.contains("expected closing parenthesis ')'"));
    assert!(output.contains("found identifier 'test'"));
}

#[test]
fn test_execution_errors_with_stack() {
    // Create a call stack
    let stack = vec![
        StackFrame {
            name: FullyQualifiedName {
                module: Some("math".into()),
                name: "safe-divide".into(),
            },
            args: vec!["10".to_string(), "0".to_string()],
            fn_type: StackFunctionType::SFDefun,
            info: SpanInfo { start: 50, end: 65 },
        },
        StackFrame {
            name: FullyQualifiedName {
                module: None,
                name: "calculate".into(),
            },
            args: vec!["100".to_string()],
            fn_type: StackFunctionType::SFDefun,
            info: SpanInfo { start: 100, end: 120 },
        },
    ];
    
    let error = PactError::PEExecutionError(
        EvalError::DivisionByZero,
        stack,
        SpanInfo { start: 55, end: 60 }
    );
    
    let source = r#"(module math GOV
  (defun safe-divide (x y)
    (/ x y)))
    
(defun calculate (total)
  (math.safe-divide total 0))"#;
    
    let config = DiagnosticConfig::default();
    let source_info = SourceInfo {
        filename: "calc.pact",
        source,
    };
    
    let output = render_diagnostic(&error, Some(&source_info), &config);
    assert!(output.contains("Division by zero"));
    assert!(output.contains("Call stack:"));
    assert!(output.contains("math.safe-divide(10, 0)"));
    assert!(output.contains("calculate(100)"));
}

#[test]
fn test_type_errors() {
    let error = PactError::PEExecutionError(
        EvalError::TypeMismatch {
            expected: "integer".to_string(),
            found: "string".to_string(),
            context: "arithmetic operation".to_string(),
        },
        vec![],
        SpanInfo { start: 15, end: 22 }
    );
    
    let source = r#"(+ 5 "hello")"#;
    let config = DiagnosticConfig::default();
    let source_info = SourceInfo {
        filename: "<repl>",
        source,
    };
    
    let output = render_diagnostic(&error, Some(&source_info), &config);
    assert!(output.contains("Type mismatch"));
    assert!(output.contains("expected integer"));
    assert!(output.contains("found string"));
}

#[test]
fn test_user_recoverable_errors() {
    let stack = vec![
        StackFrame {
            name: FullyQualifiedName {
                module: Some("bank".into()),
                name: "transfer".into(),
            },
            args: vec!["alice".to_string(), "bob".to_string(), "1000".to_string()],
            fn_type: StackFunctionType::SFDefun,
            info: SpanInfo { start: 200, end: 250 },
        },
    ];
    
    let error = PactError::PEUserRecoverableError(
        UserRecoverableError::EnforceFailure("Insufficient balance".into()),
        stack,
        SpanInfo { start: 210, end: 230 }
    );
    
    let source = r#"(module bank GOV
  (defun transfer (from to amount)
    (enforce (>= (get-balance from) amount) "Insufficient balance")
    (debit from amount)
    (credit to amount)))"#;
    
    let config = DiagnosticConfig::default();
    let source_info = SourceInfo {
        filename: "bank.pact",
        source,
    };
    
    let output = render_diagnostic(&error, Some(&source_info), &config);
    assert!(output.contains("Enforce failure: Insufficient balance"));
    assert!(output.contains("bank.transfer(alice, bob, 1000)"));
}

#[test]
fn test_multiple_errors() {
    let errors = vec![
        PactError::PELexerError(
            LexerError::InvalidNumber("123abc".to_string()),
            SpanInfo { start: 0, end: 6 }
        ),
        PactError::PEParseError(
            ParseError::SyntaxError("Invalid expression".to_string()),
            SpanInfo { start: 10, end: 15 }
        ),
    ];
    
    let source = "123abc (+ 1 invalid)";
    let config = DiagnosticConfig::default();
    let source_info = SourceInfo {
        filename: "errors.pact",
        source,
    };
    
    let output = render_diagnostics(&errors, Some(source_info), &config);
    assert!(output.contains("Invalid number: 123abc"));
    assert!(output.contains("Syntax error: Invalid expression"));
    assert!(output.contains("──────")); // separator between errors
}

#[test]
fn test_capability_errors() {
    let stack = vec![
        StackFrame {
            name: FullyQualifiedName {
                module: Some("coin".into()),
                name: "transfer".into(),
            },
            args: vec!["alice".to_string(), "bob".to_string(), "100.0".to_string()],
            fn_type: StackFunctionType::SFDefun,
            info: SpanInfo { start: 300, end: 350 },
        },
    ];
    
    let error = PactError::PEExecutionError(
        EvalError::CapabilityNotGranted("coin.TRANSFER".to_string()),
        stack,
        SpanInfo { start: 320, end: 340 }
    );
    
    let source = r#"(module coin GOV
  (defcap TRANSFER (from to amount)
    (enforce-keyset (+ from "-keyset")))
    
  (defun transfer (from to amount)
    (with-capability (TRANSFER from to amount)
      (debit from amount)
      (credit to amount))))"#;
    
    let config = DiagnosticConfig::default();
    let source_info = SourceInfo {
        filename: "coin.pact",
        source,
    };
    
    let output = render_diagnostic(&error, Some(&source_info), &config);
    assert!(output.contains("Capability not granted: coin.TRANSFER"));
    assert!(output.contains("coin.transfer(alice, bob, 100.0)"));
}

#[test]
fn test_no_color_output() {
    let error = PactError::PELexerError(
        LexerError::InvalidToken("@@@".to_string()),
        SpanInfo { start: 5, end: 8 }
    );
    
    let source = "(def @@@)";
    let config = DiagnosticConfig {
        use_colors: false,
        ..Default::default()
    };
    let source_info = SourceInfo {
        filename: "test.pact",
        source,
    };
    
    let output = render_diagnostic(&error, Some(&source_info), &config);
    // Should not contain ANSI escape codes
    assert!(!output.contains("\x1b["));
    assert!(output.contains("Invalid token: @@@"));
}

#[test]
fn test_no_unicode_output() {
    let error = PactError::PEParseError(
        ParseError::SyntaxError("Missing parenthesis".to_string()),
        SpanInfo { start: 0, end: 5 }
    );
    
    let source = "defun";
    let config = DiagnosticConfig {
        use_unicode: false,
        ..Default::default()
    };
    let source_info = SourceInfo {
        filename: "test.pact",
        source,
    };
    
    let output = render_diagnostic(&error, Some(&source_info), &config);
    // Should use ASCII characters instead of unicode
    assert!(output.contains("-->") || output.contains("--["));
    assert!(!output.contains("╭") && !output.contains("│"));
}