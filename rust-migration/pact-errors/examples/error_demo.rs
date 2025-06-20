//! Example demonstrating the complete error reporting system

use pact_errors::*;
use pact_shared_types::SpanInfo;

fn main() {
    // Example 1: Simple parse error
    println!("=== Example 1: Parse Error ===");
    let parse_error = PactError::PEParseError(
        ParseError::UnexpectedToken {
            expected: "closing parenthesis ')'".to_string(),
            found: "end of input".to_string(),
        },
        SpanInfo { start: 45, end: 46 },
    );
    
    let source = r#"(defun add (x y)
  (+ x y
"#;
    
    let config = DiagnosticConfig::default();
    let source_info = SourceInfo {
        filename: "example.pact",
        source,
    };
    
    println!("{}", render_diagnostic(&parse_error, Some(&source_info), &config));
    
    // Example 2: Runtime error with call stack
    println!("\n=== Example 2: Runtime Error with Stack ===");
    
    let stack = vec![
        StackFrame {
            name: FullyQualifiedName {
                module: Some("math".into()),
                name: "divide".into(),
            },
            args: vec!["10".to_string(), "0".to_string()],
            fn_type: StackFunctionType::SFDefun,
            info: SpanInfo { start: 120, end: 135 },
        },
        StackFrame {
            name: FullyQualifiedName {
                module: Some("calculator".into()),
                name: "calculate-ratio".into(),
            },
            args: vec!["100".to_string(), "0".to_string()],
            fn_type: StackFunctionType::SFDefun,
            info: SpanInfo { start: 200, end: 230 },
        },
    ];
    
    let runtime_error = PactError::PEExecutionError(
        EvalError::DivisionByZero,
        stack,
        SpanInfo { start: 125, end: 130 },
    );
    
    let source2 = r#"(module math GOV
  (defun divide (x y)
    (/ x y))  ; Error happens here
    
  (defun multiply (x y)
    (* x y)))

(module calculator GOV
  (defun calculate-ratio (total part)
    (math.divide total part)))
"#;
    
    let source_info2 = SourceInfo {
        filename: "calculator.pact",
        source: source2,
    };
    
    println!("{}", render_diagnostic(&runtime_error, Some(&source_info2), &config));
    
    // Example 3: Type error
    println!("\n=== Example 3: Type Error ===");
    
    let type_error = PactError::PEExecutionError(
        EvalError::TypeMismatch {
            expected: "integer".to_string(),
            found: "string".to_string(),
            context: "type checking".to_string(),
        },
        vec![],
        SpanInfo { start: 15, end: 22 },
    );
    
    let source3 = r#"(+ 5 "hello")"#;
    
    let source_info3 = SourceInfo {
        filename: "<repl>",
        source: source3,
    };
    
    println!("{}", render_diagnostic(&type_error, Some(&source_info3), &config));
    
    // Example 4: Multiple errors
    println!("\n=== Example 4: Multiple Errors ===");
    
    let errors = vec![
        PactError::PELexerError(
            LexerError::UnterminatedString,
            SpanInfo { start: 10, end: 20 },
        ),
        PactError::PEParseError(
            ParseError::SyntaxError("Invalid expression".to_string()),
            SpanInfo { start: 25, end: 30 },
        ),
    ];
    
    let source4 = r#"(let ((x "unterminated string)
  invalid-syntax)"#;
    
    let source_info4 = SourceInfo {
        filename: "errors.pact",
        source: source4,
    };
    
    println!("{}", render_diagnostics(&errors, Some(source_info4), &config));
}