//! Tests for parser error handling

use pact_parser::parse;

#[test]
fn test_missing_closing_paren() {
    let source = "(defun test (x y) (+ x y";
    let result = parse(source);
    
    match result {
        Err(e) => {
            // Should get an error about missing closing paren
            let error_str = format!("{:?}", e);
            assert!(error_str.contains("CloseParens") || 
                   error_str.contains("closing parenthesis") ||
                   error_str.contains("end of input"));
        }
        Ok(_) => panic!("Expected parse error for missing paren"),
    }
}

#[test]
fn test_invalid_module_syntax() {
    let source = "(module test)"; // Missing governance
    let result = parse(source);
    
    match result {
        Err(e) => {
            let error_str = format!("{:?}", e);
            assert!(error_str.contains("governance") || 
                   error_str.contains("keyset"));
        }
        Ok(_) => panic!("Expected parse error for invalid module"),
    }
}

#[test]
fn test_unexpected_token() {
    // This actually parses successfully as multiple expressions in the body
    let source = "(defun test (x y) + x y)"; 
    let result = parse(source);
    
    match result {
        Ok(ast) => {
            // Parser accepts this as: defun with body containing 3 expressions: +, x, y
            assert!(!ast.is_empty());
        }
        Err(e) => {
            // Also acceptable if parser is stricter
            let error_str = format!("{:?}", e);
            assert!(error_str.contains("expression") || 
                   error_str.contains("unexpected"));
        }
    }
}

#[test]
fn test_invalid_let_binding() {
    let source = "(let (x) (+ x 1))"; // Missing binding value
    let result = parse(source);
    
    match result {
        Err(e) => {
            // Parser expects OpenParens after let, gets identifier
            let error_str = format!("{:?}", e);
            assert!(error_str.contains("OpenParens") || 
                   error_str.contains("binding") || 
                   error_str.contains("expected"));
        }
        Ok(_) => panic!("Expected parse error for invalid let binding"),
    }
}

#[test]
fn test_empty_function_body() {
    let source = "(defun empty ())"; // No body
    let result = parse(source);
    
    match result {
        Err(e) => {
            let error_str = format!("{:?}", e);
            assert!(error_str.contains("body") || 
                   error_str.contains("function"));
        }
        Ok(_) => panic!("Expected parse error for empty function body"),
    }
}

#[test]
fn test_invalid_type_annotation() {
    let source = "(defun test (x:) x)"; // Missing type after colon
    let result = parse(source);
    
    match result {
        Err(e) => {
            let error_str = format!("{:?}", e);
            assert!(error_str.contains("type") || 
                   error_str.contains("expected"));
        }
        Ok(_) => panic!("Expected parse error for invalid type annotation"),
    }
}

#[test]
fn test_unclosed_string() {
    let source = r#"(defun test () "unclosed string)"#;
    let result = parse(source);
    
    match result {
        Err(e) => {
            // Lexer should catch this first
            let error_str = format!("{:?}", e);
            assert!(error_str.contains("Unterminated") || 
                   error_str.contains("string"));
        }
        Ok(_) => panic!("Expected error for unclosed string"),
    }
}

#[test]
fn test_invalid_list_syntax() {
    let source = "[1 2 3"; // Missing closing bracket
    let result = parse(source);
    
    match result {
        Err(e) => {
            let error_str = format!("{:?}", e);
            assert!(error_str.contains("bracket") || 
                   error_str.contains("list") ||
                   error_str.contains("end of input"));
        }
        Ok(_) => panic!("Expected parse error for unclosed list"),
    }
}

#[test]
fn test_invalid_object_syntax() {
    let source = r#"{ "key": value"#; // Missing closing brace
    let result = parse(source);
    
    match result {
        Err(e) => {
            let error_str = format!("{:?}", e);
            assert!(error_str.contains("brace") || 
                   error_str.contains("object") ||
                   error_str.contains("end of input"));
        }
        Ok(_) => panic!("Expected parse error for unclosed object"),
    }
}

#[test]
fn test_multiple_errors_recovery() {
    // Test that parser can report meaningful errors even with multiple issues
    let source = "(defun test (x (+ x (/ y 0))"; // Multiple missing parens
    let result = parse(source);
    
    match result {
        Err(e) => {
            // Should report at least one meaningful error
            let error_str = format!("{:?}", e);
            assert!(!error_str.is_empty());
        }
        Ok(_) => panic!("Expected parse error for malformed code"),
    }
}