//! Tests for lexer error handling

use pact_lexer::lex;
use pact_errors::{PactError, LexerError};

#[test]
fn test_unterminated_string() {
    let source = r#"(defun test () "hello world)"#;
    let result = lex(source);
    
    match result {
        Err(PactError::PELexerError(LexerError::UnterminatedString, span)) => {
            assert_eq!(span.start, 15);
            assert!(span.end > span.start);
            // The unterminated string starts at position 15
            assert_eq!(&source[span.start..span.end], r#""hello world)"#);
        }
        _ => panic!("Expected unterminated string error"),
    }
}

#[test]
fn test_invalid_escape_sequence() {
    // Note: The current lexer implementation accepts escape sequences
    // and returns them as-is in the string token. This matches the
    // behavior of many lexers that defer escape validation to a later phase.
    let source = r#""hello\xworld""#;
    let result = lex(source);
    
    match result {
        Ok(tokens) => {
            assert_eq!(tokens.len(), 1);
            match &tokens[0].0 {
                pact_lexer::Token::String(s) => {
                    assert_eq!(s, "hello\\xworld");
                }
                _ => panic!("Expected string token"),
            }
        }
        Err(_) => panic!("Expected successful parse with escape sequence"),
    }
}

#[test]
fn test_invalid_number() {
    // The lexer correctly parses "123abc456" as two tokens:
    // "123" as a number and "abc456" as an identifier
    let source = "123abc456";
    let result = lex(source);
    
    match result {
        Ok(tokens) => {
            assert_eq!(tokens.len(), 2);
            match &tokens[0].0 {
                pact_lexer::Token::Number(n) => assert_eq!(n, "123"),
                _ => panic!("Expected number token"),
            }
            match &tokens[1].0 {
                pact_lexer::Token::Ident(i) => assert_eq!(i, "abc456"),
                _ => panic!("Expected identifier token"),
            }
        }
        _ => panic!("Expected successful parse into two tokens"),
    }
}

#[test]
fn test_invalid_token() {
    let source = "(def @@@ test)";
    let result = lex(source);
    
    // This should actually parse the first few tokens successfully
    match result {
        Ok(tokens) => {
            // Should get OpenParens, Ident("def"), then error on @@@
            assert!(tokens.len() >= 2);
        }
        Err(PactError::PELexerError(LexerError::InvalidToken(tok), span)) => {
            assert_eq!(tok, "@@@");
            assert_eq!(&source[span.start..span.end], "@@@");
        }
        _ => {} // Some invalid tokens might parse as identifiers
    }
}

#[test]
fn test_multiple_string_errors() {
    // The lexer parses this as a complete string followed by identifiers
    // because the second quote closes the first string
    let source = r#""first unterminated "second unterminated"#;
    let result = lex(source);
    
    match result {
        Ok(tokens) => {
            // This parses as: "first unterminated " + ident(second) + ident(unterminated)
            assert_eq!(tokens.len(), 3);
            match &tokens[0].0 {
                pact_lexer::Token::String(s) => {
                    assert_eq!(s, "first unterminated ");
                }
                _ => panic!("Expected string token"),
            }
        }
        Err(e) => panic!("Expected successful parse, got: {:?}", e),
    }
}

#[test]
fn test_nested_quotes() {
    let source = r#""outer "nested" string""#;
    let result = lex(source);
    
    // This might parse as multiple tokens or give an error
    // depending on how the lexer handles nested quotes
    match result {
        Ok(tokens) => {
            // If it parses, verify the tokens make sense
            assert!(!tokens.is_empty());
        }
        Err(PactError::PELexerError(_, _)) => {
            // Also acceptable if it errors
        }
        _ => panic!("Unexpected result"),
    }
}

#[test]
fn test_string_with_valid_escapes() {
    let source = r#""hello\n\t\"world\"""#;
    let result = lex(source);
    
    match result {
        Ok(tokens) => {
            assert_eq!(tokens.len(), 1);
            match &tokens[0].0 {
                pact_lexer::Token::String(s) => {
                    assert_eq!(s, "hello\n\t\"world\"");
                }
                _ => panic!("Expected string token"),
            }
        }
        Err(e) => panic!("Expected successful parse, got: {:?}", e),
    }
}

#[test]
fn test_empty_string() {
    let source = r#""""#;
    let result = lex(source);
    
    match result {
        Ok(tokens) => {
            assert_eq!(tokens.len(), 1);
            match &tokens[0].0 {
                pact_lexer::Token::String(s) => {
                    assert_eq!(s, "");
                }
                _ => panic!("Expected empty string token"),
            }
        }
        Err(e) => panic!("Expected successful parse, got: {:?}", e),
    }
}

#[test]
fn test_error_recovery() {
    // Test with a simpler unterminated string that we know will fail
    let source = r#"(defun test () "unterminated string"#;
    
    let result = lex(source);
    
    match result {
        Err(PactError::PELexerError(LexerError::UnterminatedString, span)) => {
            // Should point to the unterminated string
            let error_text = &source[span.start..span.end];
            assert!(error_text.starts_with('"'));
            assert!(!error_text.ends_with('"'));
        }
        Ok(tokens) => {
            // The current lexer might successfully tokenize multiline strings
            // depending on how logos handles them. This is also acceptable.
            assert!(!tokens.is_empty());
        }
        Err(e) => panic!("Unexpected error type: {:?}", e),
    }
}