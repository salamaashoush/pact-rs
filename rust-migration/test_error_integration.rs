use pact_lexer::lex;
use pact_parser::parse;
use pact_errors::render_diagnostic;
use pact_errors::{DiagnosticConfig, SourceInfo};

fn test_lexer_error() {
    println!("=== Testing Lexer Error ===");
    let source = r#"(defun test () "unterminated string"#;
    
    match lex(source) {
        Ok(_) => println!("Unexpected success"),
        Err(error) => {
            let config = DiagnosticConfig::default();
            let source_info = SourceInfo {
                filename: "test.pact",
                source,
            };
            
            let diagnostic = render_diagnostic(&error, Some(&source_info), &config);
            println!("{}", diagnostic);
        }
    }
}

fn test_parser_error() {
    println!("\n=== Testing Parser Error ===");
    let source = "(defun test (x y) (+ x y";
    
    match parse(source) {
        Ok(_) => println!("Unexpected success"),
        Err(error) => {
            let config = DiagnosticConfig::default();
            let source_info = SourceInfo {
                filename: "test.pact",
                source,
            };
            
            let diagnostic = render_diagnostic(&error, Some(&source_info), &config);
            println!("{}", diagnostic);
        }
    }
}

fn test_no_colors() {
    println!("\n=== Testing No Colors ===");
    let source = "(module incomplete";
    
    match parse(source) {
        Ok(_) => println!("Unexpected success"),
        Err(error) => {
            let config = DiagnosticConfig {
                use_colors: false,
                use_unicode: false,
                ..Default::default()
            };
            let source_info = SourceInfo {
                filename: "test.pact",
                source,
            };
            
            let diagnostic = render_diagnostic(&error, Some(&source_info), &config);
            println!("{}", diagnostic);
        }
    }
}

fn main() {
    test_lexer_error();
    test_parser_error();
    test_no_colors();
}