use pact_lexer::lex;
use pact_errors::PactError;

fn main() {
    // Test invalid escape
    println!("Testing invalid escape:");
    let result = lex(r#""hello\xworld""#);
    match &result {
        Ok(tokens) => println!("  Success: {:?}", tokens),
        Err(e) => println!("  Error: {:?}", e),
    }
    
    // Test invalid number
    println!("\nTesting invalid number:");
    let result = lex("123abc456");
    match &result {
        Ok(tokens) => println!("  Success: {:?}", tokens),
        Err(e) => println!("  Error: {:?}", e),
    }
    
    // Test unterminated string
    println!("\nTesting unterminated string:");
    let result = lex(r#""first unterminated"#);
    match &result {
        Ok(tokens) => println!("  Success: {:?}", tokens),
        Err(e) => println!("  Error: {:?}", e),
    }
    
    // Test multiline unterminated
    println!("\nTesting multiline unterminated:");
    let source = r#"
    (module test GOV
      (defun broken () 
        "this string is not terminated
        (+ 1 2))
      
      (defun another () "also broken)
    )
    "#;
    let result = lex(source);
    match &result {
        Ok(tokens) => println!("  Success: {} tokens", tokens.len()),
        Err(e) => println!("  Error: {:?}", e),
    }
}