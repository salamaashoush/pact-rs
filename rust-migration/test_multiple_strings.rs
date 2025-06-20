use pact_lexer::lex;

fn main() {
    let source = r#""first unterminated "second unterminated"#;
    println!("Testing: {:?}", source);
    let result = lex(source);
    match &result {
        Ok(tokens) => {
            println!("Success: {} tokens", tokens.len());
            for (i, (tok, span)) in tokens.iter().enumerate() {
                println!("  {}: {:?} at {:?}", i, tok, span);
            }
        }
        Err(e) => println!("Error: {:?}", e),
    }
}