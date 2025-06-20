use pact_parser::parse;

fn main() {
    // Test invalid let binding
    println!("Testing invalid let binding:");
    let source = "(let (x) (+ x 1))";
    let result = parse(source);
    match &result {
        Ok(ast) => println!("  Success: {:?}", ast),
        Err(e) => println!("  Error: {:?}", e),
    }
    
    // Test unexpected token
    println!("\nTesting unexpected token:");
    let source = "(defun test (x y) + x y)";
    let result = parse(source);
    match &result {
        Ok(ast) => println!("  Success: parsed successfully"),
        Err(e) => println!("  Error: {:?}", e),
    }
}