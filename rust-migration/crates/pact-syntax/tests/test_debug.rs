use pact_parser::*;

#[test]
fn debug_use_parsing() {
    // Test the exact failing case
    let source = r#"(use coin)
(use coin "hash123456")
(use coin [transfer create-account])
(use ns.module "hash" [function1 function2])"#;

    println!("Testing full source:");

    let result = parse(source);
    match &result {
        Ok(program) => {
            println!("Success! Parsed {} items", program.len());
            for (i, item) in program.iter().enumerate() {
                println!("Item {}: {:?}", i, item);
            }
        }
        Err(e) => {
            println!("Parse error: {:?}", e);
            // Try to parse line by line to find the issue
            for (i, line) in source.lines().enumerate() {
                println!("\nTrying line {}: {}", i, line);
                match parse(line) {
                    Ok(_) => println!("  OK"),
                    Err(e) => println!("  Error: {:?}", e),
                }
            }
        }
    }
}
