use pact_parser::*;

#[test]
fn test_minimal_module() {
    let code = "(module test 'test-keyset)";
    let result = parse(code);
    match &result {
        Err(e) => panic!("Parse error: {:?}", e),
        Ok(p) => println!("Success: {:?}", p),
    }
}

#[test]
fn test_module_with_one_def() {
    let code = "(module test 'test-keyset (defconst PI 3.14))";
    let result = parse(code);
    match &result {
        Err(e) => panic!("Parse error: {:?}", e),
        Ok(p) => println!("Success: {:?}", p),
    }
}
