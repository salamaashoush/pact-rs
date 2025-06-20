use pact_parser::parse;

fn main() {
    let simple_module = r#"(module test 'admin (defun hello () true))"#;
    
    match parse(simple_module) {
        Ok(ast) => {
            println!("Successfully parsed AST:");
            println!("{:#?}", ast);
        }
        Err(e) => {
            println!("Parse error: {:?}", e);
        }
    }
}