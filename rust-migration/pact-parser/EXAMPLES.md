# Pact Parser Examples

This document provides practical examples of using the Pact parser in various scenarios, from basic expression parsing to complex module analysis.

## Basic Usage

### Simple Expression Parsing

```rust
use pact_parser::Parser;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = "(+ 1 2)";
    let expr = Parser::parse_expr_from_source(source)?;
    
    println!("Parsed expression: {:?}", expr);
    
    Ok(())
}
```

**Output:**
```
Parsed expression: App(
    Var(ParsedName("+")), 
    [Constant(LInteger(1)), Constant(LInteger(2))], 
    SpanInfo { start: 0, end: 7 }
)
```

### Function Definition Parsing

```rust
use pact_parser::{Parser, ast::*};

fn parse_function() -> Result<(), Box<dyn std::error::Error>> {
    let source = r#"
    (defun add (x:integer y:integer):integer
      @doc "Add two integers"  
      (+ x y))
    "#;
    
    let repl_items = Parser::parse_repl_program_from_source(source)?;
    
    match &repl_items[0] {
        ReplTopLevel::RTLDefun(defun) => {
            println!("Function name: {}", defun.name);
            println!("Parameter count: {}", defun.args.len());
            println!("Has documentation: {}", defun.doc.is_some());
            
            // Print parameter details
            for (arg, _info) in &defun.args {
                match &arg.ty {
                    Some(ty) => println!("  Parameter '{}': {:?}", arg.name, ty),
                    None => println!("  Parameter '{}': untyped", arg.name),
                }
            }
        }
        _ => println!("Expected function definition"),
    }
    
    Ok(())
}
```

## Working with AST Nodes

### Expression Analysis

```rust
use pact_parser::{Parser, ast::*};

fn analyze_expression(expr: &ParsedExpr<SpanInfo>) {
    match expr {
        ParsedExpr::Constant(literal, span) => {
            println!("Literal at {}..{}: {:?}", span.start, span.end, literal);
        }
        ParsedExpr::Var(name, span) => {
            println!("Variable at {}..{}: {}", span.start, span.end, name);
        }
        ParsedExpr::App(func, args, span) => {
            println!("Function call at {}..{} with {} arguments", 
                    span.start, span.end, args.len());
            
            // Recursively analyze function and arguments
            analyze_expression(func);
            for arg in args {
                analyze_expression(arg);
            }
        }
        ParsedExpr::Conditional(test, then_expr, else_expr, span) => {
            println!("Conditional at {}..{}", span.start, span.end);
            println!("  Test:");
            analyze_expression(test);
            println!("  Then:");
            analyze_expression(then_expr);
            println!("  Else:");
            analyze_expression(else_expr);
        }
        ParsedExpr::Lambda(args, body, span) => {
            println!("Lambda at {}..{} with {} parameters", 
                    span.start, span.end, args.len());
            analyze_expression(body);
        }
        ParsedExpr::Let(bindings, body, span) => {
            println!("Let expression at {}..{} with {} bindings", 
                    span.start, span.end, bindings.len());
            analyze_expression(body);
        }
        _ => println!("Other expression type: {:?}", expr),
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = "(if (> x 0) (+ x 1) (- x 1))";
    let expr = Parser::parse_expr_from_source(source)?;
    
    analyze_expression(&expr);
    
    Ok(())
}
```

### Module Structure Analysis

```rust
use pact_parser::{Parser, ast::*};

fn analyze_module(module: &ParsedModule<SpanInfo>) {
    println!("Module: {}", module.name);
    
    // Analyze governance
    match &module.governance {
        Governance::KeyGov(keyset) => {
            println!("  Governance: Keyset '{}'", keyset);
        }
        Governance::CapGov(capability) => {
            println!("  Governance: Capability '{}'", capability);
        }
    }
    
    // Print documentation if present
    if let Some(doc) = &module.docs {
        println!("  Documentation: {}", doc);
    }
    
    // Analyze definitions
    println!("  Definitions ({}):", module.definitions.len());
    for def in &module.definitions {
        match def {
            ParsedDef::Defun(defun) => {
                println!("    Function: {} ({})", defun.name, defun.args.len());
            }
            ParsedDef::Defcap(defcap) => {
                println!("    Capability: {} ({})", defcap.name, defcap.args.len());
            }
            ParsedDef::Defschema(schema) => {
                println!("    Schema: {} ({} fields)", schema.name, schema.fields.len());
            }
            ParsedDef::Deftable(table) => {
                println!("    Table: {} : {}", table.name, table.schema);
            }
            ParsedDef::Defconst(const_def) => {
                println!("    Constant: {}", const_def.name);
            }
            ParsedDef::Defpact(defpact) => {
                println!("    Pact: {} ({})", defpact.name, defpact.args.len());
            }
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = r#"
    (module coin GOVERNANCE
      @doc "A simple coin contract"
      
      (defschema account-schema
        balance:decimal
        guard:guard)
      
      (deftable accounts:{account-schema})
      
      (defcap TRANSFER (sender:string receiver:string amount:decimal)
        (enforce (> amount 0.0) "Positive amount"))
      
      (defun transfer (sender:string receiver:string amount:decimal)
        @doc "Transfer coins"
        (with-capability (TRANSFER sender receiver amount)
          (debit sender amount)
          (credit receiver amount))))
    "#;
    
    let program = Parser::parse_program_from_source(source)?;
    
    match &program[0] {
        ParsedTopLevel::TLModule(module) => analyze_module(module),
        _ => println!("Expected module"),
    }
    
    Ok(())
}
```

## Advanced Examples

### AST Transformation

```rust
use pact_parser::{Parser, ast::*};

// Transform all variable names to uppercase
fn transform_expr(expr: ParsedExpr<SpanInfo>) -> ParsedExpr<SpanInfo> {
    match expr {
        ParsedExpr::Var(name, info) => {
            let new_name = ParsedName::new(name.as_str().to_uppercase());
            ParsedExpr::Var(new_name, info)
        }
        ParsedExpr::App(func, args, info) => {
            let new_func = Box::new(transform_expr(*func));
            let new_args = args.into_iter().map(transform_expr).collect();
            ParsedExpr::App(new_func, new_args, info)
        }
        ParsedExpr::Conditional(test, then_expr, else_expr, info) => {
            ParsedExpr::Conditional(
                Box::new(transform_expr(*test)),
                Box::new(transform_expr(*then_expr)),
                Box::new(transform_expr(*else_expr)),
                info
            )
        }
        ParsedExpr::Lambda(args, body, info) => {
            // Transform argument names
            let new_args = args.into_iter().map(|(arg, arg_info)| {
                let new_name = ParsedName::new(arg.name.as_str().to_uppercase());
                (Arg { name: new_name, ty: arg.ty, info: arg.info }, arg_info)
            }).collect();
            
            ParsedExpr::Lambda(new_args, Box::new(transform_expr(*body)), info)
        }
        ParsedExpr::Let(bindings, body, info) => {
            let new_bindings = bindings.into_iter().map(|(arg, expr, bind_info)| {
                let new_name = ParsedName::new(arg.name.as_str().to_uppercase());
                let new_arg = Arg { name: new_name, ty: arg.ty, info: arg.info };
                (new_arg, transform_expr(expr), bind_info)
            }).collect();
            
            ParsedExpr::Let(new_bindings, Box::new(transform_expr(*body)), info)
        }
        // Other expression types remain unchanged
        _ => expr,
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = "(lambda (x y) (+ x y))";
    let expr = Parser::parse_expr_from_source(source)?;
    
    println!("Original: {:?}", expr);
    
    let transformed = transform_expr(expr);
    println!("Transformed: {:?}", transformed);
    
    Ok(())
}
```

### Symbol Collection

```rust
use pact_parser::{Parser, ast::*};
use std::collections::HashSet;

struct SymbolCollector {
    symbols: HashSet<String>,
}

impl SymbolCollector {
    fn new() -> Self {
        Self {
            symbols: HashSet::new(),
        }
    }
    
    fn collect_from_expr(&mut self, expr: &ParsedExpr<SpanInfo>) {
        match expr {
            ParsedExpr::Var(name, _) => {
                self.symbols.insert(name.as_str().to_string());
            }
            ParsedExpr::App(func, args, _) => {
                self.collect_from_expr(func);
                for arg in args {
                    self.collect_from_expr(arg);
                }
            }
            ParsedExpr::Conditional(test, then_expr, else_expr, _) => {
                self.collect_from_expr(test);
                self.collect_from_expr(then_expr);
                self.collect_from_expr(else_expr);
            }
            ParsedExpr::Lambda(args, body, _) => {
                for (arg, _) in args {
                    self.symbols.insert(arg.name.as_str().to_string());
                }
                self.collect_from_expr(body);
            }
            ParsedExpr::Let(bindings, body, _) => {
                for (arg, expr, _) in bindings {
                    self.symbols.insert(arg.name.as_str().to_string());
                    self.collect_from_expr(expr);
                }
                self.collect_from_expr(body);
            }
            _ => {} // Literals don't have symbols
        }
    }
    
    fn collect_from_module(&mut self, module: &ParsedModule<SpanInfo>) {
        // Module name
        self.symbols.insert(module.name.as_str().to_string());
        
        // Governance
        match &module.governance {
            Governance::KeyGov(name) | Governance::CapGov(name) => {
                self.symbols.insert(name.as_str().to_string());
            }
        }
        
        // Definitions
        for def in &module.definitions {
            match def {
                ParsedDef::Defun(defun) => {
                    self.symbols.insert(defun.name.as_str().to_string());
                    self.collect_from_expr(&defun.term);
                }
                ParsedDef::Defcap(defcap) => {
                    self.symbols.insert(defcap.name.as_str().to_string());
                    self.collect_from_expr(&defcap.term);
                }
                ParsedDef::Defschema(schema) => {
                    self.symbols.insert(schema.name.as_str().to_string());
                }
                ParsedDef::Deftable(table) => {
                    self.symbols.insert(table.name.as_str().to_string());
                }
                ParsedDef::Defconst(const_def) => {
                    self.symbols.insert(const_def.name.as_str().to_string());
                    self.collect_from_expr(&const_def.term);
                }
                ParsedDef::Defpact(defpact) => {
                    self.symbols.insert(defpact.name.as_str().to_string());
                    self.collect_from_expr(&defpact.term);
                }
            }
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = r#"
    (module test GOVERNANCE
      (defun calculate (x y)
        (let ((result (+ x y)))
          (if (> result 10) result 0))))
    "#;
    
    let program = Parser::parse_program_from_source(source)?;
    let mut collector = SymbolCollector::new();
    
    match &program[0] {
        ParsedTopLevel::TLModule(module) => {
            collector.collect_from_module(module);
        }
        _ => {}
    }
    
    println!("Symbols found: {:?}", collector.symbols);
    
    Ok(())
}
```

### Error Handling and Recovery

```rust
use pact_parser::{Parser, error::ParseError};

fn handle_parse_errors(source: &str) {
    match Parser::parse_program_from_source(source) {
        Ok(program) => {
            println!("Successfully parsed {} top-level items", program.len());
        }
        Err(ParseError::UnexpectedToken { expected, found, position }) => {
            println!("Syntax error at position {}: expected {}, found {}", 
                    position, expected, found);
            
            // Show context around the error
            let start = position.saturating_sub(10);
            let end = (position + 10).min(source.len());
            let context = &source[start..end];
            println!("Context: '{}'", context);
        }
        Err(ParseError::UnexpectedEOF { expected }) => {
            println!("Unexpected end of file, expected: {}", expected);
        }
        Err(ParseError::InvalidNumber(msg)) => {
            println!("Invalid number: {}", msg);
        }
        Err(ParseError::Expected(what)) => {
            println!("Expected: {}", what);
        }
        Err(ParseError::LexError(msg)) => {
            println!("Lexer error: {}", msg);
        }
    }
}

fn main() {
    let test_cases = vec![
        // Valid code
        "(+ 1 2)",
        
        // Missing closing paren
        "(+ 1 2",
        
        // Empty application
        "()",
        
        // Invalid number
        "(+ 1 999999999999999999999999999999999999)",
        
        // Unterminated string
        r#"(print "hello"#,
    ];
    
    for (i, case) in test_cases.iter().enumerate() {
        println!("\nTest case {}: {}", i + 1, case);
        handle_parse_errors(case);
    }
}
```

### Pretty Printing AST

```rust
use pact_parser::{Parser, ast::*};

struct PrettyPrinter {
    indent_level: usize,
}

impl PrettyPrinter {
    fn new() -> Self {
        Self { indent_level: 0 }
    }
    
    fn indent(&self) -> String {
        "  ".repeat(self.indent_level)
    }
    
    fn print_expr(&mut self, expr: &ParsedExpr<SpanInfo>) -> String {
        match expr {
            ParsedExpr::Constant(literal, _) => {
                format!("{:?}", literal)
            }
            ParsedExpr::Var(name, _) => {
                name.as_str().to_string()
            }
            ParsedExpr::App(func, args, _) => {
                let func_str = self.print_expr(func);
                
                if args.is_empty() {
                    format!("({})", func_str)
                } else {
                    let mut result = format!("({}", func_str);
                    for arg in args {
                        result.push(' ');
                        result.push_str(&self.print_expr(arg));
                    }
                    result.push(')');
                    result
                }
            }
            ParsedExpr::Conditional(test, then_expr, else_expr, _) => {
                format!("(if {} {} {})",
                       self.print_expr(test),
                       self.print_expr(then_expr),
                       self.print_expr(else_expr))
            }
            ParsedExpr::Lambda(args, body, _) => {
                let args_str = args.iter()
                    .map(|(arg, _)| arg.name.as_str())
                    .collect::<Vec<_>>()
                    .join(" ");
                format!("(lambda ({}) {})", args_str, self.print_expr(body))
            }
            ParsedExpr::Let(bindings, body, _) => {
                let mut result = String::from("(let (");
                for (i, (arg, expr, _)) in bindings.iter().enumerate() {
                    if i > 0 { result.push(' '); }
                    result.push_str(&format!("({} {})", 
                                            arg.name.as_str(), 
                                            self.print_expr(expr)));
                }
                result.push_str(&format!(") {})", self.print_expr(body)));
                result
            }
            _ => format!("<other-expr: {:?}>", expr),
        }
    }
    
    fn print_program(&mut self, program: &Program<SpanInfo>) -> String {
        let mut result = String::new();
        
        for (i, top_level) in program.iter().enumerate() {
            if i > 0 { result.push('\n'); }
            
            match top_level {
                ParsedTopLevel::TLModule(module) => {
                    result.push_str(&format!("{}(module {}", self.indent(), module.name));
                    // Add governance, definitions, etc.
                    result.push(')');
                }
                ParsedTopLevel::TLTerm(expr) => {
                    result.push_str(&self.print_expr(expr));
                }
                _ => {
                    result.push_str(&format!("{}<other-top-level>", self.indent()));
                }
            }
        }
        
        result
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = r#"
    (+ (* 2 3) (if (> x 0) x (- x)))
    "#;
    
    let program = Parser::parse_program_from_source(source)?;
    let mut printer = PrettyPrinter::new();
    let formatted = printer.print_program(&program);
    
    println!("Formatted AST:\n{}", formatted);
    
    Ok(())
}
```

## Performance Examples

### Benchmarking Parser Performance

```rust
use std::time::Instant;
use pact_parser::Parser;

fn benchmark_parsing() -> Result<(), Box<dyn std::error::Error>> {
    let test_cases = vec![
        ("small_expr", "(+ 1 2)"),
        ("function_def", r#"(defun test (x:integer) (+ x 1))"#),
        ("complex_expr", r#"(let ((x 10) (y 20)) (if (> (+ x y) 25) "big" "small"))"#),
        ("module", include_str!("../test-files/example-module.pact")),
    ];
    
    for (name, source) in test_cases {
        let start = Instant::now();
        let result = Parser::parse_program_from_source(source);
        let duration = start.elapsed();
        
        match result {
            Ok(program) => {
                println!("{}: {} items parsed in {:?}", 
                        name, program.len(), duration);
            }
            Err(e) => {
                println!("{}: parse error in {:?}: {:?}", name, duration, e);
            }
        }
    }
    
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    benchmark_parsing()
}
```

### Memory Usage Analysis

```rust
use pact_parser::{Parser, ast::*};
use std::mem;

fn analyze_ast_memory(program: &Program<SpanInfo>) {
    let mut total_size = 0;
    let mut node_count = 0;
    
    fn analyze_expr(expr: &ParsedExpr<SpanInfo>, total: &mut usize, count: &mut usize) {
        *total += mem::size_of_val(expr);
        *count += 1;
        
        match expr {
            ParsedExpr::App(func, args, _) => {
                analyze_expr(func, total, count);
                for arg in args {
                    analyze_expr(arg, total, count);
                }
            }
            ParsedExpr::Conditional(test, then_expr, else_expr, _) => {
                analyze_expr(test, total, count);
                analyze_expr(then_expr, total, count);
                analyze_expr(else_expr, total, count);
            }
            ParsedExpr::Lambda(_, body, _) => {
                analyze_expr(body, total, count);
            }
            ParsedExpr::Let(bindings, body, _) => {
                for (_, expr, _) in bindings {
                    analyze_expr(expr, total, count);
                }
                analyze_expr(body, total, count);
            }
            _ => {} // Leaf nodes
        }
    }
    
    for top_level in program {
        total_size += mem::size_of_val(top_level);
        node_count += 1;
        
        match top_level {
            ParsedTopLevel::TLTerm(expr) => {
                analyze_expr(expr, &mut total_size, &mut node_count);
            }
            ParsedTopLevel::TLModule(module) => {
                total_size += mem::size_of_val(module);
                // Analyze module definitions...
            }
            _ => {}
        }
    }
    
    println!("AST Memory Analysis:");
    println!("  Total nodes: {}", node_count);
    println!("  Total size: {} bytes", total_size);
    println!("  Average per node: {:.1} bytes", total_size as f64 / node_count as f64);
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = r#"
    (module test GOVERNANCE
      (defun factorial (n:integer):integer
        (if (<= n 1) 
            1 
            (* n (factorial (- n 1))))))
    "#;
    
    let program = Parser::parse_program_from_source(source)?;
    analyze_ast_memory(&program);
    
    Ok(())
}
```

These examples demonstrate the practical usage of the Pact parser for various applications, from basic parsing to advanced AST manipulation and analysis. The parser's rich AST structure and comprehensive error handling make it suitable for building sophisticated development tools and language processors.