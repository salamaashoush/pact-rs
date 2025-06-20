# Pact Lexer Examples

This document provides practical examples of using the Pact lexer in various scenarios.

## Basic Usage

### Simple Expression Tokenization

```rust
use pact_lexer::haskell_compat_token::{Token, lex_with_spans};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = "(+ 1 2)";
    let tokens = lex_with_spans(source)?;
    
    for (token, span) in tokens {
        println!("{:?} at {}..{}", token, span.start, span.end);
    }
    
    Ok(())
}
```

**Output:**
```
TokenOpenParen at 0..1
TokenIdent("+") at 1..2
TokenInteger("1") at 3..4
TokenInteger("2") at 5..6
TokenCloseParen at 6..7
```

### Function Definition

```rust
use pact_lexer::haskell_compat_token::{Token, lex_with_spans};

fn tokenize_function() -> Result<(), Box<dyn std::error::Error>> {
    let source = r#"
    (defun add (x:integer y:integer):integer
      @doc "Add two integers"
      (+ x y))
    "#;
    
    let tokens = lex_with_spans(source)?;
    
    // Print just the token types for brevity
    for (token, _) in tokens {
        println!("{:?}", token);
    }
    
    Ok(())
}
```

**Output:**
```
TokenOpenParen
TokenDefun
TokenIdent("add")
TokenOpenParen
TokenIdent("x")
TokenColon
TokenIdent("integer")
TokenIdent("y")
TokenColon
TokenIdent("integer")
TokenCloseParen
TokenColon
TokenIdent("integer")
TokenAt
TokenIdent("doc")
TokenString("Add two integers")
TokenOpenParen
TokenIdent("+")
TokenIdent("x")
TokenIdent("y")
TokenCloseParen
TokenCloseParen
```

## Working with Different Token Types

### Numbers and Decimals

```rust
use pact_lexer::haskell_compat_token::{Token, lex_with_spans};

fn handle_numbers() -> Result<(), Box<dyn std::error::Error>> {
    let test_cases = vec![
        "42",           // Integer
        "-17",          // Negative integer
        "3.14159",      // Decimal
        "-0.5",         // Negative decimal
        "0",            // Zero
        "123.456789",   // Long decimal
    ];
    
    for case in test_cases {
        let tokens = lex_with_spans(case)?;
        if let Some((token, span)) = tokens.first() {
            match token {
                Token::TokenInteger(value) => {
                    println!("Integer '{}' parsed as: {}", case, value);
                }
                Token::TokenDecimal(value) => {
                    println!("Decimal '{}' parsed as: {}", case, value);
                }
                _ => println!("Unexpected token for '{}'", case),
            }
        }
    }
    
    Ok(())
}
```

**Output:**
```
Integer '42' parsed as: 42
Integer '-17' parsed as: -17
Decimal '3.14159' parsed as: 3.14159
Decimal '-0.5' parsed as: -0.5
Integer '0' parsed as: 0
Decimal '123.456789' parsed as: 123.456789
```

### String Literals with Escapes

```rust
use pact_lexer::haskell_compat_token::{Token, lex_with_spans};

fn handle_strings() -> Result<(), Box<dyn std::error::Error>> {
    let test_cases = vec![
        r#""hello""#,                    // Simple string
        r#""""#,                        // Empty string
        r#""hello\nworld""#,             // With newline
        r#""say \"hello\"""#,            // With quote escape
        r#""path\\to\\file""#,           // With backslash escape
        r#""tab\there""#,                // With tab
    ];
    
    for case in test_cases {
        let tokens = lex_with_spans(case)?;
        if let Some((Token::TokenString(value), _)) = tokens.first() {
            println!("Source: {:<20} → Parsed: {:?}", case, value);
        }
    }
    
    Ok(())
}
```

**Output:**
```
Source: "hello"              → Parsed: "hello"
Source: ""                   → Parsed: ""
Source: "hello\nworld"       → Parsed: "hello\nworld"
Source: "say \"hello\""      → Parsed: "say \"hello\""
Source: "path\\to\\file"     → Parsed: "path\\to\\file"
Source: "tab\there"          → Parsed: "tab\there"
```

## Module System Examples

### Complete Module

```rust
use pact_lexer::haskell_compat_token::{Token, lex_with_spans};

fn tokenize_module() -> Result<(), Box<dyn std::error::Error>> {
    let source = r#"
    (module coin GOVERNANCE
      @doc "A simple coin contract"
      
      (defschema coin-schema
        @doc "Coin account schema"
        balance:decimal
        guard:guard)
      
      (deftable coin-table:{coin-schema}
        @doc "Coin accounts table")
      
      (defcap TRANSFER (sender:string receiver:string amount:decimal)
        @doc "Capability for transfers"
        (enforce (> amount 0.0) "Positive amount"))
      
      (defun transfer (sender:string receiver:string amount:decimal)
        @doc "Transfer coins between accounts"
        (with-capability (TRANSFER sender receiver amount)
          (debit sender amount)
          (credit receiver amount))))
    "#;
    
    let tokens = lex_with_spans(source)?;
    
    // Count different token types
    let mut counts = std::collections::HashMap::new();
    for (token, _) in tokens {
        let token_name = match token {
            Token::TokenModule => "Module",
            Token::TokenDefschema => "DefSchema",
            Token::TokenDeftable => "DefTable", 
            Token::TokenDefcap => "DefCap",
            Token::TokenDefun => "DefFun",
            Token::TokenIdent(_) => "Identifier",
            Token::TokenString(_) => "String",
            Token::TokenDecimal(_) => "Decimal",
            Token::TokenOpenParen => "OpenParen",
            Token::TokenCloseParen => "CloseParen",
            Token::TokenColon => "Colon",
            Token::TokenAt => "At",
            _ => "Other",
        };
        *counts.entry(token_name).or_insert(0) += 1;
    }
    
    println!("Token counts in module:");
    for (token_type, count) in counts {
        println!("  {}: {}", token_type, count);
    }
    
    Ok(())
}
```

## Error Handling Examples

### Handling Lexer Errors

```rust
use pact_lexer::haskell_compat_token::{lex_with_spans, LexError};

fn handle_errors() {
    let error_cases = vec![
        r#""unterminated string"#,      // Missing closing quote
        "§invalid_char",                // Invalid character
        r#""invalid\escape""#,          // Invalid escape sequence
    ];
    
    for case in error_cases {
        match lex_with_spans(case) {
            Ok(tokens) => {
                println!("'{}' → {} tokens", case, tokens.len());
            }
            Err(LexError { message, position }) => {
                println!("'{}' → Error at position {}: {}", case, position, message);
            }
        }
    }
}
```

### Error Recovery

```rust
use pact_lexer::haskell_compat_token::{Token, lex_with_spans};

fn error_recovery_example() {
    // Source with an error in the middle
    let source = r#"(+ 1 §invalid 2)"#;
    
    match lex_with_spans(source) {
        Ok(tokens) => {
            println!("Tokens before error:");
            for (token, span) in tokens {
                if let Token::Error = token {
                    println!("  ERROR at {}..{}", span.start, span.end);
                } else {
                    println!("  {:?} at {}..{}", token, span.start, span.end);
                }
            }
        }
        Err(err) => {
            println!("Lexer failed: {}", err.message);
        }
    }
}
```

## Advanced Usage

### Token Filtering

```rust
use pact_lexer::haskell_compat_token::{Token, lex_with_spans};

fn filter_identifiers(source: &str) -> Result<Vec<String>, Box<dyn std::error::Error>> {
    let tokens = lex_with_spans(source)?;
    
    let identifiers: Vec<String> = tokens
        .into_iter()
        .filter_map(|(token, _)| {
            if let Token::TokenIdent(name) = token {
                Some(name)
            } else {
                None
            }
        })
        .collect();
    
    Ok(identifiers)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = "(defun transfer (sender receiver amount) (+ sender receiver))";
    let identifiers = filter_identifiers(source)?;
    
    println!("Identifiers found: {:?}", identifiers);
    // Output: ["defun", "transfer", "sender", "receiver", "amount", "+", "sender", "receiver"]
    
    Ok(())
}
```

### Source Position Tracking

```rust
use pact_lexer::haskell_compat_token::{Token, lex_with_spans, SpanInfo};

fn find_token_positions(source: &str, target: &str) -> Result<Vec<SpanInfo>, Box<dyn std::error::Error>> {
    let tokens = lex_with_spans(source)?;
    
    let positions: Vec<SpanInfo> = tokens
        .into_iter()
        .filter_map(|(token, span)| {
            match token {
                Token::TokenIdent(name) if name == target => Some(span),
                _ => None,
            }
        })
        .collect();
    
    Ok(positions)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = "(defun add (x y) (+ x y))";
    let positions = find_token_positions(source, "x")?;
    
    for span in positions {
        println!("Found 'x' at position {}..{}", span.start, span.end);
        println!("  Context: '{}'", &source[span.start..span.end]);
    }
    
    Ok(())
}
```

### Token Stream Analysis

```rust
use pact_lexer::haskell_compat_token::{Token, lex_with_spans};
use std::collections::HashMap;

#[derive(Debug)]
struct TokenStats {
    total_tokens: usize,
    unique_identifiers: usize,
    string_literals: usize,
    numeric_literals: usize,
    operators: usize,
    keywords: usize,
}

fn analyze_tokens(source: &str) -> Result<TokenStats, Box<dyn std::error::Error>> {
    let tokens = lex_with_spans(source)?;
    
    let mut unique_identifiers = std::collections::HashSet::new();
    let mut string_literals = 0;
    let mut numeric_literals = 0;
    let mut operators = 0;
    let mut keywords = 0;
    
    for (token, _) in &tokens {
        match token {
            Token::TokenIdent(name) => {
                unique_identifiers.insert(name.clone());
            }
            Token::TokenString(_) => string_literals += 1,
            Token::TokenInteger(_) | Token::TokenDecimal(_) => numeric_literals += 1,
            Token::TokenPlus | Token::TokenMinus | Token::TokenStar | Token::TokenSlash |
            Token::TokenGT | Token::TokenLT | Token::TokenEQ | Token::TokenNEQ |
            Token::TokenAnd | Token::TokenOr | Token::TokenNot => operators += 1,
            Token::TokenDefun | Token::TokenDefcap | Token::TokenModule | Token::TokenIf |
            Token::TokenLet | Token::TokenLambda => keywords += 1,
            _ => {}
        }
    }
    
    Ok(TokenStats {
        total_tokens: tokens.len(),
        unique_identifiers: unique_identifiers.len(),
        string_literals,
        numeric_literals,
        operators,
        keywords,
    })
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = r#"
    (defun calculate (x:decimal y:decimal):decimal
      @doc "Calculate something complex"
      (let ((result (+ (* x 2.0) (/ y 3.0))))
        (if (> result 10.0)
            "large"
            "small")))
    "#;
    
    let stats = analyze_tokens(source)?;
    println!("Token Analysis: {:#?}", stats);
    
    Ok(())
}
```

## Integration Examples

### With Parser

```rust
use pact_lexer::haskell_compat_token::lex_with_spans;
use pact_parser::Parser;

fn parse_expression(source: &str) -> Result<(), Box<dyn std::error::Error>> {
    // Step 1: Tokenize
    let tokens = lex_with_spans(source)?;
    println!("Lexed {} tokens", tokens.len());
    
    // Step 2: Parse
    let mut parser = Parser::new(tokens);
    let expr = parser.parse_expr()?;
    
    println!("Parsed expression: {:?}", expr);
    
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    parse_expression("(+ (* 2 3) (- 10 5))")
}
```

### Custom Token Processor

```rust
use pact_lexer::haskell_compat_token::{Token, lex_with_spans, SpanInfo};

trait TokenProcessor {
    fn process_token(&mut self, token: &Token, span: &SpanInfo);
    fn finalize(&mut self);
}

struct SyntaxHighlighter {
    output: String,
    source: String,
    last_pos: usize,
}

impl SyntaxHighlighter {
    fn new(source: String) -> Self {
        Self {
            output: String::new(),
            source,
            last_pos: 0,
        }
    }
}

impl TokenProcessor for SyntaxHighlighter {
    fn process_token(&mut self, token: &Token, span: &SpanInfo) {
        // Add any text between last position and current token
        if span.start > self.last_pos {
            self.output.push_str(&self.source[self.last_pos..span.start]);
        }
        
        // Add styled token
        let token_text = &self.source[span.start..span.end];
        let styled = match token {
            Token::TokenDefun | Token::TokenDefcap | Token::TokenModule => {
                format!("<keyword>{}</keyword>", token_text)
            }
            Token::TokenString(_) => {
                format!("<string>{}</string>", token_text)
            }
            Token::TokenInteger(_) | Token::TokenDecimal(_) => {
                format!("<number>{}</number>", token_text)
            }
            Token::TokenIdent(_) => {
                format!("<identifier>{}</identifier>", token_text)
            }
            _ => token_text.to_string(),
        };
        
        self.output.push_str(&styled);
        self.last_pos = span.end;
    }
    
    fn finalize(&mut self) {
        // Add any remaining text
        if self.last_pos < self.source.len() {
            self.output.push_str(&self.source[self.last_pos..]);
        }
    }
}

fn highlight_syntax(source: &str) -> Result<String, Box<dyn std::error::Error>> {
    let tokens = lex_with_spans(source)?;
    let mut highlighter = SyntaxHighlighter::new(source.to_string());
    
    for (token, span) in tokens {
        highlighter.process_token(&token, &span);
    }
    highlighter.finalize();
    
    Ok(highlighter.output)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = r#"(defun greet (name:string) "Hello")"#;
    let highlighted = highlight_syntax(source)?;
    println!("Highlighted: {}", highlighted);
    
    Ok(())
}
```

## Performance Examples

### Benchmarking

```rust
use std::time::Instant;
use pact_lexer::haskell_compat_token::lex_with_spans;

fn benchmark_lexing() -> Result<(), Box<dyn std::error::Error>> {
    let test_cases = vec![
        ("small", "(+ 1 2)"),
        ("medium", r#"(defun test (x:integer) (+ x 1))"#),
        ("large", include_str!("../test-files/large-module.pact")), // Load large test file
    ];
    
    for (name, source) in test_cases {
        let start = Instant::now();
        let tokens = lex_with_spans(source)?;
        let duration = start.elapsed();
        
        println!("{}: {} tokens in {:?} ({:.2} tokens/ms)", 
                name, 
                tokens.len(), 
                duration,
                tokens.len() as f64 / duration.as_millis() as f64);
    }
    
    Ok(())
}
```

### Memory Usage

```rust
use pact_lexer::haskell_compat_token::{Token, lex_with_spans};

fn analyze_memory_usage(source: &str) -> Result<(), Box<dyn std::error::Error>> {
    let tokens = lex_with_spans(source)?;
    
    let mut total_memory = 0;
    let mut string_memory = 0;
    
    for (token, _) in &tokens {
        total_memory += std::mem::size_of_val(token);
        
        match token {
            Token::TokenIdent(s) | Token::TokenString(s) | 
            Token::TokenInteger(s) | Token::TokenDecimal(s) => {
                string_memory += s.capacity();
            }
            _ => {}
        }
    }
    
    println!("Memory usage analysis:");
    println!("  Token count: {}", tokens.len());
    println!("  Token struct memory: {} bytes", total_memory);
    println!("  String data memory: {} bytes", string_memory);
    println!("  Total estimated: {} bytes", total_memory + string_memory);
    println!("  Average per token: {:.1} bytes", 
             (total_memory + string_memory) as f64 / tokens.len() as f64);
    
    Ok(())
}
```

These examples demonstrate the practical usage of the Pact lexer in various scenarios, from basic tokenization to advanced integration patterns. The lexer's design makes it easy to integrate into larger systems while providing detailed information about source code structure and errors.