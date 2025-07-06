//! Pact Syntax Library
//!
//! This crate provides comprehensive lexical analysis and parsing for the Pact smart contract language.
//! It consolidates what were previously separate lexer and parser crates into a unified syntax library:
//!
//! - **Lexer**: Tokenization and lexical analysis (`lexer` module)
//! - **Parser**: Syntax analysis and AST construction (`parser` module)
//!
//! This design follows common language implementation patterns where lexing and parsing
//! are tightly coupled and benefit from being in the same compilation unit.

pub mod lexer;
pub mod parser;

// Re-export common types for convenience
pub use lexer::{Token, lex, Lexer};
pub use parser::{
    Parser, ParsedExpr, ParsedModule, 
    Program, ParsedTopLevel, AstArena
};

// Re-export pact-core types that are commonly used with syntax
pub use pact_core::{
    PactValue, SpanInfo, PactError, PactErrorI,
    ModuleName, QualifiedName
};

/// Version information for the pact-syntax crate
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

// Re-export the parser convenience functions
pub use parser::parse_expression;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_expression() {
        let result = parse_expression("(+ 1 2)");
        assert!(result.is_ok());
    }

    #[test]
    fn test_lex_simple_expression() {
        let result = lex("(+ 1 2)");
        assert!(result.is_ok());
    }
}