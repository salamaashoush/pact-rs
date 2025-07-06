//! Pact parser crate - High-performance parsing with arena allocation
//!
//! This crate provides fast, memory-efficient syntax analysis for the Pact smart contract language.
//! It converts a stream of tokens into an Abstract Syntax Tree (AST) using optimized allocation strategies.
//!
//! # Features
//! - Arena allocation for zero-copy AST construction
//! - String interning for memory deduplication
//! - Comprehensive performance statistics
//! - Clean, fast implementation
//!
//! # Example
//! ```no_run
//! use pact_syntax::{parse, parse_with_stats};
//!
//! // Basic parsing
//! let source = "(module test 'keyset (defun hello () true))";
//! let ast = parse(source).unwrap();
//!
//! // Parse with performance statistics  
//! let (_ast, stats) = parse_with_stats(source);
//! println!("Memory usage: {} bytes", stats.arena_stats.current_memory);
//! ```

pub mod ast;
pub mod ast_arena;
pub mod error;
pub mod parser;

pub use ast::*;
pub use ast_arena::{ArenaStats, AstArena};
pub use error::Result;
pub use parser::{parse, parse_expression, parse_with_stats, Parser, ParserStats};

// Type aliases for convenience
pub type Expr<I> = ParsedExpr<I>;
pub type TopLevel<I> = ParsedTopLevel<I>;
