//! Pact lexer - fast, Haskell-compatible lexical analysis
//!
//! ```rust
//! use pact_lexer::{lex, Token};
//! use pact_shared_types::SpanInfo;
//!
//! let tokens = lex("(defun hello () \"world\")").unwrap();
//! for (token, span) in tokens {
//!     println!("{:?} at {}..{}", token, span.start, span.end);
//! }
//! ```

pub mod token;

#[cfg(test)]
pub mod property_tests;

pub use token::{lex, Lexer, MemoryUsage, Token};
pub use pact_shared_types::SpanInfo;
