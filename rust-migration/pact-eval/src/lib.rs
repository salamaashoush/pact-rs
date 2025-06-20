//! Pact Static Analysis
//!
//! This crate provides static analysis tools for Pact including:
//! - Type checking
//! - Constant evaluation
//! - Literal evaluation
//!
//! Note: Runtime evaluation has moved to pact-compiler which integrates
//! with the CEK machine in pact-cek.

pub mod const_eval;
pub mod literal_eval;
pub mod type_checker;

#[cfg(test)]
pub mod integration_tests;

// Re-export main types
pub use const_eval::*;
pub use literal_eval::*;
pub use type_checker::{TypeCheckContext, TypedTerm, type_check_module};
