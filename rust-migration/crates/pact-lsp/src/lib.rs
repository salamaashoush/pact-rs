//! Language Server Protocol implementation for Pact
//!
//! This crate provides LSP support for the Pact smart contract language,
//! including diagnostics, completions, hover information, and more.

pub mod backend;
pub mod capabilities;
pub mod code_actions;
pub mod completions;
pub mod diagnostics;
pub mod document;
pub mod formatting;
pub mod hover;
pub mod inlay_hints;
pub mod live_validation;
pub mod semantic;
pub mod semantic_highlighting;
pub mod server;
pub mod symbols;
pub mod workspace_symbols;

// Re-export main types
pub use backend::PactLanguageBackend;
pub use server::{run_lsp_server, PactLanguageServer};