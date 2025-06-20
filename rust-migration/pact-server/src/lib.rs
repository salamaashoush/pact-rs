//! Pact HTTP API Server
//!
//! This crate implements the HTTP API server for Pact, providing endpoints
//! for command submission, polling, listening, and local execution.
//!
//! This is the Rust equivalent of the Haskell pact-request-api's server module.

pub mod api;
pub mod command;
pub mod config;
pub mod handlers;
pub mod server;
pub mod types;

// Re-export main types
pub use api::PactApi;
pub use command::{Command, CommandResult, Payload, RequestKey};
pub use config::ServerConfig;
pub use server::run_server;
pub use types::{ApiRequest, ApiResponse, ExecRequest, ContRequest};