//! Pact HTTP API Server with Integrated Request API
//!
//! This crate combines the HTTP server implementation with the public API interface,
//! providing a complete solution for Pact API services.
//!
//! This consolidates the Haskell pact-request-api and server modules into a single crate.
//!
//! # Main Components
//!
//! ## Public API (from pact-request-api)
//! - **Command Types**: Core command and result types for blockchain operations
//! - **Client API**: Functions for building and signing commands
//! - **RPC Types**: Message types for remote procedure calls
//! - **Crypto**: Cryptographic operations for command signing
//!
//! ## HTTP Server
//! - **Server**: HTTP server implementation for Pact services
//! - **Handlers**: Request handlers for different API endpoints
//! - **Configuration**: Server configuration and settings

// Public API modules (from pact-request-api)
pub mod client;
pub mod crypto;
pub mod rpc;
pub mod utils;

// Server modules
pub mod api;
pub mod config;
pub mod handlers;
pub mod server;

// Shared modules (both had these)
pub mod command;
pub mod types;

// Re-export main types from public API
pub use command::{
    Command, CommandResult, ExecPayload, ContPayload, Meta, Payload, 
    RequestKey, Signature, PactResult, PactError, PactEvent,
    CommandBuilder, Signer, Capability,
};

// Re-export PPKScheme from pact-crypto
pub use pact_crypto::PPKScheme;

pub use client::{
    mk_command, mk_unsigned_command, mk_exec, mk_cont,
    add_sigs_req, combine_sigs, sign_command,
};

pub use rpc::{PactRPC, ExecMsg, ContMsg};

pub use utils::{json_to_pact_value, pact_value_to_json};

// Re-export API types  
pub use types::{
    ApiRequest, ApiResponse, SendRequest, SendResponse,
    PollRequest, PollResponse, ListenRequest, ListenResponse,
    LocalRequest, LocalResponse, ExecRequest, ContRequest,
    HealthResponse, VersionInfo, HealthStatus, KeyPair,
};

// Re-export server types
pub use api::PactApi;
pub use config::ServerConfig;
pub use server::run_server;

/// Prelude module for convenient imports
pub mod prelude {
    pub use crate::command::*;
    pub use crate::client::*;
    pub use crate::rpc::*;
    pub use crate::server::*;
    pub use crate::api::*;
}