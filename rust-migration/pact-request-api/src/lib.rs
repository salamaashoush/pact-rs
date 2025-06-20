//! Pact Request API - Public Interface
//!
//! This crate provides the public API interface for Pact, serving as the primary
//! point of interaction for downstream users like chainweb-node and pact-cli.
//!
//! This is the Rust equivalent of the Haskell pact-request-api library.
//!
//! # Main Components
//!
//! - **Command Types**: Core command and result types for blockchain operations
//! - **Client API**: Functions for building and signing commands
//! - **Server API**: HTTP server implementation for Pact services
//! - **RPC Types**: Message types for remote procedure calls
//! - **Crypto**: Cryptographic operations for command signing

pub mod client;
pub mod command;
pub mod crypto;
pub mod rpc;
pub mod types;
pub mod utils;

// Re-export main types at crate root for convenience
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

/// Prelude module for convenient imports
pub mod prelude {
    pub use crate::command::*;
    pub use crate::client::*;
    pub use crate::rpc::*;
}