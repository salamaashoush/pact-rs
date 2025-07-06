//! RPC message types

use pact_core::values::PactValue;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Pact RPC message types
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum PactRPC {
    /// Execute new code
    #[serde(rename = "exec")]
    Exec(ExecMsg),
    
    /// Continue a pact execution
    #[serde(rename = "cont")]  
    Cont(ContMsg),
}

/// Execute message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecMsg {
    /// Code to execute
    pub code: String,
    
    /// Environment data
    #[serde(default)]
    pub data: HashMap<String, PactValue>,
    
    /// Optional capabilities to grant
    #[serde(skip_serializing_if = "Option::is_none")]
    pub caps: Option<Vec<UserCapability>>,
}

/// Continue message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContMsg {
    /// Pact ID to continue
    #[serde(rename = "pactId")]
    pub pact_id: String,
    
    /// Step number
    pub step: u32,
    
    /// Whether this is a rollback
    #[serde(default)]
    pub rollback: bool,
    
    /// Environment data
    #[serde(default)]
    pub data: HashMap<String, PactValue>,
    
    /// Proof for SPV
    #[serde(skip_serializing_if = "Option::is_none")]
    pub proof: Option<String>,
}

// Use the unified Capability type from command module
pub use crate::command::Capability as UserCapability;