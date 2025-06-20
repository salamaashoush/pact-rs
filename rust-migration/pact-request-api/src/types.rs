//! API request and response types

use crate::command::{Command, CommandResult, RequestKey};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Base API request wrapper
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiRequest<T> {
    /// List of commands
    pub cmds: Vec<T>,
}

/// Base API response wrapper
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiResponse<T> {
    /// Response data
    #[serde(flatten)]
    pub data: T,
}

/// Send endpoint request
pub type SendRequest = ApiRequest<Command>;

/// Send endpoint response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SendResponse {
    /// Request keys for submitted commands
    #[serde(rename = "requestKeys")]
    pub request_keys: Vec<RequestKey>,
}

/// Poll endpoint request
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PollRequest {
    /// Request keys to poll for
    #[serde(rename = "requestKeys")]
    pub request_keys: Vec<RequestKey>,
}

/// Poll endpoint response
pub type PollResponse = HashMap<RequestKey, CommandResult>;

/// Listen endpoint request
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListenRequest {
    /// Request key to wait for
    pub listen: RequestKey,
}

/// Listen endpoint response
pub type ListenResponse = CommandResult;

/// Local endpoint request
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LocalRequest {
    /// Command to execute locally
    #[serde(flatten)]
    pub cmd: Command,
}

/// Local endpoint response
pub type LocalResponse = CommandResult;

/// Exec request for simple API
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecRequest {
    /// Code to execute
    pub code: String,
    
    /// Optional environment data
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<HashMap<String, serde_json::Value>>,
    
    /// Optional key pairs for signing
    #[serde(rename = "keyPairs", skip_serializing_if = "Option::is_none")]
    pub key_pairs: Option<Vec<KeyPair>>,
    
    /// Optional metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub meta: Option<serde_json::Value>,
    
    /// Optional nonce
    #[serde(skip_serializing_if = "Option::is_none")]
    pub nonce: Option<String>,
}

/// Continuation request for simple API
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContRequest {
    /// Pact ID
    #[serde(rename = "pactId")]
    pub pact_id: String,
    
    /// Step number
    pub step: u32,
    
    /// Whether this is a rollback
    #[serde(default)]
    pub rollback: bool,
    
    /// Optional environment data
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<HashMap<String, serde_json::Value>>,
    
    /// Optional key pairs for signing
    #[serde(rename = "keyPairs", skip_serializing_if = "Option::is_none")]
    pub key_pairs: Option<Vec<KeyPair>>,
    
    /// Optional proof
    #[serde(skip_serializing_if = "Option::is_none")]
    pub proof: Option<String>,
}

/// Key pair for signing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KeyPair {
    /// Public key (hex)
    pub public: String,
    
    /// Secret key (hex)
    pub secret: String,
}

/// Version information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VersionInfo {
    /// Server version
    pub version: String,
    
    /// API version
    #[serde(rename = "apiVersion")]
    pub api_version: String,
    
    /// Chain web version (if applicable)
    #[serde(rename = "chainwebVersion", skip_serializing_if = "Option::is_none")]
    pub chainweb_version: Option<String>,
    
    /// Node version (if applicable)
    #[serde(rename = "nodeVersion", skip_serializing_if = "Option::is_none")]
    pub node_version: Option<String>,
}

/// Health check response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthResponse {
    /// Health status
    pub status: HealthStatus,
    
    /// Optional message
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
    
    /// Component statuses
    #[serde(skip_serializing_if = "Option::is_none")]
    pub components: Option<HashMap<String, HealthStatus>>,
}

/// Health status
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum HealthStatus {
    Healthy,
    Degraded,
    Unhealthy,
}