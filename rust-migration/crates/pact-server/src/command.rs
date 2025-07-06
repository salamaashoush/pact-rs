//! Command types - public interface matching Haskell's Pact.Core.Command.Types

use pact_crypto::{PPKScheme, hash::pact_hash, signature::sign_ed25519};
use pact_core::values::PactValue;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Request key - unique identifier for commands
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RequestKey(pub String);

impl RequestKey {
    /// Create a new request key from hash
    pub fn new(hash: String) -> Self {
        RequestKey(hash)
    }
    
    /// Generate from command content
    pub fn from_command(cmd: &Command) -> Self {
        let json = serde_json::to_string(cmd).unwrap_or_default();
        let hash = pact_hash(json.as_bytes());
        RequestKey(hash.to_base64url())
    }
}

/// Pact command - matches Haskell Command type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Command {
    /// Command hash
    pub hash: String,
    
    /// List of signatures
    pub sigs: Vec<Signature>,
    
    /// Command payload
    pub cmd: String, // JSON-encoded Payload
}

/// Signature on a command
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Signature {
    /// Signature hex string
    pub sig: String,
    
    /// Public key hex string
    #[serde(rename = "pubKey")]
    pub pub_key: String,
    
    /// Signature scheme
    #[serde(skip_serializing_if = "Option::is_none")]
    pub scheme: Option<PPKScheme>,
}

/// Command payload
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Payload {
    /// Network ID
    #[serde(rename = "networkId")]
    pub network_id: Option<String>,
    
    /// Execution payload
    pub exec: Option<ExecPayload>,
    
    /// Continuation payload
    pub cont: Option<ContPayload>,
    
    /// Metadata
    pub meta: Meta,
    
    /// Nonce
    pub nonce: String,
}

/// Execution payload
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecPayload {
    /// Code to execute
    pub code: String,
    
    /// Environment data
    #[serde(default)]
    pub data: HashMap<String, PactValue>,
}

/// Continuation payload
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContPayload {
    /// Pact ID
    #[serde(rename = "pactId")]
    pub pact_id: String,
    
    /// Step number
    pub step: u32,
    
    /// Whether this is a rollback
    pub rollback: bool,
    
    /// Environment data
    #[serde(default)]
    pub data: HashMap<String, PactValue>,
    
    /// Proof (for SPV)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub proof: Option<String>,
}

/// Command metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Meta {
    /// Chain ID
    #[serde(rename = "chainId")]
    pub chain_id: String,
    
    /// Sender account
    pub sender: String,
    
    /// Gas limit
    #[serde(rename = "gasLimit")]
    pub gas_limit: u64,
    
    /// Gas price
    #[serde(rename = "gasPrice")]
    pub gas_price: f64,
    
    /// Time to live (in seconds)
    pub ttl: u32,
    
    /// Creation time (POSIX seconds)
    #[serde(rename = "creationTime")]
    pub creation_time: u64,
}

/// Command result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CommandResult {
    /// Request key
    #[serde(rename = "reqKey")]
    pub req_key: RequestKey,
    
    /// Transaction ID
    #[serde(rename = "txId", skip_serializing_if = "Option::is_none")]
    pub tx_id: Option<u64>,
    
    /// Result payload
    pub result: PactResult,
    
    /// Gas consumed
    pub gas: u64,
    
    /// Logs emitted
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub logs: Vec<String>,
    
    /// Events emitted
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub events: Vec<PactEvent>,
    
    /// Metadata
    #[serde(rename = "metaData", skip_serializing_if = "Option::is_none")]
    pub meta_data: Option<serde_json::Value>,
}

/// Pact execution result
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "status")]
pub enum PactResult {
    #[serde(rename = "success")]
    Success { data: PactValue },
    
    #[serde(rename = "failure")]
    Failure { error: PactError },
}

/// Pact error
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PactError {
    /// Error message
    pub message: String,
    
    /// Error details
    #[serde(skip_serializing_if = "Option::is_none")]
    pub detail: Option<String>,
}

/// Pact event
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PactEvent {
    /// Event name
    pub name: String,
    
    /// Event parameters
    pub params: Vec<PactValue>,
    
    /// Module that emitted the event
    pub module: String,
    
    /// Module hash
    #[serde(rename = "moduleHash")]
    pub module_hash: String,
}

/// Capability representation for signatures and RPC
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Capability {
    /// Capability name
    pub name: String,
    
    /// Capability arguments
    pub args: Vec<PactValue>,
}

/// Command builder for easier construction
pub struct CommandBuilder {
    payload: Payload,
    signers: Vec<Signer>,
}

/// Signer information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Signer {
    /// Public key
    pub pub_key: String,
    
    /// Signature scheme
    pub scheme: Option<PPKScheme>,
    
    /// Capabilities this signer is signing for
    pub caps: Vec<Capability>,
}

impl CommandBuilder {
    /// Create a new command builder with exec payload
    pub fn exec(code: String) -> Self {
        Self {
            payload: Payload {
                network_id: None,
                exec: Some(ExecPayload {
                    code,
                    data: HashMap::new(),
                }),
                cont: None,
                meta: Meta {
                    chain_id: "0".to_string(),
                    sender: "sender00".to_string(),
                    gas_limit: 150000,
                    gas_price: 0.00000001,
                    ttl: 28800,
                    creation_time: chrono::Utc::now().timestamp() as u64,
                },
                nonce: uuid::Uuid::new_v4().to_string(),
            },
            signers: Vec::new(),
        }
    }
    
    /// Create a new command builder with continuation payload
    pub fn cont(pact_id: String, step: u32, rollback: bool) -> Self {
        Self {
            payload: Payload {
                network_id: None,
                exec: None,
                cont: Some(ContPayload {
                    pact_id,
                    step,
                    rollback,
                    data: HashMap::new(),
                    proof: None,
                }),
                meta: Meta {
                    chain_id: "0".to_string(),
                    sender: "sender00".to_string(),
                    gas_limit: 150000,
                    gas_price: 0.00000001,
                    ttl: 28800,
                    creation_time: chrono::Utc::now().timestamp() as u64,
                },
                nonce: uuid::Uuid::new_v4().to_string(),
            },
            signers: Vec::new(),
        }
    }
    
    /// Set network ID
    pub fn network_id(mut self, network_id: String) -> Self {
        self.payload.network_id = Some(network_id);
        self
    }
    
    /// Set chain ID
    pub fn chain_id(mut self, chain_id: String) -> Self {
        self.payload.meta.chain_id = chain_id;
        self
    }
    
    /// Set sender
    pub fn sender(mut self, sender: String) -> Self {
        self.payload.meta.sender = sender;
        self
    }
    
    /// Set gas limit
    pub fn gas_limit(mut self, gas_limit: u64) -> Self {
        self.payload.meta.gas_limit = gas_limit;
        self
    }
    
    /// Set gas price
    pub fn gas_price(mut self, gas_price: f64) -> Self {
        self.payload.meta.gas_price = gas_price;
        self
    }
    
    /// Set TTL
    pub fn ttl(mut self, ttl: u32) -> Self {
        self.payload.meta.ttl = ttl;
        self
    }
    
    /// Set creation time
    pub fn creation_time(mut self, time: u64) -> Self {
        self.payload.meta.creation_time = time;
        self
    }
    
    /// Set nonce
    pub fn nonce(mut self, nonce: String) -> Self {
        self.payload.nonce = nonce;
        self
    }
    
    /// Add data to exec payload
    pub fn data(mut self, key: String, value: PactValue) -> Self {
        if let Some(ref mut exec) = self.payload.exec {
            exec.data.insert(key, value);
        } else if let Some(ref mut cont) = self.payload.cont {
            cont.data.insert(key, value);
        }
        self
    }
    
    /// Add a signer
    pub fn signer(mut self, signer: Signer) -> Self {
        self.signers.push(signer);
        self
    }
    
    /// Build unsigned command
    pub fn build_unsigned(self) -> Command {
        let cmd_json = serde_json::to_string(&self.payload).unwrap();
        let hash = pact_hash(cmd_json.as_bytes()).to_hex();
        
        Command {
            hash,
            sigs: vec![],
            cmd: cmd_json,
        }
    }
    
    /// Build and sign command
    pub fn build_signed(self, keypairs: Vec<(String, String)>) -> Result<Command, anyhow::Error> {
        let mut cmd = self.build_unsigned();
        let hash_obj = pact_hash(cmd.hash.as_bytes());
        
        // Sign with each keypair
        for (pub_key, secret_key) in keypairs {
            let signing_key = pact_crypto::signature::parse_ed25519_secret_key_hex(&secret_key)
                .map_err(|e| anyhow::anyhow!("Failed to parse secret key: {}", e))?;
            
            let signature = sign_ed25519(&signing_key, &hash_obj);
            
            cmd.sigs.push(Signature {
                sig: hex::encode(signature.to_bytes()),
                pub_key: pub_key.clone(),
                scheme: Some(PPKScheme::ED25519),
            });
        }
        
        Ok(cmd)
    }
}