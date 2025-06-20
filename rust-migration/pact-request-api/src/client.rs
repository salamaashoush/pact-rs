//! Client API for building and signing commands

use crate::command::{Command, ExecPayload, ContPayload, Meta, Payload, Signature};
use anyhow::{Context, Result};
use pact_crypto::PPKScheme;
use pact_values::PactValue;
use serde_yaml;
use std::collections::HashMap;
use std::fs;
use std::path::Path;

/// Create a command from components
pub fn mk_command(
    signers: Vec<(String, Option<PPKScheme>)>,
    meta: Meta,
    nonce: String,
    network_id: Option<String>,
    payload: serde_json::Value,
) -> Command {
    let payload_obj = Payload {
        network_id,
        exec: None, // Will be set from JSON
        cont: None, // Will be set from JSON
        meta,
        nonce,
    };
    
    let cmd_json = serde_json::to_string(&payload_obj).unwrap();
    let hash = pact_crypto::hash::pact_hash(cmd_json.as_bytes()).to_hex();
    
    Command {
        hash,
        sigs: signers.into_iter().map(|(pub_key, scheme)| Signature {
            sig: String::new(), // Unsigned
            pub_key,
            scheme,
        }).collect(),
        cmd: cmd_json,
    }
}

/// Create an unsigned command
pub fn mk_unsigned_command(
    meta: Meta,
    nonce: String,
    network_id: Option<String>,
    payload: serde_json::Value,
) -> Command {
    mk_command(vec![], meta, nonce, network_id, payload)
}

/// Create an exec command
pub fn mk_exec(
    code: String,
    data: Option<HashMap<String, PactValue>>,
    meta: Meta,
    nonce: String,
    network_id: Option<String>,
) -> Command {
    let payload = Payload {
        network_id,
        exec: Some(ExecPayload {
            code,
            data: data.unwrap_or_default(),
        }),
        cont: None,
        meta,
        nonce,
    };
    
    let cmd_json = serde_json::to_string(&payload).unwrap();
    let hash = pact_crypto::hash::pact_hash(cmd_json.as_bytes()).to_hex();
    
    Command {
        hash,
        sigs: vec![],
        cmd: cmd_json,
    }
}

/// Create a continuation command
pub fn mk_cont(
    pact_id: String,
    step: u32,
    rollback: bool,
    data: Option<HashMap<String, PactValue>>,
    proof: Option<String>,
    meta: Meta,
    nonce: String,
    network_id: Option<String>,
) -> Command {
    let payload = Payload {
        network_id,
        exec: None,
        cont: Some(ContPayload {
            pact_id,
            step,
            rollback,
            data: data.unwrap_or_default(),
            proof,
        }),
        meta,
        nonce,
    };
    
    let cmd_json = serde_json::to_string(&payload).unwrap();
    let hash = pact_crypto::hash::pact_hash(cmd_json.as_bytes()).to_hex();
    
    Command {
        hash,
        sigs: vec![],
        cmd: cmd_json,
    }
}

/// Add signatures to a command from a YAML file
pub fn add_sigs_req(yaml_file: &Path, unsigned: bool) -> Result<Vec<Command>> {
    let content = fs::read_to_string(yaml_file)
        .with_context(|| format!("Failed to read YAML file: {:?}", yaml_file))?;
    
    let yaml_value: serde_yaml::Value = serde_yaml::from_str(&content)
        .context("Failed to parse YAML")?;
    
    // Parse commands from YAML
    let commands = parse_yaml_commands(yaml_value, unsigned)?;
    
    Ok(commands)
}

/// Combine signatures from multiple commands
pub fn combine_sigs(commands: Vec<Command>) -> Result<Vec<Command>> {
    use std::collections::HashMap;
    
    // Group commands by hash
    let mut grouped: HashMap<String, Vec<Command>> = HashMap::new();
    for cmd in commands {
        grouped.entry(cmd.hash.clone()).or_insert_with(Vec::new).push(cmd);
    }
    
    // Combine signatures for each unique command
    let mut result = Vec::new();
    for (_hash, cmds) in grouped {
        let mut combined = cmds[0].clone();
        
        // Collect all unique signatures
        let mut sig_map: HashMap<String, Signature> = HashMap::new();
        for cmd in cmds {
            for sig in cmd.sigs {
                sig_map.insert(sig.pub_key.clone(), sig);
            }
        }
        
        combined.sigs = sig_map.into_values().collect();
        result.push(combined);
    }
    
    Ok(result)
}

/// Sign a command with a keypair
pub fn sign_command(
    command: &mut Command,
    pub_key: String,
    secret_key: String,
    scheme: PPKScheme,
) -> Result<()> {
    match scheme {
        PPKScheme::ED25519 => {
            let signing_key = pact_crypto::signature::parse_ed25519_secret_key_hex(&secret_key)
                .map_err(|e| anyhow::anyhow!("Failed to parse secret key: {}", e))?;
            
            let hash = pact_crypto::hash::pact_hash(command.hash.as_bytes());
            let signature = pact_crypto::signature::sign_ed25519(&signing_key, &hash);
            
            // Update or add signature
            let sig_hex = hex::encode(signature.to_bytes());
            
            if let Some(existing) = command.sigs.iter_mut().find(|s| s.pub_key == pub_key) {
                existing.sig = sig_hex;
                existing.scheme = Some(scheme);
            } else {
                command.sigs.push(Signature {
                    sig: sig_hex,
                    pub_key,
                    scheme: Some(scheme),
                });
            }
            
            Ok(())
        }
        PPKScheme::WebAuthn => {
            anyhow::bail!("WebAuthn signing not yet implemented")
        }
    }
}

/// Parse commands from YAML
fn parse_yaml_commands(yaml: serde_yaml::Value, _unsigned: bool) -> Result<Vec<Command>> {
    // This is a simplified version - full implementation would handle
    // all the YAML format variations from the Haskell version
    
    let commands = if let Some(cmds) = yaml.get("cmds").and_then(|v| v.as_sequence()) {
        cmds.iter()
            .map(|cmd_yaml| {
                // Parse command from YAML
                let cmd_str = cmd_yaml.as_str()
                    .map(|s| s.to_string())
                    .or_else(|| serde_yaml::to_string(cmd_yaml).ok())
                    .context("Invalid command in YAML")?;
                
                serde_json::from_str(&cmd_str)
                    .context("Failed to parse command JSON")
            })
            .collect::<Result<Vec<Command>>>()?
    } else {
        vec![]
    };
    
    Ok(commands)
}