//! API request formatting command

use anyhow::{Context, Result};
use colored::*;
use pact_request_api::{CommandBuilder, Command, json_to_pact_value};
use pact_core::values::PactValue;
use serde_json::json;
use std::fs;
use std::path::PathBuf;

/// Format an exec command
pub fn format_exec(
    code: String,
    data_file: Option<String>,
    keypair_files: Vec<String>,
    nonce: Option<String>,
    output: Option<String>,
    chain_id: Option<String>,
) -> Result<()> {
    // Create command builder
    let mut builder = CommandBuilder::exec(code);
    
    // Set chain ID if provided
    if let Some(cid) = chain_id {
        builder = builder.chain_id(cid);
    }
    
    // Set nonce if provided
    if let Some(n) = nonce {
        builder = builder.nonce(n);
    }
    
    // Load data if provided
    if let Some(file) = data_file {
        let content = fs::read_to_string(&file)
            .with_context(|| format!("Failed to read data file: {}", file))?;
        let data: serde_json::Value = serde_json::from_str(&content)
            .with_context(|| format!("Failed to parse JSON from: {}", file))?;
        
        // Convert JSON to PactValue and add to builder
        if let serde_json::Value::Object(obj) = data {
            for (k, v) in obj {
                builder = builder.data(k, json_to_pact_value(v));
            }
        }
    }
    
    // Build and sign command
    let command = if keypair_files.is_empty() {
        builder.build_unsigned()
    } else {
        let keypairs = load_keypairs(&keypair_files)?;
        builder.build_signed(keypairs)?
    };
    
    // Create API request
    let api_request = json!({
        "cmds": [serde_json::to_string(&command)?]
    });
    
    // Output or save
    output_request(api_request, output)
}

/// Format a continuation command
pub fn format_cont(
    pact_id: String,
    step: u32,
    rollback: bool,
    data_file: Option<String>,
    keypair_files: Vec<String>,
    nonce: Option<String>,
    output: Option<String>,
    chain_id: Option<String>,
) -> Result<()> {
    // Create command builder
    let mut builder = CommandBuilder::cont(pact_id, step, rollback);
    
    // Set chain ID if provided
    if let Some(cid) = chain_id {
        builder = builder.chain_id(cid);
    }
    
    // Set nonce if provided
    if let Some(n) = nonce {
        builder = builder.nonce(n);
    }
    
    // Load data if provided
    if let Some(file) = data_file {
        let content = fs::read_to_string(&file)
            .with_context(|| format!("Failed to read data file: {}", file))?;
        let data: serde_json::Value = serde_json::from_str(&content)
            .with_context(|| format!("Failed to parse JSON from: {}", file))?;
        
        // Convert JSON to PactValue and add to builder
        if let serde_json::Value::Object(obj) = data {
            for (k, v) in obj {
                builder = builder.data(k, json_to_pact_value(v));
            }
        }
    }
    
    // Build and sign command
    let command = if keypair_files.is_empty() {
        builder.build_unsigned()
    } else {
        let keypairs = load_keypairs(&keypair_files)?;
        builder.build_signed(keypairs)?
    };
    
    // Create API request
    let api_request = json!({
        "cmds": [serde_json::to_string(&command)?]
    });
    
    // Output or save
    output_request(api_request, output)
}

/// Load keypairs from files
fn load_keypairs(keypair_files: &[String]) -> Result<Vec<(String, String)>> {
    let mut keypairs = Vec::new();
    
    for file in keypair_files {
        if file.ends_with(".pub") {
            // Just a public key file - can't sign with this
            anyhow::bail!("Cannot sign with just a public key file: {}", file);
        } else if file.ends_with(".key") {
            // Secret key file - derive public key using pact-crypto
            let secret_key_hex = fs::read_to_string(file)
                .with_context(|| format!("Failed to read secret key: {}", file))?
                .trim()
                .to_string();
            
            // Derive public key from secret key using pact-crypto
            use pact_crypto::signature::create_keypair_from_secret_hex;
            
            let (public_key_hex, _signing_key) = create_keypair_from_secret_hex(&secret_key_hex)
                .context("Failed to create keypair from secret key")?;
            
            keypairs.push((public_key_hex, secret_key_hex));
        } else {
            // Try to load as keypair prefix
            let pub_file = format!("{}.pub", file);
            let key_file = format!("{}.key", file);
            
            if PathBuf::from(&pub_file).exists() && PathBuf::from(&key_file).exists() {
                let pub_key = fs::read_to_string(&pub_file)
                    .with_context(|| format!("Failed to read public key: {}", pub_file))?
                    .trim()
                    .to_string();
                let secret_key = fs::read_to_string(&key_file)
                    .with_context(|| format!("Failed to read secret key: {}", key_file))?
                    .trim()
                    .to_string();
                keypairs.push((pub_key, secret_key));
            } else {
                anyhow::bail!("Cannot find keypair files for: {}", file);
            }
        }
    }
    
    Ok(keypairs)
}

/// Output request to stdout or file
fn output_request(api_request: serde_json::Value, output: Option<String>) -> Result<()> {
    let output_json = serde_json::to_string_pretty(&api_request)?;
    
    if let Some(output_file) = output {
        fs::write(&output_file, &output_json)
            .with_context(|| format!("Failed to write to: {}", output_file))?;
        println!("{} {}", "✓ API request saved to:".bright_green(), output_file);
    } else {
        println!("{}", "API Request:".bright_blue().bold());
        println!("{}", output_json);
    }
    
    Ok(())
}

