//! Cryptographic commands for the Pact CLI
//!
//! Provides key generation and cryptographic utilities using pact-crypto.

use anyhow::{Context, Result};
use colored::*;
use pact_crypto::signature::Ed25519KeyPair;
use std::fs;
use std::path::Path;

/// Generate an ED25519 keypair
pub fn genkey(output_prefix: Option<String>) -> Result<()> {
    // Generate the keypair using pact-crypto
    let keypair = Ed25519KeyPair::generate()
        .context("Failed to generate ED25519 keypair")?;
    
    // Convert keys to hex format
    let public_key_hex = hex::encode(keypair.public.as_bytes());
    let secret_key_hex = hex::encode(keypair.secret.to_bytes());
    
    if let Some(prefix) = output_prefix {
        // Write to files
        let pub_file = format!("{}.pub", prefix);
        let key_file = format!("{}.key", prefix);
        
        fs::write(&pub_file, &public_key_hex)
            .with_context(|| format!("Failed to write public key to {}", pub_file))?;
        
        fs::write(&key_file, &secret_key_hex)
            .with_context(|| format!("Failed to write secret key to {}", key_file))?;
        
        println!("{}", "ED25519 keypair generated successfully!".bright_green());
        println!("Public key saved to: {}", pub_file.bright_blue());
        println!("Secret key saved to: {}", key_file.bright_blue());
        
        // Display warning about secret key
        println!();
        println!("{}", "⚠️  WARNING: Keep your secret key secure!".bright_yellow());
        println!("{}", "    Never share it or commit it to version control.".yellow());
    } else {
        // Output to stdout
        println!("{}", "ED25519 Keypair Generated:".bright_green());
        println!();
        println!("{}: {}", "Public".bright_blue(), public_key_hex);
        println!("{}: {}", "Secret".bright_red(), secret_key_hex);
        println!();
        println!("{}", "⚠️  WARNING: This is your secret key - keep it secure!".bright_yellow());
    }
    
    Ok(())
}

/// Sign a message with an ED25519 secret key
pub fn sign(secret_key_file: &Path, message_file: &Path, output: Option<&Path>) -> Result<()> {
    use pact_crypto::signature::{sign_ed25519, parse_ed25519_secret_key_hex};
    
    // Read secret key
    let secret_key_hex = fs::read_to_string(secret_key_file)
        .with_context(|| format!("Failed to read secret key from {:?}", secret_key_file))?
        .trim()
        .to_string();
    
    // Parse secret key using pact-crypto
    let signing_key = parse_ed25519_secret_key_hex(&secret_key_hex)
        .context("Failed to parse secret key")?;
    
    // Read message
    let message = fs::read_to_string(message_file)
        .with_context(|| format!("Failed to read message from {:?}", message_file))?;
    
    // Hash the message using Blake2b
    use pact_crypto::hash::pact_hash;
    let hash = pact_hash(message.as_bytes());
    
    // Sign the hash
    let signature = sign_ed25519(&signing_key, &hash);
    let signature_hex = hex::encode(signature.to_bytes());
    
    if let Some(output_path) = output {
        fs::write(output_path, &signature_hex)
            .with_context(|| format!("Failed to write signature to {:?}", output_path))?;
        println!("{}", "Signature created successfully!".bright_green());
        println!("Signature saved to: {:?}", output_path);
    } else {
        println!("{}: {}", "Signature".bright_green(), signature_hex);
    }
    
    Ok(())
}

/// Verify a signature
pub fn verify(
    public_key_file: &Path, 
    message_file: &Path, 
    signature_file: &Path
) -> Result<()> {
    use pact_crypto::signature::{parse_ed25519_public_key_hex, verify_ed25519_signature, parse_ed25519_signature_hex};
    
    // Read public key
    let public_key_hex = fs::read_to_string(public_key_file)
        .with_context(|| format!("Failed to read public key from {:?}", public_key_file))?
        .trim()
        .to_string();
    
    // Parse public key
    let verifying_key = parse_ed25519_public_key_hex(&public_key_hex)
        .context("Failed to parse public key")?;
    
    // Read message
    let message = fs::read_to_string(message_file)
        .with_context(|| format!("Failed to read message from {:?}", message_file))?;
    
    // Hash the message using Blake2b
    use pact_crypto::hash::pact_hash;
    let hash = pact_hash(message.as_bytes());
    
    // Read signature
    let signature_hex = fs::read_to_string(signature_file)
        .with_context(|| format!("Failed to read signature from {:?}", signature_file))?
        .trim()
        .to_string();
    
    // Parse signature using pact-crypto
    let signature = parse_ed25519_signature_hex(&signature_hex)
        .context("Failed to parse signature")?;
    
    // Verify signature
    match verify_ed25519_signature(&hash, &verifying_key, &signature) {
        Ok(()) => {
            println!("{}", "✓ Signature is valid!".bright_green());
            Ok(())
        }
        Err(_) => {
            println!("{}", "✗ Signature verification failed!".bright_red());
            std::process::exit(1);
        }
    }
}