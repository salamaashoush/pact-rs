//! Cryptographic utilities for command signing

use anyhow::Result;
use pact_crypto::{
    signature::{Ed25519KeyPair, parse_ed25519_secret_key_hex, parse_ed25519_public_key_hex, verify_ed25519_signature},
    hash::pact_hash
};

/// Generate a new ED25519 keypair
pub fn generate_keypair() -> Result<(String, String)> {
    let keypair = Ed25519KeyPair::generate()
        .map_err(|e| anyhow::anyhow!("Failed to generate keypair: {}", e))?;
    
    let public_hex = hex::encode(keypair.public.as_bytes());
    let secret_hex = hex::encode(keypair.secret.as_bytes());
    
    Ok((public_hex, secret_hex))
}

/// Sign a message with ED25519
pub fn sign_ed25519(message: &[u8], secret_key_hex: &str) -> Result<String> {
    let signing_key = parse_ed25519_secret_key_hex(secret_key_hex)
        .map_err(|e| anyhow::anyhow!("Failed to parse secret key: {}", e))?;
    
    let hash = pact_hash(message);
    let signature = pact_crypto::signature::sign_ed25519(&signing_key, &hash);
    Ok(hex::encode(signature.to_bytes()))
}

/// Verify an ED25519 signature
pub fn verify_ed25519(
    message: &[u8],
    signature_hex: &str,
    public_key_hex: &str,
) -> Result<bool> {
    let verifying_key = parse_ed25519_public_key_hex(public_key_hex)
        .map_err(|e| anyhow::anyhow!("Failed to parse public key: {}", e))?;
    
    let signature = pact_crypto::signature::parse_ed25519_signature_hex(signature_hex)
        .map_err(|e| anyhow::anyhow!("Failed to parse signature: {}", e))?;
    
    let hash = pact_hash(message);
    
    Ok(verify_ed25519_signature(&hash, &verifying_key, &signature).is_ok())
}

/// Hash command for signing
pub fn hash_command(command_json: &str) -> String {
    let hash = pact_hash(command_json.as_bytes());
    hash.to_hex()
}