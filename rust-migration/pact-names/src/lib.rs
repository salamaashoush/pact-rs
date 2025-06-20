//! Module names and qualification system for Pact
//!
//! This crate provides module naming, namespaces, and name resolution.

pub mod parsed;

pub use parsed::{BareName, DynamicName, FullyQualifiedName, HashedModuleName, ParsedName};

/// Fixed hash length (32 bytes / 256 bits) - matches Haskell Hash
pub const HASH_LENGTH: usize = 32;

/// Pact hash type - matches Haskell `newtype Hash = Hash { unHash :: ShortByteString }`
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PactHash {
    /// Raw hash bytes (always 32 bytes)
    bytes: [u8; HASH_LENGTH],
}

impl PactHash {
    /// Create a new hash from bytes
    pub fn new(bytes: [u8; HASH_LENGTH]) -> Self {
        PactHash { bytes }
    }

    /// Create from slice (validates length)
    pub fn from_slice(bytes: &[u8]) -> Result<Self, String> {
        if bytes.len() != HASH_LENGTH {
            return Err(format!(
                "Hash must be {} bytes, got {}",
                HASH_LENGTH,
                bytes.len()
            ));
        }

        let mut hash_bytes = [0u8; HASH_LENGTH];
        hash_bytes.copy_from_slice(bytes);
        Ok(PactHash::new(hash_bytes))
    }

    /// Get the raw bytes
    pub fn bytes(&self) -> &[u8; HASH_LENGTH] {
        &self.bytes
    }

    /// Convert to Base64URL unpadded encoding (matches Haskell display)
    pub fn to_base64url(&self) -> String {
        use base64ct::{Base64UrlUnpadded, Encoding};
        let mut buf = vec![0u8; 43]; // 32 bytes -> 43 chars base64
        let encoded = Base64UrlUnpadded::encode(&self.bytes, &mut buf).unwrap();
        encoded.to_string()
    }

    /// Parse from Base64URL unpadded string
    pub fn from_base64url(s: &str) -> Result<Self, String> {
        use base64ct::{Base64UrlUnpadded, Encoding};
        let mut buf = vec![0u8; HASH_LENGTH];
        let decoded = Base64UrlUnpadded::decode(s.as_bytes(), &mut buf)
            .map_err(|e| format!("Invalid Base64URL encoding: {}", e))?;
        if decoded.len() != HASH_LENGTH {
            return Err(format!(
                "Decoded hash must be {} bytes, got {}",
                HASH_LENGTH,
                decoded.len()
            ));
        }
        let mut hash_bytes = [0u8; HASH_LENGTH];
        hash_bytes.copy_from_slice(decoded);
        Ok(PactHash::new(hash_bytes))
    }

    /// Convert to hex string (for debugging)
    pub fn to_hex(&self) -> String {
        hex::encode(&self.bytes)
    }

    /// Parse from hex string
    pub fn from_hex(s: &str) -> Result<Self, String> {
        let bytes = hex::decode(s).map_err(|e| format!("Invalid hex encoding: {}", e))?;
        Self::from_slice(&bytes)
    }
}

impl fmt::Display for PactHash {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_base64url())
    }
}

impl AsRef<[u8]> for PactHash {
    fn as_ref(&self) -> &[u8] {
        &self.bytes
    }
}
use serde::{Deserialize, Serialize};
use std::fmt;

/// Module name representation - matches Haskell ModuleName
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ModuleName {
    /// Module name string
    pub name: String,
    /// Optional namespace
    pub namespace: Option<String>,
}

impl ModuleName {
    /// Create a simple module name without namespace
    pub fn simple<S: Into<String>>(name: S) -> Self {
        ModuleName {
            name: name.into(),
            namespace: None,
        }
    }

    /// Create a module name with namespace
    pub fn namespaced<S1: Into<String>, S2: Into<String>>(namespace: S1, name: S2) -> Self {
        ModuleName {
            name: name.into(),
            namespace: Some(namespace.into()),
        }
    }

    /// Render as qualified string
    pub fn render(&self) -> String {
        match &self.namespace {
            Some(ns) => format!("{}.{}", ns, self.name),
            None => self.name.clone(),
        }
    }

    /// Parse from qualified string
    pub fn parse(s: &str) -> Self {
        if let Some(dot_pos) = s.rfind('.') {
            let namespace = s[..dot_pos].to_string();
            let name = s[dot_pos + 1..].to_string();
            ModuleName::namespaced(namespace, name)
        } else {
            ModuleName::simple(s)
        }
    }
}

impl fmt::Display for ModuleName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.render())
    }
}

/// Namespace name
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct NamespaceName(pub String);

impl NamespaceName {
    pub fn new<S: Into<String>>(name: S) -> Self {
        NamespaceName(name.into())
    }
    
    /// Parse from string with validation
    pub fn parse(s: &str) -> Result<Self, String> {
        if s.is_empty() {
            return Err("Namespace name cannot be empty".to_string());
        }
        // Add additional validation if needed
        Ok(NamespaceName(s.to_string()))
    }
}

impl fmt::Display for NamespaceName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Module hash for versioning
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ModuleHash(pub PactHash);

impl ModuleHash {
    pub fn new(hash: PactHash) -> Self {
        ModuleHash(hash)
    }
}

impl fmt::Display for ModuleHash {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

// KeySetName is now defined in pact-shared-types to avoid duplication
pub use pact_shared_types::KeySetName;

/// Qualified name for functions, schemas, etc. within modules
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct QualifiedName {
    /// Module name
    pub module: ModuleName,
    /// Local name within the module  
    pub name: String,
}

impl QualifiedName {
    /// Create a new qualified name
    pub fn new(module: ModuleName, name: String) -> Self {
        QualifiedName { module, name }
    }

    /// Create from module name string and local name
    pub fn from_strs(module_name: &str, name: &str) -> Self {
        QualifiedName {
            module: ModuleName::parse(module_name),
            name: name.to_string(),
        }
    }

    /// Render as fully qualified string
    pub fn render(&self) -> String {
        format!("{}.{}", self.module.render(), self.name)
    }
}

impl fmt::Display for QualifiedName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.render())
    }
}

#[cfg(test)]
mod tests;
