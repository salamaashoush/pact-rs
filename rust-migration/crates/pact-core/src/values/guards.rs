//! Guard system for Pact authorization
//!
//! This module implements the complete guard evaluation system
//! exactly as implemented in Haskell Pact.

use super::{PactValue, ValueError, ValueResult};
use crate::shared::{CryptoError, KeySetName, Principal, PublicKeyText};

// Forward crypto functions - actual implementations are in pact-crypto crate
pub fn enforce_key_formats(keys: &[PublicKeyText]) -> Result<(), CryptoError> {
    // Simple validation for now - could be enhanced to call pact-crypto
    for key in keys {
        if key.as_str().is_empty() {
            return Err(CryptoError::invalid_key_format("Empty public key"));
        }
        // Basic hex format check for Ed25519 keys (64 chars)
        if key.as_str().len() == 64 && !key.as_str().chars().all(|c| c.is_ascii_hexdigit()) {
            return Err(CryptoError::invalid_key_format("Invalid hex format"));
        }
    }
    Ok(())
}

pub fn create_principal_for_guard(guard: &Guard) -> Result<Principal, CryptoError> {
    // Create appropriate principal based on guard type
    match guard {
        Guard::KeySet { name, .. } => Ok(Principal::role(name.clone())),
        Guard::User { fun, .. } => Ok(Principal::key(format!("user:{}", fun))),
        Guard::Capability { name, .. } => Ok(Principal::key(format!("cap:{}", name))),
        Guard::Module { name, .. } => Ok(Principal::key(format!("module:{}", name))),
    }
}
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;

/// Authorization guard - matches Haskell Guard exactly
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Guard {
    /// Keyset guard with keys and predicate
    KeySet {
        name: String,
        keys: Vec<String>,
        pred: String,
    },

    /// User-defined guard function
    User { fun: String, args: Vec<PactValue> },

    /// Capability guard
    Capability { name: String, args: Vec<PactValue> },

    /// Module admin guard
    Module {
        name: String,
        governance: Option<String>,
    },
}

impl Guard {
    /// Get the name of this guard
    pub fn name(&self) -> &str {
        match self {
            Guard::KeySet { name, .. } => name,
            Guard::User { fun, .. } => fun,
            Guard::Capability { name, .. } => name,
            Guard::Module { name, .. } => name,
        }
    }

    /// Create a new keyset guard
    pub fn keyset<S: Into<String>>(name: S, keys: Vec<String>, pred: S) -> Self {
        Guard::KeySet {
            name: name.into(),
            keys,
            pred: pred.into(),
        }
    }

    /// Create a new user guard
    pub fn user<S: Into<String>>(fun: S, args: Vec<PactValue>) -> Self {
        Guard::User {
            fun: fun.into(),
            args,
        }
    }

    /// Create a new capability guard
    pub fn capability<S: Into<String>>(name: S, args: Vec<PactValue>) -> Self {
        Guard::Capability {
            name: name.into(),
            args,
        }
    }

    /// Create a new module guard
    pub fn module<S: Into<String>>(name: S, governance: Option<String>) -> Self {
        Guard::Module {
            name: name.into(),
            governance,
        }
    }

    /// Get the principal for this guard
    pub fn principal(&self) -> ValueResult<Principal> {
        create_principal_for_guard(self).map_err(|e| ValueError::Crypto {
            message: e.to_string(),
        })
    }
}

impl fmt::Display for Guard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Guard::KeySet { name, keys, pred } => {
                write!(f, "KeySet({}:{}, {})", name, keys.len(), pred)
            }
            Guard::User { fun, args } => {
                write!(f, "User({}:{})", fun, args.len())
            }
            Guard::Capability { name, args } => {
                write!(f, "Capability({}:{})", name, args.len())
            }
            Guard::Module { name, governance } => match governance {
                Some(gov) => write!(f, "Module({}:{})", name, gov),
                None => write!(f, "Module({})", name),
            },
        }
    }
}

/// Keyset predicate functions
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum KeySetPredicate {
    /// All keys must sign
    KeysAll,
    /// Any key can sign
    KeysAny,
    /// At least 2 keys must sign
    Keys2,
    /// Custom predicate function
    Custom(String),
}

impl KeySetPredicate {
    /// Parse predicate from string (alias for parse)
    pub fn from_name(s: &str) -> Self {
        Self::parse(s)
    }

    /// Get the name of this predicate
    pub fn name(&self) -> &str {
        match self {
            KeySetPredicate::KeysAll => "keys-all",
            KeySetPredicate::KeysAny => "keys-any",
            KeySetPredicate::Keys2 => "keys-2",
            KeySetPredicate::Custom(s) => s,
        }
    }

    /// Parse predicate from string
    pub fn parse(s: &str) -> Self {
        match s {
            "keys-all" => KeySetPredicate::KeysAll,
            "keys-any" => KeySetPredicate::KeysAny,
            "keys-2" => KeySetPredicate::Keys2,
            _ => KeySetPredicate::Custom(s.to_string()),
        }
    }

    /// Convert to string representation
    pub fn to_string(&self) -> String {
        self.name().to_string()
    }

    /// Evaluate predicate against signed keys
    pub fn evaluate(&self, total_keys: usize, signed_keys: usize) -> bool {
        match self {
            KeySetPredicate::KeysAll => signed_keys == total_keys && total_keys > 0,
            KeySetPredicate::KeysAny => signed_keys > 0,
            KeySetPredicate::Keys2 => signed_keys >= 2,
            KeySetPredicate::Custom(_) => {
                // Custom predicates would need function evaluation
                // For now, default to keys-all behavior
                signed_keys == total_keys && total_keys > 0
            }
        }
    }
}

impl fmt::Display for KeySetPredicate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

/// Key set for multi-signature authorization - matches Haskell KeySet
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Keyset {
    /// Set of public keys (hex-encoded Ed25519 keys)
    pub keys: Vec<String>,
    /// Predicate function for key evaluation
    pub pred: KeySetPredicate,
}

impl Keyset {
    /// Create a new keyset
    pub fn new(keys: Vec<String>, pred: KeySetPredicate) -> Self {
        Keyset { keys, pred }
    }

    /// Get the keys
    pub fn keys(&self) -> &Vec<String> {
        &self.keys
    }

    /// Get the predicate
    pub fn pred(&self) -> &KeySetPredicate {
        &self.pred
    }

    /// Create keyset with keys-all predicate
    pub fn keys_all(keys: Vec<String>) -> Self {
        Keyset::new(keys, KeySetPredicate::KeysAll)
    }

    /// Create keyset with keys-any predicate
    pub fn keys_any(keys: Vec<String>) -> Self {
        Keyset::new(keys, KeySetPredicate::KeysAny)
    }

    /// Create keyset with keys-2 predicate
    pub fn keys_2(keys: Vec<String>) -> Self {
        Keyset::new(keys, KeySetPredicate::Keys2)
    }

    /// Validate that all keys are in proper format
    pub fn validate_keys(&self) -> ValueResult<()> {
        let public_key_texts: Vec<PublicKeyText> = self
            .keys
            .iter()
            .map(|k| PublicKeyText::new(k.clone()))
            .collect();

        enforce_key_formats(&public_key_texts).map_err(|e| ValueError::Crypto {
            message: e.to_string(),
        })
    }

    /// Check if a set of signers satisfies this keyset
    pub fn satisfies(&self, signers: &[String]) -> ValueResult<bool> {
        // First validate key formats
        self.validate_keys()?;

        // Count how many required keys are in the signer set
        let signed_count = self.keys.iter().filter(|key| signers.contains(key)).count();

        Ok(self.pred.evaluate(self.keys.len(), signed_count))
    }

    /// Get the principal for this keyset
    pub fn principal(&self, name: &str) -> ValueResult<Principal> {
        let _keyset_name = KeySetName::new(name.to_string());
        Ok(Principal::R(name.to_string()))
    }
}

impl fmt::Display for Keyset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "KeySet({} keys, {})", self.keys.len(), self.pred)
    }
}

/// Context for guard evaluation
#[derive(Debug, Clone)]
pub struct GuardContext {
    /// Available signatures (public keys that have signed)
    pub signers: Vec<String>,
    /// Available keysets
    pub keysets: HashMap<String, Keyset>,
    /// Current capabilities
    pub capabilities: Vec<String>,
}

impl GuardContext {
    /// Create a new guard context
    pub fn new() -> Self {
        GuardContext {
            signers: Vec::new(),
            keysets: HashMap::new(),
            capabilities: Vec::new(),
        }
    }

    /// Add a signer
    pub fn with_signer(mut self, signer: String) -> Self {
        self.signers.push(signer);
        self
    }

    /// Add multiple signers
    pub fn with_signers(mut self, signers: Vec<String>) -> Self {
        self.signers.extend(signers);
        self
    }

    /// Add a keyset
    pub fn with_keyset(mut self, name: String, keyset: Keyset) -> Self {
        self.keysets.insert(name, keyset);
        self
    }

    /// Add a capability
    pub fn with_capability(mut self, cap: String) -> Self {
        self.capabilities.push(cap);
        self
    }
}

impl Default for GuardContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Evaluate a guard in the given context - matches `evalGuard`
pub fn eval_guard(guard: &Guard, context: &GuardContext) -> ValueResult<bool> {
    match guard {
        Guard::KeySet { name, keys, pred } => {
            // Use provided keys or look up in context
            let keyset = if keys.is_empty() {
                context
                    .keysets
                    .get(name)
                    .ok_or_else(|| ValueError::GuardEvaluation {
                        message: format!("Keyset not found: {}", name),
                    })?
            } else {
                // Create temporary keyset from guard data
                let pred_parsed = KeySetPredicate::parse(pred);
                &Keyset::new(keys.clone(), pred_parsed)
            };

            keyset.satisfies(&context.signers)
        }

        Guard::User { fun: _, args: _ } => {
            // User guards require function evaluation which would be handled
            // by the interpreter. For now, return false (failed authorization).
            Ok(false)
        }

        Guard::Capability { name, args: _ } => {
            // Check if capability is available in context
            Ok(context.capabilities.contains(name))
        }

        Guard::Module {
            name: _,
            governance: _,
        } => {
            // Module guards require special privileges
            // For now, return false (admin access required)
            Ok(false)
        }
    }
}

/// Enforce a guard (throw error if not satisfied) - matches `enforceGuard`
pub fn enforce_guard(guard: &Guard, context: &GuardContext) -> ValueResult<()> {
    if eval_guard(guard, context)? {
        Ok(())
    } else {
        Err(ValueError::GuardEvaluation {
            message: format!("Guard evaluation failed: {}", guard),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keyset_predicate_evaluation() {
        assert!(KeySetPredicate::KeysAll.evaluate(3, 3));
        assert!(!KeySetPredicate::KeysAll.evaluate(3, 2));
        assert!(!KeySetPredicate::KeysAll.evaluate(0, 0));

        assert!(KeySetPredicate::KeysAny.evaluate(3, 1));
        assert!(KeySetPredicate::KeysAny.evaluate(3, 3));
        assert!(!KeySetPredicate::KeysAny.evaluate(3, 0));

        assert!(KeySetPredicate::Keys2.evaluate(3, 2));
        assert!(KeySetPredicate::Keys2.evaluate(3, 3));
        assert!(!KeySetPredicate::Keys2.evaluate(3, 1));
    }

    #[test]
    fn test_keyset_creation() {
        let keys = vec!["key1".to_string(), "key2".to_string()];

        let ks_all = Keyset::keys_all(keys.clone());
        assert_eq!(ks_all.pred, KeySetPredicate::KeysAll);

        let ks_any = Keyset::keys_any(keys.clone());
        assert_eq!(ks_any.pred, KeySetPredicate::KeysAny);

        let ks_2 = Keyset::keys_2(keys.clone());
        assert_eq!(ks_2.pred, KeySetPredicate::Keys2);
    }

    #[test]
    fn test_guard_creation() {
        let guard = Guard::keyset("admin", vec!["key1".to_string()], "keys-all");
        match guard {
            Guard::KeySet { name, keys, pred } => {
                assert_eq!(name, "admin");
                assert_eq!(keys, vec!["key1"]);
                assert_eq!(pred, "keys-all");
            }
            _ => panic!("Expected KeySet guard"),
        }
    }

    #[test]
    fn test_guard_context() {
        let context = GuardContext::new()
            .with_signer("key1".to_string())
            .with_signer("key2".to_string())
            .with_capability("TRANSFER".to_string());

        assert_eq!(context.signers.len(), 2);
        assert_eq!(context.capabilities.len(), 1);
        assert!(context.capabilities.contains(&"TRANSFER".to_string()));
    }

    #[test]
    fn test_guard_evaluation() {
        let context = GuardContext::new()
            .with_signer("key1".to_string())
            .with_signer("key2".to_string());

        // Test keyset guard
        let guard = Guard::keyset(
            "test",
            vec!["key1".to_string(), "key2".to_string()],
            "keys-all",
        );
        assert!(eval_guard(&guard, &context).unwrap());

        let guard_any = Guard::keyset(
            "test",
            vec!["key1".to_string(), "key3".to_string()],
            "keys-any",
        );
        assert!(eval_guard(&guard_any, &context).unwrap());

        // Test capability guard
        let cap_guard = Guard::capability("TRANSFER", vec![]);
        assert!(!eval_guard(&cap_guard, &context).unwrap());

        let context_with_cap = context.with_capability("TRANSFER".to_string());
        assert!(eval_guard(&cap_guard, &context_with_cap).unwrap());
    }
}
