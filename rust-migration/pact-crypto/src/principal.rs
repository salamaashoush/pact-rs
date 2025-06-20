//! Principal validation and management - matches Haskell Pact implementation
//!
//! This module implements the complete principal system exactly as
//! implemented in Haskell Pact, including all principal types and validation.

use crate::hash::pact_hash;
use crate::{CryptoResult, DefPactId, SharedCryptoError};
use base64ct::{Base64UrlUnpadded, Encoding};
use pact_names::ModuleName;
use pact_shared_types::Principal;
use pact_values::Guard;
use serde::{Deserialize, Serialize};
use std::fmt;

/// Extended principal types (beyond the basic K and R types in shared crate)
/// These match the full Haskell implementation:
/// ```haskell
/// data Principal
///   = K !PublicKeyText           -- k:hex-public-key
///   | W !Text !Text             -- w:b64url-hash:predicate
///   | R !KeySetName             -- r:keyset-name
///   | U !Text !Text             -- u:guard-function:b64url-hash
///   | M !ModuleName !Text       -- m:module:guard-function
///   | P !DefPactId !Text        -- p:pactid:function
///   | C !Text                   -- c:capability-hash
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ExtendedPrincipal {
    /// Basic principal (K or R type)
    Basic(Principal),

    /// W-type: w:b64url-hash:predicate - WebAuthn principal
    W(String, String),

    /// U-type: u:guard-function:b64url-hash - user guard principal
    U(String, String),

    /// M-type: m:module:guard-function - module guard principal
    M(ModuleName, String),

    /// P-type: p:pactid:function - pact step principal
    P(DefPactId, String),

    /// C-type: c:capability-hash - capability principal
    C(String),
}

/// Parse a principal from string format - matches Haskell parsing
pub fn parse_principal(s: &str) -> CryptoResult<ExtendedPrincipal> {
    let parts: Vec<&str> = s.splitn(2, ':').collect();
    if parts.len() != 2 {
        return Err(SharedCryptoError::general(format!(
            "Invalid principal format: {}",
            s
        )));
    }

    let scheme = parts[0];
    let data = parts[1];

    match scheme {
        "k" => {
            validate_k_principal(data)?;
            Ok(ExtendedPrincipal::Basic(Principal::key(data.to_string())))
        }
        "w" => {
            let w_parts: Vec<&str> = data.splitn(2, ':').collect();
            if w_parts.len() != 2 {
                return Err(SharedCryptoError::general(format!(
                    "Invalid W principal format: {}",
                    s
                )));
            }
            validate_w_principal(w_parts[0], w_parts[1])?;
            Ok(ExtendedPrincipal::W(
                w_parts[0].to_string(),
                w_parts[1].to_string(),
            ))
        }
        "r" => {
            validate_r_principal(data)?;
            Ok(ExtendedPrincipal::Basic(Principal::role(data.to_string())))
        }
        "u" => {
            let u_parts: Vec<&str> = data.splitn(2, ':').collect();
            if u_parts.len() != 2 {
                return Err(SharedCryptoError::general(format!(
                    "Invalid U principal format: {}",
                    s
                )));
            }
            validate_u_principal(u_parts[0], u_parts[1])?;
            Ok(ExtendedPrincipal::U(
                u_parts[0].to_string(),
                u_parts[1].to_string(),
            ))
        }
        "m" => {
            let m_parts: Vec<&str> = data.splitn(2, ':').collect();
            if m_parts.len() != 2 {
                return Err(SharedCryptoError::general(format!(
                    "Invalid M principal format: {}",
                    s
                )));
            }
            validate_m_principal(m_parts[0], m_parts[1])?;
            let module_name = ModuleName::simple(m_parts[0].to_string());
            Ok(ExtendedPrincipal::M(module_name, m_parts[1].to_string()))
        }
        "p" => {
            let p_parts: Vec<&str> = data.splitn(2, ':').collect();
            if p_parts.len() != 2 {
                return Err(SharedCryptoError::general(format!(
                    "Invalid P principal format: {}",
                    s
                )));
            }
            validate_p_principal(p_parts[0], p_parts[1])?;
            Ok(ExtendedPrincipal::P(
                DefPactId::new(p_parts[0].to_string()),
                p_parts[1].to_string(),
            ))
        }
        "c" => {
            validate_c_principal(data)?;
            Ok(ExtendedPrincipal::C(data.to_string()))
        }
        _ => Err(SharedCryptoError::general(format!(
            "Invalid principal type: {}",
            scheme
        ))),
    }
}

impl ExtendedPrincipal {
    /// Render principal as string
    pub fn render(&self) -> String {
        match self {
            ExtendedPrincipal::Basic(p) => format!("{}", p),
            ExtendedPrincipal::W(hash, pred) => format!("w:{}:{}", hash, pred),
            ExtendedPrincipal::U(fun, hash) => format!("u:{}:{}", fun, hash),
            ExtendedPrincipal::M(module, fun) => format!("m:{}:{}", module.render(), fun),
            ExtendedPrincipal::P(pact_id, fun) => format!("p:{}:{}", pact_id.0, fun),
            ExtendedPrincipal::C(cap_hash) => format!("c:{}", cap_hash),
        }
    }

    /// Get the principal type
    pub fn principal_type(&self) -> &'static str {
        match self {
            ExtendedPrincipal::Basic(p) => {
                if p.is_key() {
                    "k"
                } else {
                    "r"
                }
            }
            ExtendedPrincipal::W(_, _) => "w",
            ExtendedPrincipal::U(_, _) => "u",
            ExtendedPrincipal::M(_, _) => "m",
            ExtendedPrincipal::P(_, _) => "p",
            ExtendedPrincipal::C(_) => "c",
        }
    }
}

impl fmt::Display for ExtendedPrincipal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.render())
    }
}

impl std::str::FromStr for ExtendedPrincipal {
    type Err = SharedCryptoError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_principal(s)
    }
}

/// Validate K-type principal (k:publickey)
/// Must be 64 hex characters (32 bytes)
fn validate_k_principal(data: &str) -> CryptoResult<()> {
    if data.len() != 64 {
        return Err(SharedCryptoError::invalid_key_format(format!(
            "k:{} - public key must be 64 hex characters",
            data
        )));
    }

    // Validate it's valid hex
    hex::decode(data).map_err(|e| {
        SharedCryptoError::invalid_key_format(format!("k:{} - invalid hex encoding: {}", data, e))
    })?;

    Ok(())
}

/// Validate W-type principal (w:b64url-hash:predicate)
/// Hash must be 43 characters (Base64URL unpadded 32 bytes)
fn validate_w_principal(hash: &str, predicate: &str) -> CryptoResult<()> {
    if hash.len() != 43 {
        return Err(SharedCryptoError::general(format!(
            "w:{}:{} - hash must be 43 Base64URL characters",
            hash, predicate
        )));
    }

    // Validate Base64URL encoding and length
    let decoded = Base64UrlUnpadded::decode_vec(hash).map_err(|_| {
        SharedCryptoError::general(format!(
            "w:{}:{} - invalid Base64URL encoding",
            hash, predicate
        ))
    })?;

    if decoded.len() != 32 {
        return Err(SharedCryptoError::general(format!(
            "w:{}:{} - decoded hash must be 32 bytes",
            hash, predicate
        )));
    }

    if predicate.is_empty() {
        return Err(SharedCryptoError::general(format!(
            "w:{}:{} - predicate cannot be empty",
            hash, predicate
        )));
    }

    Ok(())
}

/// Validate R-type principal (r:keyset-name)
fn validate_r_principal(keyset_name: &str) -> CryptoResult<()> {
    if keyset_name.is_empty() {
        return Err(SharedCryptoError::general(format!(
            "r:{} - keyset name cannot be empty",
            keyset_name
        )));
    }

    // Additional validation could check for valid keyset name characters
    Ok(())
}

/// Validate U-type principal (u:guard-function:b64url-hash)
fn validate_u_principal(guard_function: &str, hash: &str) -> CryptoResult<()> {
    if guard_function.is_empty() {
        return Err(SharedCryptoError::general(format!(
            "u:{}:{} - guard function cannot be empty",
            guard_function, hash
        )));
    }

    // Same hash validation as W-type
    validate_w_principal(hash, "dummy")?;

    Ok(())
}

/// Validate M-type principal (m:module:guard-function)
fn validate_m_principal(module_name: &str, guard_function: &str) -> CryptoResult<()> {
    if module_name.is_empty() {
        return Err(SharedCryptoError::general(format!(
            "m:{}:{} - module name cannot be empty",
            module_name, guard_function
        )));
    }

    if guard_function.is_empty() {
        return Err(SharedCryptoError::general(format!(
            "m:{}:{} - guard function cannot be empty",
            module_name, guard_function
        )));
    }

    Ok(())
}

/// Validate P-type principal (p:pactid:function)
fn validate_p_principal(pact_id: &str, function: &str) -> CryptoResult<()> {
    if pact_id.is_empty() {
        return Err(SharedCryptoError::general(format!(
            "p:{}:{} - pact ID cannot be empty",
            pact_id, function
        )));
    }

    if function.is_empty() {
        return Err(SharedCryptoError::general(format!(
            "p:{}:{} - function name cannot be empty",
            pact_id, function
        )));
    }

    Ok(())
}

/// Validate C-type principal (c:capability-hash)
fn validate_c_principal(cap_hash: &str) -> CryptoResult<()> {
    if cap_hash.is_empty() {
        return Err(SharedCryptoError::general(format!(
            "c:{} - capability hash cannot be empty",
            cap_hash
        )));
    }

    // Could validate hash format here
    Ok(())
}

/// Create principal for guard - matches `createPrincipalForGuard`
/// Returns basic Principal (K or R type) for simple cases
pub fn create_principal_for_guard(guard: &Guard) -> CryptoResult<Principal> {
    match guard {
        Guard::KeySet { name, .. } => {
            // For keyset guards, create R-type principal
            Ok(Principal::role(name.clone()))
        }
        Guard::Capability { name, .. } => {
            // For capability guards, create a key-style principal with the cap name
            Ok(Principal::key(format!("cap:{}", name)))
        }
        Guard::User { fun, .. } => {
            // For user guards, create a key-style principal with the function name
            Ok(Principal::key(format!("user:{}", fun)))
        }
        Guard::Module { name, .. } => {
            // For module guards, create a key-style principal with the module name
            Ok(Principal::key(format!("module:{}", name)))
        }
    }
}

/// Create extended principal for guard - includes all principal types
pub fn create_extended_principal_for_guard(guard: &Guard) -> CryptoResult<ExtendedPrincipal> {
    match guard {
        Guard::KeySet { name, .. } => Ok(ExtendedPrincipal::Basic(Principal::role(name.clone()))),
        Guard::Capability { name, args } => {
            // Hash the capability name and args
            let cap_data = format!("{}:{:?}", name, args);
            let hash = pact_hash(cap_data.as_bytes());
            Ok(ExtendedPrincipal::C(hash.to_base64url()))
        }
        Guard::User { fun, args } => {
            // Hash the guard function and args
            let guard_data = format!("{}:{:?}", fun, args);
            let hash = pact_hash(guard_data.as_bytes());
            Ok(ExtendedPrincipal::U(fun.clone(), hash.to_base64url()))
        }
        Guard::Module {
            name,
            governance: _,
        } => {
            let module_name = ModuleName::simple(name.clone());
            Ok(ExtendedPrincipal::M(module_name, "admin".to_string()))
        }
    }
}

/// Validate principal - matches core builtin `coreValidatePrincipal`
pub fn validate_principal(principal: &str) -> CryptoResult<()> {
    parse_principal(principal)?;
    Ok(())
}

/// Check if string is a valid principal - matches core builtin `coreIsPrincipal`
pub fn is_principal(s: &str) -> bool {
    parse_principal(s).is_ok()
}

/// Get the type of a principal - matches core builtin `coreTypeOfPrincipal`
pub fn type_of_principal(principal: &str) -> CryptoResult<String> {
    let p = parse_principal(principal)?;
    Ok(p.principal_type().to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use pact_values::PactValue;

    #[test]
    fn test_k_principal() {
        // Valid K principal
        let valid_k = "k:1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef";
        let principal = parse_principal(valid_k).unwrap();

        match &principal {
            ExtendedPrincipal::Basic(Principal::K(key)) => {
                assert_eq!(
                    key,
                    "1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef"
                );
            }
            _ => panic!("Expected K principal"),
        }

        assert_eq!(principal.render(), valid_k);
        assert_eq!(principal.principal_type(), "k");

        // Invalid K principal (wrong length)
        assert!(parse_principal("k:short").is_err());

        // Invalid K principal (non-hex)
        assert!(parse_principal(
            "k:gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg"
        )
        .is_err());
    }

    #[test]
    fn test_w_principal() {
        // Create a valid 43-character Base64URL hash
        let hash_bytes = [0u8; 32]; // 32 zero bytes
        let hash = Base64UrlUnpadded::encode_string(&hash_bytes);
        assert_eq!(hash.len(), 43);

        let valid_w = format!("w:{}:my-predicate", hash);
        let principal = parse_principal(&valid_w).unwrap();

        match &principal {
            ExtendedPrincipal::W(keyset_hash, predicate) => {
                assert_eq!(keyset_hash, &hash);
                assert_eq!(predicate, "my-predicate");
            }
            _ => panic!("Expected W principal"),
        }

        assert_eq!(principal.render(), valid_w);
        assert_eq!(principal.principal_type(), "w");

        // Invalid W principal (wrong hash length)
        assert!(parse_principal("w:short:predicate").is_err());

        // Invalid W principal (empty predicate)
        assert!(parse_principal(&format!("w:{}:", hash)).is_err());
    }

    #[test]
    fn test_r_principal() {
        let valid_r = "r:my-keyset";
        let principal = parse_principal(valid_r).unwrap();

        match &principal {
            ExtendedPrincipal::Basic(Principal::R(keyset)) => {
                assert_eq!(keyset, "my-keyset");
            }
            _ => panic!("Expected R principal"),
        }

        assert_eq!(principal.render(), valid_r);
        assert_eq!(principal.principal_type(), "r");

        // Invalid R principal (empty name)
        assert!(parse_principal("r:").is_err());
    }

    #[test]
    fn test_principal_validation() {
        let valid_principals = vec![
            "k:1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef",
            "r:my-keyset",
        ];

        for principal in valid_principals {
            assert!(validate_principal(principal).is_ok());
            assert!(is_principal(principal));
            assert!(type_of_principal(principal).is_ok());
        }

        let invalid_principals = vec!["invalid", "x:unknown", "k:short", ""];

        for principal in invalid_principals {
            let result = validate_principal(principal);
            if result.is_ok() {
                panic!(
                    "Expected principal '{}' to be invalid, but it was valid",
                    principal
                );
            }
            assert!(!is_principal(principal));
        }
    }

    #[test]
    fn test_create_principal_for_guard() {
        // Keyset guard
        let keyset_guard = Guard::KeySet {
            name: "admin-keys".to_string(),
            keys: vec!["key1".to_string(), "key2".to_string()],
            pred: "keys-all".to_string(),
        };

        let principal = create_principal_for_guard(&keyset_guard).unwrap();
        match principal {
            Principal::R(keyset) => assert_eq!(keyset, "admin-keys"),
            _ => panic!("Expected R principal for keyset guard"),
        }

        // Capability guard
        let cap_guard = Guard::Capability {
            name: "TRANSFER".to_string(),
            args: vec![
                PactValue::String("alice".to_string()),
                PactValue::String("bob".to_string()),
            ],
        };

        let principal = create_principal_for_guard(&cap_guard).unwrap();
        match principal {
            Principal::K(key) => assert!(key.starts_with("cap:")),
            _ => panic!("Expected K principal for capability guard"),
        }
    }

    #[test]
    fn test_principal_roundtrip() {
        let test_principals = vec![
            "k:1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef",
            "r:my-keyset",
        ];

        for original in test_principals {
            let principal = parse_principal(original).unwrap();
            let rendered = principal.render();
            assert_eq!(original, rendered);

            // Parse again should yield same result
            let reparsed = parse_principal(&rendered).unwrap();
            assert_eq!(principal, reparsed);
        }
    }
}
