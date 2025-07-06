//! Capability guard implementation
//!
//! This module implements capability-based guards that integrate with Pact's
//! guard system, matching the Haskell implementation.

use super::types::CapToken;
use compact_str::CompactString;
use crate::names::QualifiedName;
use crate::values::PactValue;
use serde::{Deserialize, Serialize};

/// Capability guard - matches Haskell CapabilityGuard
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CapabilityGuard {
    /// Capability name
    pub name: QualifiedName,
    /// Arguments to the capability
    pub args: Vec<PactValue>,
    /// Optional DefPact ID for pact-specific guards
    pub pact_id: Option<CompactString>,
}

/// DefPact guard - matches Haskell DefPactGuard
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DefPactGuard {
    /// DefPact ID
    pub pact_id: CompactString,
    /// DefPact name
    pub name: QualifiedName,
}

/// Module guard - matches Haskell ModuleGuard
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ModuleGuard {
    /// Module name
    pub module_name: CompactString,
    /// Module guard name
    pub name: CompactString,
}

/// User guard - matches Haskell UserGuard  
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UserGuard {
    /// User guard function
    pub fun: QualifiedName,
    /// Arguments to the guard function
    pub args: Vec<PactValue>,
}

impl CapabilityGuard {
    /// Create a new capability guard
    pub fn new(name: QualifiedName, args: Vec<PactValue>) -> Self {
        CapabilityGuard {
            name,
            args,
            pact_id: None,
        }
    }
    
    /// Create a capability guard with pact ID
    pub fn with_pact_id(name: QualifiedName, args: Vec<PactValue>, pact_id: CompactString) -> Self {
        CapabilityGuard {
            name,
            args,
            pact_id: Some(pact_id),
        }
    }
    
    /// Convert to capability token for evaluation
    pub fn to_cap_token(&self) -> CapToken {
        CapToken {
            name: self.name.clone(),
            args: self.args.clone(),
        }
    }
}

impl DefPactGuard {
    /// Create a new DefPact guard
    pub fn new(pact_id: CompactString, name: QualifiedName) -> Self {
        DefPactGuard { pact_id, name }
    }
}

impl ModuleGuard {
    /// Create a new module guard
    pub fn new(module_name: CompactString, name: CompactString) -> Self {
        ModuleGuard { module_name, name }
    }
}

impl UserGuard {
    /// Create a new user guard
    pub fn new(fun: QualifiedName, args: Vec<PactValue>) -> Self {
        UserGuard { fun, args }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::names::ModuleName;
    
    #[test]
    fn test_capability_guard_creation() {
        let name = QualifiedName {
            name: "test-cap".into(),
            module: ModuleName {
                name: "test-module".into(),
                namespace: None,
            },
        };
        let args = vec![PactValue::String("arg1".to_string())];
        
        let guard = CapabilityGuard::new(name.clone(), args.clone());
        assert_eq!(guard.name, name);
        assert_eq!(guard.args, args);
        assert!(guard.pact_id.is_none());
        
        let token = guard.to_cap_token();
        assert_eq!(token.name, name);
        assert_eq!(token.args, args);
    }
    
    #[test]
    fn test_capability_guard_with_pact_id() {
        let name = QualifiedName {
            name: "test-cap".into(),
            module: ModuleName {
                name: "test-module".into(),
                namespace: None,
            },
        };
        let args = vec![PactValue::String("arg1".to_string())];
        let pact_id = "pact-123".into();
        
        let guard = CapabilityGuard::with_pact_id(name.clone(), args.clone(), pact_id);
        assert_eq!(guard.name, name);
        assert_eq!(guard.args, args);
        assert_eq!(guard.pact_id, Some("pact-123".into()));
    }
}