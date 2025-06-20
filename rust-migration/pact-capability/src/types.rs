//! Core capability types
//!
//! This module defines the core data structures for Pact's capability system,
//! exactly matching the Haskell implementation.

use compact_str::CompactString;
use pact_names::{ModuleName, QualifiedName};
use pact_shared_types::SpanInfo;
use pact_values::PactValue;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::hash::{Hash, Hasher};

/// Capability metadata types - matches Haskell DefCapMeta
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum DefCapMeta {
    /// Event capabilities
    DefEvent,
    /// Managed capabilities
    DefManaged(DefManagedMeta),
    /// Regular unmanaged capabilities  
    Unmanaged,
}

/// Managed capability metadata - matches Haskell DefManagedMeta
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum DefManagedMeta {
    /// User-managed with parameter info and manager function name
    DefManagedMeta {
        param_ix: usize,
        param_name: CompactString,
        manager_name: QualifiedName,
    },
    /// Auto-managed by runtime
    AutoManagedMeta,
}

/// Core capability token - matches Haskell CapToken
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CapToken {
    /// Capability name
    pub name: QualifiedName,
    /// Arguments passed to capability
    pub args: Vec<PactValue>,
}

impl Hash for CapToken {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        // Hash the number of args since we can't hash PactValue directly
        self.args.len().hash(state);
    }
}

impl Eq for CapToken {}

/// Capability slot for composition - matches Haskell CapSlot
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CapSlot {
    /// Main capability
    pub cap: CapToken,
    /// Composed capabilities
    pub composed: Vec<CapToken>,
}

/// Managed capability with metadata - matches Haskell ManagedCap
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ManagedCap {
    /// The capability token
    pub token: CapToken,
    /// Original capability when acquired (for managed parameter tracking)
    pub original: CapToken,
    /// Metadata about the managed capability
    pub meta: DefManagedMeta,
}

impl Hash for ManagedCap {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.token.hash(state);
        self.meta.hash(state);
    }
}

impl Eq for ManagedCap {}

/// Overall capability state - matches Haskell CapState
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct CapState {
    /// Active capability stack
    pub slots: Vec<CapSlot>,
    /// Installed managed capabilities
    pub managed: HashSet<ManagedCap>,
    /// Module admin capabilities
    pub module_admin: HashSet<ModuleName>,
    /// Autonomous capabilities
    pub autonomous: HashSet<CapToken>,
    /// Capabilities currently being evaluated (to prevent recursion)
    pub caps_being_evaluated: HashSet<CapToken>,
}

/// Magic capabilities for special system operations - matches Haskell MagicCap
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum MagicCap {
    /// Keyset definition capability
    DefineKeysetCap(CompactString),
    /// Module keyset access capability
    ModuleKeysetCap(CompactString),
    /// Namespace definition capability
    DefineNamespaceCap(CompactString),
    /// Namespace ownership capability
    NamespaceOwnerCap(CompactString),
}

/// Event data emitted by capabilities
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PactEvent {
    /// Event name (capability name)
    pub name: QualifiedName,
    /// Event parameters
    pub params: Vec<PactValue>,
    /// Module that emitted the event
    pub module: ModuleName,
    /// Hash of the module that emitted the event
    pub module_hash: CompactString,
}

/// Capability definition for tracking in loaded modules
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CapDef {
    /// Capability name
    pub name: QualifiedName,
    /// Argument names and types
    pub args: Vec<(CompactString, Option<CompactString>)>,
    /// Capability metadata
    pub meta: DefCapMeta,
    /// Whether it's a managed capability
    pub managed: bool,
    /// Source location info
    pub info: SpanInfo,
}

impl CapToken {
    /// Create a new capability token
    pub fn new(name: QualifiedName, args: Vec<PactValue>) -> Self {
        CapToken { name, args }
    }
    
    /// Create a capability token with no arguments
    pub fn nullary(name: QualifiedName) -> Self {
        CapToken {
            name,
            args: vec![],
        }
    }
}

impl CapSlot {
    /// Create a new capability slot
    pub fn new(cap: CapToken) -> Self {
        CapSlot {
            cap,
            composed: vec![],
        }
    }
    
    /// Add a composed capability to this slot
    pub fn compose(&mut self, cap: CapToken) {
        self.composed.push(cap);
    }
}

impl ManagedCap {
    /// Create a new managed capability
    pub fn new(token: CapToken, original: CapToken, meta: DefManagedMeta) -> Self {
        ManagedCap {
            token,
            original,
            meta,
        }
    }
}

impl CapState {
    /// Create a new empty capability state
    pub fn new() -> Self {
        CapState::default()
    }
    
    /// Check if a capability is currently in the stack
    pub fn is_cap_in_stack(&self, cap: &CapToken) -> bool {
        self.slots.iter().any(|slot| {
            slot.cap == *cap || slot.composed.contains(cap)
        })
    }
    
    /// Check if a capability is currently being evaluated
    pub fn is_cap_being_evaluated(&self, cap: &CapToken) -> bool {
        self.caps_being_evaluated.contains(cap)
    }
    
    /// Push a capability onto the stack
    pub fn push_cap(&mut self, cap: CapToken) {
        self.slots.push(CapSlot::new(cap));
    }
    
    /// Pop the most recent capability from the stack
    pub fn pop_cap(&mut self) -> Option<CapSlot> {
        self.slots.pop()
    }
    
    /// Install a managed capability
    pub fn install_managed(&mut self, managed_cap: ManagedCap) {
        self.managed.insert(managed_cap);
    }
    
    /// Check if a managed capability is installed
    pub fn is_managed_installed(&self, cap: &CapToken) -> bool {
        self.managed.iter().any(|mc| mc.token == *cap)
    }
    
    /// Add module admin capability
    pub fn add_module_admin(&mut self, module: ModuleName) {
        self.module_admin.insert(module);
    }
    
    /// Check if module admin is granted
    pub fn has_module_admin(&self, module: &ModuleName) -> bool {
        self.module_admin.contains(module)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pact_names::ModuleName;
    
    #[test]
    fn test_cap_token_creation() {
        let name = QualifiedName {
            name: "test-cap".into(),
            module: ModuleName {
                name: "test-module".into(),
                namespace: None,
            },
        };
        let token = CapToken::nullary(name.clone());
        
        assert_eq!(token.name, name);
        assert!(token.args.is_empty());
    }
    
    #[test]
    fn test_cap_state_operations() {
        let mut state = CapState::new();
        let cap = CapToken::nullary(QualifiedName {
            name: "test-cap".into(),
            module: ModuleName {
                name: "test-module".into(),
                namespace: None,
            },
        });
        
        assert!(!state.is_cap_in_stack(&cap));
        
        state.push_cap(cap.clone());
        assert!(state.is_cap_in_stack(&cap));
        
        let popped = state.pop_cap();
        assert!(popped.is_some());
        assert!(!state.is_cap_in_stack(&cap));
    }
}