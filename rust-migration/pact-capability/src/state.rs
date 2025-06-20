//! Capability state management
//!
//! This module implements capability state operations for the Pact evaluator,
//! matching the Haskell implementation patterns.

use crate::types::{CapState, CapToken, ManagedCap, PactEvent};
use pact_errors::{PactError, EvalError};
use pact_shared_types::SpanInfo;
use pact_names::{ModuleName, QualifiedName};
use pact_values::PactValue;

/// Capability manager for the evaluator
#[derive(Debug, Clone)]
pub struct CapabilityManager {
    /// Current capability state
    state: CapState,
    /// Events collected during evaluation
    events: Vec<PactEvent>,
}

impl Default for CapabilityManager {
    fn default() -> Self {
        Self::new()
    }
}

impl CapabilityManager {
    /// Create a new capability manager
    pub fn new() -> Self {
        CapabilityManager {
            state: CapState::new(),
            events: vec![],
        }
    }
    
    /// Get the current capability state
    pub fn current_state(&self) -> CapState {
        self.state.clone()
    }
    
    /// Set the capability state
    pub fn set_state(&mut self, state: CapState) {
        self.state = state;
    }
    
    /// Check if a capability is currently granted
    pub fn is_capability_granted(&self, cap: &CapToken) -> bool {
        self.state.is_cap_in_stack(cap) || 
        self.state.is_managed_installed(cap) ||
        self.state.autonomous.contains(cap)
    }
    
    /// Require that a capability is granted (for require-capability builtin)
    pub fn require_capability(&self, cap: &CapToken) -> Result<(), PactError<SpanInfo>> {
        if self.is_capability_granted(cap) {
            Ok(())
        } else {
            Err(PactError::PEExecutionError(
                EvalError::CapabilityNotGranted(cap.name.name.clone()),
                vec![], // No stack frames for this error
                SpanInfo { start: 0, end: 0 }, // Default span
            ))
        }
    }
    
    /// Install a capability (for with-capability)
    pub fn install_capability(&mut self, name: String, args: Vec<PactValue>) -> Result<(), PactError<SpanInfo>> {
        let qualified_name = QualifiedName {
            name: name.into(),
            module: ModuleName {
                name: "current".into(), // This should come from context
                namespace: None,
            },
        };
        let cap = CapToken::new(qualified_name, args);
        
        // Check for recursion
        if self.state.is_cap_being_evaluated(&cap) {
            return Err(PactError::PEExecutionError(
                EvalError::CapabilityNotGranted(format!("Recursive capability evaluation: {}", cap.name.name)),
                vec![], // No stack frames for this error
                SpanInfo { start: 0, end: 0 }, // Default span
            ));
        }
        
        // Mark as being evaluated
        self.state.caps_being_evaluated.insert(cap.clone());
        
        // Install the capability
        self.state.push_cap(cap.clone());
        
        Ok(())
    }
    
    /// Remove a capability from the stack (when exiting with-capability)
    pub fn pop_capability(&mut self, name: &str) -> Result<(), PactError<SpanInfo>> {
        if let Some(slot) = self.state.pop_cap() {
            // Remove from being evaluated set
            self.state.caps_being_evaluated.remove(&slot.cap);
            
            // Verify we're popping the right capability
            if slot.cap.name.name.as_str() != name {
                return Err(PactError::PEExecutionError(
                    EvalError::CapabilityNotGranted(format!("Capability stack mismatch: expected {}, got {}", 
                                    name, slot.cap.name.name)),
                    vec![], // No stack frames for this error
                    SpanInfo { start: 0, end: 0 }, // Default span
                ));
            }
            Ok(())
        } else {
            Err(PactError::PEExecutionError(
                EvalError::CapabilityNotGranted("No capability to pop from stack".to_string()),
                vec![], // No stack frames for this error
                SpanInfo { start: 0, end: 0 }, // Default span
            ))
        }
    }
    
    /// Install a managed capability
    pub fn install_managed_capability(&mut self, managed_cap: ManagedCap) -> Result<(), PactError<SpanInfo>> {
        self.state.install_managed(managed_cap);
        Ok(())
    }
    
    /// Compose a capability within a defcap (for compose-capability builtin)
    pub fn compose_capability(&mut self, cap: CapToken) -> Result<(), PactError<SpanInfo>> {
        if let Some(current_slot) = self.state.slots.last_mut() {
            current_slot.compose(cap);
            Ok(())
        } else {
            Err(PactError::PEExecutionError(
                EvalError::CapabilityNotGranted("Cannot compose capability: no active capability context".to_string()),
                vec![], // No stack frames for this error
                SpanInfo { start: 0, end: 0 }, // Default span
            ))
        }
    }
    
    /// Emit an event (for emit-event builtin)
    pub fn emit_event(&mut self, event: PactEvent) {
        self.events.push(event);
    }
    
    /// Get all emitted events
    pub fn get_events(&self) -> &[PactEvent] {
        &self.events
    }
    
    /// Clear all emitted events
    pub fn clear_events(&mut self) {
        self.events.clear();
    }
    
    /// Add module admin capability
    pub fn add_module_admin(&mut self, module: ModuleName) {
        self.state.add_module_admin(module);
    }
    
    /// Check if module admin is granted
    pub fn has_module_admin(&self, module: &ModuleName) -> bool {
        self.state.has_module_admin(module)
    }
    
    /// Install an autonomous capability
    pub fn install_autonomous_capability(&mut self, cap: CapToken) {
        self.state.autonomous.insert(cap);
    }
    
    /// Begin a new capability scope (for nested with-capability)
    pub fn begin_scope(&mut self) {
        // Mark the current stack depth for scope management
        // This is handled by the CEK machine through continuation management
    }
    
    /// End the current capability scope
    pub fn end_scope(&mut self) {
        // Scope management is handled by the CEK machine
    }
    
    /// Reset capability state (for fresh evaluation contexts)
    pub fn reset(&mut self) {
        self.state = CapState::new();
        self.events.clear();
    }
    
    /// Check if currently inside a capability evaluation
    pub fn in_capability_context(&self) -> bool {
        !self.state.slots.is_empty()
    }
    
    /// Get the current capability stack depth
    pub fn stack_depth(&self) -> usize {
        self.state.slots.len()
    }
    
    /// Get all granted capabilities - implements getAllStackCaps from Haskell
    pub fn get_all_granted_capabilities(&self) -> std::collections::HashSet<String> {
        let mut caps = std::collections::HashSet::new();
        
        // Add all capabilities from the capability stack
        for slot in &self.state.slots {
            caps.insert(format!("{}:{}", slot.cap.name.name, slot.cap.args.len()));
            
            // Add composed capabilities
            for composed in &slot.composed {
                caps.insert(format!("{}:{}", composed.name.name, composed.args.len()));
            }
        }
        
        // Add autonomous capabilities
        for cap in &self.state.autonomous {
            caps.insert(format!("{}:{}", cap.name.name, cap.args.len()));
        }
        
        caps
    }
    
    /// Get autonomous capabilities
    pub fn get_autonomous_caps(&self) -> std::collections::HashSet<String> {
        self.state.autonomous
            .iter()
            .map(|cap| format!("{}:{}", cap.name.name, cap.args.len()))
            .collect()
    }
    
    /// Check if capability is granted by name (simplified version for string-based lookup)
    pub fn is_capability_granted_by_name(&self, cap_name: &str) -> bool {
        // Check if any granted capability matches the name
        let granted = self.get_all_granted_capabilities();
        granted.iter().any(|cap| cap.starts_with(&format!("{}:", cap_name)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pact_names::ModuleName;
    
    #[test]
    fn test_capability_manager_basic_operations() {
        let mut manager = CapabilityManager::new();
        
        // Test initial state
        assert!(!manager.in_capability_context());
        assert_eq!(manager.stack_depth(), 0);
        
        // Test capability installation
        let result = manager.install_capability(
            "test-cap".to_string(),
            vec![PactValue::String("arg1".to_string())],
        );
        assert!(result.is_ok());
        assert!(manager.in_capability_context());
        assert_eq!(manager.stack_depth(), 1);
        
        // Test capability removal
        let result = manager.pop_capability("test-cap");
        assert!(result.is_ok());
        assert!(!manager.in_capability_context());
        assert_eq!(manager.stack_depth(), 0);
    }
    
    #[test]
    fn test_module_admin() {
        let mut manager = CapabilityManager::new();
        let module = ModuleName {
            name: "test-module".into(),
            namespace: None,
        };
        
        assert!(!manager.has_module_admin(&module));
        manager.add_module_admin(module.clone());
        assert!(manager.has_module_admin(&module));
    }
    
    #[test]
    fn test_event_emission() {
        let mut manager = CapabilityManager::new();
        
        assert_eq!(manager.get_events().len(), 0);
        
        let event = PactEvent {
            name: QualifiedName {
                name: "test-event".into(),
                module: ModuleName {
                    name: "test-module".into(),
                    namespace: None,
                },
            },
            params: vec![PactValue::String("param1".to_string())],
            module: ModuleName {
                name: "test-module".into(),
                namespace: None,
            },
            module_hash: "hash123".into(),
        };
        
        manager.emit_event(event);
        assert_eq!(manager.get_events().len(), 1);
        
        manager.clear_events();
        assert_eq!(manager.get_events().len(), 0);
    }
}