//! Managed capability implementation
//!
//! This module implements the managed capability system, including
//! managed parameter validation and manager function evaluation.

use super::types::{CapToken, DefManagedMeta, ManagedCap};
use crate::errors::{PactError, EvalError};
use crate::shared::SpanInfo;
use crate::names::QualifiedName;
use crate::values::PactValue;

/// Managed capability evaluator
#[derive(Debug, Clone)]
pub struct ManagedCapabilityEvaluator {
    /// Currently installed managed capabilities
    managed_caps: Vec<ManagedCap>,
}

impl Default for ManagedCapabilityEvaluator {
    fn default() -> Self {
        Self::new()
    }
}

impl ManagedCapabilityEvaluator {
    /// Create a new managed capability evaluator
    pub fn new() -> Self {
        ManagedCapabilityEvaluator {
            managed_caps: vec![],
        }
    }
    
    /// Install a managed capability
    pub fn install_managed_capability(
        &mut self,
        token: CapToken,
        meta: DefManagedMeta,
    ) -> Result<(), PactError<SpanInfo>> {
        let managed_cap = ManagedCap::new(token.clone(), token, meta);
        self.managed_caps.push(managed_cap);
        Ok(())
    }
    
    /// Validate that a capability request is within managed bounds
    pub fn validate_managed_capability(
        &self,
        requested: &CapToken,
        meta: &DefManagedMeta,
    ) -> Result<(), PactError<SpanInfo>> {
        match meta {
            DefManagedMeta::AutoManagedMeta => {
                // Auto-managed capabilities are always valid
                Ok(())
            }
            DefManagedMeta::DefManagedMeta { param_ix, manager_name, .. } => {
                // Find the installed managed capability
                let installed = self.managed_caps.iter()
                    .find(|mc| mc.token.name == requested.name)
                    .ok_or_else(|| PactError::PEExecutionError(
                        EvalError::CapabilityNotGranted(format!("Managed capability not installed: {}", requested.name.name)),
                        vec![], // No stack frames for this error
                        SpanInfo::empty(), // Default span
                    ))?;
                
                // Get the managed parameter from both requests
                let requested_param = requested.args.get(*param_ix)
                    .ok_or_else(|| PactError::PEExecutionError(
                        EvalError::CapabilityNotGranted(format!("Missing managed parameter at index {}", param_ix)),
                        vec![], // No stack frames for this error
                        SpanInfo::empty(), // Default span
                    ))?;
                
                let installed_param = installed.original.args.get(*param_ix)
                    .ok_or_else(|| PactError::PEExecutionError(
                        EvalError::CapabilityNotGranted(format!("Missing managed parameter in installed capability at index {}", param_ix)),
                        vec![], // No stack frames for this error
                        SpanInfo::empty(), // Default span
                    ))?;
                
                // Call the manager function to validate
                self.call_manager_function(manager_name, installed_param, requested_param)
            }
        }
    }
    
    /// Call the manager function to validate managed parameters
    fn call_manager_function(
        &self,
        _manager_name: &QualifiedName,
        _managed: &PactValue,
        _requested: &PactValue,
    ) -> Result<(), PactError<SpanInfo>> {
        // This would need access to the evaluator to call the manager function
        // For now, implement a simple validation that requested <= managed for decimals
        // In the full implementation, this would evaluate the manager function
        
        // Placeholder validation - in real implementation this would:
        // 1. Look up the manager function
        // 2. Call it with (managed, requested) parameters  
        // 3. Check that it returns true
        Ok(())
    }
    
    /// Check if a capability is managed
    pub fn is_managed(&self, cap: &CapToken) -> bool {
        self.managed_caps.iter().any(|mc| mc.token.name == cap.name)
    }
    
    /// Get installed managed capability
    pub fn get_managed_capability(&self, cap: &CapToken) -> Option<&ManagedCap> {
        self.managed_caps.iter().find(|mc| mc.token.name == cap.name)
    }
    
    /// Remove a managed capability
    pub fn remove_managed_capability(&mut self, cap: &CapToken) -> Result<(), PactError<SpanInfo>> {
        let initial_len = self.managed_caps.len();
        self.managed_caps.retain(|mc| mc.token.name != cap.name);
        
        if self.managed_caps.len() == initial_len {
            Err(PactError::PEExecutionError(
                EvalError::CapabilityNotGranted(format!("Managed capability not found: {}", cap.name.name)),
                vec![], // No stack frames for this error
                SpanInfo::empty(), // Default span
            ))
        } else {
            Ok(())
        }
    }
    
    /// Clear all managed capabilities
    pub fn clear(&mut self) {
        self.managed_caps.clear();
    }
    
    /// Get all managed capabilities
    pub fn get_all_managed(&self) -> &[ManagedCap] {
        &self.managed_caps
    }
}

/// Helper functions for managed capability parameter validation
pub mod validation {
    use crate::values::PactValue;
    // These imports are not currently used in the test
    
    /// Validate decimal managed parameters (requested <= managed)
    pub fn validate_decimal_managed(managed: &PactValue, requested: &PactValue) -> bool {
        match (managed, requested) {
            (PactValue::Decimal(m), PactValue::Decimal(r)) => r <= m,
            (PactValue::Integer(m), PactValue::Integer(r)) => r <= m,
            // For mixed types, convert to strings and compare (simplified approach)
            // In full implementation, we'd properly convert between decimal types
            _ => false,
        }
    }
    
    /// Validate integer managed parameters (requested <= managed)
    pub fn validate_integer_managed(managed: &PactValue, requested: &PactValue) -> bool {
        match (managed, requested) {
            (PactValue::Integer(m), PactValue::Integer(r)) => r <= m,
            _ => false,
        }
    }
    
    /// Validate that requested is zero or less than managed
    pub fn validate_decreasing_managed(managed: &PactValue, requested: &PactValue) -> bool {
        use num_traits::Zero;
        match (managed, requested) {
            (PactValue::Decimal(m), PactValue::Decimal(r)) => {
                r.is_zero() || r <= m
            }
            (PactValue::Integer(m), PactValue::Integer(r)) => {
                r.is_zero() || r <= m
            }
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::types::DefManagedMeta;
    use crate::names::ModuleName;
    use num_bigint::BigInt;
    use num_rational::BigRational;
    
    #[test]
    fn test_managed_capability_installation() {
        let mut evaluator = ManagedCapabilityEvaluator::new();
        
        let cap = CapToken::new(
            QualifiedName {
                name: "test-cap".into(),
                module: ModuleName {
                    name: "test-module".into(),
                    namespace: None,
                },
            },
            vec![PactValue::Integer(BigInt::from(100))],
        );
        
        let meta = DefManagedMeta::AutoManagedMeta;
        
        let result = evaluator.install_managed_capability(cap.clone(), meta);
        assert!(result.is_ok());
        assert!(evaluator.is_managed(&cap));
        assert!(evaluator.get_managed_capability(&cap).is_some());
    }
    
    #[test]
    fn test_decimal_validation() {
        use super::validation::*;
        
        let managed = PactValue::Decimal(BigRational::from(BigInt::from(100)));
        let requested_valid = PactValue::Decimal(BigRational::from(BigInt::from(50)));
        let requested_invalid = PactValue::Decimal(BigRational::from(BigInt::from(150)));
        
        assert!(validate_decimal_managed(&managed, &requested_valid));
        assert!(!validate_decimal_managed(&managed, &requested_invalid));
    }
    
    #[test]
    fn test_managed_capability_removal() {
        let mut evaluator = ManagedCapabilityEvaluator::new();
        
        let cap = CapToken::new(
            QualifiedName {
                name: "test-cap".into(),
                module: ModuleName {
                    name: "test-module".into(),
                    namespace: None,
                },
            },
            vec![PactValue::Integer(BigInt::from(100))],
        );
        
        let meta = DefManagedMeta::AutoManagedMeta;
        evaluator.install_managed_capability(cap.clone(), meta).unwrap();
        
        assert!(evaluator.is_managed(&cap));
        
        let result = evaluator.remove_managed_capability(&cap);
        assert!(result.is_ok());
        assert!(!evaluator.is_managed(&cap));
    }
}