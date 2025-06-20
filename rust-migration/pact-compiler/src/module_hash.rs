//! Module hashing implementation
//!
//! This module provides cryptographic hashing of modules following the
//! Haskell implementation exactly. Modules are hashed using CBOR serialization
//! and Blake2b-256 to create deterministic module hashes.

use pact_ir::{EvalModule, ModuleHash, Hash, Name, Type, CoreBuiltin, SpanInfo, CoreModuleData, ModuleName, FullyQualifiedName};
use serde::Serialize;
use std::collections::HashMap;
use blake2::{Blake2b512, Digest};

/// Error type for module hashing operations
#[derive(Debug, thiserror::Error)]
pub enum ModuleHashError {
    #[error("Serialization error: {0}")]
    Serialization(#[from] ciborium::ser::Error<std::io::Error>),
    #[error("Module name parsing error: {0}")]
    NameParsing(String),
    #[error("Hash computation error: {0}")]
    HashComputation(String),
}

/// A wrapper for CBOR serialization that excludes hash, tx_hash, and info fields
/// This matches the Haskell SerialiseV1 implementation
#[derive(Debug, Clone, Serialize)]
struct ModuleForHashing<'a> {
    name: &'a ModuleName,
    governance: &'a pact_ir::Governance,
    definitions: &'a Vec<pact_ir::Def<Name, Type, CoreBuiltin, SpanInfo>>,
    blessed: &'a std::collections::HashSet<ModuleHash>,
    imports: &'a Vec<pact_ir::Import<SpanInfo>>,
    implements: &'a Vec<ModuleName>,
    code: &'a pact_ir::ModuleCode,
}

impl<'a> From<&'a pact_ir::CoreEvalModule> for ModuleForHashing<'a> {
    fn from(module: &'a pact_ir::CoreEvalModule) -> Self {
        Self {
            name: &module.name,
            governance: &module.governance,
            definitions: &module.definitions,
            blessed: &module.blessed,
            imports: &module.imports,
            implements: &module.implements,
            code: &module.code,
        }
    }
}

/// Compute the hash of a module
/// 
/// This function:
/// 1. Creates a serializable version of the module excluding hash fields
/// 2. Serializes it to CBOR
/// 3. Computes Blake2b-256 hash of the serialized data
/// 4. Returns the hex-encoded hash
pub fn compute_module_hash(module: &pact_ir::CoreEvalModule) -> Result<ModuleHash, ModuleHashError> {
    // Create a version for hashing that excludes hash, tx_hash, and info
    let module_for_hash = ModuleForHashing::from(module);
    
    // Serialize to CBOR
    let mut cbor_data = Vec::new();
    ciborium::ser::into_writer(&module_for_hash, &mut cbor_data)?;
    
    // Compute Blake2b-256 hash
    let mut hasher = Blake2b512::new();
    hasher.update(&cbor_data);
    let hash_result = hasher.finalize();
    
    // Take first 32 bytes (256 bits) and convert to hex
    let hash_hex = hex::encode(&hash_result[..32]);
    
    Ok(ModuleHash(hash_hex.into()))
}

/// Update all term references in a module with new hash
/// 
/// This function walks through all terms in the module and updates any
/// resolved names that reference this module with the new hash.
pub fn update_module_hash_references(
    module: &mut pact_ir::CoreEvalModule,
    old_hash: &ModuleHash,
    new_hash: &ModuleHash,
) -> Result<(), ModuleHashError> {
    // Update all definitions
    for def in &mut module.definitions {
        update_def_hash_references(def, &module.name, old_hash, new_hash)?;
    }
    
    // Update the module's own hash
    module.hash = new_hash.clone();
    
    Ok(())
}

/// Update hash references in a definition
fn update_def_hash_references(
    def: &mut pact_ir::Def<Name, Type, CoreBuiltin, SpanInfo>,
    module_name: &ModuleName,
    old_hash: &ModuleHash,
    new_hash: &ModuleHash,
) -> Result<(), ModuleHashError> {
    use pact_ir::Def;
    
    match def {
        Def::Dfun(dfun) => {
            update_term_hash_references(&mut dfun.body, module_name, old_hash, new_hash)?;
        }
        Def::DConst(dconst) => {
            update_term_hash_references(&mut dconst.value, module_name, old_hash, new_hash)?;
        }
        Def::DCap(dcap) => {
            update_term_hash_references(&mut dcap.body, module_name, old_hash, new_hash)?;
        }
        Def::DPact(dpact) => {
            for step in &mut dpact.steps {
                use pact_ir::PactStep;
                match step {
                    PactStep::Step { entity, expr } => {
                        if let Some(entity) = entity {
                            update_term_hash_references(entity, module_name, old_hash, new_hash)?;
                        }
                        update_term_hash_references(expr, module_name, old_hash, new_hash)?;
                    }
                    PactStep::StepWithRollback { entity, expr, rollback } => {
                        if let Some(entity) = entity {
                            update_term_hash_references(entity, module_name, old_hash, new_hash)?;
                        }
                        update_term_hash_references(expr, module_name, old_hash, new_hash)?;
                        update_term_hash_references(rollback, module_name, old_hash, new_hash)?;
                    }
                }
            }
        }
        Def::DSchema(_) | Def::DTable(_) => {
            // Schemas and tables don't contain terms to update
        }
    }
    
    Ok(())
}

/// Update hash references in a term
fn update_term_hash_references(
    term: &mut pact_ir::Term<Name, Type, CoreBuiltin, SpanInfo>,
    module_name: &ModuleName,
    old_hash: &ModuleHash,
    new_hash: &ModuleHash,
) -> Result<(), ModuleHashError> {
    use pact_ir::Term;
    
    match term {
        Term::Var(name, _) => {
            update_name_hash_references(name, module_name, old_hash, new_hash);
        }
        Term::Lam { body, .. } => {
            update_term_hash_references(body, module_name, old_hash, new_hash)?;
        }
        Term::Let { expr, body, .. } => {
            update_term_hash_references(expr, module_name, old_hash, new_hash)?;
            update_term_hash_references(body, module_name, old_hash, new_hash)?;
        }
        Term::App { func, args, .. } => {
            update_term_hash_references(func, module_name, old_hash, new_hash)?;
            for arg in args {
                update_term_hash_references(arg, module_name, old_hash, new_hash)?;
            }
        }
        Term::BuiltinForm { form, .. } => {
            update_builtin_form_hash_references(form, module_name, old_hash, new_hash)?;
        }
        Term::Sequence { first, second, .. } => {
            update_term_hash_references(first, module_name, old_hash, new_hash)?;
            update_term_hash_references(second, module_name, old_hash, new_hash)?;
        }
        Term::Nullary { expr, .. } => {
            update_term_hash_references(expr, module_name, old_hash, new_hash)?;
        }
        Term::ListLit { elements, .. } => {
            for element in elements {
                update_term_hash_references(element, module_name, old_hash, new_hash)?;
            }
        }
        Term::ObjectLit { fields, .. } => {
            for (_, field_term) in fields {
                update_term_hash_references(field_term, module_name, old_hash, new_hash)?;
            }
        }
        Term::Builtin(_, _) | Term::Constant(_, _) | Term::InlineValue { .. } => {
            // These don't contain name references to update
        }
    }
    
    Ok(())
}

/// Update hash references in builtin forms
fn update_builtin_form_hash_references(
    form: &mut pact_ir::BuiltinForm<Box<pact_ir::Term<Name, Type, CoreBuiltin, SpanInfo>>>,
    module_name: &ModuleName,
    old_hash: &ModuleHash,
    new_hash: &ModuleHash,
) -> Result<(), ModuleHashError> {
    use pact_ir::BuiltinForm;
    
    match form {
        BuiltinForm::CAnd(left, right) | BuiltinForm::COr(left, right) => {
            update_term_hash_references(left, module_name, old_hash, new_hash)?;
            update_term_hash_references(right, module_name, old_hash, new_hash)?;
        }
        BuiltinForm::CIf { cond, then_expr, else_expr } => {
            update_term_hash_references(cond, module_name, old_hash, new_hash)?;
            update_term_hash_references(then_expr, module_name, old_hash, new_hash)?;
            if let Some(else_expr) = else_expr {
                update_term_hash_references(else_expr, module_name, old_hash, new_hash)?;
            }
        }
        BuiltinForm::CEnforce { cond, msg } => {
            update_term_hash_references(cond, module_name, old_hash, new_hash)?;
            update_term_hash_references(msg, module_name, old_hash, new_hash)?;
        }
        BuiltinForm::CEnforceOne { conditions } => {
            for condition in conditions {
                update_term_hash_references(condition, module_name, old_hash, new_hash)?;
            }
        }
        BuiltinForm::CWithCapability { cap, body } => {
            update_term_hash_references(cap, module_name, old_hash, new_hash)?;
            for term in body {
                update_term_hash_references(term, module_name, old_hash, new_hash)?;
            }
        }
        BuiltinForm::CCreateUserGuard { name, args } => {
            update_term_hash_references(name, module_name, old_hash, new_hash)?;
            for arg in args {
                update_term_hash_references(arg, module_name, old_hash, new_hash)?;
            }
        }
        BuiltinForm::CTry { expr, handler } => {
            update_term_hash_references(expr, module_name, old_hash, new_hash)?;
            update_term_hash_references(handler, module_name, old_hash, new_hash)?;
        }
        BuiltinForm::CMap { func, list } | BuiltinForm::CFilter { func, list } => {
            update_term_hash_references(func, module_name, old_hash, new_hash)?;
            update_term_hash_references(list, module_name, old_hash, new_hash)?;
        }
        BuiltinForm::CFold { func, init, list } => {
            update_term_hash_references(func, module_name, old_hash, new_hash)?;
            update_term_hash_references(init, module_name, old_hash, new_hash)?;
            update_term_hash_references(list, module_name, old_hash, new_hash)?;
        }
        BuiltinForm::CZip { func, list1, list2 } => {
            update_term_hash_references(func, module_name, old_hash, new_hash)?;
            update_term_hash_references(list1, module_name, old_hash, new_hash)?;
            update_term_hash_references(list2, module_name, old_hash, new_hash)?;
        }
        BuiltinForm::CCond { conditions } => {
            for (cond, result) in conditions {
                update_term_hash_references(cond, module_name, old_hash, new_hash)?;
                update_term_hash_references(result, module_name, old_hash, new_hash)?;
            }
        }
        BuiltinForm::CSuspend(term) => {
            update_term_hash_references(term, module_name, old_hash, new_hash)?;
        }
    }
    
    Ok(())
}

/// Update hash references in a name
fn update_name_hash_references(
    name: &mut Name,
    module_name: &ModuleName,
    old_hash: &ModuleHash,
    new_hash: &ModuleHash,
) {
    if let Name::Resolved(resolved) = name {
        if &resolved.module == module_name {
            if let Some(ref hash) = resolved.hash {
                if hash == &old_hash.0 {
                    resolved.hash = Some(new_hash.0.clone());
                }
            }
        }
    }
}

/// Parse a module name from a string
/// 
/// Supports both simple names and namespace.module format
pub fn parse_module_name(input: &str) -> Result<ModuleName, ModuleHashError> {
    if let Some(dot_pos) = input.find('.') {
        let namespace = &input[..dot_pos];
        let module = &input[dot_pos + 1..];
        Ok(ModuleName {
            name: module.into(),
            namespace: Some(pact_ir::NamespaceName(namespace.into())),
        })
    } else {
        Ok(ModuleName {
            name: input.into(),
            namespace: None,
        })
    }
}

/// Update module dependencies after hash change
pub fn update_module_dependencies(
    module_data: &mut CoreModuleData,
    old_hash: &ModuleHash,
    new_hash: &ModuleHash,
) -> Result<(), ModuleHashError> {
    let deps = match module_data {
        pact_ir::ModuleData::ModuleData { dependencies, .. } => dependencies,
        pact_ir::ModuleData::InterfaceData { dependencies, .. } => dependencies,
    };
    
    // Update FQN keys that reference the old hash
    let old_keys: Vec<_> = deps.keys()
        .filter(|fqn| &fqn.hash == old_hash)
        .cloned()
        .collect();
    
    for old_key in old_keys {
        if let Some(eval_def) = deps.remove(&old_key) {
            let new_key = FullyQualifiedName {
                module: old_key.module,
                name: old_key.name,
                hash: new_hash.clone(),
            };
            deps.insert(new_key, eval_def);
        }
    }
    
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use pact_ir::*;
    
    #[test]
    fn test_module_hash_computation() {
        // Create a minimal module for testing
        let module = EvalModule {
            name: ModuleName {
                name: "test-module".into(),
                namespace: None,
            },
            governance: Governance::KeyGov("test-keyset".into()),
            definitions: vec![],
            blessed: std::collections::HashSet::new(),
            imports: vec![],
            implements: vec![],
            hash: ModuleHash("placeholder".into()),
            tx_hash: Hash("placeholder".into()),
            code: ModuleCode::new("(module test-module)"),
            info: SpanInfo::empty(),
        };
        
        let hash = compute_module_hash(&module).unwrap();
        
        // Hash should be deterministic
        let hash2 = compute_module_hash(&module).unwrap();
        assert_eq!(hash, hash2);
        
        // Hash should be hex-encoded and 64 characters (32 bytes * 2)
        assert_eq!(hash.0.len(), 64);
        assert!(hash.0.chars().all(|c| c.is_ascii_hexdigit()));
    }
    
    #[test]
    fn test_parse_module_name() {
        // Simple module name
        let simple = parse_module_name("coin").unwrap();
        assert_eq!(simple.name, "coin");
        assert_eq!(simple.namespace, None);
        
        // Namespaced module name
        let namespaced = parse_module_name("user.my-module").unwrap();
        assert_eq!(namespaced.name, "my-module");
        assert_eq!(namespaced.namespace, Some(NamespaceName("user".into())));
    }
}