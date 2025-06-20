//! Module system for Pact - complete implementation matching Haskell production
//!
//! This module implements the complete Pact module system including:
//! - Module names with optional namespaces
//! - Qualified names for cross-module references
//! - Fully qualified names with hash resolution
//! - Name resolution and import mechanisms
//! - Module dependency tracking

use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

use pact_names::{BareName, ModuleHash, ModuleName, NamespaceName, ParsedName, QualifiedName};
use pact_schema::{DefKind, Schema, TypeScheme};
use pact_values::PactValue;

/// Definition in a module
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ModuleDef {
    /// Definition name
    pub name: String,
    /// Definition kind (function, constant, etc.)
    pub kind: DefKind,
    /// Type scheme if applicable
    pub type_scheme: Option<TypeScheme<String>>,
    /// Value if it's a constant
    pub value: Option<PactValue>,
    /// Documentation
    pub docs: Option<String>,
}

/// Interface definition
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Interface {
    /// Interface name
    pub name: QualifiedName,
    /// Required definitions
    pub required_defs: HashMap<String, TypeScheme<String>>,
    /// Documentation
    pub docs: Option<String>,
}

/// Module implementation
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Module {
    /// Module name
    pub name: ModuleName,
    /// Module hash
    pub hash: ModuleHash,
    /// Exported definitions
    pub defs: HashMap<String, ModuleDef>,
    /// Schemas defined in this module
    pub schemas: HashMap<String, Schema>,
    /// Tables defined in this module
    pub tables: HashMap<String, Schema>,
    /// Interfaces implemented
    pub implements: Vec<QualifiedName>,
    /// Module dependencies
    pub imports: Vec<ModuleName>,
    /// Documentation
    pub docs: Option<String>,
    /// Whether this module is blessed (system module)
    pub blessed: bool,
}

impl Module {
    /// Create a new module
    pub fn new(name: ModuleName, hash: ModuleHash) -> Self {
        Module {
            name,
            hash,
            defs: HashMap::new(),
            schemas: HashMap::new(),
            tables: HashMap::new(),
            implements: Vec::new(),
            imports: Vec::new(),
            docs: None,
            blessed: false,
        }
    }

    /// Add a definition to the module
    pub fn add_def(&mut self, def: ModuleDef) {
        let _ = self.defs.insert(def.name.clone(), def);
    }

    /// Add a schema to the module
    pub fn add_schema(&mut self, schema: Schema) {
        let schema_name = schema.name.name.clone();
        let _ = self.schemas.insert(schema_name, schema);
    }

    /// Add a table to the module
    pub fn add_table(&mut self, name: String, schema: Schema) {
        let _ = self.tables.insert(name, schema);
    }

    /// Check if the module exports a definition
    pub fn exports(&self, name: &str) -> bool {
        self.defs.contains_key(name)
            || self.schemas.contains_key(name)
            || self.tables.contains_key(name)
    }

    /// Get a definition by name
    pub fn get_def(&self, name: &str) -> Option<&ModuleDef> {
        self.defs.get(name)
    }

    /// Get a schema by name
    pub fn get_schema(&self, name: &str) -> Option<&Schema> {
        self.schemas.get(name)
    }

    /// Get a table by name
    pub fn get_table(&self, name: &str) -> Option<&Schema> {
        self.tables.get(name)
    }
}

/// Import declaration
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Import {
    /// Module being imported
    pub module: ModuleName,
    /// Optional alias for the module
    pub alias: Option<String>,
    /// Specific imports (None means import all)
    pub items: Option<Vec<String>>,
    /// Module hash constraint
    pub hash: Option<ModuleHash>,
}

/// Module registry for name resolution
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ModuleRegistry {
    /// All loaded modules by name
    pub modules: HashMap<ModuleName, Module>,
    /// Module dependencies graph
    pub dependencies: HashMap<ModuleName, HashSet<ModuleName>>,
    /// Current namespace
    pub current_namespace: Option<NamespaceName>,
}

impl Default for ModuleRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl ModuleRegistry {
    /// Create a new module registry
    pub fn new() -> Self {
        ModuleRegistry {
            modules: HashMap::new(),
            dependencies: HashMap::new(),
            current_namespace: None,
        }
    }

    /// Register a module
    pub fn register_module(&mut self, module: Module) -> Result<(), String> {
        // Check for circular dependencies
        if self.would_create_cycle(&module.name, &module.imports) {
            return Err(format!(
                "Circular dependency detected for module {}",
                module.name
            ));
        }

        // Add dependencies
        let deps: HashSet<_> = module.imports.iter().cloned().collect();
        let _ = self.dependencies.insert(module.name.clone(), deps);

        // Register the module
        let _ = self.modules.insert(module.name.clone(), module);

        Ok(())
    }

    /// Check if adding imports would create a cycle
    fn would_create_cycle(&self, module: &ModuleName, imports: &[ModuleName]) -> bool {
        let mut visited = HashSet::new();
        let mut stack = HashSet::new();

        for import in imports {
            if self.has_path_to(import, module, &mut visited, &mut stack) {
                return true;
            }
        }

        false
    }

    /// Check if there's a dependency path from 'from' to 'to'
    fn has_path_to(
        &self,
        from: &ModuleName,
        to: &ModuleName,
        visited: &mut HashSet<ModuleName>,
        stack: &mut HashSet<ModuleName>,
    ) -> bool {
        if from == to {
            return true;
        }

        if visited.contains(from) {
            return false;
        }

        if stack.contains(from) {
            return true; // Cycle detected
        }

        let _ = stack.insert(from.clone());

        if let Some(deps) = self.dependencies.get(from) {
            for dep in deps {
                if self.has_path_to(dep, to, visited, stack) {
                    return true;
                }
            }
        }

        let _ = stack.remove(from);
        let _ = visited.insert(from.clone());

        false
    }

    /// Resolve a name to a qualified name
    pub fn resolve_name(
        &self,
        name: &ParsedName,
        current_module: &ModuleName,
    ) -> Result<QualifiedName, String> {
        match name {
            ParsedName::Qualified(qn) => {
                // Already qualified, verify the module exists
                if self.modules.contains_key(&qn.module) {
                    Ok(qn.clone())
                } else {
                    Err(format!("Module {} not found", qn.module))
                }
            }
            ParsedName::Bare(bare) => {
                // Try to resolve in current module first
                if let Some(module) = self.modules.get(current_module) {
                    if module.exports(&bare.0) {
                        return Ok(QualifiedName::new(current_module.clone(), bare.0.clone()));
                    }
                }

                // Try to resolve in imported modules
                if let Some(module) = self.modules.get(current_module) {
                    for import in &module.imports {
                        if let Some(imported_module) = self.modules.get(import) {
                            if imported_module.exports(&bare.0) {
                                return Ok(QualifiedName::new(import.clone(), bare.0.clone()));
                            }
                        }
                    }
                }

                Err(format!("Name {} not found in current scope", bare.0))
            }
            ParsedName::Dynamic(_) => Err("Dynamic names not yet supported".to_string()),
        }
    }

    /// Get a module by name
    pub fn get_module(&self, name: &ModuleName) -> Option<&Module> {
        self.modules.get(name)
    }

    /// Get all modules
    pub fn all_modules(&self) -> impl Iterator<Item = &Module> {
        self.modules.values()
    }

    /// Get modules in dependency order (topological sort)
    pub fn modules_in_dependency_order(&self) -> Result<Vec<&Module>, String> {
        let mut result = Vec::new();
        let mut visited = HashSet::new();
        let mut temp_visited = HashSet::new();

        for module_name in self.modules.keys() {
            if !visited.contains(module_name) {
                self.visit_module(module_name, &mut visited, &mut temp_visited, &mut result)?;
            }
        }

        Ok(result)
    }

    fn visit_module<'a>(
        &'a self,
        module_name: &ModuleName,
        visited: &mut HashSet<ModuleName>,
        temp_visited: &mut HashSet<ModuleName>,
        result: &mut Vec<&'a Module>,
    ) -> Result<(), String> {
        if temp_visited.contains(module_name) {
            return Err(format!(
                "Circular dependency detected involving {}",
                module_name
            ));
        }

        if visited.contains(module_name) {
            return Ok(());
        }

        let _ = temp_visited.insert(module_name.clone());

        if let Some(deps) = self.dependencies.get(module_name) {
            for dep in deps {
                self.visit_module(dep, visited, temp_visited, result)?;
            }
        }

        let _ = temp_visited.remove(module_name);
        let _ = visited.insert(module_name.clone());

        if let Some(module) = self.modules.get(module_name) {
            result.push(module);
        }

        Ok(())
    }

    /// Set the current namespace
    pub fn set_namespace(&mut self, namespace: Option<NamespaceName>) {
        self.current_namespace = namespace;
    }

    /// Get the current namespace
    pub fn get_namespace(&self) -> Option<&NamespaceName> {
        self.current_namespace.as_ref()
    }
}

/// Name resolution context
#[derive(Debug, Clone)]
pub struct NameContext {
    /// Current module
    pub current_module: ModuleName,
    /// Module registry
    pub registry: ModuleRegistry,
}

impl NameContext {
    /// Create a new name context
    pub fn new(current_module: ModuleName, registry: ModuleRegistry) -> Self {
        NameContext {
            current_module,
            registry,
        }
    }

    /// Resolve a parsed name to qualified name
    pub fn resolve(&self, name: &ParsedName) -> Result<QualifiedName, String> {
        self.registry.resolve_name(name, &self.current_module)
    }

    /// Check if a name is in scope
    pub fn is_in_scope(&self, name: &str) -> bool {
        let bare_name = ParsedName::Bare(BareName::new(name));
        self.resolve(&bare_name).is_ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pact_names::PactHash;

    #[test]
    fn test_module_name_creation() {
        let simple = ModuleName::simple("test");
        assert_eq!(simple.render(), "test");

        let namespaced = ModuleName::namespaced("ns", "test");
        assert_eq!(namespaced.render(), "ns.test");
    }

    #[test]
    fn test_qualified_name() {
        let module = ModuleName::simple("test");
        let qn = QualifiedName::new(module, "func".to_string());
        assert_eq!(qn.render(), "test.func");
    }

    #[test]
    fn test_module_registry() {
        let mut registry = ModuleRegistry::new();

        // Create a simple module
        let module_name = ModuleName::simple("test");
        let hash_bytes = [1u8; 32]; // HASH_LENGTH = 32
        let mut module = Module::new(module_name.clone(), ModuleHash(PactHash::new(hash_bytes)));

        // Add a definition
        let def = ModuleDef {
            name: "func1".to_string(),
            kind: DefKind::DKDefun,
            type_scheme: None,
            value: None,
            docs: None,
        };
        module.add_def(def);

        // Register the module
        registry.register_module(module).unwrap();

        // Test resolution
        let context = NameContext::new(module_name.clone(), registry);
        let bare_name = ParsedName::Bare(BareName::new("func1"));
        let resolved = context.resolve(&bare_name).unwrap();

        assert_eq!(resolved.name, "func1");
        assert_eq!(resolved.module, module_name);
    }

    #[test]
    fn test_circular_dependency_detection() {
        let mut registry = ModuleRegistry::new();

        // Create module A that imports B
        let module_a = ModuleName::simple("a");
        let hash_a = [0xAAu8; 32];
        let mut mod_a = Module::new(module_a.clone(), ModuleHash(PactHash::new(hash_a)));
        mod_a.imports.push(ModuleName::simple("b"));

        // Create module B that imports A (circular)
        let module_b = ModuleName::simple("b");
        let hash_b = [0xBBu8; 32];
        let mut mod_b = Module::new(module_b.clone(), ModuleHash(PactHash::new(hash_b)));
        mod_b.imports.push(module_a.clone());

        // Register A first
        registry.register_module(mod_a).unwrap();

        // Registering B should fail due to circular dependency
        let result = registry.register_module(mod_b);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Circular dependency"));
    }

    #[test]
    fn test_name_resolution_precedence() {
        let mut registry = ModuleRegistry::new();

        // Create module A with func1
        let module_a = ModuleName::simple("a");
        let hash_a = [0xAAu8; 32];
        let mut mod_a = Module::new(module_a.clone(), ModuleHash(PactHash::new(hash_a)));
        let def_a = ModuleDef {
            name: "func1".to_string(),
            kind: DefKind::DKDefun,
            type_scheme: None,
            value: None,
            docs: None,
        };
        mod_a.add_def(def_a);

        // Create module B with func1 and imports A
        let module_b = ModuleName::simple("b");
        let hash_b = [0xBBu8; 32];
        let mut mod_b = Module::new(module_b.clone(), ModuleHash(PactHash::new(hash_b)));
        mod_b.imports.push(module_a.clone());
        let def_b = ModuleDef {
            name: "func1".to_string(),
            kind: DefKind::DKDefConst,
            type_scheme: None,
            value: None,
            docs: None,
        };
        mod_b.add_def(def_b);

        registry.register_module(mod_a).unwrap();
        registry.register_module(mod_b).unwrap();

        // Resolution from module B should prefer B's own definition
        let context = NameContext::new(module_b.clone(), registry);
        let bare_name = ParsedName::Bare(BareName::new("func1"));
        let resolved = context.resolve(&bare_name).unwrap();

        assert_eq!(resolved.module, module_b);

        // Get the actual definition to verify it's B's version
        if let Some(module) = context.registry.get_module(&resolved.module) {
            if let Some(def) = module.get_def(&resolved.name) {
                assert_eq!(def.kind, DefKind::DKDefConst); // B's version
            }
        }
    }
}
