//! Module definitions and related types
//!
//! This module provides the complete module system structures that match
//! the Haskell implementation exactly, including module hashing, storage,
//! and loading mechanisms.

use crate::term::{ModuleName, Name, Type, CoreBuiltin, SpanInfo, Def, Import, Governance, ExtDecl};
use compact_str::CompactString;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

/// Cryptographic hash of a module's contents
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ModuleHash(pub CompactString);

impl std::fmt::Display for ModuleHash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// General cryptographic hash (for transaction hashes, etc.)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Hash(pub CompactString);

impl std::fmt::Display for Hash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Module source code wrapper
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ModuleCode {
    pub code: CompactString,
}

impl ModuleCode {
    pub fn new(code: impl Into<CompactString>) -> Self {
        Self { code: code.into() }
    }
    
    pub fn as_str(&self) -> &str {
        &self.code
    }
}

/// Complete module definition matching Haskell exactly
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EvalModule<N, T, B, I> {
    /// Module name
    pub name: ModuleName,
    /// Governance structure
    pub governance: Governance,
    /// Module definitions (functions, constants, etc.)
    pub definitions: Vec<Def<N, T, B, I>>,
    /// Set of blessed module hashes
    pub blessed: HashSet<ModuleHash>,
    /// Module imports
    pub imports: Vec<Import<I>>,
    /// List of implemented interfaces
    pub implements: Vec<ModuleName>,
    /// Module's computed hash
    pub hash: ModuleHash,
    /// Transaction hash where module was deployed
    pub tx_hash: Hash,
    /// Original source code
    pub code: ModuleCode,
    /// Span information
    pub info: I,
}

/// Complete interface definition matching Haskell exactly
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EvalInterface<N, T, B, I> {
    /// Interface name
    pub name: ModuleName,
    /// Interface definitions
    pub definitions: Vec<Def<N, T, B, I>>,
    /// Set of blessed module hashes
    pub blessed: HashSet<ModuleHash>,
    /// Interface imports
    pub imports: Vec<Import<I>>,
    /// Interface's computed hash
    pub hash: ModuleHash,
    /// Transaction hash where interface was deployed
    pub tx_hash: Hash,
    /// Original source code
    pub code: ModuleCode,
    /// Span information
    pub info: I,
}

/// Module name with its hash for storage keys
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct HashedModuleName {
    pub name: ModuleName,
    pub hash: ModuleHash,
}

impl HashedModuleName {
    pub fn new(name: ModuleName, hash: ModuleHash) -> Self {
        Self { name, hash }
    }
}

impl std::fmt::Display for HashedModuleName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}#{}", self.name, self.hash)
    }
}

/// Module data for storage - either a module or interface with dependencies
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ModuleData<B, I> {
    ModuleData {
        module: EvalModule<Name, Type, B, I>,
        dependencies: HashMap<FullyQualifiedName, EvalDef<Name, Type, B, I>>,
    },
    InterfaceData {
        interface: EvalInterface<Name, Type, B, I>,
        dependencies: HashMap<FullyQualifiedName, EvalDef<Name, Type, B, I>>,
    },
}

impl<B, I> ModuleData<B, I> {
    /// Get the module name regardless of whether this is a module or interface
    pub fn name(&self) -> &ModuleName {
        match self {
            ModuleData::ModuleData { module, .. } => &module.name,
            ModuleData::InterfaceData { interface, .. } => &interface.name,
        }
    }
    
    /// Get the module hash regardless of whether this is a module or interface
    pub fn hash(&self) -> &ModuleHash {
        match self {
            ModuleData::ModuleData { module, .. } => &module.hash,
            ModuleData::InterfaceData { interface, .. } => &interface.hash,
        }
    }
    
    /// Get the transaction hash regardless of whether this is a module or interface
    pub fn tx_hash(&self) -> &Hash {
        match self {
            ModuleData::ModuleData { module, .. } => &module.tx_hash,
            ModuleData::InterfaceData { interface, .. } => &interface.tx_hash,
        }
    }
    
    /// Get the source code regardless of whether this is a module or interface
    pub fn code(&self) -> &ModuleCode {
        match self {
            ModuleData::ModuleData { module, .. } => &module.code,
            ModuleData::InterfaceData { interface, .. } => &interface.code,
        }
    }
    
    /// Get dependencies
    pub fn dependencies(&self) -> &HashMap<FullyQualifiedName, EvalDef<Name, Type, B, I>> {
        match self {
            ModuleData::ModuleData { dependencies, .. } => dependencies,
            ModuleData::InterfaceData { dependencies, .. } => dependencies,
        }
    }
}

/// Fully qualified name for definitions
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct FullyQualifiedName {
    pub module: ModuleName,
    pub name: CompactString,
    pub hash: ModuleHash,
}

impl std::fmt::Display for FullyQualifiedName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}#{}", self.module, self.name, self.hash)
    }
}

impl Ord for FullyQualifiedName {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Compare by module name first, then by name, then by hash
        // This provides deterministic ordering for consensus
        match self.module.render().cmp(&other.module.render()) {
            std::cmp::Ordering::Equal => {
                match self.name.cmp(&other.name) {
                    std::cmp::Ordering::Equal => self.hash.0.cmp(&other.hash.0),
                    ordering => ordering,
                }
            }
            ordering => ordering,
        }
    }
}

impl PartialOrd for FullyQualifiedName {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

/// Definition kind for tracking what type of definition something is
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum DefKind {
    DefFun,
    DefConst,
    DefCap,
    DefSchema,
    DefTable,
    DefPact,
}

/// Evaluated definition wrapper
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EvalDef<N, T, B, I> {
    pub def: Def<N, T, B, I>,
    pub kind: DefKind,
}

/// Namespace information
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Namespace {
    pub name: CompactString,
    pub user_guard: CompactString,  // Simplified for now
    pub admin_guard: CompactString, // Simplified for now
}

/// Complete loaded module environment
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Loaded<B, I> {
    /// All loaded modules by name
    pub modules: HashMap<ModuleName, ModuleData<B, I>>,
    /// Top-level names mapping to their fully qualified names
    pub toplevel: HashMap<CompactString, (FullyQualifiedName, DefKind)>,
    /// Current namespace
    pub namespace: Option<Namespace>,
    /// All loaded definitions by fully qualified name
    pub all_loaded: HashMap<FullyQualifiedName, Def<Name, Type, B, I>>,
}

impl<B, I> Default for Loaded<B, I> {
    fn default() -> Self {
        Self {
            modules: HashMap::new(),
            toplevel: HashMap::new(),
            namespace: None,
            all_loaded: HashMap::new(),
        }
    }
}

/// Storage domain types for different kinds of data
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Domain {
    /// User table storage
    UserTable(CompactString),
    /// Keyset storage
    KeysetTable,
    /// Module storage 
    ModuleTable,
    /// Namespace storage
    NamespaceTable,
    /// DefPact execution storage
    DefPactTable,
    /// Module source code storage
    ModuleSourceTable,
}

/// Type aliases for convenience
pub type CoreModuleData = ModuleData<CoreBuiltin, SpanInfo>;
pub type CoreEvalModule = EvalModule<Name, Type, CoreBuiltin, SpanInfo>;
pub type CoreEvalInterface = EvalInterface<Name, Type, CoreBuiltin, SpanInfo>;
pub type CoreLoaded = Loaded<CoreBuiltin, SpanInfo>;

/// Conversion from IR Module to EvalModule (needs hash computation)
impl<N, T, B, I> From<crate::term::Module<N, T, B, I>> for EvalModule<N, T, B, I> 
where 
    N: Clone,
    T: Clone, 
    B: Clone,
    I: Clone
{
    fn from(module: crate::term::Module<N, T, B, I>) -> Self {
        // For now, create placeholder values - proper hash computation will be added
        Self {
            name: ModuleName {
                name: module.name.clone(),
                namespace: None, // Extract from proper parsing
            },
            governance: module.governance,
            definitions: module.definitions,
            blessed: HashSet::new(), // Extract from ExtDecl::ExtBless
            imports: {
                let mut imports = Vec::new();
                for decl in &module.imports {
                    if let ExtDecl::ExtImport(import) = decl {
                        imports.push(import.clone());
                    }
                }
                imports
            },
            implements: {
                let mut implements = Vec::new();
                for decl in &module.imports {
                    if let ExtDecl::ExtImplements { module: mod_name, .. } = decl {
                        implements.push(mod_name.clone());
                    }
                }
                implements
            },
            hash: ModuleHash("placeholder-hash".into()), // Will be computed during compilation
            tx_hash: Hash("placeholder-tx-hash".into()), // Will be set during deployment
            code: ModuleCode::new(""), // Will be set from source
            info: module.info,
        }
    }
}