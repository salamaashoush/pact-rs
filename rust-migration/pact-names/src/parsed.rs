//! Parsed name types for name resolution
//!
//! This module provides types for representing parsed names without
//! circular dependencies on the module system.

use crate::{ModuleHash, ModuleName, QualifiedName};
use serde::{Deserialize, Serialize};
use std::fmt;

/// Bare name without module qualification
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct BareName(pub String);

impl BareName {
    /// Create a new bare name
    pub fn new<S: Into<String>>(name: S) -> Self {
        BareName(name.into())
    }
}

impl fmt::Display for BareName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Dynamic name for runtime resolution
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct DynamicName {
    /// Function name
    pub name: String,
    /// Call site context
    pub call: String,
}

impl DynamicName {
    /// Create a new dynamic name
    pub fn new<S1: Into<String>, S2: Into<String>>(name: S1, call: S2) -> Self {
        DynamicName {
            name: name.into(),
            call: call.into(),
        }
    }
}

impl fmt::Display for DynamicName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}::{}", self.name, self.call)
    }
}

/// Parsed name that can be bare, qualified, or dynamic
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ParsedName {
    /// Qualified name (module.member)
    Qualified(QualifiedName),
    /// Bare name (just member)
    Bare(BareName),
    /// Dynamic name (runtime resolution)
    Dynamic(DynamicName),
}

impl fmt::Display for ParsedName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParsedName::Qualified(qn) => write!(f, "{}", qn),
            ParsedName::Bare(bn) => write!(f, "{}", bn),
            ParsedName::Dynamic(dn) => write!(f, "{}", dn),
        }
    }
}

/// Hashed module name with version info
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct HashedModuleName {
    /// Module name
    pub name: ModuleName,
    /// Module hash for versioning
    pub hash: ModuleHash,
}

impl HashedModuleName {
    /// Create a new hashed module name
    pub fn new(name: ModuleName, hash: ModuleHash) -> Self {
        HashedModuleName { name, hash }
    }
}

/// Fully qualified name with hash resolution
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct FullyQualifiedName {
    /// Member name
    pub name: String,
    /// Hashed module reference
    pub module: HashedModuleName,
}

impl FullyQualifiedName {
    /// Create a new fully qualified name
    pub fn new<S: Into<String>>(name: S, module: HashedModuleName) -> Self {
        FullyQualifiedName {
            name: name.into(),
            module,
        }
    }

    /// Render for display
    pub fn render(&self) -> String {
        format!("{}.{}#{}", self.module.name, self.name, self.module.hash)
    }
}

impl fmt::Display for FullyQualifiedName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.render())
    }
}
