//! Pact Intermediate Representation (IR)
//!
//! This crate provides the core IR types and transformations for the Pact language,
//! bridging the gap between the parsed AST and the evaluator.
//!
//! The IR is designed to match the Haskell implementation exactly, providing:
//! - Term representation with proper variable binding
//! - Special form handling (and, or, if, enforce, with-capability)
//! - Builtin function resolution and arity handling
//! - Name resolution and DeBruijn index assignment

pub mod term;
pub mod module;
pub mod desugar;
pub mod builtin_forms;
pub mod special_forms;
pub mod renamer;
pub mod lowering;
// pub mod pipeline; // Disabled due to type mismatches

pub use term::*;
// Re-export module types with specific names to avoid conflicts
pub use module::{
    ModuleHash, Hash, ModuleCode, EvalModule, EvalInterface, HashedModuleName,
    ModuleData as ModulePersistenceData, FullyQualifiedName, DefKind as ModuleDefKind,
    EvalDef, Namespace, Loaded, Domain, CoreModuleData, CoreEvalModule, CoreEvalInterface, CoreLoaded
};
pub use desugar::*;
pub use builtin_forms::*;
pub use special_forms::*;
pub use renamer::*;
pub use lowering::*;
// pub use pipeline::*;
