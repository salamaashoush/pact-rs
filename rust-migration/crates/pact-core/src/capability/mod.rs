//! Capability-based security system for Pact
//!
//! This module implements Pact's capability-based security model,
//! closely following the Haskell implementation architecture.

pub mod types;
pub mod state;
pub mod guards;
pub mod manager;

// Re-export main types
pub use types::*;
pub use state::*;
pub use guards::*;
pub use manager::*;