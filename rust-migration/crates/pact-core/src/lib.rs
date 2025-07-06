//! Pact Core Foundation Library
//!
//! This crate provides the core foundation types and systems for the Pact smart contract language.
//! It consolidates what were previously separate crates into a unified foundation layer:
//!
//! - **Values**: Runtime value system (`pact_values` module)
//! - **Names**: Module names and qualification system (`pact_names` module)  
//! - **Errors**: Error types and diagnostics (`pact_errors` module)
//! - **Gas**: Gas metering system (`pact_gas` module)
//! - **Capabilities**: Capability-based security system (`pact_capability` module)
//!
//! This design follows the Haskell reference implementation's unified approach and
//! eliminates the over-segmentation that was causing maintenance issues.

// Re-export all the consolidated modules
pub mod shared;
pub mod values;
pub mod names;
pub mod errors;
pub mod gas;
pub mod capability;

// Convenience re-exports for common types
pub use shared::{Principal, PublicKeyText, KeySetName, ChainId, CryptoError, CryptoResult, DefPactId, SpanInfo};
pub use values::{PactValue, Object, Guard, Keyset};
pub use names::{ModuleName, QualifiedName, PactHash};
pub use errors::{PactError, PactErrorI, EvalError};
pub use gas::{MilliGas, MilliGasLimit, GasLimit, GasArgs};
pub use capability::{CapabilityManager};

/// Version information for the pact-core crate
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Feature flags for conditional compilation
#[cfg(feature = "test-helpers")]
pub mod test_helpers {
    //! Test utilities for the core foundation types
    pub use crate::values::test_helpers::*;
    pub use crate::names::test_helpers::*;
    pub use crate::errors::test_helpers::*;
}