//! Pact REPL (Read-Eval-Print Loop) implementation

use anyhow::Result;
use pact_repl::Repl;
use std::path::PathBuf;

/// Start the Pact REPL
pub fn start(_load_file: Option<PathBuf>, _script_dir: Option<PathBuf>) -> Result<()> {
    let mut repl = Repl::new()?;
    repl.run()?;
    Ok(())
}