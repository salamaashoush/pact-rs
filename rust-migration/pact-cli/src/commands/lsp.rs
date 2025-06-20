//! LSP command - delegates to pact-lsp crate

use anyhow::Result;
use colored::*;
use pact_lsp::run_lsp_server;

/// Start the Pact Language Server
pub async fn start() -> Result<()> {
    println!("{}", "Starting Pact Language Server".bright_blue().bold());
    println!("{}", "Listening on stdin/stdout".dimmed());
    
    // Run LSP server
    run_lsp_server().await
}