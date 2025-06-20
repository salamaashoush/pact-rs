//! Pact CLI - Command Line Interface for the Pact Smart Contract Language
//!
//! This is the main entry point for the Pact CLI, providing all features
//! from the Haskell implementation including REPL, server mode, crypto tools,
//! and more.

mod cli;
mod commands;

use anyhow::Result;
use clap::Parser;
use colored::*;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

use crate::cli::{Cli, Commands};

#[tokio::main]
async fn main() -> Result<()> {
    // Parse command line arguments
    let cli = Cli::parse();

    // Initialize logging based on verbosity
    init_logging(cli.verbose);

    // Handle version flag first
    if cli.version {
        commands::version::execute()?;
        return Ok(());
    }

    // Handle commands
    match cli.command {
        Some(Commands::Version) => {
            commands::version::execute()?;
        }
        Some(Commands::GenKey { output }) => {
            commands::crypto::genkey(output)?;
        }
        Some(Commands::Sign { secret_key, message, output }) => {
            commands::crypto::sign(&secret_key, &message, output.as_deref())?;
        }
        Some(Commands::Verify { public_key, message, signature }) => {
            commands::crypto::verify(&public_key, &message, &signature)?;
        }
        Some(Commands::Builtins { output }) => {
            commands::builtins::execute(output)?;
        }
        Some(Commands::Execute { file, trace, find_script }) => {
            commands::execute::run(file, trace, find_script)?;
        }
        Some(Commands::Repl { load, script_dir }) => {
            commands::repl::start(load, script_dir)?;
        }
        Some(Commands::Lsp) => {
            // TODO: Fix LSP compilation errors
            eprintln!("LSP command is temporarily disabled while we fix compilation issues");
            std::process::exit(1);
        }
        Some(Commands::Server { port: _, config: _, db: _, log_dir: _, persist_dir: _ }) => {
            // TODO: Fix server compilation errors
            eprintln!("Server command is temporarily disabled while we fix compilation issues");
            std::process::exit(1);
        }
        Some(Commands::ApiReq { command }) => {
            match command {
                cli::ApiCommands::Exec { code, data, keypairs, nonce, output, chain_id } => {
                    commands::api::format_exec(code, data, keypairs, nonce, output, chain_id)?;
                }
                cli::ApiCommands::Cont { pact_id, step, rollback, data, keypairs, nonce, output, chain_id } => {
                    commands::api::format_cont(pact_id, step, rollback, data, keypairs, nonce, output, chain_id)?;
                }
            }
        }
        Some(Commands::CheckNatives { file }) => {
            commands::check::check_shadowing(&file)?;
        }
        None => {
            // No command specified, print help
            println!("{}", "Pact Smart Contract Language CLI".bright_blue().bold());
            println!("Try 'pact --help' for more information.");
        }
    }

    Ok(())
}

/// Initialize logging/tracing based on verbosity level
fn init_logging(verbose: u8) {
    let filter_level = match verbose {
        0 => "warn",
        1 => "info",
        2 => "debug",
        _ => "trace",
    };

    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| filter_level.into()),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();
}