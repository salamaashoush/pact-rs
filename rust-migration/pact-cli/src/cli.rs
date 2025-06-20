//! Command-line interface definition using clap
//!
//! This module defines all CLI commands, options, and flags that match
//! the functionality of the Haskell Pact implementation.

use clap::{Parser, Subcommand};
use std::path::PathBuf;

/// Pact Smart Contract Language CLI
#[derive(Parser, Debug)]
#[command(name = "pact")]
#[command(version)]
#[command(disable_version_flag = true)]
#[command(about = "Execute Pact smart contracts and interact with the Pact system")]
#[command(long_about = "
Pact is a smart contract language designed for blockchains.

When run without arguments, starts an interactive REPL.
With a file argument, executes the file (or associated .repl script).
")]
pub struct Cli {
    /// File to execute (with .pact or .repl extension)
    #[arg(value_name = "FILE")]
    pub file: Option<PathBuf>,
    
    /// Display version information
    #[arg(short = 'V', long = "version")]
    pub version: bool,
    
    /// List all built-in functions
    #[arg(short = 'b', long)]
    pub builtins: bool,
    
    /// Enable trace output
    #[arg(short = 't', long)]
    pub trace: bool,
    
    /// For .pact files, attempts to find and execute a .repl script
    #[arg(short = 'r', long = "findscript")]
    pub find_script: bool,
    
    /// Increase verbosity (can be used multiple times)
    #[arg(short = 'v', long, action = clap::ArgAction::Count)]
    pub verbose: u8,
    
    /// Subcommand to execute
    #[command(subcommand)]
    pub command: Option<Commands>,
}

/// Available subcommands
#[derive(Subcommand, Debug)]
pub enum Commands {
    /// Display version information
    Version,
    
    /// List all built-in functions
    Builtins {
        /// Output format (json, yaml, or text)
        #[arg(short, long, default_value = "text")]
        output: String,
    },
    
    /// Start the Language Server Protocol (LSP) server
    Lsp,
    
    /// Run Pact HTTP API server
    Server {
        /// Port to listen on
        #[arg(short, long, default_value = "8080")]
        port: u16,
        
        /// Server configuration file (YAML format)
        #[arg(short = 'c', long)]
        config: Option<PathBuf>,
        
        /// Database path
        #[arg(short = 'd', long)]
        db: Option<String>,
        
        /// Log directory
        #[arg(short = 'l', long)]
        log_dir: Option<String>,
        
        /// Persist directory
        #[arg(short = 'P', long)]
        persist_dir: Option<String>,
    },
    
    /// Generate ED25519 keypair
    #[command(name = "genkey")]
    GenKey {
        /// Output file prefix (will create .pub and .key files)
        #[arg(short, long)]
        output: Option<String>,
    },
    
    /// Sign a message with ED25519 key
    Sign {
        /// Secret key file path
        #[arg(short = 'k', long)]
        secret_key: PathBuf,
        
        /// Message file to sign
        #[arg(short = 'm', long)]
        message: PathBuf,
        
        /// Output file for signature
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
    
    /// Verify a signature
    Verify {
        /// Public key file path
        #[arg(short = 'k', long)]
        public_key: PathBuf,
        
        /// Message file
        #[arg(short = 'm', long)]
        message: PathBuf,
        
        /// Signature file
        #[arg(short = 's', long)]
        signature: PathBuf,
    },
    
    /// Format API request
    #[command(name = "apireq")]
    ApiReq {
        #[command(subcommand)]
        command: ApiCommands,
    },
    
    /// Check for native function shadowing
    #[command(name = "check-natives")]
    CheckNatives {
        /// Pact file to check
        file: PathBuf,
    },
    
    /// Execute a Pact file or script
    Execute {
        /// File to execute
        file: PathBuf,
        
        /// Enable trace output
        #[arg(short, long)]
        trace: bool,
        
        /// Find and execute associated .repl script
        #[arg(short = 'r', long = "findscript")]
        find_script: bool,
    },
    
    /// Start interactive REPL
    Repl {
        /// Load a file on startup
        #[arg(short = 'l', long)]
        load: Option<PathBuf>,
        
        /// Set script directory for relative paths
        #[arg(short = 's', long)]
        script_dir: Option<PathBuf>,
    },
}

/// API request subcommands
#[derive(Subcommand, Debug)]
pub enum ApiCommands {
    /// Format an exec command
    Exec {
        /// Code to execute
        code: String,
        
        /// Data file (JSON)
        #[arg(short, long)]
        data: Option<String>,
        
        /// Keypair files
        #[arg(short, long)]
        keypairs: Vec<String>,
        
        /// Nonce value
        #[arg(short, long)]
        nonce: Option<String>,
        
        /// Output file
        #[arg(short, long)]
        output: Option<String>,
        
        /// Chain ID
        #[arg(short = 'c', long)]
        chain_id: Option<String>,
    },
    
    /// Format a continuation command
    Cont {
        /// Pact ID
        pact_id: String,
        
        /// Step number
        step: u32,
        
        /// Rollback flag
        #[arg(short, long)]
        rollback: bool,
        
        /// Data file (JSON)
        #[arg(short, long)]
        data: Option<String>,
        
        /// Keypair files
        #[arg(short, long)]
        keypairs: Vec<String>,
        
        /// Nonce value
        #[arg(short, long)]
        nonce: Option<String>,
        
        /// Output file
        #[arg(short, long)]
        output: Option<String>,
        
        /// Chain ID
        #[arg(short = 'c', long)]
        chain_id: Option<String>,
    },
}