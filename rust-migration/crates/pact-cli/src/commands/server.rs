//! Server command - delegates to pact-server crate

use anyhow::Result;
use colored::*;
use pact_server::{ServerConfig, run_server};
use std::path::PathBuf;

/// Start the Pact HTTP API server
pub async fn start(
    port: u16,
    config: Option<PathBuf>,
    db: Option<String>,
    log_dir: Option<String>,
    persist_dir: Option<String>,
) -> Result<()> {
    println!("{}", "Starting Pact HTTP API Server".bright_blue().bold());
    
    // Load or create config
    let mut server_config = if let Some(config_path) = config {
        println!("{} {}", "Loading config:".bright_cyan(), config_path.display());
        ServerConfig::from_yaml_file(&config_path)?
    } else {
        ServerConfig::default()
    };
    
    // Override with command line options
    server_config.port = port;
    if let Some(db_path) = db {
        server_config.db_path = Some(PathBuf::from(db_path));
    }
    if let Some(dir) = log_dir {
        server_config.log_dir = Some(PathBuf::from(dir));
    }
    if let Some(dir) = persist_dir {
        server_config.persist_dir = Some(PathBuf::from(dir));
    }
    
    // Display configuration
    println!("{}: http://{}:{}", "Address".bright_cyan(), server_config.host, server_config.port);
    if let Some(ref db) = server_config.db_path {
        println!("{}: {}", "Database".bright_cyan(), db.display());
    }
    if let Some(ref logs) = server_config.log_dir {
        println!("{}: {}", "Log directory".bright_cyan(), logs.display());
    }
    if let Some(ref persist) = server_config.persist_dir {
        println!("{}: {}", "Persist directory".bright_cyan(), persist.display());
    }
    
    println!();
    println!("{}", "Endpoints:".bright_green());
    println!("  POST /api/v1/send   - Submit commands");
    println!("  POST /api/v1/poll   - Check command status");
    println!("  POST /api/v1/listen - Wait for results");
    println!("  POST /api/v1/local  - Local execution");
    println!("  POST /api/v1/exec   - Simple execution");
    println!("  POST /api/v1/cont   - Continue pact");
    println!("  GET  /health        - Health check");
    println!("  GET  /version       - Version info");
    println!();
    println!("{}", "Press Ctrl+C to stop".dimmed());
    
    // Run server
    run_server(server_config).await
}