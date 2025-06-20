//! Server configuration

use serde::{Deserialize, Serialize};
use std::net::SocketAddr;
use std::path::PathBuf;

/// Server configuration matching Haskell's ServerConfig
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServerConfig {
    /// Port to listen on
    #[serde(default = "default_port")]
    pub port: u16,
    
    /// Host address to bind to
    #[serde(default = "default_host")]
    pub host: String,
    
    /// Database file path
    pub db_path: Option<PathBuf>,
    
    /// Log directory for transaction logs
    pub log_dir: Option<PathBuf>,
    
    /// Persistence directory for blockchain state
    pub persist_dir: Option<PathBuf>,
    
    /// Enable debug mode
    #[serde(default)]
    pub debug: bool,
    
    /// Verbose logging
    #[serde(default)]
    pub verbose: bool,
    
    /// Gas limit configuration
    #[serde(default)]
    pub gas_config: GasConfig,
    
    /// Entity name for server identification
    #[serde(default = "default_entity")]
    pub entity: String,
}

/// Gas configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GasConfig {
    /// Default gas limit
    #[serde(default = "default_gas_limit")]
    pub gas_limit: u64,
    
    /// Gas price
    #[serde(default = "default_gas_price")]
    pub gas_price: f64,
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            port: default_port(),
            host: default_host(),
            db_path: None,
            log_dir: None,
            persist_dir: None,
            debug: false,
            verbose: false,
            gas_config: GasConfig::default(),
            entity: default_entity(),
        }
    }
}

impl Default for GasConfig {
    fn default() -> Self {
        Self {
            gas_limit: default_gas_limit(),
            gas_price: default_gas_price(),
        }
    }
}

impl ServerConfig {
    /// Load configuration from YAML file
    pub fn from_yaml_file(path: &PathBuf) -> Result<Self, anyhow::Error> {
        let content = std::fs::read_to_string(path)?;
        let config = serde_yaml::from_str(&content)?;
        Ok(config)
    }
    
    /// Get socket address for binding
    pub fn socket_addr(&self) -> Result<SocketAddr, anyhow::Error> {
        let addr = format!("{}:{}", self.host, self.port);
        addr.parse()
            .map_err(|e| anyhow::anyhow!("Invalid socket address: {}", e))
    }
}

fn default_port() -> u16 {
    8080
}

fn default_host() -> String {
    "127.0.0.1".to_string()
}

fn default_gas_limit() -> u64 {
    150000
}

fn default_gas_price() -> f64 {
    0.00000001
}

fn default_entity() -> String {
    "pact-server".to_string()
}