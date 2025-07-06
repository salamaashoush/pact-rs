//! Main server implementation

use crate::api::PactApi;
use crate::config::ServerConfig;
use crate::handlers::AppState;
use anyhow::{Context, Result};
use axum::Router;
use tower_http::cors::CorsLayer;
use tracing::info;
use std::sync::Arc;

/// Run the Pact HTTP API server
pub async fn run_server(config: ServerConfig) -> Result<()> {
    info!("Starting Pact server on {}:{}", config.host, config.port);

    // Create a simple in-memory database for now
    let db = Arc::new(pact_db::MockDb::new());
    
    // Create application state
    let state = AppState { 
        db: db.clone()
    };

    // Build the router with all API routes
    let app = Router::new()
        .merge(PactApi::routes())
        .with_state(state)
        .layer(CorsLayer::permissive());

    // Start the server
    let addr = format!("{}:{}", config.host, config.port);
    let listener = tokio::net::TcpListener::bind(&addr).await
        .with_context(|| format!("Failed to bind to {}", addr))?;
    
    info!("Server listening on {}", addr);
    
    axum::serve(listener, app)
        .await
        .with_context(|| "Server failed to start")?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_server_config() {
        let config = ServerConfig {
            host: "127.0.0.1".to_string(),
            port: 8080,
        };
        assert_eq!(config.host, "127.0.0.1");
        assert_eq!(config.port, 8080);
    }
}