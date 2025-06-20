//! Main server implementation

use crate::api::PactApi;
use crate::config::ServerConfig;
use crate::handlers::AppState;
use anyhow::{Context, Result};
use axum::Router;
use tower_http::cors::CorsLayer;
use tracing::info;
use std::sync::Arc;
use pact_cek_clean::{PactDb, Domain, RowKey, RowData, TableSchema, EvalM};

/// Wrapper to adapt pact_db::MockDb to pact_cek_clean::PactDb
#[derive(Debug)]
struct DbWrapper {
    inner: pact_db::MockDb,
}

impl DbWrapper {
    fn new(inner: pact_db::MockDb) -> Self {
        Self { inner }
    }
}

impl PactDb for DbWrapper {
    fn read(&self, _domain: Domain, _key: RowKey) -> EvalM<Option<RowData>> {
        // For now, just return None - proper implementation would convert between types
        EvalM::pure_value(None)
    }

    fn write(&self, _domain: Domain, _key: RowKey, _data: RowData) -> EvalM<()> {
        // For now, just return success - proper implementation would convert between types
        EvalM::pure_value(())
    }

    fn keys(&self, _domain: Domain) -> EvalM<Vec<RowKey>> {
        // For now, just return empty vec
        EvalM::pure_value(vec![])
    }

    fn select(&self, _domain: Domain, _filter: Option<EvalM<bool>>) -> EvalM<Vec<(RowKey, RowData)>> {
        // For now, just return empty vec
        EvalM::pure_value(vec![])
    }

    fn create_table(&self, _table: String, _schema: TableSchema) -> EvalM<()> {
        // For now, just return success
        EvalM::pure_value(())
    }

    fn begin_tx(&self) -> EvalM<()> {
        // For now, just return success
        EvalM::pure_value(())
    }

    fn commit_tx(&self) -> EvalM<()> {
        // For now, just return success
        EvalM::pure_value(())
    }

    fn rollback_tx(&self) -> EvalM<()> {
        // For now, just return success
        EvalM::pure_value(())
    }
}

/// Run the Pact HTTP server
pub async fn run_server(config: ServerConfig) -> Result<()> {
    // Create a wrapper around MockDb that implements pact_eval::PactDb
    let mock_db = pact_db::MockDb::new();
    let pact_db: Arc<dyn PactDb> = Arc::new(DbWrapper::new(mock_db));
    
    // Initialize app state
    let state = AppState::new(pact_db);
    
    // Build router
    let app = Router::new()
        .merge(PactApi::router())
        .with_state(state)
        .layer(CorsLayer::permissive());
    
    // Get socket address
    let addr = config.socket_addr()?;
    
    info!("Starting Pact server on {}", addr);
    
    // Create TCP listener
    let listener = tokio::net::TcpListener::bind(addr)
        .await
        .with_context(|| format!("Failed to bind to {}", addr))?;
    
    // Run server
    axum::serve(listener, app)
        .await
        .context("Server error")?;
    
    Ok(())
}