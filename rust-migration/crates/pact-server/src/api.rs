//! API route definitions

use crate::handlers;
use crate::types::*;
use axum::{
    routing::{get, post},
    Router,
};

use crate::handlers::AppState;

/// Pact API v1 routes
pub struct PactApi;

impl PactApi {
    /// Create API v1 router
    pub fn v1_router() -> Router<AppState> {
        Router::new()
            // Command endpoints
            .route("/send", post(handlers::send_handler))
            .route("/poll", post(handlers::poll_handler))
            .route("/listen", post(handlers::listen_handler))
            .route("/local", post(handlers::local_handler))
            
            // Simple API endpoints
            .route("/exec", post(handlers::exec_handler))
            .route("/cont", post(handlers::cont_handler))
    }
    
    /// Create full API router with all versions
    pub fn router() -> Router<AppState> {
        Router::new()
            .nest("/api/v1", Self::v1_router())
            // Health and version endpoints
            .route("/health", get(handlers::health_handler))
            .route("/version", get(handlers::version_handler))
    }
}