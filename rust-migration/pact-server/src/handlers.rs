//! Request handlers for all API endpoints

use crate::command::{
    Command, CommandResult, ExecPayload, Meta, Payload, PactError, PactResult, RequestKey,
};
use crate::types::*;
use axum::{
    extract::{Json, State},
    http::StatusCode,
    response::IntoResponse,
};
use pact_cek_clean::PactDb;
use pact_compiler::{CompilePipeline, RawCode, compile_pact_source, compile_value_to_pact_value};
use pact_values::PactValue;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;

/// Application state
#[derive(Clone)]
pub struct AppState {
    /// Database backend
    pub pact_db: Arc<dyn PactDb>,
    
    /// Pending results cache
    pub results: Arc<Mutex<HashMap<RequestKey, CommandResult>>>,
}

impl AppState {
    /// Create new app state
    pub fn new(pact_db: Arc<dyn PactDb>) -> Self {
        Self {
            pact_db,
            results: Arc::new(Mutex::new(HashMap::new())),
        }
    }
}

/// Send handler - submit commands for execution
pub async fn send_handler(
    State(state): State<AppState>,
    Json(req): Json<SendRequest>,
) -> impl IntoResponse {
    let mut request_keys = Vec::new();
    
    for cmd in req.cmds {
        let req_key = RequestKey::from_command(&cmd);
        request_keys.push(req_key.clone());
        
        // Parse payload
        let payload: Payload = match serde_json::from_str(&cmd.cmd) {
            Ok(p) => p,
            Err(_) => return (StatusCode::BAD_REQUEST, Json("Invalid payload")).into_response(),
        };
        
        // Execute command asynchronously
        let state = state.clone();
        let req_key_clone = req_key.clone();
        tokio::spawn(async move {
            let result = execute_command(state.clone(), payload).await;
            
            // Store result
            let mut results = state.results.lock().await;
            results.insert(req_key_clone, result);
        });
    }
    
    Json(SendResponse { request_keys }).into_response()
}

/// Poll handler - check status of submitted commands
pub async fn poll_handler(
    State(state): State<AppState>,
    Json(req): Json<PollRequest>,
) -> impl IntoResponse {
    let results = state.results.lock().await;
    let mut response = HashMap::new();
    
    for req_key in req.request_keys {
        if let Some(result) = results.get(&req_key) {
            response.insert(req_key, result.clone());
        }
    }
    
    Json(response).into_response()
}

/// Listen handler - wait for a specific command to complete
pub async fn listen_handler(
    State(state): State<AppState>,
    Json(req): Json<ListenRequest>,
) -> impl IntoResponse {
    // Simple polling implementation - in production would use proper async notification
    for _ in 0..300 { // 30 seconds timeout
        let results = state.results.lock().await;
        if let Some(result) = results.get(&req.listen) {
            return Json(result.clone()).into_response();
        }
        drop(results);
        tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
    }
    
    StatusCode::REQUEST_TIMEOUT.into_response()
}

/// Local handler - execute command locally without blockchain
pub async fn local_handler(
    State(state): State<AppState>,
    Json(req): Json<LocalRequest>,
) -> impl IntoResponse {
    let payload: Payload = match serde_json::from_str(&req.cmd.cmd) {
        Ok(p) => p,
        Err(_) => return StatusCode::BAD_REQUEST.into_response(),
    };
    
    let result = execute_command(state, payload).await;
    Json(result).into_response()
}

/// Exec handler - simple execution endpoint
pub async fn exec_handler(
    State(state): State<AppState>,
    Json(req): Json<ExecRequest>,
) -> impl IntoResponse {
    // Convert to command format
    let payload = Payload {
        network_id: None,
        exec: Some(ExecPayload {
            code: req.code,
            data: req.data.unwrap_or_default()
                .into_iter()
                .map(|(k, v)| {
                    // Convert JSON value to PactValue (simplified)
                    let pact_val = json_to_pact_value(v);
                    (k, pact_val)
                })
                .collect(),
        }),
        cont: None,
        meta: Meta {
            chain_id: "0".to_string(),
            sender: "sender00".to_string(),
            gas_limit: 150000,
            gas_price: 0.00000001,
            ttl: 28800,
            creation_time: chrono::Utc::now().timestamp() as u64,
        },
        nonce: req.nonce.unwrap_or_else(|| uuid::Uuid::new_v4().to_string()),
    };
    
    let result = execute_command(state, payload).await;
    Json(result).into_response()
}

/// Cont handler - continuation execution endpoint
pub async fn cont_handler(
    State(state): State<AppState>,
    Json(req): Json<ContRequest>,
) -> impl IntoResponse {
    // TODO: Implement continuation handling
    Json(CommandResult {
        req_key: RequestKey::new("cont-not-implemented".to_string()),
        tx_id: None,
        result: PactResult::Failure {
            error: PactError {
                message: "Continuation execution not yet implemented".to_string(),
                detail: None,
            },
        },
        gas: 0,
        logs: vec![],
        events: vec![],
        meta_data: None,
    }).into_response()
}

/// Health check handler
pub async fn health_handler() -> impl IntoResponse {
    Json(HealthResponse {
        status: HealthStatus::Healthy,
        message: Some("Pact server is running".to_string()),
        components: None,
    })
}

/// Version handler
pub async fn version_handler() -> impl IntoResponse {
    Json(VersionInfo {
        version: env!("CARGO_PKG_VERSION").to_string(),
        api_version: "1.0".to_string(),
        chainweb_version: None,
        node_version: None,
    })
}

/// Execute a command payload
async fn execute_command(state: AppState, payload: Payload) -> CommandResult {
    let req_key = RequestKey::new(uuid::Uuid::new_v4().to_string());
    
    // Execute based on payload type
    let result = if let Some(exec) = payload.exec {
        // Compile and execute code using pact-compiler
        match compile_pact_source(&exec.code) {
            Ok(compile_result) => {
                // Create pipeline for execution
                let mut pipeline = CompilePipeline::new();
                
                // Execute the compiled code
                match pipeline.interpret_top_level(
                    RawCode(exec.code.clone()),
                    compile_result.top_level,
                    state.pact_db.clone(),
                ) {
                    Ok(compile_value) => {
                        let pact_value = compile_value_to_pact_value(&compile_value);
                        PactResult::Success { data: pact_value }
                    }
                    Err(err) => PactResult::Failure {
                        error: PactError {
                            message: err.to_string(),
                            detail: None,
                        },
                    },
                }
            }
            Err(err) => PactResult::Failure {
                error: PactError {
                    message: format!("Compilation error: {}", err),
                    detail: None,
                },
            },
        }
    } else if let Some(_cont) = payload.cont {
        // Handle continuation
        PactResult::Failure {
            error: PactError {
                message: "Continuations not yet implemented".to_string(),
                detail: None,
            },
        }
    } else {
        PactResult::Failure {
            error: PactError {
                message: "Invalid payload: must have either exec or cont".to_string(),
                detail: None,
            },
        }
    };
    
    CommandResult {
        req_key,
        tx_id: None,
        result,
        gas: 0, // TODO: Track gas
        logs: vec![],
        events: vec![],
        meta_data: None,
    }
}

/// Convert JSON value to PactValue (simplified)
fn json_to_pact_value(val: serde_json::Value) -> PactValue {
    use num_bigint::BigInt;
    
    match val {
        serde_json::Value::Null => PactValue::Unit,
        serde_json::Value::Bool(b) => PactValue::Bool(b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                PactValue::Integer(BigInt::from(i))
            } else if let Some(f) = n.as_f64() {
                // Convert to Pact decimal
                PactValue::Decimal(pact_values::Decimal::from_f64(f))
            } else {
                PactValue::Integer(BigInt::from(0))
            }
        }
        serde_json::Value::String(s) => PactValue::String(s),
        serde_json::Value::Array(arr) => {
            PactValue::List(arr.into_iter().map(json_to_pact_value).collect())
        }
        serde_json::Value::Object(obj) => {
            let mut pact_obj = pact_values::Object::new();
            for (k, v) in obj {
                pact_obj.insert(k, json_to_pact_value(v));
            }
            PactValue::Object(pact_obj)
        }
    }
}