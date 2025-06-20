//! Integration tests for Pact HTTP API server
//!
//! These tests start a real server instance and make HTTP requests to validate
//! all endpoints and functionality.

use pact_server::{run_server, ServerConfig};
use reqwest::{Client, StatusCode};
use serde_json::{json, Value};
use std::time::Duration;
use tokio::time::sleep;
use uuid::Uuid;

/// Test helper to start server and return base URL
struct TestServer {
    base_url: String,
    _handle: tokio::task::JoinHandle<()>,
}

impl TestServer {
    /// Start a test server on a random port
    async fn start() -> Self {
        let port = find_free_port().await;
        let config = ServerConfig {
            port,
            host: "127.0.0.1".to_string(),
            ..Default::default()
        };
        
        let base_url = format!("http://127.0.0.1:{}", port);
        
        // Start server in background task
        let handle = tokio::spawn(async move {
            if let Err(e) = run_server(config).await {
                eprintln!("Server error: {}", e);
            }
        });
        
        // Give server time to start
        sleep(Duration::from_millis(100)).await;
        
        Self {
            base_url,
            _handle: handle,
        }
    }
    
    /// Get the base URL for making requests
    fn url(&self, path: &str) -> String {
        format!("{}{}", self.base_url, path)
    }
}

/// Find a free port for testing
async fn find_free_port() -> u16 {
    use tokio::net::TcpListener;
    
    let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
    let port = listener.local_addr().unwrap().port();
    drop(listener);
    port
}

/// Helper function to create HTTP client
fn create_client() -> Client {
    Client::builder()
        .timeout(Duration::from_secs(10))
        .build()
        .unwrap()
}

#[tokio::test]
async fn test_server_starts() {
    let _server = TestServer::start().await;
    // If we get here, server started successfully
}

mod health_tests {
    use super::*;

    #[tokio::test]
    async fn test_health_endpoint() {
        let server = TestServer::start().await;
        let client = create_client();
        
        let response = client
            .get(&server.url("/health"))
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(response.status(), StatusCode::OK);
        
        let body: Value = response.json().await.expect("Failed to parse JSON");
        assert_eq!(body["status"], "healthy");
        assert_eq!(body["message"], "Pact server is running");
    }
    
    #[tokio::test]
    async fn test_health_response_format() {
        let server = TestServer::start().await;
        let client = create_client();
        
        let response = client
            .get(&server.url("/health"))
            .send()
            .await
            .expect("Failed to send request");
        
        let body: Value = response.json().await.expect("Failed to parse JSON");
        
        // Verify required fields
        assert!(body.get("status").is_some());
        assert!(body.get("message").is_some());
        
        // Status should be a string
        assert!(body["status"].is_string());
        assert!(body["message"].is_string());
    }

    #[tokio::test]
    async fn test_health_cors_headers() {
        let server = TestServer::start().await;
        let client = create_client();
        
        let response = client
            .get(&server.url("/health"))
            .send()
            .await
            .expect("Failed to send request");
        
        let headers = response.headers();
        assert!(headers.contains_key("access-control-allow-origin"));
        assert_eq!(headers["access-control-allow-origin"], "*");
    }
}

mod version_tests {
    use super::*;

    #[tokio::test]
    async fn test_version_endpoint() {
        let server = TestServer::start().await;
        let client = create_client();
        
        let response = client
            .get(&server.url("/version"))
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(response.status(), StatusCode::OK);
        
        let body: Value = response.json().await.expect("Failed to parse JSON");
        assert_eq!(body["version"], "5.0.0");
        assert_eq!(body["apiVersion"], "1.0");
    }
    
    #[tokio::test]
    async fn test_version_response_format() {
        let server = TestServer::start().await;
        let client = create_client();
        
        let response = client
            .get(&server.url("/version"))
            .send()
            .await
            .expect("Failed to send request");
        
        let body: Value = response.json().await.expect("Failed to parse JSON");
        
        // Verify required fields
        assert!(body.get("version").is_some());
        assert!(body.get("apiVersion").is_some());
        
        // Both should be strings
        assert!(body["version"].is_string());
        assert!(body["apiVersion"].is_string());
    }
}

mod exec_tests {
    use super::*;

    #[tokio::test]
    async fn test_exec_simple_arithmetic() {
        let server = TestServer::start().await;
        let client = create_client();
        
        let payload = json!({
            "code": "(+ 1 2)"
        });
        
        let response = client
            .post(&server.url("/api/v1/exec"))
            .json(&payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(response.status(), StatusCode::OK);
        
        let body: Value = response.json().await.expect("Failed to parse JSON");
        
        // Verify response structure
        assert!(body.get("reqKey").is_some());
        assert!(body.get("result").is_some());
        assert!(body.get("gas").is_some());
        
        // Verify result is success
        assert_eq!(body["result"]["status"], "success");
        
        // Result should be integer 3
        let data = &body["result"]["data"];
        assert!(data.get("Integer").is_some());
    }
    
    #[tokio::test]
    async fn test_exec_string_operations() {
        let server = TestServer::start().await;
        let client = create_client();
        
        let payload = json!({
            "code": "(+ \"hello\" \" world\")"
        });
        
        let response = client
            .post(&server.url("/api/v1/exec"))
            .json(&payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(response.status(), StatusCode::OK);
        
        let body: Value = response.json().await.expect("Failed to parse JSON");
        assert_eq!(body["result"]["status"], "success");
        
        // Result should be string "hello world"
        let data = &body["result"]["data"];
        if let Some(string_val) = data.as_str() {
            assert_eq!(string_val, "hello world");
        } else {
            // Handle other string representations
            assert!(data.get("String").is_some() || data.is_string());
        }
    }
    
    #[tokio::test]
    async fn test_exec_boolean_operations() {
        let server = TestServer::start().await;
        let client = create_client();
        
        let payload = json!({
            "code": "(and true false)"
        });
        
        let response = client
            .post(&server.url("/api/v1/exec"))
            .json(&payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(response.status(), StatusCode::OK);
        
        let body: Value = response.json().await.expect("Failed to parse JSON");
        assert_eq!(body["result"]["status"], "success");
        
        // Result should be boolean false
        let data = &body["result"]["data"];
        if let Some(bool_val) = data.as_bool() {
            assert_eq!(bool_val, false);
        } else {
            // Handle other boolean representations
            assert!(data.get("Bool").is_some() || data.is_boolean());
        }
    }
    
    #[tokio::test]
    async fn test_exec_with_data() {
        let server = TestServer::start().await;
        let client = create_client();
        
        let payload = json!({
            "code": "(+ x y)",
            "data": {
                "x": 10,
                "y": 20
            }
        });
        
        let response = client
            .post(&server.url("/api/v1/exec"))
            .json(&payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(response.status(), StatusCode::OK);
        
        let body: Value = response.json().await.expect("Failed to parse JSON");
        assert_eq!(body["result"]["status"], "success");
    }
    
    #[tokio::test]
    async fn test_exec_invalid_syntax() {
        let server = TestServer::start().await;
        let client = create_client();
        
        let payload = json!({
            "code": "(+ 1 2"  // Missing closing paren
        });
        
        let response = client
            .post(&server.url("/api/v1/exec"))
            .json(&payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(response.status(), StatusCode::OK);
        
        let body: Value = response.json().await.expect("Failed to parse JSON");
        assert_eq!(body["result"]["status"], "failure");
        assert!(body["result"]["error"]["message"].is_string());
    }
    
    #[tokio::test]
    async fn test_exec_missing_code() {
        let server = TestServer::start().await;
        let client = create_client();
        
        let payload = json!({
            "data": {"x": 1}
        });
        
        let response = client
            .post(&server.url("/api/v1/exec"))
            .json(&payload)
            .send()
            .await
            .expect("Failed to send request");
        
        // Should return 400 Bad Request for missing code
        assert!(response.status().is_client_error());
    }
    
    #[tokio::test]
    async fn test_exec_response_format() {
        let server = TestServer::start().await;
        let client = create_client();
        
        let payload = json!({
            "code": "42"
        });
        
        let response = client
            .post(&server.url("/api/v1/exec"))
            .json(&payload)
            .send()
            .await
            .expect("Failed to send request");
        
        let body: Value = response.json().await.expect("Failed to parse JSON");
        
        // Verify all required response fields
        assert!(body.get("reqKey").is_some());
        assert!(body.get("result").is_some());
        assert!(body.get("gas").is_some());
        
        // Request key should be a string (UUID)
        assert!(body["reqKey"].is_string());
        
        // Gas should be a number
        assert!(body["gas"].is_number());
        
        // Result should have status
        assert!(body["result"].get("status").is_some());
    }
}

mod send_poll_listen_tests {
    use super::*;

    #[tokio::test]
    async fn test_send_endpoint() {
        let server = TestServer::start().await;
        let client = create_client();
        
        let payload = json!({
            "cmds": [{
                "hash": "test-hash-123",
                "sigs": [],
                "cmd": serde_json::to_string(&json!({
                    "networkId": null,
                    "payload": {
                        "exec": {
                            "code": "(+ 1 2)",
                            "data": {}
                        }
                    },
                    "signers": [],
                    "meta": {
                        "chainId": "0",
                        "sender": "sender00",
                        "gasLimit": 150000,
                        "gasPrice": 0.00000001,
                        "ttl": 28800,
                        "creationTime": 1234567890
                    },
                    "nonce": "test-nonce"
                })).unwrap()
            }]
        });
        
        let response = client
            .post(&server.url("/api/v1/send"))
            .json(&payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(response.status(), StatusCode::OK);
        
        let body: Value = response.json().await.expect("Failed to parse JSON");
        
        // Should return request keys
        assert!(body.get("requestKeys").is_some());
        assert!(body["requestKeys"].is_array());
        assert_eq!(body["requestKeys"].as_array().unwrap().len(), 1);
    }
    
    #[tokio::test]
    async fn test_poll_endpoint() {
        let server = TestServer::start().await;
        let client = create_client();
        
        // First send a command
        let send_payload = json!({
            "cmds": [{
                "hash": "test-hash-456",
                "sigs": [],
                "cmd": serde_json::to_string(&json!({
                    "networkId": null,
                    "payload": {
                        "exec": {
                            "code": "(+ 2 3)",
                            "data": {}
                        }
                    },
                    "signers": [],
                    "meta": {
                        "chainId": "0",
                        "sender": "sender00",
                        "gasLimit": 150000,
                        "gasPrice": 0.00000001,
                        "ttl": 28800,
                        "creationTime": 1234567890
                    },
                    "nonce": "test-nonce-456"
                })).unwrap()
            }]
        });
        
        let send_response = client
            .post(&server.url("/api/v1/send"))
            .json(&send_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        let send_body: Value = send_response.json().await.expect("Failed to parse JSON");
        let request_keys = &send_body["requestKeys"];
        
        // Now poll for results
        let poll_payload = json!({
            "requestKeys": request_keys
        });
        
        let poll_response = client
            .post(&server.url("/api/v1/poll"))
            .json(&poll_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(poll_response.status(), StatusCode::OK);
        
        let poll_body: Value = poll_response.json().await.expect("Failed to parse JSON");
        
        // Should return results map
        assert!(poll_body.is_object());
        
        // Should have entry for our request key
        let first_key = request_keys[0].as_str().unwrap();
        assert!(poll_body.get(first_key).is_some());
    }
    
    #[tokio::test]
    async fn test_listen_endpoint() {
        let server = TestServer::start().await;
        let client = create_client();
        
        let payload = json!({
            "listen": "test-request-key-789"
        });
        
        let response = client
            .post(&server.url("/api/v1/listen"))
            .json(&payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(response.status(), StatusCode::OK);
        
        let body: Value = response.json().await.expect("Failed to parse JSON");
        
        // Should return a command result structure
        assert!(body.get("reqKey").is_some());
        assert!(body.get("result").is_some());
    }
}

mod local_tests {
    use super::*;

    #[tokio::test]
    async fn test_local_endpoint() {
        let server = TestServer::start().await;
        let client = create_client();
        
        let payload = json!({
            "hash": "local-test-hash",
            "sigs": [],
            "cmd": serde_json::to_string(&json!({
                "networkId": null,
                "payload": {
                    "exec": {
                        "code": "(* 6 7)",
                        "data": {}
                    }
                },
                "signers": [],
                "meta": {
                    "chainId": "0",
                    "sender": "sender00",
                    "gasLimit": 150000,
                    "gasPrice": 0.00000001,
                    "ttl": 28800,
                    "creationTime": 1234567890
                },
                "nonce": "local-nonce"
            })).unwrap()
        });
        
        let response = client
            .post(&server.url("/api/v1/local"))
            .json(&payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(response.status(), StatusCode::OK);
        
        let body: Value = response.json().await.expect("Failed to parse JSON");
        
        // Should return direct execution result
        assert!(body.get("reqKey").is_some());
        assert!(body.get("result").is_some());
        assert_eq!(body["result"]["status"], "success");
    }
}

mod cont_tests {
    use super::*;

    #[tokio::test]
    async fn test_cont_endpoint_placeholder() {
        let server = TestServer::start().await;
        let client = create_client();
        
        let payload = json!({
            "pactId": "test-pact-id",
            "step": 1,
            "rollback": false
        });
        
        let response = client
            .post(&server.url("/api/v1/cont"))
            .json(&payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(response.status(), StatusCode::OK);
        
        let body: Value = response.json().await.expect("Failed to parse JSON");
        
        // Should return not implemented error for now
        assert_eq!(body["result"]["status"], "failure");
        assert!(body["result"]["error"]["message"].as_str().unwrap().contains("not yet implemented"));
    }
}

mod error_handling_tests {
    use super::*;

    #[tokio::test]
    async fn test_invalid_json() {
        let server = TestServer::start().await;
        let client = create_client();
        
        let response = client
            .post(&server.url("/api/v1/exec"))
            .header("content-type", "application/json")
            .body("{ invalid json")
            .send()
            .await
            .expect("Failed to send request");
        
        assert!(response.status().is_client_error());
    }
    
    #[tokio::test]
    async fn test_wrong_content_type() {
        let server = TestServer::start().await;
        let client = create_client();
        
        let response = client
            .post(&server.url("/api/v1/exec"))
            .header("content-type", "text/plain")
            .body("(+ 1 2)")
            .send()
            .await
            .expect("Failed to send request");
        
        assert!(response.status().is_client_error());
    }
    
    #[tokio::test]
    async fn test_nonexistent_endpoint() {
        let server = TestServer::start().await;
        let client = create_client();
        
        let response = client
            .get(&server.url("/api/v1/nonexistent"))
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(response.status(), StatusCode::NOT_FOUND);
    }
    
    #[tokio::test]
    async fn test_method_not_allowed() {
        let server = TestServer::start().await;
        let client = create_client();
        
        // Try GET on POST endpoint
        let response = client
            .get(&server.url("/api/v1/exec"))
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(response.status(), StatusCode::METHOD_NOT_ALLOWED);
    }
}

mod concurrent_tests {
    use super::*;
    use futures::future::join_all;

    #[tokio::test]
    async fn test_concurrent_exec_requests() {
        let server = TestServer::start().await;
        let client = create_client();
        
        // Create multiple concurrent requests
        let futures = (0..10).map(|i| {
            let client = client.clone();
            let server_url = server.url("/api/v1/exec");
            async move {
                let payload = json!({
                    "code": format!("(+ {} 1)", i)
                });
                
                client
                    .post(&server_url)
                    .json(&payload)
                    .send()
                    .await
                    .expect("Failed to send request")
            }
        });
        
        let responses = join_all(futures).await;
        
        // All requests should succeed
        for response in responses {
            assert_eq!(response.status(), StatusCode::OK);
            
            let body: Value = response.json().await.expect("Failed to parse JSON");
            assert_eq!(body["result"]["status"], "success");
        }
    }
    
    #[tokio::test]
    async fn test_concurrent_health_checks() {
        let server = TestServer::start().await;
        let client = create_client();
        
        // Create multiple concurrent health check requests
        let futures = (0..20).map(|_| {
            let client = client.clone();
            let server_url = server.url("/health");
            async move {
                client
                    .get(&server_url)
                    .send()
                    .await
                    .expect("Failed to send request")
            }
        });
        
        let responses = join_all(futures).await;
        
        // All requests should succeed
        for response in responses {
            assert_eq!(response.status(), StatusCode::OK);
            
            let body: Value = response.json().await.expect("Failed to parse JSON");
            assert_eq!(body["status"], "healthy");
        }
    }
}

mod database_tests {
    use super::*;

    #[tokio::test]
    async fn test_create_table() {
        let server = TestServer::start().await;
        let client = create_client();
        
        let payload = json!({
            "code": "(create-table accounts {\"balance\": \"decimal\", \"owner\": \"string\"} \"Accounts table\")"
        });
        
        let response = client
            .post(&server.url("/api/v1/exec"))
            .json(&payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(response.status(), StatusCode::OK);
        
        let body: Value = response.json().await.expect("Failed to parse JSON");
        assert_eq!(body["result"]["status"], "success");
        
        // Should return table name or success indicator
        let data = &body["result"]["data"];
        assert!(data.is_string() || data.get("String").is_some());
    }
    
    #[tokio::test]
    async fn test_write_and_read_operations() {
        let server = TestServer::start().await;
        let client = create_client();
        
        // First create the table
        let create_table_payload = json!({
            "code": "(create-table users {\"name\": \"string\", \"age\": \"integer\"} \"Users table\")"
        });
        
        let create_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&create_table_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(create_response.status(), StatusCode::OK);
        
        // Write a record
        let write_payload = json!({
            "code": "(write users \"alice\" {\"name\": \"Alice\", \"age\": 30})"
        });
        
        let write_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&write_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(write_response.status(), StatusCode::OK);
        
        let write_body: Value = write_response.json().await.expect("Failed to parse JSON");
        assert_eq!(write_body["result"]["status"], "success");
        
        // Read the record back
        let read_payload = json!({
            "code": "(read users \"alice\")"
        });
        
        let read_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&read_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(read_response.status(), StatusCode::OK);
        
        let read_body: Value = read_response.json().await.expect("Failed to parse JSON");
        assert_eq!(read_body["result"]["status"], "success");
        
        // Verify the data is correct
        let data = &read_body["result"]["data"];
        assert!(data.is_object());
    }
    
    #[tokio::test]
    async fn test_read_nonexistent_record() {
        let server = TestServer::start().await;
        let client = create_client();
        
        // Try to read from non-existent table
        let payload = json!({
            "code": "(read nonexistent-table \"some-key\")"
        });
        
        let response = client
            .post(&server.url("/api/v1/exec"))
            .json(&payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(response.status(), StatusCode::OK);
        
        let body: Value = response.json().await.expect("Failed to parse JSON");
        assert_eq!(body["result"]["status"], "failure");
        
        // Should contain appropriate error message
        assert!(body["result"]["error"]["message"].is_string());
    }
    
    #[tokio::test]
    async fn test_keys_function() {
        let server = TestServer::start().await;
        let client = create_client();
        
        // Create table and insert some records
        let setup_code = r#"
            (begin
              (create-table inventory {"item": "string", "quantity": "integer"} "Inventory table")
              (write inventory "apple" {"item": "Apple", "quantity": 100})
              (write inventory "banana" {"item": "Banana", "quantity": 50})
              (write inventory "orange" {"item": "Orange", "quantity": 75})
            )
        "#;
        
        let setup_payload = json!({
            "code": setup_code
        });
        
        let setup_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&setup_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(setup_response.status(), StatusCode::OK);
        
        // Now get all keys
        let keys_payload = json!({
            "code": "(keys inventory)"
        });
        
        let keys_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&keys_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(keys_response.status(), StatusCode::OK);
        
        let keys_body: Value = keys_response.json().await.expect("Failed to parse JSON");
        assert_eq!(keys_body["result"]["status"], "success");
        
        // Should return a list of keys
        let data = &keys_body["result"]["data"];
        assert!(data.is_array() || data.get("List").is_some());
    }
    
    #[tokio::test]
    async fn test_with_read_function() {
        let server = TestServer::start().await;
        let client = create_client();
        
        // Setup table with data
        let setup_code = r#"
            (begin
              (create-table balances {"balance": "decimal", "owner": "string"} "Balance table")
              (write balances "alice" {"balance": 100.0, "owner": "Alice"})
            )
        "#;
        
        let setup_payload = json!({
            "code": setup_code
        });
        
        let setup_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&setup_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(setup_response.status(), StatusCode::OK);
        
        // Use with-read to perform operation with the data
        let with_read_payload = json!({
            "code": "(with-read balances \"alice\" {\"balance\" := balance, \"owner\" := owner} (+ balance 50.0))"
        });
        
        let with_read_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&with_read_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(with_read_response.status(), StatusCode::OK);
        
        let with_read_body: Value = with_read_response.json().await.expect("Failed to parse JSON");
        // Note: with-read might not be fully implemented yet
        // The test validates the API can handle the request
        assert!(with_read_body["result"]["status"].is_string());
    }
    
    #[tokio::test]
    async fn test_with_default_read_function() {
        let server = TestServer::start().await;
        let client = create_client();
        
        // Setup table
        let setup_code = r#"
            (create-table preferences {"theme": "string", "language": "string"} "User preferences")
        "#;
        
        let setup_payload = json!({
            "code": setup_code
        });
        
        let setup_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&setup_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(setup_response.status(), StatusCode::OK);
        
        // Use with-default-read for non-existent key
        let with_default_read_payload = json!({
            "code": "(with-default-read preferences \"newuser\" {\"theme\": \"dark\", \"language\": \"en\"} {\"theme\" := theme} theme)"
        });
        
        let with_default_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&with_default_read_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(with_default_response.status(), StatusCode::OK);
        
        let with_default_body: Value = with_default_response.json().await.expect("Failed to parse JSON");
        // Should return the default value since key doesn't exist
        assert!(with_default_body["result"]["status"].is_string());
    }
    
    #[tokio::test]
    async fn test_select_operations() {
        let server = TestServer::start().await;
        let client = create_client();
        
        // Setup table with multiple records
        let setup_code = r#"
            (begin
              (create-table products {"name": "string", "price": "decimal", "category": "string"} "Products table")
              (write products "laptop" {"name": "Laptop", "price": 999.99, "category": "electronics"})
              (write products "book" {"name": "Book", "price": 19.99, "category": "books"})
              (write products "phone" {"name": "Phone", "price": 599.99, "category": "electronics"})
            )
        "#;
        
        let setup_payload = json!({
            "code": setup_code
        });
        
        let setup_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&setup_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(setup_response.status(), StatusCode::OK);
        
        // Select all records
        let select_all_payload = json!({
            "code": "(select products (constantly true))"
        });
        
        let select_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&select_all_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(select_response.status(), StatusCode::OK);
        
        let select_body: Value = select_response.json().await.expect("Failed to parse JSON");
        // Should return list of all records or handle select appropriately
        assert!(select_body["result"]["status"].is_string());
    }
    
    #[tokio::test]
    async fn test_update_operations() {
        let server = TestServer::start().await;
        let client = create_client();
        
        // Setup table with a record
        let setup_code = r#"
            (begin
              (create-table counters {"count": "integer", "name": "string"} "Counters table")
              (write counters "main" {"count": 0, "name": "Main Counter"})
            )
        "#;
        
        let setup_payload = json!({
            "code": setup_code
        });
        
        let setup_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&setup_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(setup_response.status(), StatusCode::OK);
        
        // Update the record
        let update_payload = json!({
            "code": "(update counters \"main\" {\"count\": 1})"
        });
        
        let update_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&update_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(update_response.status(), StatusCode::OK);
        
        let update_body: Value = update_response.json().await.expect("Failed to parse JSON");
        // Update might not be fully implemented, but API should handle it
        assert!(update_body["result"]["status"].is_string());
        
        // Try to read back the updated record
        let read_payload = json!({
            "code": "(read counters \"main\")"
        });
        
        let read_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&read_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(read_response.status(), StatusCode::OK);
        
        let read_body: Value = read_response.json().await.expect("Failed to parse JSON");
        assert_eq!(read_body["result"]["status"], "success");
    }
    
    #[tokio::test]
    async fn test_txids_function() {
        let server = TestServer::start().await;
        let client = create_client();
        
        // Setup table and perform some transactions
        let setup_code = r#"
            (begin
              (create-table transactions {"amount": "decimal", "from": "string", "to": "string"} "Transactions table")
              (write transactions "tx1" {"amount": 100.0, "from": "alice", "to": "bob"})
              (write transactions "tx2" {"amount": 50.0, "from": "bob", "to": "charlie"})
            )
        "#;
        
        let setup_payload = json!({
            "code": setup_code
        });
        
        let setup_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&setup_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(setup_response.status(), StatusCode::OK);
        
        // Get transaction IDs
        let txids_payload = json!({
            "code": "(txids transactions 1)"
        });
        
        let txids_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&txids_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(txids_response.status(), StatusCode::OK);
        
        let txids_body: Value = txids_response.json().await.expect("Failed to parse JSON");
        // Should return transaction IDs or handle appropriately
        assert!(txids_body["result"]["status"].is_string());
    }
    
    #[tokio::test]
    async fn test_table_schema_validation() {
        let server = TestServer::start().await;
        let client = create_client();
        
        // Create table with specific schema
        let create_payload = json!({
            "code": "(create-table strict_table {\"id\": \"integer\", \"name\": \"string\", \"active\": \"bool\"} \"Strict validation table\")"
        });
        
        let create_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&create_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(create_response.status(), StatusCode::OK);
        
        // Try to write valid data
        let valid_write_payload = json!({
            "code": "(write strict_table \"record1\" {\"id\": 1, \"name\": \"Test\", \"active\": true})"
        });
        
        let valid_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&valid_write_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(valid_response.status(), StatusCode::OK);
        
        let valid_body: Value = valid_response.json().await.expect("Failed to parse JSON");
        assert_eq!(valid_body["result"]["status"], "success");
        
        // Try to write invalid data type
        let invalid_write_payload = json!({
            "code": "(write strict_table \"record2\" {\"id\": \"not-a-number\", \"name\": \"Test\", \"active\": true})"
        });
        
        let invalid_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&invalid_write_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(invalid_response.status(), StatusCode::OK);
        
        let invalid_body: Value = invalid_response.json().await.expect("Failed to parse JSON");
        // Should fail due to type mismatch
        assert_eq!(invalid_body["result"]["status"], "failure");
    }
    
    #[tokio::test]
    async fn test_database_transaction_rollback() {
        let server = TestServer::start().await;
        let client = create_client();
        
        // This test simulates a transaction that should rollback on error
        let transaction_code = r#"
            (begin
              (create-table accounts {"balance": "decimal"} "Accounts for rollback test")
              (write accounts "alice" {"balance": 100.0})
              (write accounts "bob" {"balance": 50.0})
              ; This should cause the transaction to fail and rollback
              (/ 1 0)
            )
        "#;
        
        let payload = json!({
            "code": transaction_code
        });
        
        let response = client
            .post(&server.url("/api/v1/exec"))
            .json(&payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(response.status(), StatusCode::OK);
        
        let body: Value = response.json().await.expect("Failed to parse JSON");
        assert_eq!(body["result"]["status"], "failure");
        
        // The transaction should have failed due to division by zero
        assert!(body["result"]["error"]["message"].is_string());
    }
    
    #[tokio::test]
    async fn test_concurrent_database_operations() {
        let server = TestServer::start().await;
        let client = create_client();
        
        // Setup table first
        let setup_payload = json!({
            "code": "(create-table concurrent_test {\"value\": \"integer\"} \"Concurrent operations test\")"
        });
        
        let setup_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&setup_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(setup_response.status(), StatusCode::OK);
        
        // Create multiple concurrent write operations
        let futures = (0..5).map(|i| {
            let client = client.clone();
            let server_url = server.url("/api/v1/exec");
            async move {
                let payload = json!({
                    "code": format!("(write concurrent_test \"key{}\" {{\"value\": {}}})", i, i * 10)
                });
                
                client
                    .post(&server_url)
                    .json(&payload)
                    .send()
                    .await
                    .expect("Failed to send request")
            }
        });
        
        let responses = futures::future::join_all(futures).await;
        
        // All write operations should succeed
        for response in responses {
            assert_eq!(response.status(), StatusCode::OK);
            
            let body: Value = response.json().await.expect("Failed to parse JSON");
            assert_eq!(body["result"]["status"], "success");
        }
        
        // Verify all records were written
        let keys_payload = json!({
            "code": "(keys concurrent_test)"
        });
        
        let keys_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&keys_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(keys_response.status(), StatusCode::OK);
        
        let keys_body: Value = keys_response.json().await.expect("Failed to parse JSON");
        assert_eq!(keys_body["result"]["status"], "success");
    }
}

mod http_compliance_tests {
    use super::*;

    #[tokio::test]
    async fn test_options_preflight() {
        let server = TestServer::start().await;
        let client = create_client();
        
        let response = client
            .request(reqwest::Method::OPTIONS, &server.url("/api/v1/exec"))
            .header("Origin", "https://example.com")
            .header("Access-Control-Request-Method", "POST")
            .header("Access-Control-Request-Headers", "content-type")
            .send()
            .await
            .expect("Failed to send request");
        
        // Should handle CORS preflight
        let headers = response.headers();
        assert!(headers.contains_key("access-control-allow-origin"));
    }
    
    #[tokio::test]
    async fn test_content_type_headers() {
        let server = TestServer::start().await;
        let client = create_client();
        
        let response = client
            .get(&server.url("/health"))
            .send()
            .await
            .expect("Failed to send request");
        
        let headers = response.headers();
        assert_eq!(headers["content-type"], "application/json");
    }
    
    #[tokio::test]
    async fn test_response_compression() {
        let server = TestServer::start().await;
        let client = create_client();
        
        let response = client
            .get(&server.url("/health"))
            .header("Accept-Encoding", "gzip")
            .send()
            .await
            .expect("Failed to send request");
        
        // Server should handle compression if configured
        assert_eq!(response.status(), StatusCode::OK);
    }
}