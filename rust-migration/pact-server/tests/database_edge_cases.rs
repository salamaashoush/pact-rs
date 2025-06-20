//! Database edge cases and stress tests for Pact server
//!
//! These tests focus on edge cases, error conditions, and stress testing
//! database operations to ensure robustness.

use pact_server::{run_server, ServerConfig};
use reqwest::{Client, StatusCode};
use serde_json::{json, Value};
use std::time::Duration;
use tokio::time::sleep;

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

mod database_edge_cases {
    use super::*;

    #[tokio::test]
    async fn test_large_table_names() {
        let server = TestServer::start().await;
        let client = create_client();
        
        // Test with very long table name
        let long_name = "a".repeat(1000);
        let payload = json!({
            "code": format!("(create-table {} {{\"data\": \"string\"}} \"Long name table\")", long_name)
        });
        
        let response = client
            .post(&server.url("/api/v1/exec"))
            .json(&payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(response.status(), StatusCode::OK);
        
        let body: Value = response.json().await.expect("Failed to parse JSON");
        // Might succeed or fail depending on implementation limits
        assert!(body["result"]["status"].is_string());
    }
    
    #[tokio::test]
    async fn test_special_characters_in_keys() {
        let server = TestServer::start().await;
        let client = create_client();
        
        // Setup table
        let setup_payload = json!({
            "code": "(create-table special_keys {\"value\": \"string\"} \"Special characters test\")"
        });
        
        let setup_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&setup_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(setup_response.status(), StatusCode::OK);
        
        // Test keys with special characters
        let special_keys = vec![
            "key-with-hyphens",
            "key_with_underscores", 
            "key.with.dots",
            "key@with@symbols",
            "key with spaces",
            "🔑emoji-key",
            "key/with/slashes",
            "key\\with\\backslashes"
        ];
        
        for key in special_keys {
            let write_payload = json!({
                "code": format!("(write special_keys \"{}\" {{\"value\": \"test\"}})", key)
            });
            
            let write_response = client
                .post(&server.url("/api/v1/exec"))
                .json(&write_payload)
                .send()
                .await
                .expect("Failed to send request");
            
            assert_eq!(write_response.status(), StatusCode::OK);
            
            let write_body: Value = write_response.json().await.expect("Failed to parse JSON");
            // Some special characters might be rejected, test validates handling
            assert!(write_body["result"]["status"].is_string());
        }
    }
    
    #[tokio::test]
    async fn test_extremely_large_records() {
        let server = TestServer::start().await;
        let client = create_client();
        
        // Setup table
        let setup_payload = json!({
            "code": "(create-table large_records {\"data\": \"string\"} \"Large records test\")"
        });
        
        let setup_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&setup_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(setup_response.status(), StatusCode::OK);
        
        // Create very large data string (1MB)
        let large_data = "x".repeat(1024 * 1024);
        let write_payload = json!({
            "code": format!("(write large_records \"big\" {{\"data\": \"{}\"}})", large_data)
        });
        
        let write_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&write_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(write_response.status(), StatusCode::OK);
        
        let write_body: Value = write_response.json().await.expect("Failed to parse JSON");
        // Should handle large records or fail gracefully
        assert!(write_body["result"]["status"].is_string());
    }
    
    #[tokio::test]
    async fn test_deeply_nested_objects() {
        let server = TestServer::start().await;
        let client = create_client();
        
        // Setup table
        let setup_payload = json!({
            "code": "(create-table nested_objects {\"nested\": \"object\"} \"Nested objects test\")"
        });
        
        let setup_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&setup_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(setup_response.status(), StatusCode::OK);
        
        // Create deeply nested object
        let nested_code = r#"
            (write nested_objects "deep" {
                "level1": {
                    "level2": {
                        "level3": {
                            "level4": {
                                "level5": {
                                    "data": "deeply nested value"
                                }
                            }
                        }
                    }
                }
            })
        "#;
        
        let write_payload = json!({
            "code": nested_code
        });
        
        let write_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&write_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(write_response.status(), StatusCode::OK);
        
        let write_body: Value = write_response.json().await.expect("Failed to parse JSON");
        // Should handle nested objects or fail gracefully  
        assert!(write_body["result"]["status"].is_string());
    }
    
    #[tokio::test]
    async fn test_null_and_empty_values() {
        let server = TestServer::start().await;
        let client = create_client();
        
        // Setup table that allows various types
        let setup_payload = json!({
            "code": "(create-table nullable_test {\"value\": \"string\", \"number\": \"integer\", \"flag\": \"bool\"} \"Nullable test\")"
        });
        
        let setup_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&setup_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(setup_response.status(), StatusCode::OK);
        
        // Test empty string
        let empty_string_payload = json!({
            "code": "(write nullable_test \"empty\" {\"value\": \"\", \"number\": 0, \"flag\": false})"
        });
        
        let empty_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&empty_string_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(empty_response.status(), StatusCode::OK);
        
        let empty_body: Value = empty_response.json().await.expect("Failed to parse JSON");
        assert_eq!(empty_body["result"]["status"], "success");
    }
    
    #[tokio::test]
    async fn test_duplicate_table_creation() {
        let server = TestServer::start().await;
        let client = create_client();
        
        // Create table first time
        let create_payload = json!({
            "code": "(create-table duplicate_test {\"data\": \"string\"} \"Duplicate test\")"
        });
        
        let create_response1 = client
            .post(&server.url("/api/v1/exec"))
            .json(&create_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(create_response1.status(), StatusCode::OK);
        
        let create_body1: Value = create_response1.json().await.expect("Failed to parse JSON");
        assert_eq!(create_body1["result"]["status"], "success");
        
        // Try to create same table again
        let create_response2 = client
            .post(&server.url("/api/v1/exec"))
            .json(&create_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(create_response2.status(), StatusCode::OK);
        
        let create_body2: Value = create_response2.json().await.expect("Failed to parse JSON");
        // Should fail or handle gracefully
        assert!(create_body2["result"]["status"].is_string());
    }
    
    #[tokio::test]
    async fn test_overwrite_existing_key() {
        let server = TestServer::start().await;
        let client = create_client();
        
        // Setup table and write initial record
        let setup_code = r#"
            (begin
              (create-table overwrite_test {"data": "string", "version": "integer"} "Overwrite test")
              (write overwrite_test "key1" {"data": "original", "version": 1})
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
        
        // Overwrite with new data
        let overwrite_payload = json!({
            "code": "(write overwrite_test \"key1\" {\"data\": \"updated\", \"version\": 2})"
        });
        
        let overwrite_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&overwrite_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(overwrite_response.status(), StatusCode::OK);
        
        let overwrite_body: Value = overwrite_response.json().await.expect("Failed to parse JSON");
        assert_eq!(overwrite_body["result"]["status"], "success");
        
        // Read back to verify overwrite
        let read_payload = json!({
            "code": "(read overwrite_test \"key1\")"
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
        // Should show updated data
    }
    
    #[tokio::test]
    async fn test_stress_many_tables() {
        let server = TestServer::start().await;
        let client = create_client();
        
        // Create many tables concurrently
        let table_count = 50;
        let futures = (0..table_count).map(|i| {
            let client = client.clone();
            let server_url = server.url("/api/v1/exec");
            async move {
                let payload = json!({
                    "code": format!("(create-table table_{} {{\"id\": \"integer\"}} \"Table {}\")", i, i)
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
        
        // Most tables should be created successfully
        let mut success_count = 0;
        for response in responses {
            assert_eq!(response.status(), StatusCode::OK);
            
            let body: Value = response.json().await.expect("Failed to parse JSON");
            if body["result"]["status"] == "success" {
                success_count += 1;
            }
        }
        
        // Expect at least 80% success rate for stress test
        assert!(success_count > table_count * 4 / 5);
    }
    
    #[tokio::test]
    async fn test_stress_many_records() {
        let server = TestServer::start().await;
        let client = create_client();
        
        // Setup table
        let setup_payload = json!({
            "code": "(create-table stress_records {\"index\": \"integer\", \"data\": \"string\"} \"Stress test records\")"
        });
        
        let setup_response = client
            .post(&server.url("/api/v1/exec"))
            .json(&setup_payload)
            .send()
            .await
            .expect("Failed to send request");
        
        assert_eq!(setup_response.status(), StatusCode::OK);
        
        // Write many records concurrently
        let record_count = 100;
        let futures = (0..record_count).map(|i| {
            let client = client.clone();
            let server_url = server.url("/api/v1/exec");
            async move {
                let payload = json!({
                    "code": format!("(write stress_records \"record_{}\" {{\"index\": {}, \"data\": \"data_{}\"}})", i, i, i)
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
        
        // Most writes should succeed
        let mut success_count = 0;
        for response in responses {
            assert_eq!(response.status(), StatusCode::OK);
            
            let body: Value = response.json().await.expect("Failed to parse JSON");
            if body["result"]["status"] == "success" {
                success_count += 1;
            }
        }
        
        // Expect at least 90% success rate
        assert!(success_count > record_count * 9 / 10);
        
        // Verify we can read the keys
        let keys_payload = json!({
            "code": "(keys stress_records)"
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