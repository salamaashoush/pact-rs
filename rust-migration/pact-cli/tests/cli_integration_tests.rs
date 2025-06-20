//! CLI Integration Tests for Pact CLI
//!
//! These tests validate all CLI functionality including file execution,
//! REPL scripts, error handling, and command-line argument processing.

use std::process::Command;
use std::path::PathBuf;
use std::fs;
use tempfile::TempDir;

/// Helper to get the CLI binary path
fn get_cli_path() -> PathBuf {
    let mut path = std::env::current_exe().unwrap();
    path.pop(); // Remove test binary name
    if path.ends_with("deps") {
        path.pop(); // Remove deps directory
    }
    path.push("pact");
    path
}

/// Helper to create temporary test files
struct TestFiles {
    _temp_dir: TempDir,
    dir: PathBuf,
}

impl TestFiles {
    fn new() -> Self {
        let temp_dir = TempDir::new().unwrap();
        let dir = temp_dir.path().to_path_buf();
        Self { _temp_dir: temp_dir, dir }
    }
    
    fn write(&self, name: &str, content: &str) -> PathBuf {
        let path = self.dir.join(name);
        fs::write(&path, content).unwrap();
        path
    }
    
    fn path(&self, name: &str) -> PathBuf {
        self.dir.join(name)
    }
}

#[test]
fn test_cli_version() {
    let output = Command::new(get_cli_path())
        .arg("--version")
        .output()
        .expect("Failed to run CLI");
    
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("5.0.0") || stdout.contains("version"));
}

#[test]
fn test_cli_help() {
    let output = Command::new(get_cli_path())
        .arg("--help")
        .output()
        .expect("Failed to run CLI");
    
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("Execute Pact smart contracts"));
    assert!(stdout.contains("execute"));
    assert!(stdout.contains("server"));
    assert!(stdout.contains("repl"));
}

#[test]
fn test_execute_simple_pact_file() {
    let test_files = TestFiles::new();
    let pact_file = test_files.write("simple.pact", r#"
;; Simple arithmetic
(+ 1 2)
(* 3 4)
(- 10 5)
(/ 20 4)
"#);
    
    let output = Command::new(get_cli_path())
        .arg("execute")
        .arg(&pact_file)
        .output()
        .expect("Failed to run CLI");
    
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("Result 1: 3"));
    assert!(stdout.contains("Result 2: 12"));
    assert!(stdout.contains("Result 3: 5"));
    assert!(stdout.contains("Result 4: 5"));
    assert!(stdout.contains("✓ Execution completed"));
}

#[test]
fn test_execute_boolean_operations() {
    let test_files = TestFiles::new();
    let pact_file = test_files.write("boolean.pact", r#"
;; Boolean operations
(and true false)
(or true false)
(not true)
(not false)
(if true 42 0)
(if false 0 24)
"#);
    
    let output = Command::new(get_cli_path())
        .arg("execute")
        .arg(&pact_file)
        .output()
        .expect("Failed to run CLI");
    
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("Result 1: false"));
    assert!(stdout.contains("Result 2: true"));
    assert!(stdout.contains("Result 3: false"));
    assert!(stdout.contains("Result 4: true"));
    assert!(stdout.contains("Result 5: 42"));
    assert!(stdout.contains("Result 6: 24"));
}

#[test]
fn test_execute_repl_script() {
    let test_files = TestFiles::new();
    let repl_file = test_files.write("test.repl", r#"
;; Test REPL script
(+ 1 2)
(* 4 5)
(and true true)
(or false true)
(if (> 5 3) "greater" "less")
"#);
    
    let output = Command::new(get_cli_path())
        .arg("execute")
        .arg(&repl_file)
        .output()
        .expect("Failed to run CLI");
    
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("Executing REPL script"));
    assert!(stdout.contains("✓"));
    assert!(stdout.contains("Test Summary"));
    assert!(stdout.contains("Total: 5"));
    assert!(stdout.contains("Passed: 5"));
    assert!(stdout.contains("Failed: 0"));
}

#[test]
fn test_script_discovery() {
    let test_files = TestFiles::new();
    
    // Create a .pact file
    let pact_file = test_files.write("discover.pact", r#"
;; Pact file for discovery test
(+ 10 20)
"#);
    
    // Create corresponding .repl file
    let _repl_file = test_files.write("discover.repl", r#"
;; Associated REPL script
(+ 1 2)
(* 3 4)
"#);
    
    let output = Command::new(get_cli_path())
        .arg("execute")
        .arg(&pact_file)
        .arg("--findscript")
        .output()
        .expect("Failed to run CLI");
    
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("Found REPL script"));
    assert!(stdout.contains("Executing REPL script"));
    assert!(stdout.contains("Total: 2"));
    assert!(stdout.contains("Passed: 2"));
}

#[test]
fn test_script_discovery_no_repl() {
    let test_files = TestFiles::new();
    
    // Create a .pact file without corresponding .repl
    let pact_file = test_files.write("no_repl.pact", r#"
;; Pact file without REPL script
(+ 5 7)
"#);
    
    let output = Command::new(get_cli_path())
        .arg("execute")
        .arg(&pact_file)
        .arg("--findscript")
        .output()
        .expect("Failed to run CLI");
    
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(!stdout.contains("Found REPL script"));
    assert!(stdout.contains("Executing:"));
    assert!(stdout.contains("Result 1: 12"));
}

#[test]
fn test_trace_mode() {
    let test_files = TestFiles::new();
    let pact_file = test_files.write("trace.pact", r#"
;; Simple operation for trace test
(+ 1 2)
"#);
    
    let output = Command::new(get_cli_path())
        .arg("execute")
        .arg(&pact_file)
        .arg("--trace")
        .output()
        .expect("Failed to run CLI");
    
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("Trace mode not yet implemented") || 
            stdout.contains("Result 1: 3"));
}

#[test]
fn test_execution_error() {
    let test_files = TestFiles::new();
    let error_file = test_files.write("error.pact", r#"
;; File with syntax error
(+ 1 2
;; Missing closing parenthesis
"#);
    
    let output = Command::new(get_cli_path())
        .arg("execute")
        .arg(&error_file)
        .output()
        .expect("Failed to run CLI");
    
    assert!(!output.status.success());
    let stderr = String::from_utf8(output.stderr).unwrap();
    assert!(stderr.contains("Execution error") || stderr.contains("Parse error"));
}

#[test]
fn test_repl_script_with_errors() {
    let test_files = TestFiles::new();
    let repl_file = test_files.write("error.repl", r#"
;; REPL script with some errors
(+ 1 2)      ; Should work
(/ 1 0)      ; Division by zero
(+ 3 4)      ; Should work again
"#);
    
    let output = Command::new(get_cli_path())
        .arg("execute")
        .arg(&repl_file)
        .output()
        .expect("Failed to run CLI");
    
    assert!(!output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("✓")); // Some tests pass
    assert!(stdout.contains("✗")); // Some tests fail
    assert!(stdout.contains("Failed: 1"));
}

#[test]
fn test_repl_commands() {
    let test_files = TestFiles::new();
    
    // Create a file to be loaded
    let load_file = test_files.write("load_me.pact", r#"
(+ 100 200)
"#);
    
    let repl_file = test_files.write("commands.repl", r#"
;; Test REPL commands
(+ 1 2)
.reset
(+ 3 4)
"#);
    
    let output = Command::new(get_cli_path())
        .arg("execute")
        .arg(&repl_file)
        .output()
        .expect("Failed to run CLI");
    
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("Total: 2"));
    assert!(stdout.contains("Passed: 2"));
}

#[test]
fn test_file_not_found() {
    let output = Command::new(get_cli_path())
        .arg("execute")
        .arg("nonexistent.pact")
        .output()
        .expect("Failed to run CLI");
    
    assert!(!output.status.success());
    let stderr = String::from_utf8(output.stderr).unwrap();
    assert!(stderr.contains("Failed to read file") || 
            stderr.contains("No such file"));
}

#[test]
fn test_unknown_extension() {
    let test_files = TestFiles::new();
    let unknown_file = test_files.write("test.unknown", r#"
;; File with unknown extension
(+ 1 2)
"#);
    
    let output = Command::new(get_cli_path())
        .arg("execute")
        .arg(&unknown_file)
        .output()
        .expect("Failed to run CLI");
    
    // Should still try to execute as Pact code
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("Result 1: 3"));
}

#[test]
fn test_empty_file() {
    let test_files = TestFiles::new();
    let empty_file = test_files.write("empty.pact", "");
    
    let output = Command::new(get_cli_path())
        .arg("execute")
        .arg(&empty_file)
        .output()
        .expect("Failed to run CLI");
    
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("✓ Execution completed"));
}

#[test]
fn test_comments_only() {
    let test_files = TestFiles::new();
    let comments_file = test_files.write("comments.pact", r#"
;; This file only has comments
;; No executable code
;; Should complete successfully
"#);
    
    let output = Command::new(get_cli_path())
        .arg("execute")
        .arg(&comments_file)
        .output()
        .expect("Failed to run CLI");
    
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("✓ Execution completed"));
}

#[test]
fn test_repl_script_comments() {
    let test_files = TestFiles::new();
    let repl_file = test_files.write("comments.repl", r#"
;; This is a comment and should be skipped
(+ 1 2)
;; Another comment
(* 3 4)
;; Final comment
"#);
    
    let output = Command::new(get_cli_path())
        .arg("execute")
        .arg(&repl_file)
        .output()
        .expect("Failed to run CLI");
    
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("Total: 2"));
    assert!(stdout.contains("Passed: 2"));
    assert!(stdout.contains("Failed: 0"));
}

#[test]
fn test_complex_expressions() {
    let test_files = TestFiles::new();
    let complex_file = test_files.write("complex.pact", r#"
;; Complex nested expressions
(+ (* 2 3) (- 10 5))
(if (> (+ 2 3) 4) (* 6 7) (/ 8 2))
(and (or true false) (not false))
"#);
    
    let output = Command::new(get_cli_path())
        .arg("execute")
        .arg(&complex_file)
        .output()
        .expect("Failed to run CLI");
    
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("Result 1: 11")); // (+ 6 5)
    assert!(stdout.contains("Result 2: 42")); // 6 * 7
    assert!(stdout.contains("Result 3: true")); // (and true true)
}

#[test]
fn test_execution_timing() {
    let test_files = TestFiles::new();
    let timing_file = test_files.write("timing.pact", r#"
;; Simple operation to test timing
(+ 1 2)
"#);
    
    let output = Command::new(get_cli_path())
        .arg("execute")
        .arg(&timing_file)
        .output()
        .expect("Failed to run CLI");
    
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("✓ Execution completed in"));
    assert!(stdout.contains("µs") || stdout.contains("ms") || stdout.contains("s"));
}

#[test]
fn test_multiple_results() {
    let test_files = TestFiles::new();
    let multi_file = test_files.write("multiple.pact", r#"
;; Multiple expressions
(+ 1 1)
(+ 2 2)
(+ 3 3)
(+ 4 4)
(+ 5 5)
"#);
    
    let output = Command::new(get_cli_path())
        .arg("execute")
        .arg(&multi_file)
        .output()
        .expect("Failed to run CLI");
    
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("Result 1: 2"));
    assert!(stdout.contains("Result 2: 4"));
    assert!(stdout.contains("Result 3: 6"));
    assert!(stdout.contains("Result 4: 8"));
    assert!(stdout.contains("Result 5: 10"));
}

#[test]
fn test_repl_script_mixed_success_failure() {
    let test_files = TestFiles::new();
    let mixed_file = test_files.write("mixed.repl", r#"
;; Mixed success and failure
(+ 1 2)           ; Success
(* 3 4)           ; Success  
(/ 1 0)           ; Failure (division by zero)
(- 10 5)          ; Success
(unknown-func 42) ; Failure (unknown function)
(+ 7 8)           ; Success
"#);
    
    let output = Command::new(get_cli_path())
        .arg("execute")
        .arg(&mixed_file)
        .output()
        .expect("Failed to run CLI");
    
    assert!(!output.status.success()); // Should fail due to errors
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("Total: 6"));
    assert!(stdout.contains("Passed: 4"));
    assert!(stdout.contains("Failed: 2"));
    assert!(stdout.contains("✓")); // Some successes
    assert!(stdout.contains("✗")); // Some failures
}

mod server_tests {
    use super::*;
    use std::time::Duration;
    use std::thread;

    #[test]
    fn test_server_help() {
        let output = Command::new(get_cli_path())
            .arg("server")
            .arg("--help")
            .output()
            .expect("Failed to run CLI");
        
        assert!(output.status.success());
        let stdout = String::from_utf8(output.stdout).unwrap();
        assert!(stdout.contains("Run Pact HTTP API server"));
        assert!(stdout.contains("--port"));
        assert!(stdout.contains("--config"));
    }
    
    #[test]
    fn test_server_startup_and_shutdown() {
        // Start server in background
        let mut child = Command::new(get_cli_path())
            .arg("server")
            .arg("--port")
            .arg("0") // Use port 0 to get random free port
            .spawn()
            .expect("Failed to start server");
        
        // Give server time to start
        thread::sleep(Duration::from_millis(500));
        
        // Check if server is still running
        match child.try_wait() {
            Ok(Some(_)) => panic!("Server exited immediately"),
            Ok(None) => {
                // Server is running, kill it
                child.kill().expect("Failed to kill server");
                child.wait().expect("Failed to wait for server");
            }
            Err(e) => panic!("Error checking server status: {}", e),
        }
    }
}

mod lsp_tests {
    use super::*;

    #[test]
    fn test_lsp_help() {
        let output = Command::new(get_cli_path())
            .arg("lsp")
            .arg("--help")
            .output()
            .expect("Failed to run CLI");
        
        assert!(output.status.success());
        let stdout = String::from_utf8(output.stdout).unwrap();
        assert!(stdout.contains("Language Server Protocol"));
    }
}

mod crypto_tests {
    use super::*;

    #[test]
    fn test_genkey_help() {
        let output = Command::new(get_cli_path())
            .arg("genkey")
            .arg("--help")
            .output()
            .expect("Failed to run CLI");
        
        assert!(output.status.success());
        let stdout = String::from_utf8(output.stdout).unwrap();
        assert!(stdout.contains("Generate ED25519 keypair"));
    }
    
    #[test]
    fn test_sign_help() {
        let output = Command::new(get_cli_path())
            .arg("sign")
            .arg("--help")
            .output()
            .expect("Failed to run CLI");
        
        assert!(output.status.success());
        let stdout = String::from_utf8(output.stdout).unwrap();
        assert!(stdout.contains("Sign a message"));
    }
    
    #[test]
    fn test_verify_help() {
        let output = Command::new(get_cli_path())
            .arg("verify")
            .arg("--help")
            .output()
        .expect("Failed to run CLI");
        
        assert!(output.status.success());
        let stdout = String::from_utf8(output.stdout).unwrap();
        assert!(stdout.contains("Verify a signature"));
    }
}

mod builtins_tests {
    use super::*;

    #[test]
    fn test_builtins_text() {
        let output = Command::new(get_cli_path())
            .arg("builtins")
            .output()
            .expect("Failed to run CLI");
        
        assert!(output.status.success());
        let stdout = String::from_utf8(output.stdout).unwrap();
        assert!(stdout.contains("Built-in Functions") || stdout.contains("builtins"));
    }
    
    #[test]
    fn test_builtins_json() {
        let output = Command::new(get_cli_path())
            .arg("builtins")
            .arg("--output")
            .arg("json")
            .output()
            .expect("Failed to run CLI");
        
        assert!(output.status.success());
        let stdout = String::from_utf8(output.stdout).unwrap();
        // Should be valid JSON or at least contain JSON-like structure
        assert!(stdout.contains("{") || stdout.contains("json"));
    }
}

#[test]
fn test_no_command() {
    let output = Command::new(get_cli_path())
        .output()
        .expect("Failed to run CLI");
    
    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.contains("Pact Smart Contract Language CLI") || 
            stdout.contains("Try 'pact --help'"));
}