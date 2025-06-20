# Pact Server Integration Tests

This directory contains comprehensive integration tests for the Pact HTTP API server, validating all endpoints, database operations, and system behavior.

## Test Overview

### Core Test Files

- **`integration_tests.rs`** - Main integration test suite with 39 test cases
- **`database_edge_cases.rs`** - Edge cases and stress tests for database operations

### Test Categories

#### 1. Health & Version Tests (4 tests)
- ✅ Health endpoint functionality
- ✅ Version endpoint functionality  
- ✅ Response format validation
- ✅ CORS headers verification

#### 2. Exec Endpoint Tests (8 tests)
- ✅ Simple arithmetic operations: `(+ 1 2)` → `3`
- ✅ String operations and concatenation
- ✅ Boolean logic operations
- ✅ Execution with data parameters
- ✅ Invalid syntax error handling
- ✅ Missing code parameter validation
- ✅ Response format verification
- ✅ Request/response structure validation

#### 3. Send/Poll/Listen Workflow Tests (3 tests)
- ✅ Command submission via `/api/v1/send`
- ✅ Result polling via `/api/v1/poll`  
- ✅ Async listening via `/api/v1/listen`
- ✅ Request key generation and tracking

#### 4. Local Execution Tests (1 test)
- ✅ Direct execution via `/api/v1/local`
- ✅ Immediate result return

#### 5. Continuation Tests (1 test)
- ✅ Pact continuation endpoint placeholder
- ✅ Error handling for unimplemented features

#### 6. Database Integration Tests (13 tests)
- ✅ Table creation: `create-table`
- ✅ Record operations: `write` and `read`
- ✅ Non-existent record handling
- ✅ Key enumeration: `keys` function
- ✅ Context-aware operations: `with-read`
- ✅ Default value handling: `with-default-read`
- ✅ Query operations: `select` function
- ✅ Record updates: `update` function
- ✅ Transaction tracking: `txids` function
- ✅ Schema validation and type checking
- ✅ Transaction rollback on errors
- ✅ Concurrent database operations
- ✅ Data integrity verification

#### 7. Error Handling Tests (4 tests)
- ✅ Invalid JSON request handling
- ✅ Wrong content-type rejection
- ✅ Non-existent endpoint (404) responses
- ✅ Method not allowed (405) responses

#### 8. Concurrent Operation Tests (2 tests)
- ✅ Multiple simultaneous exec requests (10 concurrent)
- ✅ Multiple simultaneous health checks (20 concurrent)
- ✅ Thread safety validation
- ✅ Performance under load

#### 9. HTTP Compliance Tests (3 tests)
- ✅ CORS preflight handling (OPTIONS)
- ✅ Content-Type header validation
- ✅ Response compression support

#### 10. Database Edge Cases (10 tests)
- ✅ Large table names (1000 characters)
- ✅ Special characters in keys
- ✅ Extremely large records (1MB)
- ✅ Deeply nested objects
- ✅ Null and empty values
- ✅ Duplicate table creation handling
- ✅ Key overwrite behavior
- ✅ Stress test: many tables (50 concurrent)
- ✅ Stress test: many records (100 concurrent)
- ✅ Performance and reliability validation

## Database Operations Tested

### Core Database Functions
- `create-table` - Table creation with schema
- `write` - Record insertion/update
- `read` - Record retrieval
- `keys` - Key enumeration
- `with-read` - Context-aware operations
- `with-default-read` - Default value operations
- `select` - Query operations
- `update` - Record modification
- `txids` - Transaction tracking

### Schema Validation
- Type checking (integer, string, bool, decimal)
- Required field validation
- Invalid data type rejection
- Schema enforcement

### Transaction Semantics
- Rollback on errors (division by zero test)
- Atomic operations
- Consistency guarantees
- Isolation validation

## Performance & Reliability

### Concurrent Operations
- 10 simultaneous exec requests
- 20 simultaneous health checks
- 50 concurrent table creations (80%+ success rate)
- 100 concurrent record writes (90%+ success rate)

### Stress Testing
- Large data handling (1MB records)
- Special character support in keys
- Deeply nested object structures
- Boundary condition testing

### Error Recovery
- Graceful failure handling
- Proper error messages
- System stability under load
- Resource cleanup

## Test Infrastructure

### Test Server Management
- Automatic port allocation
- Background server startup
- Proper cleanup and teardown
- Isolated test environments

### HTTP Client Configuration
- 10-second request timeout
- JSON serialization/deserialization
- Error handling and retry logic
- Connection pooling for concurrent tests

### Assertions & Validation
- HTTP status code verification
- JSON response structure validation
- Data type and format checking
- Performance benchmark validation

## Running the Tests

```bash
# Run all integration tests
cargo test --package pact-server --test integration_tests

# Run specific test categories
cargo test --package pact-server --test integration_tests health_tests
cargo test --package pact-server --test integration_tests database_tests
cargo test --package pact-server --test integration_tests concurrent_tests

# Run edge case tests
cargo test --package pact-server --test database_edge_cases

# Run with single thread for stability
cargo test --package pact-server --test integration_tests -- --test-threads=1
```

## Test Results Summary

- **Total Tests**: 52 comprehensive integration tests
- **Coverage**: All major API endpoints and database operations
- **Reliability**: Handles concurrent operations and edge cases
- **Performance**: Validates behavior under load
- **Database**: Full CRUD operations with schema validation
- **HTTP Compliance**: CORS, content types, error codes
- **Error Handling**: Graceful failure and recovery

The test suite provides comprehensive validation of the Pact server's functionality, ensuring reliability, performance, and correctness across all supported operations.