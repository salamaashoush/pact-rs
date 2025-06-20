# Pact 5 Rust Migration Plan

## Overview

This document outlines a comprehensive, incremental migration strategy for porting Pact 5 from Haskell to Rust. The migration is designed to be AI-agent friendly, with clear boundaries and testable milestones for each phase.

## Migration Principles

1. **Incremental Migration**: Start with leaf dependencies, gradually moving to core components
2. **Maintain Compatibility**: Ensure byte-for-byte compatibility with existing Pact behavior
3. **Performance First**: Leverage Rust's zero-cost abstractions and memory safety
4. **Test-Driven**: Port tests alongside code, ensuring correctness at each step
5. **FFI Bridge**: Use FFI to gradually replace Haskell components with Rust

## Phase 1: Foundation and Tooling (Weeks 1-3)

### 1.1 Project Setup
```toml
# Workspace structure
pact-rust/
├── Cargo.toml (workspace)
├── pact-core/          # Core types and traits
├── pact-crypto/        # Cryptographic primitives
├── pact-types/         # Basic types and serialization
├── pact-gas/           # Gas calculation
└── pact-ffi/           # FFI bridge to Haskell
```

**Tasks:**
- [ ] Initialize Rust workspace with proper structure
- [ ] Set up CI/CD pipeline (GitHub Actions)
- [ ] Configure benchmarking framework (criterion)
- [ ] Set up property-based testing (proptest)
- [ ] Create FFI bridge scaffolding

### 1.2 Core Type System
**Module: `pact-types`**

```rust
// Example type definitions
#[derive(Clone, Debug, PartialEq)]
pub enum PrimType {
    Integer,
    Decimal,
    String,
    Bool,
    Time,
    Guard,
    Unit,
}

#[derive(Clone, Debug)]
pub enum Type {
    Prim(PrimType),
    List(Box<Type>),
    Object(Schema),
    Table(Schema),
    ModRef(ModuleName),
    // ...
}
```

**Tasks:**
- [ ] Port `PrimType` and `Type` enums
- [ ] Implement `Schema` type for objects/tables
- [ ] Port `TypeVar` and type inference structures
- [ ] Implement serialization/deserialization (serde)
- [ ] Create property-based tests for type equality

### 1.3 Mathematical Operations
**Module: `pact-math`**

Replace MPFR FFI with Rust-native implementations:
- [ ] Use `rug` crate for arbitrary precision (MPFR bindings)
- [ ] Implement transcendental functions (exp, ln, log, pow, sqrt)
- [ ] Create comprehensive test suite comparing with Haskell output
- [ ] Benchmark against current implementation

## Phase 2: Cryptographic Primitives (Weeks 4-5)

### 2.1 Core Crypto
**Module: `pact-crypto`**

```rust
pub trait HashFunction {
    fn hash(&self, input: &[u8]) -> Vec<u8>;
}

pub struct Keccak256;
pub struct PoseidonHash;
pub struct Blake2b256;
```

**Tasks:**
- [ ] Port Poseidon hash implementation
- [ ] Implement Keccak256 using `tiny-keccak`
- [ ] Port BN254 pairing operations using `ark-bn254`
- [ ] Implement WebAuthn support
- [ ] Create test vectors from Haskell implementation

### 2.2 Performance Optimizations
- [ ] SIMD optimizations for hash functions
- [ ] Const generics for fixed-size operations
- [ ] Zero-copy where possible

## Phase 3: Parser and Lexer (Weeks 6-8)

### 3.1 Lexer Implementation
**Module: `pact-lexer`**

Options:
1. Use `logos` for fast lexing
2. Port Alex lexer rules to Rust

```rust
#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[regex(r"[a-zA-Z][a-zA-Z0-9_-]*")]
    Identifier(String),
    // ...
}
```

**Tasks:**
- [ ] Define token enum with all Pact tokens
- [ ] Implement lexer with proper error recovery
- [ ] Port whitespace and comment handling
- [ ] Create fuzz tests for lexer robustness

### 3.2 Parser Implementation
**Module: `pact-parser`**

Use `lalrpop` or `nom` for parsing:
- [ ] Port Happy grammar to chosen parser generator
- [ ] Implement error recovery and reporting
- [ ] Create AST types matching Haskell structure
- [ ] Extensive testing with existing Pact code

## Phase 4: IR and Core Evaluation (Weeks 9-14)

### 4.1 IR Term Structure
**Module: `pact-ir`**

```rust
pub enum Term {
    Var(Name),
    Lam(Vec<Arg>, Box<Term>),
    App(Box<Term>, Vec<Term>),
    Builtin(BuiltinFn),
    Constant(Literal),
    // ...
}
```

**Tasks:**
- [ ] Port IR term representation
- [ ] Implement module and interface types
- [ ] Port defun/defcap/defpact structures
- [ ] Create IR builder API

### 4.2 CEK Machine
**Module: `pact-eval-cek`**

```rust
pub struct CEKEnv {
    frames: Vec<Frame>,
    env: HashMap<Name, Value>,
    gas: GasCounter,
}

pub enum Cont {
    Mt,
    Fn(Vec<Term>, Env, Box<Cont>),
    Arg(Vec<Value>, Vec<Term>, Env, Box<Cont>),
    // ...
}
```

**Tasks:**
- [ ] Port CEK machine states and transitions
- [ ] Implement continuation types
- [ ] Port evaluation rules
- [ ] Integrate gas counting
- [ ] Create step-by-step debugging support

### 4.3 Direct Evaluator
**Module: `pact-eval-direct`**

Alternative evaluation strategy:
- [ ] Port direct evaluation logic
- [ ] Ensure parity with CEK results
- [ ] Benchmark both evaluators

## Phase 5: Builtin Functions (Weeks 15-18)

### 5.1 Core Builtins
**Module: `pact-builtins`**

Structure builtins by category:
```rust
pub mod arithmetic {
    pub fn add(args: Vec<Value>) -> Result<Value> { ... }
    pub fn multiply(args: Vec<Value>) -> Result<Value> { ... }
}

pub mod string {
    pub fn concat(args: Vec<Value>) -> Result<Value> { ... }
    pub fn length(args: Vec<Value>) -> Result<Value> { ... }
}
```

**Tasks:**
- [ ] Port arithmetic operations
- [ ] Port string/list operations
- [ ] Port database operations
- [ ] Port time/date functions
- [ ] Port cryptographic builtins
- [ ] Comprehensive testing for each builtin

### 5.2 Gas Integration
- [ ] Accurate gas metering for each builtin
- [ ] Optimization for common patterns

## Phase 6: Persistence Layer (Weeks 19-21)

### 6.1 Database Interface
**Module: `pact-db`**

```rust
pub trait PactDb: Send + Sync {
    fn create_table(&mut self, name: &str, schema: &Schema) -> Result<()>;
    fn insert(&mut self, table: &str, key: &str, value: &Object) -> Result<()>;
    fn select(&self, table: &str, key: &str) -> Result<Option<Object>>;
    // ...
}
```

**Tasks:**
- [ ] Define database trait
- [ ] SQLite implementation using `rusqlite`
- [ ] In-memory implementation for testing
- [ ] Transaction support
- [ ] Migration from existing databases

### 6.2 Module Storage
- [ ] Port module serialization
- [ ] Implement module loading/caching
- [ ] Version compatibility

## Phase 7: REPL and Tools (Weeks 22-24)

### 7.1 REPL Implementation
**Module: `pact-repl`**

```rust
pub struct ReplState {
    env: EvalEnv,
    loaded_modules: HashMap<ModuleName, Module>,
    tx_state: Option<TxState>,
}
```

**Tasks:**
- [ ] Port REPL commands
- [ ] Implement line editing (rustyline)
- [ ] Port `.repl` file loading
- [ ] Transaction simulation
- [ ] Pretty printing

### 7.2 Documentation Generation
- [ ] Port builtin documentation
- [ ] Generate markdown/HTML docs
- [ ] Example extraction

## Phase 8: LSP Server (Weeks 25-27)

### 8.1 LSP Implementation
**Module: `pact-lsp`**

Use `tower-lsp` framework:
- [ ] Port completion logic
- [ ] Port hover information
- [ ] Port diagnostics
- [ ] Implement incremental parsing
- [ ] Add performance optimizations

## Phase 9: Integration and Migration (Weeks 28-30)

### 9.1 FFI Integration
**Module: `pact-ffi`**

Create C API for gradual migration:
```rust
#[no_mangle]
pub extern "C" fn pact_eval(
    code: *const c_char,
    env: *const c_char,
) -> *mut c_char { ... }
```

**Tasks:**
- [ ] Design stable C API
- [ ] Implement Haskell bindings
- [ ] Gradual component replacement
- [ ] Performance testing

### 9.2 Compatibility Testing
- [ ] Port entire test suite
- [ ] Byte-for-byte output comparison
- [ ] Performance benchmarking
- [ ] Memory usage analysis

## Phase 10: Optimization and Polish (Weeks 31-33)

### 10.1 Performance Optimizations
- [ ] Profile and optimize hot paths
- [ ] Implement parallel evaluation where safe
- [ ] Optimize memory allocations
- [ ] SIMD optimizations for batch operations

### 10.2 Advanced Features
- [ ] Async/await for IO operations
- [ ] Streaming parser for large files
- [ ] JIT compilation exploration
- [ ] WASM target support

## Testing Strategy

### Unit Tests
- Port all existing Haskell tests
- Property-based testing for core logic
- Fuzzing for parser/lexer

### Integration Tests
- Full Pact program execution
- Cross-implementation testing
- Performance regression tests

### Benchmarks
```rust
criterion_group!(
    benches,
    parser_benchmark,
    eval_benchmark,
    crypto_benchmark
);
```

## Performance Targets

1. **Parser**: 2-3x faster than Haskell implementation
2. **Evaluator**: 1.5-2x faster for typical contracts
3. **Crypto**: Match or exceed current performance
4. **Memory**: 50% reduction in memory usage

## Risk Mitigation

1. **Semantic Differences**: Extensive testing against Haskell implementation
2. **Performance Regressions**: Continuous benchmarking
3. **Migration Complexity**: FFI bridge for gradual migration
4. **Feature Parity**: Comprehensive test coverage

## Tooling Requirements

- Rust 1.75+ (for stable features)
- `cargo-criterion` for benchmarking
- `cargo-flamegraph` for profiling
- `cargo-fuzz` for fuzzing
- `miri` for undefined behavior detection

## Success Metrics

1. 100% test compatibility with Haskell implementation
2. Performance improvements across all benchmarks
3. Reduced memory footprint
4. Improved error messages
5. Better tooling integration

## Maintenance and Documentation

- Comprehensive API documentation
- Architecture decision records (ADRs)
- Performance tuning guide
- Migration guide for users

This migration plan provides a structured approach to porting Pact 5 to Rust while maintaining compatibility and improving performance. Each phase is designed to be self-contained and testable, making it suitable for AI agent implementation.