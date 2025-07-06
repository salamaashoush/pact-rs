# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Memory

- Stop named things Haskell compact, and making bridges and workarounds. If we change the API, change everywhere to use the new API. The code is still in flux, and it is okay to break things and change to adapt to newer architecture as we discover. Stop duplicating code and having multiple versions of the same things or bridging one version to another. Ensure we have one coherent codebase.
- Always check the Haskell code to collect info and verify that the architecture is compatible and 100% complete
- Please dont simplify things just to fix errors, never comment code, add TODO like comments just fix the code and keep it fully functional, and always always follow the Haskell reference implementation for correctness
- Never simplify functionality to fix errors

## Commands

### Build Commands
```bash
# Build the main Pact executable
cabal build exe:pact

# Build all components
cabal build all

# Run Pact directly
cabal run exe:pact

# Install Pact to your PATH
cabal install exe:pact

# Build the gas model tool
cabal build exe:gasmodel
```

### Test Commands
```bash
# Run all tests
cabal test

# Run specific test suite
cabal test core-tests

# Run tests with detailed output
cabal test --test-show-details=direct
```

### Development Commands
```bash
# Enter Nix development shell (if using Nix)
nix develop

# Run Pact REPL
cabal run exe:pact

# Start Language Server
cabal run exe:pact -- --lsp
```

### Rust Development Commands
```bash
# Change to Rust migration directory
cd rust-migration/

# Build all Rust components
cargo build --all

# Run tests for specific crate
cargo test -p pact-lexer
cargo test -p pact-parser
cargo test -p pact-eval

# Run all tests
cargo test --all

# Run benchmarks
cargo bench

# Check code without building
cargo check --all

# Format code
cargo fmt --all

# Run linter
cargo clippy --all -- -D warnings
```

## Architecture Overview

Pact 5 is a smart contract programming language designed to be Turing-incomplete for safety. The codebase is organized into several key components:

### Core Libraries
- **pact-tng**: Core language implementation with the compiler, interpreter, and runtime
- **pact-repl**: REPL for interactive development and testing
- **pact-request-api**: Public API consumed by chainweb-node and pact-cli
- **pact-lsp**: Language Server Protocol implementation for editor integration

### Main Entry Points
- `/app/Main.hs`: Main CLI application
- `/pact/Pact/Core/`: Core language implementation
- `/pact-repl/Pact/Core/Repl/`: REPL implementation
- `/pact-lsp/Pact/Core/LanguageServer.hs`: LSP entry point

### Key Concepts
- The language is intentionally Turing-incomplete for blockchain safety
- Supports keyset-based authorization and capabilities
- Built-in database operations with table/row semantics
- Gas modeling for transaction cost estimation
- Property-based testing throughout the codebase

### Testing Structure
- Core tests are in `/pact-tests/`
- Example contracts in `/examples/` are used for testing
- Property-based tests using QuickCheck
- Gas model benchmarks in `/gasmodel/`

## Rust Migration Context

The Pact team is actively migrating performance-critical components to Rust. The migration has achieved significant progress with 28 working crates and ~175,000 lines of code.

### Current Migration Status (Updated: December 2024)
- **Overall Completion**: ~75% across all components
- **Foundation Layer**: 95% complete (28 Rust crates, comprehensive workspace configuration)
- **Lexer/Parser**: 95% complete - All Haskell tokens and AST nodes implemented
- **Compilation Pipeline**: 80% complete - Missing transitive dependencies
- **CEK Evaluator**: 60% complete - Basic evaluation working, needs builtin integration
- **Module System**: 85% complete - Core functionality working, missing transitive deps
- **Database Layer**: 75% complete - Full abstraction, mock DB, schema validation
- **CLI/REPL**: 70% complete - Modern CLI, but LSP/Server disabled
- **Supporting Systems**: 90% complete - Error handling, values, crypto, gas metering

### Critical Gaps Identified (December 2024)
1. **Transitive Dependencies**: No equivalent to `getAllTransitiveDependencies` - blocks module compilation
2. **Builtin Functions**: ~40% of builtins missing - prevents program execution
3. **LSP/Server**: Disabled due to compilation issues - blocks production use
4. **Capability Integration**: Basic structure exists but not fully integrated with CEK

### Recent Updates
- **Lexer/Parser Enhancement**: Full parity with Haskell implementation achieved
  - All special tokens implemented (`bless`, `step`, `::`, single tick syntax)
  - Complete AST support including `EnforceOne`, `CreateUserGuard`, `Cond`
  - Property expressions for `@model` annotations
  - Qualified names with namespace support
  - Special identifier characters (`%`, `#`, `$`, etc.) supported

### Key insights from the Oxc parser guide have been incorporated into our approach:

### Parser Migration Strategy
- **Performance Focus**: Minimize memory allocations and CPU cycles
- **Architecture**: Follow standard lexer → parser → AST pipeline
- **Memory Management**: Leverage Rust's ownership model and prefer stack allocations
- **Testing**: Comprehensive testing including fuzzing and property-based tests

### Relevant Resources
- `/rust-migration/PARSER_GUIDE.md`: Detailed guide for parser migration
- `/rust-migration/PACT_RUST_MIGRATION_PLAN.md`: Overall migration strategy
- [Oxc Parser Guide](https://oxc.rs/docs/learn/parser_in_rust/intro.html): Reference implementation

### Key Principles for Rust Code
1. **Zero-cost abstractions**: Use Rust idioms that compile to efficient code
2. **Memory efficiency**: Minimize heap allocations, use arena allocators for ASTs
3. **Error handling**: Implement robust error recovery for better developer experience
4. **Incremental migration**: Maintain compatibility with existing Haskell codebase
5. **Performance benchmarking**: Compare against current implementation at each step

### Oxc-Inspired Patterns
The Oxc JavaScript toolchain provides excellent patterns for building high-performance language tools in Rust:

1. **Arena Allocation**: Use `bumpalo` for fast, bulk allocation of AST nodes
2. **Uniform AST Enums**: Keep enum variants at 16 bytes using `Box` for consistency
3. **String Interning**: Use `CompactString` for efficient identifier storage
4. **Visitor Pattern**: Generate visitor traits for AST traversal
5. **Diagnostic System**: Structured errors with labeled spans and helpful messages
6. **Workspace Organization**: Modular crate structure with clear boundaries
7. **Performance First**: Aggressive inlining, compile-time checks, and minimal allocations

### Reference Documents
- `/rust-migration/OXC_PATTERNS_REFERENCE.md`: Detailed Oxc patterns analysis
- `/rust-migration/PACT_RUST_IMPLEMENTATION_EXAMPLE.md`: Concrete example applying Oxc patterns to Pact
- `/rust-migration/INTERPRETER_PATTERNS_GUIDE.md`: Interpreter implementation patterns from Crafting Interpreters
- `/rust-migration/PACT_RUST_STUDY_PLAN.md`: Step-by-step migration plan with 10 milestones
- `/rust-migration/MILESTONE_TEMPLATES.md`: Starter code templates for each milestone
- `/rust-migration/LEARNING_GUIDE.md`: In-depth learning guide with concepts and exercises

### Interpreter Design Insights (from Crafting Interpreters)

Key implementation strategies for the Pact interpreter:

1. **Tree-Walking Interpreter**: Start with a tree-walking approach for simplicity
   - Natural fit for Rust's pattern matching
   - Sufficient for Turing-incomplete languages
   - Easier gas metering implementation

2. **Scanner/Lexer Design**:
   - Maximal munch approach for token recognition
   - Continue scanning after errors for better error reporting
   - Use Rust enums for token types with embedded literal values

3. **Parser Architecture**:
   - Recursive descent with precedence climbing
   - Implement panic-mode error recovery
   - Separate parsing from semantic analysis

4. **Environment Management**:
   - Use `Rc<RefCell<>>` for shared mutable environments
   - Implement variable resolution pass to optimize lookups
   - Cache resolved variable distances

5. **Memory Optimization**:
   - String interning for identifiers
   - Value pooling for common literals
   - Leverage Rust's ownership instead of garbage collection

6. **Testing Strategy**:
   - Unit tests for each component
   - Integration tests with real Pact programs
   - Property-based testing for parser robustness
   - Snapshot tests for error messages

### Rust Migration Study Plan

The migration follows a 10-milestone approach, building a working interpreter at each step:

1. **Calculator** (Weeks 1-2): Basic arithmetic evaluation
2. **Variables** (Weeks 3-4): Let bindings and scoping
3. **Functions** (Weeks 5-6): Lambda and function definitions
4. **Type System** (Weeks 7-8): Type checking and inference
5. **Modules** (Weeks 9-10): Module system implementation
6. **Capabilities** (Weeks 11-12): Security model
7. **Database** (Weeks 13-14): Table operations
8. **Optimization** (Weeks 15-16): Performance tuning
9. **Integration** (Weeks 17-18): FFI bindings
10. **Production** (Weeks 19-20): Polish and deployment

Each milestone includes starter code, learning objectives, and exercises to deepen understanding.

## Rust Migration - Feature Parity Roadmap

### Overview
A comprehensive plan exists to achieve 100% feature parity with the Haskell implementation. The full plan is documented in `/rust-migration/PACT_RUST_FEATURE_PARITY_PLAN.md`.

### Key Architecture Understanding

**Haskell Execution Flow**: CLI → Lexer → Parser → Governance → Desugar → Const Eval → Module Hash → Transitive Deps → Store to DB → CEK Evaluation → Result

**Rust Status**: Foundation excellent, but missing critical components for full execution flow.

### Priority Implementation Path (60 tasks total)

**Phase 1: Critical Foundations (Weeks 1-6)**
- Transitive Dependencies (7 tasks) - Create `pact-dependencies` crate
- Core Builtins (10 tasks) - Complete arithmetic, comparison, string, list, DB operations
- Infrastructure Fixes (4 tasks) - Enable LSP and HTTP server

**Phase 2: Core Functionality (Weeks 7-12)**
- Capability System (5 tasks) - Full CEK integration
- Evaluation System (5 tasks) - Governance, DefPact, error handling

**Phase 3: Development Experience (Weeks 13-16)**
- REPL Enhancement (8 tasks) - Transaction boundaries, environment commands
- Storage & CLI (7 tasks) - Module loading, versioning, YAML processing

**Phase 4: Quality & Performance (Weeks 17-20)**
- Testing (4 tasks) - Port Haskell tests, benchmarks
- Performance (3 tasks) - Optimize critical paths
- Documentation (4 tasks) - API docs, migration guide

### Critical Implementation Notes

1. **Transitive Dependencies Algorithm**: Must implement BFS-based algorithm with gas tracking, matching Haskell's `getAllTransitiveDependencies` exactly.

2. **Builtin Registration**: Follow pattern in `pact-cek/src/builtin/` - each builtin needs proper arity checking and CEK continuation support.

3. **Module Storage**: Integration exists but needs completion - modules must store with full transitive dependency closure.

4. **No Workarounds**: Follow Haskell implementation exactly - no shortcuts or simplifications that change semantics.

### Success Metrics
- All Haskell tests pass on Rust implementation
- Rust is ≥ 2x faster on key benchmarks
- Full LSP and REPL feature parity
- Successfully deploys to Kadena blockchain

### When Working on Rust Migration
1. Always check Haskell implementation first for correctness
2. Maintain exact semantic compatibility
3. Use established patterns (arena allocation, string interning)
4. Add comprehensive tests for each component
5. Document deviations or improvements clearly