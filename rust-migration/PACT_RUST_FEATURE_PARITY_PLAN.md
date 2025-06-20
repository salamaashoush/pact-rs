# Pact Rust Migration - Feature Parity Plan

**Date**: December 2024 (Updated)  
**Current Status**: ~75% Complete  
**Target**: 100% Feature Parity with Haskell Implementation

## Executive Summary

This document provides a comprehensive plan to achieve feature parity between the Rust migration and the Haskell implementation of Pact 5. **Recent analysis discovered that transitive dependencies ARE implemented in Rust**, significantly changing the priority order. The main blockers are now CEK compilation errors and missing builtin functions. The plan outlines revised tasks organized into 4 phases over approximately 12-16 weeks.

## Current Architecture Analysis

### Haskell Execution Flow (Reference)

```
📄 my-contract.pact
    ↓ [CLI Parsing]
🔧 Main.hs:runScript
    ↓ [Environment Setup]
🗄️  Mock PactDb + EvalEnv + BuiltinMap
    ↓ [File Loading]
📖 Lexer → Parser → AST
    ↓ [For each TopLevel]
🔐 Module Governance Check
    ↓ 
🔄 Desugar (Surface → Core IR)
    ↓
⚡ Constant Pre-evaluation  
    ↓
🔐 Module Hashing (CBOR + SHA256)
    ↓
📊 Transitive Dependency Resolution (BFS)
    ↓
💾 Store to PactDb:
   • ModuleData + Dependencies
   • Source Code
    ↓ [For terms]
🚀 CEK/Direct Interpreter:
   • Variable Lookup (loAllLoaded)
   • Function Application  
   • Builtin Dispatch
   • Closure Creation
    ↓
✅ PactValue Result
```

### Rust Migration Status

| Component | Status | Completeness | Key Gaps |
|-----------|--------|--------------|----------|
| **Lexer/Parser** | ✅ Excellent | 95% | Minor edge cases |
| **Compilation Pipeline** | ⚠️ Good | 85% | **RESOLVED**: Transitive deps implemented |
| **CEK Evaluator** | ❌ BLOCKED | 60% | **117 compilation errors**, missing builtins |
| **Module System** | ✅ Good | 90% | **RESOLVED**: Transitive deps implemented |
| **Database Layer** | ⚠️ Good | 75% | SQLite optimization needed |
| **CLI/REPL** | ❌ BLOCKED | 70% | LSP/Server blocked by CEK errors |
| **Supporting Systems** | ✅ Excellent | 90% | Minor gaps |

## Critical Architectural Gaps

### 1. CEK Compilation Errors (URGENT)
- **Issue**: 117 compilation errors in pact-cek due to inconsistent error types
- **Impact**: Blocks LSP server, HTTP server, and full evaluation functionality
- **Effort**: 1 week

### 2. Complete Builtin System (HIGH)
- **Missing**: 21 of 137 builtin functions not implemented (116 complete)
- **Impact**: Many Pact programs cannot execute
- **Effort**: 3-4 weeks

### 3. LSP and Server Infrastructure (HIGH)
- **Issue**: Components disabled due to CEK compilation errors
- **Impact**: Cannot integrate with development tools or blockchain
- **Effort**: 1-2 weeks after CEK fixes

### 4. Capability System Integration (MEDIUM)
- **Missing**: Full integration with CEK evaluator
- **Impact**: Security model not fully operational
- **Effort**: 2-3 weeks

### ✅ RESOLVED: Transitive Dependencies
- **Status**: Algorithm IS implemented in pact-modules crate
- **Next**: Integration with compilation pipeline needed

## Feature Parity Todo List

### Phase 1: Critical Blockers (Weeks 1-2)

#### CEK Compilation Fixes (5 tasks)
- [ ] cek-1: **URGENT** - Fix 117 compilation errors in pact-cek
- [ ] cek-2: Align error type definitions between pact-errors and pact-cek
- [ ] cek-3: Resolve type mismatches in builtin function signatures
- [ ] cek-4: Fix continuation type inconsistencies
- [ ] cek-5: Enable full CEK evaluator compilation

#### Infrastructure Restoration (3 tasks)
- [ ] infra-1: Enable pact-lsp compilation after CEK fixes
- [ ] infra-2: Enable pact-server compilation after CEK fixes
- [ ] infra-3: Verify full CLI/REPL functionality

#### ✅ COMPLETED: Transitive Dependencies
- [x] deps-1: ✅ Transitive dependency computation implemented in pact-modules
- [x] deps-2: ✅ getAllTransitiveDependencies equivalent function exists
- [x] deps-3: ✅ AST traversal for dependency extraction implemented
- [x] deps-4: ✅ Worklist-based transitive closure algorithm implemented
- [x] deps-5: ✅ Gas-aware dependency computation implemented
- [ ] deps-6: Integrate transitive dependencies into pact-compiler pipeline
- [ ] deps-7: Port TransitiveDependencyTests from Haskell to Rust

### Phase 2: Core Functionality (Weeks 3-6)

#### Complete Builtin System (21 missing functions)
- [ ] builtin-1: **Database Operations** - insert, update, select (5 functions)
- [ ] builtin-2: **Cryptographic Functions** - verify-spv, base64-encode/decode (3 functions)
- [ ] builtin-3: **Advanced List Operations** - fold variants, partition (4 functions)
- [ ] builtin-4: **Time Manipulation** - format-time, parse-time variants (3 functions)
- [ ] builtin-5: **Guard Operations** - enforce-guard variants (2 functions)
- [ ] builtin-6: **String Operations** - Advanced string functions (2 functions)
- [ ] builtin-7: **Capability Functions** - Advanced capability operations (2 functions)

**Note**: Core arithmetic, comparison, and basic operations are already implemented (116/137 complete)
- [ ] builtin-7: Add capability builtins (with-capability, require-capability, compose-capability)
- [ ] builtin-8: Implement time/date builtins (time, add-time, diff-time, format-time)
- [ ] builtin-9: Add cryptographic builtins (hash, verify-spv, base64-encode/decode)
- [ ] builtin-10: Implement guard builtins (enforce-guard, keyset-ref-guard)

#### Infrastructure Fixes (4 tasks)
- [ ] infra-1: Fix pact-lsp compilation issues and enable LSP server
- [ ] infra-2: Fix pact-server compilation issues and enable HTTP API server
- [ ] infra-3: Resolve type mismatches causing compilation failures
- [ ] infra-4: Enable disabled pact-ir pipeline module

### Phase 3: Integration and Enhancement (Weeks 7-10)

#### Capability System (5 tasks)
- [ ] cap-1: Integrate capability system with CEK evaluator
- [ ] cap-2: Implement capability stack management in evaluation
- [ ] cap-3: Add capability token creation and validation
- [ ] cap-4: Implement managed capabilities with proper scoping
- [ ] cap-5: Add capability composition and installation

#### Evaluation System (5 tasks)
- [ ] eval-1: Complete constant evaluation in pact-eval
- [ ] eval-2: Implement governance evaluation for module deployment
- [ ] eval-3: Add DefPact step execution in CEK evaluator
- [ ] eval-4: Implement pact continuation and rollback mechanisms
- [ ] eval-5: Add proper error propagation and recovery in evaluation

### Phase 4: Development Experience (Weeks 11-14)

#### REPL Enhancement (8 tasks)
- [ ] repl-1: Implement missing REPL commands (.env-data, .env-keys, .env-sigs)
- [ ] repl-2: Add transaction boundary commands (.begin-tx, .commit-tx, .rollback-tx)
- [ ] repl-3: Implement capability testing commands (.test-capability)
- [ ] repl-4: Add gas tracking commands (.env-gaslog, .env-gaslimit, .env-gas)
- [ ] repl-5: Implement chain data simulation (.env-chain-data)
- [ ] repl-6: Add verifier management (.env-verifiers)
- [ ] repl-7: Implement expect commands (expect, expect-failure, expect-that)
- [ ] repl-8: Add trace output functionality matching Haskell behavior

#### Storage & CLI (7 tasks)
- [ ] storage-1: Complete module storage integration with compilation pipeline
- [ ] storage-2: Implement module loading and caching mechanisms
- [ ] storage-3: Add module versioning and upgrade handling
- [ ] cli-1: Implement native shadowing check functionality
- [ ] cli-2: Add YAML-based API request processing
- [ ] cli-3: Complete signature combination and verification workflows
- [ ] storage-4: Optimize SQLite backend for production use

### Phase 5: Quality & Performance (Weeks 15-16)

#### Testing (4 tasks)
- [ ] test-1: Port all Haskell integration tests to Rust
- [ ] test-2: Add comprehensive property-based tests for all components
- [ ] test-3: Create performance benchmarks comparing Rust vs Haskell
- [ ] test-4: Add regression tests for all builtin functions

#### Performance (3 tasks)
- [ ] perf-1: Optimize critical path performance (lexing, parsing, evaluation)
- [ ] perf-2: Implement lazy evaluation where appropriate
- [ ] perf-3: Add memory usage optimization for large programs

#### Documentation (4 tasks)
- [ ] doc-1: Complete API documentation for all public interfaces
- [ ] doc-2: Add comprehensive examples and tutorials
- [ ] doc-3: Create migration guide from Haskell to Rust implementation
- [ ] cli-4: Add server configuration management

## Implementation Guidelines

### Transitive Dependencies Implementation

The new `pact-dependencies` crate should follow this structure:

```rust
// Core algorithm matching Haskell
pub fn get_all_transitive_dependencies(
    module: &Module,
    initial_deps: HashSet<ModuleName>,
    all_loaded: &HashMap<FullyQualifiedName, EvalDef>,
    gas_env: &GasEnv,
) -> Result<HashMap<FullyQualifiedName, EvalDef>, PactError> {
    // 1. Initialize working set with module's own definitions
    // 2. Iterate through working set
    // 3. Extract dependencies from each definition's AST
    // 4. Add new dependencies to both working and result sets
    // 5. Continue until working set is empty
    // 6. Charge gas for all operations
}
```

### Builtin Registration Pattern

Follow the established pattern in `pact-cek/src/builtin/`:

```rust
pub fn register_arithmetic_builtins(env: &mut BuiltinEnv) -> Result<(), PactError> {
    env.register_builtin(
        CoreBuiltin::CoreAdd,
        2, // arity
        |args, env, cont| {
            // Implementation matching Haskell semantics
        }
    )?;
    // Continue for all builtins...
}
```

### Testing Strategy

1. **Unit Tests**: Each component should have comprehensive unit tests
2. **Integration Tests**: Port all Haskell integration tests
3. **Property Tests**: Use `proptest` for critical algorithms
4. **Benchmarks**: Compare performance against Haskell baseline

## Success Metrics

1. **Functional Parity**: All Haskell tests pass on Rust implementation
2. **Performance**: Rust implementation is ≥ 2x faster on key benchmarks
3. **Memory Usage**: Rust uses ≤ 50% memory compared to Haskell
4. **Developer Experience**: LSP and REPL achieve feature parity
5. **Production Readiness**: Successfully deploys to Kadena blockchain

## Risk Mitigation

1. **Incremental Migration**: Each phase delivers working functionality
2. **Backward Compatibility**: Maintain serialization compatibility
3. **Extensive Testing**: Property-based testing for correctness
4. **Performance Monitoring**: Continuous benchmarking during development

## Resources Required

- **Development Team**: 2-3 senior Rust developers
- **Timeline**: 12-16 weeks for full parity (reduced due to transitive deps being complete)
- **Infrastructure**: CI/CD pipeline with benchmarking
- **Testing**: Access to production Pact programs for validation

## Conclusion

**Major Discovery**: The Rust migration is in a much better position than previously assessed. The transitive dependencies algorithm IS fully implemented in the pact-modules crate, eliminating the most critical architectural gap.

The actual critical path now focuses on:
1. **Immediate**: Fix CEK compilation errors (1 week)
2. **Short-term**: Complete remaining 21 builtin functions (3-4 weeks)
3. **Medium-term**: Integration and testing (4-6 weeks)

With focused development over 12-16 weeks, the Rust implementation will achieve full feature parity and provide a superior foundation for Pact's future evolution.