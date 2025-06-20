# Restore Todo Context for Pact Rust Migration

This file contains the complete todo list for achieving feature parity between the Rust migration and Haskell implementation. Use this to restore the todo context in future Claude sessions.

## How to Restore Context

1. Load this file into Claude
2. Use the TodoWrite tool with the task list below
3. Reference `/rust-migration/PACT_RUST_FEATURE_PARITY_PLAN.md` for detailed information

## Complete Todo List (60 tasks)

### Phase 1: Critical Foundations

#### Transitive Dependencies (Priority: HIGH)
```
deps-1: Create pact-dependencies crate with transitive dependency computation
deps-2: Implement getAllTransitiveDependencies equivalent function
deps-3: Add AST traversal for dependency extraction (getTermDependents, getDefunDependents)
deps-4: Implement worklist-based transitive closure algorithm
deps-5: Add gas-aware dependency computation with proper charging
deps-6: Integrate transitive dependencies into pact-compiler pipeline
deps-7: Port TransitiveDependencyTests from Haskell to Rust (Priority: MEDIUM)
```

#### Core Builtins (Priority: HIGH)
```
builtin-1: Complete arithmetic builtins (add, sub, mul, div, mod, pow, etc.)
builtin-2: Implement comparison builtins (eq, neq, lt, gt, leq, geq)
builtin-3: Add boolean logic builtins (and, or, not)
builtin-4: Implement string manipulation builtins (concat, length, take, drop)
builtin-5: Add list operation builtins (map, filter, fold, reverse, sort)
builtin-6: Implement database builtins (read, write, insert, update, select)
builtin-7: Add capability builtins (with-capability, require-capability, compose-capability)
builtin-8: Implement time/date builtins (time, add-time, diff-time, format-time) (Priority: MEDIUM)
builtin-9: Add cryptographic builtins (hash, verify-spv, base64-encode/decode) (Priority: MEDIUM)
builtin-10: Implement guard builtins (enforce-guard, keyset-ref-guard) (Priority: MEDIUM)
```

#### Infrastructure Fixes (Priority: HIGH)
```
infra-1: Fix pact-lsp compilation issues and enable LSP server
infra-2: Fix pact-server compilation issues and enable HTTP API server
infra-3: Resolve type mismatches causing compilation failures
infra-4: Enable disabled pact-ir pipeline module (Priority: MEDIUM)
```

### Phase 2: Core Functionality

#### Capability System (Priority: HIGH/MEDIUM)
```
cap-1: Integrate capability system with CEK evaluator (Priority: HIGH)
cap-2: Implement capability stack management in evaluation (Priority: HIGH)
cap-3: Add capability token creation and validation (Priority: HIGH)
cap-4: Implement managed capabilities with proper scoping (Priority: MEDIUM)
cap-5: Add capability composition and installation (Priority: MEDIUM)
```

#### Evaluation System (Priority: HIGH/MEDIUM)
```
eval-1: Complete constant evaluation in pact-eval (Priority: HIGH)
eval-2: Implement governance evaluation for module deployment (Priority: HIGH)
eval-3: Add DefPact step execution in CEK evaluator (Priority: MEDIUM)
eval-4: Implement pact continuation and rollback mechanisms (Priority: MEDIUM)
eval-5: Add proper error propagation and recovery in evaluation (Priority: MEDIUM)
```

### Phase 3: Development Experience

#### REPL Enhancement (Priority: MEDIUM/LOW)
```
repl-1: Implement missing REPL commands (.env-data, .env-keys, .env-sigs) (Priority: MEDIUM)
repl-2: Add transaction boundary commands (.begin-tx, .commit-tx, .rollback-tx) (Priority: MEDIUM)
repl-3: Implement capability testing commands (.test-capability) (Priority: MEDIUM)
repl-4: Add gas tracking commands (.env-gaslog, .env-gaslimit, .env-gas) (Priority: MEDIUM)
repl-5: Implement chain data simulation (.env-chain-data) (Priority: MEDIUM)
repl-6: Add verifier management (.env-verifiers) (Priority: MEDIUM)
repl-7: Implement expect commands (expect, expect-failure, expect-that) (Priority: MEDIUM)
repl-8: Add trace output functionality matching Haskell behavior (Priority: LOW)
```

#### Storage & CLI (Priority: MEDIUM/LOW)
```
storage-1: Complete module storage integration with compilation pipeline (Priority: MEDIUM)
storage-2: Implement module loading and caching mechanisms (Priority: MEDIUM)
storage-3: Add module versioning and upgrade handling (Priority: MEDIUM)
cli-1: Implement native shadowing check functionality (Priority: MEDIUM)
cli-2: Add YAML-based API request processing (Priority: MEDIUM)
cli-3: Complete signature combination and verification workflows (Priority: MEDIUM)
storage-4: Optimize SQLite backend for production use (Priority: LOW)
```

### Phase 4: Quality & Performance

#### Testing (Priority: MEDIUM/LOW)
```
test-1: Port all Haskell integration tests to Rust (Priority: MEDIUM)
test-2: Add comprehensive property-based tests for all components (Priority: MEDIUM)
test-3: Create performance benchmarks comparing Rust vs Haskell (Priority: MEDIUM)
test-4: Add regression tests for all builtin functions (Priority: LOW)
```

#### Performance (Priority: MEDIUM/LOW)
```
perf-1: Optimize critical path performance (lexing, parsing, evaluation) (Priority: MEDIUM)
perf-2: Implement lazy evaluation where appropriate (Priority: LOW)
perf-3: Add memory usage optimization for large programs (Priority: LOW)
```

#### Documentation (Priority: LOW)
```
doc-1: Complete API documentation for all public interfaces (Priority: LOW)
doc-2: Add comprehensive examples and tutorials (Priority: LOW)
doc-3: Create migration guide from Haskell to Rust implementation (Priority: LOW)
cli-4: Add server configuration management (Priority: LOW)
```

## Task Priorities Summary
- HIGH Priority: 21 tasks (Critical for basic functionality)
- MEDIUM Priority: 26 tasks (Important for feature completeness)
- LOW Priority: 10 tasks (Nice to have for polish)

## Estimated Timeline
- Phase 1: 6 weeks
- Phase 2: 6 weeks
- Phase 3: 4 weeks
- Phase 4: 4 weeks
- **Total: 20 weeks for full feature parity**

## Critical Path Dependencies
1. Transitive Dependencies → Module Compilation
2. Core Builtins → Program Execution
3. Infrastructure Fixes → Production Deployment
4. Capability System → Security Model
5. REPL/CLI → Developer Experience

## Key Implementation Files to Reference

### Haskell (Reference Implementation)
- `/pact/Pact/Core/TransitiveDependencies.hs` - Transitive dependency algorithm
- `/pact/Pact/Core/Compile.hs` - Module compilation pipeline
- `/pact/Pact/Core/IR/Eval/CEK/Evaluator.hs` - CEK machine implementation
- `/pact-repl/Main.hs` - CLI and REPL entry point

### Rust (Migration)
- `/rust-migration/pact-compiler/src/` - Compilation orchestration
- `/rust-migration/pact-cek/src/` - CEK evaluator
- `/rust-migration/pact-cli/src/` - CLI implementation
- `/rust-migration/pact-modules/src/` - Module system (needs transitive deps)

## Notes for Implementation
1. Always check Haskell implementation for correctness
2. No workarounds - match semantics exactly
3. Use established Rust patterns (arena allocation, string interning)
4. Add comprehensive tests for each component
5. Document any architectural improvements