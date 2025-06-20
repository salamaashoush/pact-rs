# Final Pact Rust Migration Consolidation Plan

**Date**: December 2024  
**Current**: 23 crates  
**Target**: 9 crates  
**Reduction**: 2.5x consolidation  
**Status**: Approved with LSP kept separate

## Executive Summary

Based on the Haskell reference implementation analysis, this plan consolidates 23 Rust crates into 9 crates while keeping LSP separate as requested. This follows the proven Haskell architecture while maintaining clear separation of concerns.

## Final Target Architecture: 9 Crates

### 1. **`pact-core`** (Foundation Layer)
**Merge 7 crates into one**:
- ✅ `pact-values` (canonical value types)
- ✅ `pact-names` (name system)  
- ✅ `pact-errors` (error hierarchy)
- ✅ `pact-gas` (gas metering)
- ✅ `pact-capability` (capability system)
- ❌ ~~`pact-math`~~ (delete - duplicate of pact-values numeric)
- ❌ ~~`pact-shared-types`~~ (delete - artificial separation)

**Rationale**: Matches Haskell's unified main library approach

### 2. **`pact-syntax`** (Language Layer)
**Merge 2 crates**:
- ✅ `pact-lexer` 
- ✅ `pact-parser`

**Rationale**: Haskell keeps these together in `Pact.Core.Syntax.*`

### 3. **`pact-ir`** (Compilation Layer)
**Keep but absorb related functionality**:
- ✅ `pact-eval` (constant evaluation belongs in IR)
- ✅ `pact-schema` (type system part of IR)

### 4. **`pact-compiler`** (Orchestration Layer)
**Keep separate but fix compilation issues**
- Fix missing `EvalM::try_from_io` method
- Central compilation orchestration

### 5. **`pact-cek`** (Evaluation Layer)
**Keep as-is** ✅
- CEK machine evaluator
- Remove duplicate error types, use `pact-core` types

### 6. **`pact-db`** (Storage Layer)
**Keep as-is** ✅
- Database abstraction layer

### 7. **`pact-repl`** (Application Layer)
**Keep as-is** ✅
- Matches Haskell `pact-repl` library exactly

### 8. **`pact-lsp`** (Application Layer) 
**Keep separate** ✅ **[As requested]**
- Language Server Protocol implementation
- Benefits from independent development
- Clean separation from server functionality

### 9. **`pact-server`** (Integration Layer)
**Merge HTTP/API functionality**:
- ✅ `pact-request-api`
- HTTP server and API endpoints

### **Optional: `pact-crypto`** (if needed)
**Keep separate if crypto functionality is substantial** ✅
- Matches Haskell `pact-crypto` library
- Can be feature-flagged

### **Remove Completely**:
- ❌ ~~`pact-tests`~~ (empty, 2 lines only)
- ❌ ~~`pact-modules`~~ (functionality goes into pact-core)
- ❌ ~~`pact-cli`~~ (merge into pact-repl or keep minimal)

## Implementation Timeline

### **Week 1: Critical Error Unification**
**Priority**: URGENT - Fixes compilation blockers

**Tasks**:
1. **Fix `pact-compiler` compilation issues**
   - Implement missing `EvalM::try_from_io` method
   - Resolve any other compilation errors

2. **Unify error types**
   - Make `pact-errors` the single source of truth
   - Remove duplicate error types from `pact-cek`
   - Fix all 117+ compilation errors

**Success Criteria**: 
- ✅ All crates compile successfully
- ✅ LSP and server can be enabled

### **Week 2: Foundation Consolidation**
**Priority**: HIGH - Core architecture

**Tasks**:
1. **Create `pact-core` mega-crate**
   - Merge `pact-values`, `pact-names`, `pact-errors`
   - Merge `pact-gas`, `pact-capability`
   - Update all dependencies to use `pact-core`

2. **Delete redundant crates**
   - Remove `pact-math` (duplicate functionality)
   - Remove `pact-shared-types` (artificial separation)
   - Remove `pact-tests` (empty)

**Success Criteria**:
- ✅ Single unified foundation crate
- ✅ No duplicate type definitions
- ✅ All tests pass

### **Week 3: Language Layer Consolidation**
**Priority**: MEDIUM - Cleanup syntax processing

**Tasks**:
1. **Create `pact-syntax` crate**
   - Merge `pact-lexer` and `pact-parser`
   - Maintain clean API boundaries
   - Update benchmarks and tests

2. **Consolidate IR layer**
   - Merge `pact-eval` into `pact-ir`
   - Merge `pact-schema` into `pact-ir`
   - Organize as modules within `pact-ir`

**Success Criteria**:
- ✅ Clean syntax processing layer
- ✅ Unified IR with evaluation
- ✅ Performance maintained or improved

### **Week 4: Final Integration & Testing**
**Priority**: MEDIUM - Polish and verification

**Tasks**:
1. **Server consolidation**
   - Merge `pact-request-api` into `pact-server`
   - Keep `pact-lsp` separate as requested

2. **Integration testing**
   - Full end-to-end testing
   - Performance benchmarking
   - Documentation updates

3. **Dependency optimization**
   - Clean up internal dependencies
   - Ensure no circular dependencies
   - Optimize build times

**Success Criteria**:
- ✅ 9 total crates (down from 23)
- ✅ All functionality working
- ✅ Performance equal or better than before

## Detailed Consolidation Steps

### Step 1: Create `pact-core` Foundation
```toml
# New pact-core/Cargo.toml
[package]
name = "pact-core"
version = "0.1.0"

# Absorb dependencies from all merged crates
[dependencies]
# Core functionality
serde = { version = "1.0", features = ["derive"] }
bigdecimal = "0.4"
num-bigint = "0.4"
# ... other deps

# Internal structure
# pact-core/src/
# ├── lib.rs           # Re-exports
# ├── values/          # From pact-values
# ├── names/           # From pact-names  
# ├── errors/          # From pact-errors
# ├── gas/             # From pact-gas
# └── capability/      # From pact-capability
```

### Step 2: Create `pact-syntax` Language Layer
```toml
# New pact-syntax/Cargo.toml
[package]
name = "pact-syntax"

[dependencies]
pact-core = { path = "../pact-core" }

# pact-syntax/src/
# ├── lib.rs       # Re-exports
# ├── lexer/       # From pact-lexer
# └── parser/      # From pact-parser
```

### Step 3: Update All Dependencies
```toml
# Example: Update pact-cek/Cargo.toml
[dependencies]
pact-core = { path = "../pact-core" }        # Instead of 7 separate deps
pact-syntax = { path = "../pact-syntax" }    # Instead of lexer + parser
# ... other deps
```

## Success Metrics

### **Quantitative Goals**:
- ✅ **9 total crates** (down from 23) = 2.5x reduction
- ✅ **0 compilation errors** (currently 117+)
- ✅ **Single type hierarchy** for each concept
- ✅ **~60% reduction** in duplicate code
- ✅ **Faster build times** (fewer dependency edges)

### **Qualitative Goals**:
- ✅ **Matches Haskell architecture** (proven design)
- ✅ **LSP kept separate** (as requested)
- ✅ **Clear separation of concerns**
- ✅ **Easier maintenance**
- ✅ **Better developer experience**

## Risk Mitigation

1. **Incremental approach**: One consolidation per week
2. **Test early and often**: Run full test suite after each step
3. **Rollback plan**: Keep git branches for each major change
4. **Documentation**: Update all docs and examples
5. **Communication**: Keep team informed of changes

## Expected Benefits

### **Developer Experience**:
- Fewer crates to navigate and understand
- Single source of truth for core types
- Faster compilation with fewer dependencies
- Clearer architecture matching Haskell reference

### **Maintenance**:
- Eliminate duplicate code maintenance
- Easier to make cross-cutting changes
- Reduced risk of type mismatches
- Simplified dependency management

### **Performance**:
- Fewer compilation units
- Better optimization opportunities
- Reduced binary size
- Faster incremental builds

This plan provides a clean, maintainable architecture that closely follows the proven Haskell implementation while keeping LSP separate as requested and maintaining all the performance benefits of the Rust migration.