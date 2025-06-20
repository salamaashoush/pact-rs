# Pact Rust Migration - Crate Consolidation Plan

**Date**: December 2024  
**Current Crates**: 23 total  
**Target**: 15-18 consolidated crates  
**Priority**: High - Addresses code duplication and CEK compilation errors

## Executive Summary

The Rust migration has significant organizational issues causing:
- **117+ CEK compilation errors** due to inconsistent type definitions
- **Massive code duplication** across multiple crates
- **Fragmented responsibilities** making maintenance difficult
- **Circular dependency risks** creating tight coupling

This plan consolidates the architecture to eliminate duplication and resolve compilation blockers.

## Critical Code Duplication Analysis

### 1. Error Type Definitions (CRITICAL - Blocking CEK)

**Problem**: Multiple error hierarchies causing 117+ compilation errors

**Current State**:
- `pact-errors/src/lib.rs`: Comprehensive `PactError<Info>`, `EvalError` (850+ lines)
- `pact-cek/src/error.rs`: Separate `UserRecoverableError`, `ErrorRecovery`
- `pact-parser/src/error.rs`: Simple parser error wrappers

**Code Duplication**:
```rust
// In pact-errors/src/lib.rs
pub enum EvalError {
    EnforceError(String),
    CapabilityNotGranted { capability: String },
    // ... 70+ variants
}

// In pact-cek/src/error.rs (DUPLICATE)
pub enum UserRecoverableError {
    EnforceError(String),
    CapabilityNotGranted { capability: String },
    // ... overlapping variants with EvalError
}
```

**Impact**: CEK machine cannot compile, blocking LSP, server, and evaluation functionality.

### 2. Value Type Representations (HIGH PRIORITY)

**Multiple PactValue definitions**:
- `pact-values/src/values.rs`: Primary runtime `PactValue` (600+ lines)
- `pact-errors/src/lib.rs`: Simplified `PactValue` for error contexts  
- `pact-cek/src/types.rs`: `CEKValue` wrapping `PactValue`

**Code Duplication**:
```rust
// pact-values/src/values.rs - Complete implementation
pub enum PactValue {
    String(String), Integer(Integer), Decimal(Decimal), Bool(bool),
    Time(PactTime), List(Vec<PactValue>), Object(Object), Guard(Guard),
    // ... complete value system
}

// pact-errors/src/lib.rs - Simplified duplicate
pub enum PactValue {
    PLiteral(String), PTime(String), PList(Vec<PactValue>), 
    PObject(ObjectData<PactValue>), PGuard(String),
    // ... partial reimplementation
}
```

### 3. Name Type Definitions (HIGH PRIORITY)

**Severe fragmentation across 4+ crates**:
- `pact-names/src/lib.rs`: Authoritative name system
- `pact-parser/src/ast.rs`: Parser-specific name types
- `pact-errors/src/lib.rs`: Error context name types
- `pact-ir/src/module.rs`: IR-specific name types

**Code Duplication**:
```rust
// ModuleName defined in 4+ places with different types:
// pact-names: ModuleName { name: String, namespace: Option<String> }
// pact-parser: ModuleName { name: CompactString, namespace: Option<CompactString> }
// pact-errors: ModuleName(pub CompactString)  
// pact-ir: ModuleName(pub CompactString)
```

### 4. Hash Type Definitions (MEDIUM PRIORITY)

**Multiple hash implementations**:
- `pact-crypto/src/hash.rs`: Full crypto hash implementation
- `pact-names/src/lib.rs`: Duplicate `PactHash` and `ModuleHash`
- `pact-ir/src/module.rs`: String-based incompatible versions

**Inconsistent Types**:
```rust
// pact-crypto: PactHash { bytes: [u8; 32] } - Proper implementation
// pact-names: PactHash { bytes: [u8; 32] } - Nearly identical duplicate
// pact-ir: Hash(pub CompactString) - String-based, incompatible
```

## Overlapping Crate Responsibilities

### Small/Underutilized Crates

**Merge Candidates**:
1. **`pact-math`** (400 lines) → **`pact-values`**
   - Redundant: `PactDecimal`, `PactInteger` already in pact-values
   - Only used internally by pact-values

2. **`pact-tests`** (2 lines) → **Remove**
   - Completely empty placeholder crate

3. **`pact-shared-types`** → **`pact-values`**
   - Artificial separation of crypto types, principal types
   - Creates re-export complexity

4. **`pact-gas`** (minimal) → **`pact-values`**
   - Basic `MilliGas`, `GasLimit` types
   - Could be a module within pact-values

### Fragmented Systems

**Capability System** spread across:
- `pact-capability/`: Core capability types and manager
- `pact-values/`: Guard and keyset types  
- `pact-cek/src/builtin/capability_ops.rs`: Capability operations

**Names System** fragmented across:
- `pact-names/src/lib.rs`: Main implementation
- `pact-names/src/parsed.rs`: Overlapping functionality

## Dependency Analysis

### Problematic Patterns

**Heavy Dependencies (>7 internal deps)**:
- `pact-compiler`: 12 internal dependencies
- `pact-cek`: 7 internal dependencies  

**Circular Dependency Risks**:
- `pact-errors` ↔ `pact-values` (through re-exports)
- `pact-names` ↔ `pact-shared-types` (KeySetName confusion)
- `pact-ir` ↔ `pact-compiler` (term/compilation interdependence)

## Consolidation Plan

### Phase 1: Critical Error Type Unification (Week 1)

**URGENT - Fixes 117 CEK compilation errors**

1. **Consolidate Error Systems**:
   - **Keep**: `pact-errors` as single source of truth
   - **Remove**: All error types from other crates
   - **Action**: Update all crates to use `pact_errors::PactError<Info>`

**Implementation Steps**:
```bash
# 1. Remove UserRecoverableError from pact-cek
# 2. Update all pact-cek error handling to use pact-errors types
# 3. Fix all 117 compilation errors
# 4. Verify LSP and server compilation
```

### Phase 2: Value Type Consolidation (Week 2)

2. **Unify Value Representations**:
   - **Primary**: `pact-values::PactValue` as canonical value type
   - **CEK Integration**: `pact-cek::CEKValue` wraps `PactValue` (no duplication)
   - **Remove**: Simplified value types from `pact-errors`

**Before**:
```rust
// Multiple PactValue definitions across crates
```

**After**:
```rust
// Single PactValue in pact-values
// CEKValue wraps PactValue cleanly
// Error contexts use references to PactValue
```

### Phase 3: Crate Merging (Weeks 3-4)

3. **Merge Small Crates**:

**Merge `pact-math` → `pact-values`**:
```rust
// pact-values/src/
├── lib.rs           
├── values.rs        
├── numeric.rs       ← Move pact-math here
├── guards.rs        
├── collections.rs
└── time.rs   
```

**Merge `pact-shared-types` → `pact-values`**:
- Move crypto types, principal types to `pact-values`
- Eliminate artificial separation

**Merge `pact-gas` → `pact-values`**:
- Add gas types as module within `pact-values`

**Remove `pact-tests`**:
- Empty crate, move any tests to appropriate crates

### Phase 4: Name System Consolidation (Week 4)

4. **Centralize Naming in `pact-names`**:
   - **Single source**: All name-related types in `pact-names`
   - **Remove duplicates**: From parser, errors, IR crates
   - **Clean API**: Export only needed types

**Before**: Name types scattered across 4+ crates  
**After**: Single authoritative `pact-names` crate

### Phase 5: Hash System Consolidation (Week 5)

5. **Centralize Hashing in `pact-crypto`**:
   - **Primary**: All hash types in `pact-crypto`
   - **Remove duplicates**: From names, IR, errors crates
   - **Consistent types**: Use `[u8; 32]` consistently

### Phase 6: Dependency Cleanup (Week 6)

6. **Optimize Dependencies**:
   - **Reduce compiler deps**: From 12 to 6-8 internal crates
   - **Break cycles**: Ensure acyclic dependency graph
   - **Clear layers**: Foundation → IR → Evaluation → Applications

## Final Recommended Architecture

### Streamlined Crate Structure (23 → 9 crates)

**Foundation Layer (1 crate)**:
- `pact-core` (merge: values, names, errors, gas, capability, math, shared-types)

**Language Layer (2 crates)**:
- `pact-syntax` (merge: lexer + parser)
- `pact-ir` (merge: eval + schema)

**Execution Layer (2 crates)**:
- `pact-compiler` (orchestration)
- `pact-cek` (CEK machine evaluation)

**Storage Layer (1 crate)**:
- `pact-db` (database abstraction)

**Application Layer (2 crates)**:
- `pact-repl` (interactive REPL)
- `pact-lsp` (language server) **[KEPT SEPARATE]**

**Integration Layer (1 crate)**:
- `pact-server` (merge: request-api + HTTP server)

### Eliminated Crates

**Merged into other crates**:
- ~~`pact-math`~~ → `pact-values`
- ~~`pact-shared-types`~~ → `pact-values`  
- ~~`pact-gas`~~ → `pact-values`
- ~~`pact-tests`~~ → Removed (empty)
- ~~`pact-request-api`~~ → Could merge into `pact-server`

## Implementation Priority

### Week 1: URGENT - Fix CEK Compilation
- **Priority**: Critical blocker
- **Goal**: Eliminate 117 compilation errors
- **Action**: Unify error types in pact-errors

### Week 2: Value System Cleanup  
- **Priority**: High
- **Goal**: Single PactValue implementation
- **Action**: Remove duplicates, clean CEK integration

### Weeks 3-4: Crate Merging
- **Priority**: Medium-High  
- **Goal**: Reduce crate count, eliminate small crates
- **Action**: Merge math, shared-types, gas into values

### Weeks 5-6: Final Consolidation
- **Priority**: Medium
- **Goal**: Clean architecture, dependency optimization
- **Action**: Hash consolidation, dependency cleanup

## Success Metrics

**After consolidation**:
- ✅ **0 compilation errors** (currently 117+)
- ✅ **Single error type hierarchy** (currently 3+)
- ✅ **Single value type system** (currently 3+ implementations)  
- ✅ **9 total crates** (down from 23)
- ✅ **Clear dependency layers** (no cycles)
- ✅ **Functional LSP and server** (currently disabled)
- ✅ **LSP kept separate** (as requested)
- ✅ **~60% reduction in duplicate code**

## Risk Mitigation

1. **Incremental changes**: Consolidate one system at a time
2. **Comprehensive testing**: Run tests after each consolidation step
3. **Backup strategy**: Keep git branches for rollback if needed
4. **Documentation**: Update all documentation after changes

This consolidation directly addresses the critical blockers identified in the migration analysis while creating a cleaner, more maintainable architecture that better reflects the Haskell reference implementation.