# Pact 5 Rust-Haskell Alignment Plan

## Executive Summary

This document outlines a comprehensive 10-week plan to achieve 100% behavioral compatibility and feature parity between the Rust implementation and the Haskell reference implementation of Pact 5.

## Strategic Approach

### Core Principles
1. **Exact Behavioral Compatibility**: Every operation must produce identical results
2. **No Workarounds**: Implement algorithms exactly as Haskell does
3. **Semantic Preservation**: Maintain gas costs, evaluation order, and error semantics
4. **Test-Driven Development**: Validate every change against Haskell reference

### Success Metrics
- [ ] All existing Pact test suites pass on Rust implementation
- [ ] Gas consumption identical within ±1% tolerance
- [ ] Serialization format 100% compatible
- [ ] Performance ≥2x improvement over Haskell
- [ ] Zero semantic differences in evaluation

---

## Phase 1: Critical Blockers (Weeks 1-2)

**Goal**: Fix issues that prevent basic functionality and cause consensus failures.

### Week 1: Lexer and Module System Fixes

#### **Task 1.1: Fix Lexer Missing Special Tokens** 
**Priority**: CRITICAL - Blocks parsing of core Pact constructs
**Estimated**: 2 days

**Implementation**:
```rust
// Add to pact-syntax/src/token.rs
#[token("with-capability")]
WithCapability,
#[token("expect-typechecks")]
ExpectTypechecks,
#[token("expect-typecheck-failure")]
ExpectTypecheckFailure,
#[token("create-user-guard")]
CreateUserGuard,
#[token("enforce")]
Enforce,
#[token("enforce-one")]
EnforceOne,
```

**Validation**: Parse test files using these constructs

#### **Task 1.2: Fix BFS Algorithm Determinism**
**Priority**: CRITICAL - Causes consensus failures
**Estimated**: 3 days

**Current Issue**:
```rust
// WRONG: Non-deterministic iteration
let fqn = self.0.iter().next().cloned();
```

**Fix**:
```rust
// Use BTreeSet for deterministic ordering
pub struct WorkingSet(BTreeSet<FullyQualifiedName>);

impl WorkingSet {
    pub fn pop(&mut self) -> Option<FullyQualifiedName> {
        if let Some(fqn) = self.0.iter().next().cloned() {
            self.0.remove(&fqn);
            Some(fqn)
        } else {
            None
        }
    }
}
```

**Validation**: Compare dependency order with Haskell on complex modules

### Week 2: Database Gas Metering and CEK Core

#### **Task 1.3: Add Database Gas Metering**
**Priority**: CRITICAL - Breaks consensus without gas accounting
**Estimated**: 4 days

**Implementation**:
```rust
// Modify pact-db/src/traits.rs
pub trait PactDb: Send + Sync {
    fn read_with_gas<K, V>(&self, domain: &Domain<K, V>, key: &K) -> GasM<Option<V>>;
    fn write_with_gas<K, V>(&self, write_type: WriteType, domain: &Domain<K, V>, 
                           key: &K, value: &V) -> GasM<()>;
}

// Add GasM monad
pub struct GasM<T> {
    computation: Box<dyn FnOnce(&mut GasEnv) -> Result<T, PactError>>,
}
```

**Validation**: Verify gas consumption matches Haskell exactly

#### **Task 1.4: Implement Top-Level Variable Lookup**
**Priority**: CRITICAL - Blocks most Pact programs
**Estimated**: 2 days

**Current Issue**:
```rust
Name::Parsed(_) | Name::Resolved(_) => {
    EvalM::pure_value(EvalResult::EvalError(/* unimplemented */))
}
```

**Fix**:
```rust
Name::Resolved(resolved) => {
    let fqn = FullyQualifiedName {
        module: resolved.module.clone(),
        name: resolved.name.clone(),
        hash: resolved.hash.clone(),
    };
    self.lookup_fqname(&fqn, &env).await
}
```

**Validation**: Test function calls and constant references

---

## Phase 2: Core Functionality (Weeks 3-6)

**Goal**: Implement missing core features for functional compatibility.

### Week 3-4: Parser Architecture Refactor

#### **Task 2.1: Refactor AST to Match Haskell Structure**
**Priority**: HIGH - Required for syntax compatibility
**Estimated**: 6 days

**Current Issue**: Over-specialized AST with hardcoded special forms
**Solution**: Generic `App` expression with post-processing

**Implementation Plan**:
1. Simplify AST to match Haskell exactly:
```rust
pub enum ParsedExpr<I> {
    Var(ParsedName, I),
    Lam(Vec<MArg<I>>, Vec<ParsedExpr<I>>, I),
    App(Box<ParsedExpr<I>>, Vec<ParsedExpr<I>>, I),
    Let(LetForm, Vec<Binder<I>>, Vec<ParsedExpr<I>>, I),
    List(Vec<ParsedExpr<I>>, I),
    Constant(Literal, I),
    Object(Vec<(Field, ParsedExpr<I>)>, I),
    Binding(Vec<(Field, MArg<I>)>, Vec<ParsedExpr<I>>, I),
}
```

2. Implement `toAppExprList` logic for mixed argument/binding handling
3. Add proper `AppBindList` parsing support

**Validation**: Parse complex expressions with mixed args and bindings

#### **Task 2.2: Add NonEmpty Constraints**
**Priority**: HIGH - Prevents semantic errors
**Estimated**: 2 days

**Implementation**:
```rust
// Use custom NonEmpty type or validation
pub struct NonEmpty<T>(Vec<T>);

impl<T> NonEmpty<T> {
    pub fn new(first: T, rest: Vec<T>) -> Self {
        let mut vec = vec![first];
        vec.extend(rest);
        NonEmpty(vec)
    }
}
```

### Week 5-6: CEK Builtin Functions

#### **Task 2.3: Implement Database Builtin Functions**
**Priority**: HIGH - Required for state access
**Estimated**: 8 days

**Missing Functions**:
- `read`, `write`, `keys`, `select`
- `create-table`, `describe-table`
- `with-read`, `with-default-read`

**Implementation Pattern**:
```rust
pub fn builtin_read(
    info: SpanInfo,
    builtin: CoreBuiltin,
    cont: Cont,
    handler: CEKErrorHandler,
    env: CEKEnv,
    args: Vec<CEKValue>,
) -> EvalM<EvalResult> {
    match args.as_slice() {
        [VPactValue(PValue::PString(table)), VPactValue(PValue::PString(key))] => {
            let domain = Domain::UserTables(TableName::new(table.clone()));
            let result = env.pact_db.read_with_gas(&domain, &RowKey::new(key.clone())).await?;
            match result {
                Some(row_data) => return_cek_value(cont, handler, VPactValue(PValue::PObject(row_data))),
                None => return_cek_error(handler, PactError::RowNotFound(table.clone(), key.clone())),
            }
        }
        _ => args_error(info, builtin, args),
    }
}
```

#### **Task 2.4: Implement Capability Builtin Functions**
**Priority**: HIGH - Required for security model
**Estimated**: 6 days

**Missing Functions**:
- `require-capability`, `install-capability`
- `compose-capability`, `with-capability`
- `create-capability-guard`

---

## Phase 3: Advanced Features (Weeks 7-8)

**Goal**: Complete feature set for full Pact language support.

### Week 7: Advanced Language Features

#### **Task 3.1: Complete Property Expression Parsing**
**Priority**: MEDIUM - Required for formal verification
**Estimated**: 4 days

**Implementation**: 
- Parse `@model` property expressions
- Implement property expression AST nodes
- Add property validation logic

#### **Task 3.2: Implement DefCap Dependency Tracking**
**Priority**: HIGH - Required for managed capabilities
**Estimated**: 3 days

**Current Issue**: Managed capability dependencies not tracked
**Fix**: Convert manager function names to FQNs during dependency analysis

### Week 8: System Integration

#### **Task 3.3: Add Type-Safe Database Domains**
**Priority**: HIGH - Required for compile-time safety
**Estimated**: 5 days

**Implementation**:
```rust
pub struct Domain<K, V> {
    domain_type: DomainType,
    _phantom: PhantomData<(K, V)>,
}

pub enum DomainType {
    UserTables(TableName),
    KeySets,
    Modules,
    Namespaces,
    DefPacts,
    ModuleSource,
}
```

#### **Task 3.4: Implement Missing System Types**
**Priority**: MEDIUM - Required for full system support
**Estimated**: 3 days

**Missing Types**:
- `KeySet`, `KeySetName`
- `DefPactExec`, `DefPactId`
- `HashedModuleName`, `ModuleCode`

---

## Phase 4: Compatibility and Polish (Weeks 9-10)

**Goal**: Achieve 100% compatibility and production readiness.

### Week 9: Serialization and Error Compatibility

#### **Task 4.1: Implement Haskell-Compatible Serialization**
**Priority**: HIGH - Required for data migration
**Estimated**: 4 days

**Implementation**:
- Use same CBOR encoding as Haskell
- Implement `StableEncoding` for deterministic serialization
- Add version compatibility handling

#### **Task 4.2: Match Haskell Error Types Exactly**
**Priority**: MEDIUM - Required for identical error reporting
**Estimated**: 3 days

**Implementation**: Map all Rust error types to exact Haskell equivalents

### Week 10: Testing and Validation

#### **Task 4.3: Comprehensive Test Suite Validation**
**Priority**: CRITICAL - Validates 100% compatibility
**Estimated**: 5 days

**Test Categories**:
1. **Unit Tests**: All existing Haskell tests must pass
2. **Property Tests**: Random program generation and comparison
3. **Integration Tests**: Full contract deployment and execution
4. **Gas Tests**: Exact gas consumption validation
5. **Serialization Tests**: Binary compatibility validation

---

## Implementation Guidelines

### Code Quality Standards
1. **Follow Haskell patterns exactly**: No "improvements" that change semantics
2. **Comprehensive error handling**: Match Haskell error types and messages
3. **Gas metering**: Every operation must charge appropriate gas
4. **Documentation**: Document deviations and compatibility notes

### Testing Strategy
1. **Test-driven development**: Write tests before implementation
2. **Cross-validation**: Compare every output with Haskell reference
3. **Property testing**: Use QuickCheck-style random testing
4. **Performance benchmarking**: Ensure 2x+ performance improvement

### Development Process
1. **Small, incremental changes**: One feature per PR
2. **Continuous validation**: Run compatibility tests on every change
3. **Code review**: Focus on Haskell semantic compatibility
4. **Integration testing**: Full end-to-end validation

---

## Risk Mitigation

### Technical Risks
1. **Performance regression**: Continuous benchmarking
2. **Semantic drift**: Strict adherence to Haskell patterns
3. **Integration issues**: Early and frequent integration testing

### Schedule Risks
1. **Underestimated complexity**: 20% buffer built into estimates
2. **Discovery of new issues**: Weekly review and re-prioritization
3. **Blocked dependencies**: Parallel development where possible

---

## Success Validation

### Milestone Criteria
- **Week 2**: Basic Pact programs parse and evaluate
- **Week 4**: Database operations work correctly
- **Week 6**: Capability system functional
- **Week 8**: Advanced features implemented
- **Week 10**: 100% test suite compatibility

### Final Acceptance Criteria
1. All Pact test suites pass without modification
2. Gas consumption within ±1% of Haskell implementation
3. Identical serialization format for all data types
4. Performance improvement ≥2x over Haskell
5. Zero known semantic differences

---

## Resource Requirements

### Development Team
- **2-3 Senior Rust developers** with systems programming experience
- **1 Haskell expert** for reference implementation guidance
- **1 Testing engineer** for comprehensive validation
- **1 DevOps engineer** for CI/CD and benchmarking

### Infrastructure
- **Continuous Integration**: Automated testing against Haskell reference
- **Performance monitoring**: Automated benchmarking and regression detection
- **Cross-platform testing**: Linux, macOS, Windows validation

This plan provides a clear path to 100% Rust-Haskell compatibility while maintaining the performance and safety benefits of the Rust implementation.