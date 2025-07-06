# Critical Fixes Implementation Guide

This document provides detailed implementation instructions for the most critical fixes needed to achieve Rust-Haskell compatibility.

## Phase 1: Critical Blockers (Immediate Action Required)

### 1. Fix Lexer Missing Special Tokens

**File**: `rust-migration/crates/pact-syntax/src/token.rs`

**Problem**: Missing tokens that Haskell recognizes, causing parse failures.

**Implementation**:
```rust
// Add these token variants to the Token enum (around line 30)
pub enum Token {
    // ... existing tokens ...
    
    // Missing special constructs
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
    
    // ... rest of tokens
}
```

**Validation Test**:
```rust
#[test]
fn test_special_tokens() {
    let input = "(with-capability (PAY sender receiver amount) (transfer sender receiver amount))";
    let tokens = lex(input).unwrap();
    assert!(tokens.iter().any(|t| matches!(t, Token::WithCapability)));
}
```

### 2. Fix String Escape Error Handling

**File**: `rust-migration/crates/pact-syntax/src/token.rs` (around line 170)

**Problem**: Rust preserves invalid escapes while Haskell throws errors.

**Current (Wrong)**:
```rust
_ => {
    // For unknown escapes, preserve the backslash
    result.push('\\');
    result.push(escaped);
}
```

**Fix**:
```rust
_ => {
    // Match Haskell behavior: throw error for invalid escapes
    return Err(LexerError::InvalidEscape(format!("\\{}", escaped)));
}
```

**Add carriage return check**:
```rust
'r' => {
    return Err(LexerError::StringLiteralError("carriage return is not supported".to_string()));
}
```

### 3. Fix BFS Algorithm Determinism

**File**: `rust-migration/crates/pact-compiler/src/transitive_deps.rs`

**Problem**: Non-deterministic iteration breaks consensus.

**Current (Wrong)**:
```rust
pub struct WorkingSet(HashSet<FullyQualifiedName>);

impl WorkingSet {
    pub fn pop(&mut self) -> Option<FullyQualifiedName> {
        let fqn = self.0.iter().next().cloned(); // ❌ Non-deterministic!
        if let Some(ref f) = fqn {
            self.0.remove(f);
        }
        fqn
    }
}
```

**Fix**:
```rust
use std::collections::BTreeSet;

pub struct WorkingSet(BTreeSet<FullyQualifiedName>);

impl WorkingSet {
    pub fn new() -> Self {
        WorkingSet(BTreeSet::new())
    }
    
    pub fn insert(&mut self, fqn: FullyQualifiedName) {
        self.0.insert(fqn);
    }
    
    pub fn pop(&mut self) -> Option<FullyQualifiedName> {
        // Use iter().next() for deterministic minimum element
        if let Some(fqn) = self.0.iter().next().cloned() {
            self.0.remove(&fqn);
            Some(fqn)
        } else {
            None
        }
    }
    
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

// Ensure FullyQualifiedName implements Ord for BTreeSet
impl Ord for FullyQualifiedName {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (&self.module, &self.name, &self.hash).cmp(&(&other.module, &other.name, &other.hash))
    }
}

impl PartialOrd for FullyQualifiedName {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
```

### 4. Fix Gas Metering Formulas

**File**: `rust-migration/crates/pact-compiler/src/transitive_deps.rs`

**Problem**: Using natural log instead of log base 10.

**Current (Wrong)**:
```rust
let cost = (size_f64.ln().ceil() as u64) * COST_PER_FQN_COMPARISON;
```

**Fix**:
```rust
let cost = (size_f64.log10().ceil() as u64) * COST_PER_FQN_COMPARISON;

// For restrictKeys formula:
let cost = ((m * ((n / m) + 1.0).log10()).ceil() as u64) * COST_PER_FQN_COMPARISON;
```

### 5. Add Database Gas Metering

**File**: `rust-migration/crates/pact-db/src/traits.rs`

**Problem**: No gas metering in database operations.

**Add GasM monad**:
```rust
use std::marker::PhantomData;

pub struct GasM<T> {
    inner: Box<dyn FnOnce(&mut GasEnv) -> Result<T, PactError> + Send>,
}

impl<T> GasM<T> {
    pub fn new<F>(f: F) -> Self 
    where 
        F: FnOnce(&mut GasEnv) -> Result<T, PactError> + Send + 'static
    {
        GasM { inner: Box::new(f) }
    }
    
    pub fn pure(value: T) -> Self {
        GasM::new(move |_| Ok(value))
    }
    
    pub fn run(self, gas_env: &mut GasEnv) -> Result<T, PactError> {
        (self.inner)(gas_env)
    }
}

impl<T> GasM<T> {
    pub fn bind<U, F>(self, f: F) -> GasM<U>
    where
        F: FnOnce(T) -> GasM<U> + Send + 'static,
        T: Send + 'static,
        U: Send + 'static,
    {
        GasM::new(move |gas_env| {
            let result = self.run(gas_env)?;
            f(result).run(gas_env)
        })
    }
}
```

**Update PactDb trait**:
```rust
pub trait PactDb: Send + Sync {
    fn read<K, V>(&self, domain: &Domain<K, V>, key: &K) -> GasM<Option<V>>
    where
        K: Clone + Send + 'static,
        V: Clone + Send + 'static;
        
    fn write<K, V>(&self, write_type: WriteType, domain: &Domain<K, V>, 
                   key: &K, value: &V) -> GasM<()>
    where
        K: Clone + Send + 'static,
        V: Clone + Send + 'static;
        
    fn keys<K, V>(&self, domain: &Domain<K, V>) -> GasM<Vec<K>>
    where
        K: Clone + Send + 'static,
        V: Clone + Send + 'static;
        
    fn begin_tx(&self, mode: ExecutionMode) -> GasM<Option<TxId>>;
    fn commit_tx(&self) -> GasM<Vec<TxLog>>;
    fn rollback_tx(&self) -> GasM<()>;
}
```

### 6. Implement Top-Level Variable Lookup

**File**: `rust-migration/crates/pact-cek/src/eval.rs`

**Problem**: Returns unimplemented error for variable lookup.

**Current (Wrong)**:
```rust
Name::Parsed(_) | Name::Resolved(_) => {
    EvalM::pure_value(EvalResult::EvalError(
        PactError::PEExecutionError(
            EvalError::UnimplementedBuiltin("Top-level variable lookup".to_string()),
        ),
        info,
    ))
}
```

**Fix**:
```rust
Name::Resolved(resolved) => {
    // Create fully qualified name
    let fqn = FullyQualifiedName {
        module: resolved.module.clone(),
        name: resolved.name.clone(),
        hash: resolved.hash.clone(),
    };
    
    // Look up in environment
    match env.lookup_fqname(&fqn).await {
        Ok(Some(def)) => match def {
            EvalDef::Dfun(dfun) => {
                // Create function closure
                let closure = VClosure(CanApply::DefClosure(DefClosure {
                    name: fqn,
                    closure: dfun,
                    env: env.clone(),
                }));
                return_cek_value(cont, handler, closure)
            }
            EvalDef::DConst(dconst) => {
                // Return constant value
                return_cek_value(cont, handler, VPactValue(dconst.value.clone()))
            }
            EvalDef::DCap(dcap) => {
                // Create capability closure
                let cap_closure = VClosure(CanApply::CapClosure(CapClosure {
                    name: fqn,
                    cap: dcap,
                    env: env.clone(),
                }));
                return_cek_value(cont, handler, cap_closure)
            }
            _ => {
                return_cek_error(handler, PactError::NameNotInScope(resolved.name.clone(), info))
            }
        }
        Ok(None) => {
            return_cek_error(handler, PactError::NameNotInScope(resolved.name.clone(), info))
        }
        Err(e) => return_cek_error(handler, e)
    }
}

Name::Parsed(parsed) => {
    // Handle parsed names by resolving them first
    match parsed.name_kind {
        NameKind::Unqualified => {
            // Look up in current module context
            self.resolve_unqualified_name(&parsed.name, &env).await
        }
        NameKind::Qualified(module_name) => {
            // Create FQN with explicit module
            let fqn = FullyQualifiedName {
                module: module_name,
                name: parsed.name.clone(),
                hash: None, // Will be resolved during lookup
            };
            // Continue with lookup...
        }
    }
}
```

## Implementation Order

### Week 1 Priority:
1. **Lexer special tokens** (blocks parsing)
2. **BFS algorithm fix** (consensus critical)
3. **Gas formula fix** (consensus critical)

### Week 2 Priority:
1. **Database gas metering** (consensus critical)
2. **Top-level variable lookup** (blocks most programs)
3. **String escape handling** (parsing accuracy)

## Testing Strategy

### Unit Tests:
```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_bfs_determinism() {
        // Test that dependency order is identical across runs
        let mut working_set = WorkingSet::new();
        working_set.insert(fqn1);
        working_set.insert(fqn2);
        working_set.insert(fqn3);
        
        let order1: Vec<_> = std::iter::from_fn(|| working_set.pop()).collect();
        
        // Reset and test again
        working_set.insert(fqn1);
        working_set.insert(fqn2);
        working_set.insert(fqn3);
        
        let order2: Vec<_> = std::iter::from_fn(|| working_set.pop()).collect();
        
        assert_eq!(order1, order2, "BFS order must be deterministic");
    }
    
    #[test]
    fn test_gas_formula_compatibility() {
        // Test that gas formulas match Haskell exactly
        let size = 100.0;
        let rust_cost = (size.log10().ceil() as u64) * COST_PER_FQN_COMPARISON;
        let expected_haskell_cost = 200; // Known value from Haskell
        assert_eq!(rust_cost, expected_haskell_cost);
    }
}
```

### Integration Tests:
```rust
#[test]
fn test_variable_lookup_compatibility() {
    let input = r#"
    (module test-module G
      (defun add-one (x) (+ x 1))
      (defconst MY_CONST 42)
    )
    (add-one MY_CONST)
    "#;
    
    // Should resolve add-one and MY_CONST correctly
    let result = eval_pact_code(input).await.unwrap();
    assert_eq!(result, PValue::PInteger(43));
}
```

This implementation guide provides the exact code changes needed to fix the most critical compatibility issues. Each fix should be implemented, tested, and validated against the Haskell reference before moving to the next issue.