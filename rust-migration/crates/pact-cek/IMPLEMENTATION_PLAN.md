# CEK Evaluator Complete Implementation Plan

This document outlines the complete implementation plan to achieve 100% Haskell compatibility for the Pact CEK evaluator in Rust.

## Overview

Total builtins to implement: **192** (138 Core + 10 Special Forms + 44 REPL)

Current implementation status: ~25% (approximately 48 builtins implemented)

## Implementation Phases

### Phase 1: Core Infrastructure Enhancement (Week 1)
- [ ] Complete module system integration
- [ ] Enhance PactDb trait with all database operations
- [ ] Implement DefPact state management
- [ ] Add namespace resolution system
- [ ] Complete guard type system

### Phase 2: Database Operations (Week 2)
Reference: `pact-tng/Pact/Core/Builtins/Db.hs`

- [ ] `create-table` - Table creation with schema
- [ ] `describe-table` - Get table schema
- [ ] `describe-keyset` - Get keyset info
- [ ] `describe-module` - Get module info
- [ ] `define-keyset` - Define new keyset
- [ ] `fold-db` - Fold over database rows
- [ ] `insert` - Insert new row (must not exist)
- [ ] `keys` - Get all keys from table
- [ ] `read` - Read row by key
- [ ] `select` - Select rows with predicate
- [ ] `update` - Update existing row
- [ ] `with-default-read` - Read with default value
- [ ] `with-read` - Read and bind to variable
- [ ] `write` - Write row (insert or update)

### Phase 3: Guard & Security Operations (Week 3)
Reference: `pact-tng/Pact/Core/Builtins/Guards.hs`

- [ ] `enforce-guard` - Enforce guard passes
- [ ] `enforce-keyset` - Enforce keyset authorization
- [ ] `keyset-ref-guard` - Create keyset reference guard
- [ ] `create-capability-guard` - Create capability guard
- [ ] `create-capability-pact-guard` - Create capability pact guard
- [ ] `create-module-guard` - Create module guard
- [ ] `create-pact-guard` - Create defpact guard
- [ ] `create-user-guard` - Create user-defined guard

### Phase 4: Capability System (Week 4)
Reference: `pact-tng/Pact/Core/Builtins/Capabilities.hs`

- [ ] `require-capability` - Require capability in scope
- [ ] `compose-capability` - Add to composition
- [ ] `install-capability` - Install for transaction
- [ ] `emit-event` - Emit capability event
- [ ] `with-capability` - Execute with capability (special form)

### Phase 5: Time Operations (Week 5)
Reference: `pact-tng/Pact/Core/Builtins/Time.hs`

- [ ] `parse-time` - Parse time from string
- [ ] `format-time` - Format time to string
- [ ] `time` - Get current time
- [ ] `add-time` - Add time delta
- [ ] `diff-time` - Time difference
- [ ] `hours` - Create hours delta
- [ ] `minutes` - Create minutes delta
- [ ] `days` - Create days delta

### Phase 6: Cryptographic Operations (Week 6)
Reference: `pact-tng/Pact/Core/Builtins/Hash.hs`

- [ ] `hash` - General hash function
- [ ] `hash-keccak256` - Keccak256 hash
- [ ] `hash-poseidon` - Poseidon hash
- [ ] `poseidon-hash-hack-a-chain` - Hackachain variant
- [ ] `validate-keypair` - Validate public/private key pair
- [ ] `tx-hash` - Get transaction hash

### Phase 7: DefPact Operations (Week 7)
Reference: `pact-tng/Pact/Core/Builtins/DefPact.hs`

- [ ] `yield` - Yield value to next step
- [ ] `resume` - Resume with yielded value
- [ ] `bind` - Bind object fields
- [ ] `continue` - Continue defpact execution
- [ ] `pact-id` - Get current pact ID

### Phase 8: Module & Namespace Operations (Week 8)
Reference: `pact-tng/Pact/Core/Builtins/Modules.hs`

- [ ] `namespace` - Set current namespace
- [ ] `define-namespace` - Define new namespace
- [ ] `describe-namespace` - Get namespace info
- [ ] `list-modules` - List loaded modules
- [ ] `acquire-module-admin` - Acquire module admin

### Phase 9: Principal Operations (Week 9)
Reference: `pact-tng/Pact/Core/Builtins/Principal.hs`

- [ ] `create-principal` - Create principal
- [ ] `is-principal` - Check if principal
- [ ] `typeof-principal` - Get principal type
- [ ] `validate-principal` - Validate principal format

### Phase 10: Advanced Math & Bitwise (Week 10)
Reference: `pact-tng/Pact/Core/Builtins/Math.hs`

- [ ] `exp` - Exponential (e^x)
- [ ] `ln` - Natural logarithm
- [ ] `sqrt` - Square root
- [ ] `log` - Logarithm with base
- [ ] `&` - Bitwise AND
- [ ] `|` - Bitwise OR
- [ ] `xor` - Bitwise XOR
- [ ] `~` - Bitwise complement
- [ ] `shift` - Bit shift

### Phase 11: ZK & SPV Operations (Week 11)
Reference: `pact-tng/Pact/Core/Builtins/ZK.hs`

- [ ] `pairing-check` - ZK pairing check
- [ ] `scalar-mult` - Scalar multiplication
- [ ] `point-add` - Point addition
- [ ] `verify-spv` - Verify SPV proof
- [ ] `enforce-verifier` - Enforce verifier

### Phase 12: Query Operations (Week 12)
Reference: `pact-tng/Pact/Core/Builtins/Query.hs`

- [ ] `and?` - Query AND
- [ ] `or?` - Query OR
- [ ] `where` - WHERE clause
- [ ] `not?` - Query NOT

### Phase 13: Special Forms (Week 13)
Reference: `pact-tng/Pact/Core/CEK.hs` (evaluator)

- [ ] `and` - Lazy AND evaluation
- [ ] `or` - Lazy OR evaluation
- [ ] `if` - Conditional (already done)
- [ ] `enforce` - Enforce with error
- [ ] `enforce-one` - Enforce at least one
- [ ] `try` - Try-catch (partially done)
- [ ] `error` - Throw error
- [ ] `pure` - Pure value wrapper
- [ ] `cond` - Multi-way conditional

### Phase 14: REPL-Only Builtins (Week 14-15)
Reference: `pact-tng/Pact/Core/Builtins/ReplOnly.hs`

- [ ] Test/Expect functions (5 builtins)
- [ ] Environment setters (15 builtins)
- [ ] Transaction control (4 builtins)
- [ ] Gas management (8 builtins)
- [ ] Pact state control (6 builtins)
- [ ] Module loading (3 builtins)
- [ ] Debug utilities (3 builtins)

### Phase 15: Integration & Testing (Week 16)
- [ ] Create comprehensive test suite
- [ ] Port all Haskell tests to Rust
- [ ] Verify 100% compatibility
- [ ] Performance benchmarking
- [ ] Documentation

## Implementation Guidelines

### For Each Builtin:

1. **Study Haskell Implementation**
   - Find the builtin in the corresponding Haskell module
   - Understand the exact semantics and edge cases
   - Note gas charging patterns
   - Check type validation logic

2. **Implement in Rust**
   - Follow the exact Haskell pattern
   - Use `NativeFunction` type signature
   - Implement proper gas charging
   - Add capability checks where needed
   - Handle all error cases

3. **Test Thoroughly**
   - Port Haskell tests
   - Add property-based tests
   - Test edge cases
   - Verify gas consumption

### Example Pattern:

```rust
fn builtin_name_implementation() -> NativeFunction {
    Box::new(|info, builtin, cont, handler, env, args| {
        // 1. Check argument count
        if args.len() != expected_arity {
            return EvalM::pure_value(EvalResult::EvalError(...));
        }
        
        // 2. Charge gas
        charge_gas_with_args("builtin-name", &args, base_cost)
            .bind(|_| {
                // 3. Extract and validate arguments
                match (&args[0], &args[1], ...) {
                    (expected_types...) => {
                        // 4. Check capabilities if needed
                        if !env.has_capability(...) {
                            return EvalM::pure_value(EvalResult::EvalError(...));
                        }
                        
                        // 5. Perform operation
                        let result = /* actual implementation */;
                        
                        // 6. Return result
                        apply_cont(cont, handler, CEKValue::VPactValue(result))
                    }
                    _ => EvalM::pure_value(EvalResult::EvalError(...))
                }
            })
            .try_with(|error| {
                unwind_capability_stack(&error)
                    .bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
            })
    })
}
```

## Success Criteria

- All 192 builtins implemented
- 100% test compatibility with Haskell
- No semantic differences
- Proper gas charging
- Complete error handling
- Full capability integration
- Performance parity or better

## Tracking Progress

Use the todo list to track implementation progress. Mark each builtin category as complete when:
1. All builtins in category implemented
2. Tests ported from Haskell
3. Integration tests pass
4. Gas charging verified
5. Error handling complete