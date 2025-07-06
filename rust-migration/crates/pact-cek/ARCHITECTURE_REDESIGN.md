# CEK Architecture Redesign Plan

## Overview

This document outlines the redesign of the pact-cek crate to properly match the Haskell CEK machine architecture. The current implementation has significant gaps that would require workarounds to implement all builtins correctly.

## Core Problems with Current Architecture

1. **Incomplete CEKValue**: Missing closure types, wrong list representation
2. **Inadequate Environment**: Missing PactDb, builtins, capability context
3. **Incomplete Continuations**: Missing 11+ critical continuation types
4. **Wrong Evaluation Pattern**: No monad stack, wrong state management
5. **Inadequate Builtin System**: Wrong integration pattern

## Target Haskell Architecture

### 1. CEKValue Structure
```haskell
data CEKValue e b i
  = VPactValue !PactValue
  | VClosure  !(CanApply e b i)

data CanApply e b i
  = C !(Closure e b i)           -- User-defined function
  | N !(NativeFn e b i)          -- Builtin function
  | CT !(CapTokenClosure i)      -- Capability token
  | LC !(LamClosure e b i)       -- Lambda closure
  | PC !(PartialClosure e b i)   -- Partially applied function
  | PN !(PartialNativeFn e b i)  -- Partially applied builtin
  | DPC !(DefPactClosure e b i)  -- DefPact closure
```

### 2. CEKEnv Structure
```haskell
data CEKEnv e b i = CEKEnv
  { _ceLocal :: RAList (CEKValue e b i)
  , _cePactDb :: PactDb b i
  , _ceBuiltins :: BuiltinEnv e b i
  , _ceDefPactStep :: Maybe DefPactStep
  , _ceInCap :: Bool
  }
```

### 3. Continuation Structure (18+ variants)
```haskell
data Cont e b i
  = Mt
  | Args !(CEKEnv e b i) i ![EvalTerm b i] !(Cont e b i)
  | Fn !(CanApply e b i) !(CEKEnv e b i) ![EvalTerm b i] ![CEKValue e b i] !(Cont e b i)
  | LetC !(CEKEnv e b i) i (Arg Type i) !(EvalTerm b i) !(Cont e b i)
  | SeqC (CEKEnv e b i) i (EvalTerm b i) (Cont e b i)
  | ListC (CEKEnv e b i) i [EvalTerm b i] [PactValue] (Cont e b i)
  | CondC (CEKEnv e b i) i (CondCont e b i) (Cont e b i)
  | BuiltinC (CEKEnv e b i) i (BuiltinCont e b i) (Cont e b i)
  | CapInvokeC (CEKEnv e b i) i (CapCont e b i) (Cont e b i)
  -- ... and 9 more variants
```

### 4. Evaluation Monad
```haskell
newtype EvalM e b i a = EvalM (ReaderT (EvalMEnv e b i) (ExceptT (PactError i) (StateT (EvalState b i) IO)) a)
```

### 5. Builtin Integration
```haskell
type NativeFunction e b i = i -> b -> Cont e b i -> CEKErrorHandler e b i -> CEKEnv e b i -> [CEKValue e b i] -> EvalM e b i (EvalResult e b i)
```

## Redesigned Rust Architecture

### Phase 1: Core Type System

#### 1.1 CEKValue (src/value.rs)
```rust
/// CEK machine values - exactly matches Haskell CEKValue
#[derive(Debug, Clone, PartialEq)]
pub enum CEKValue {
    /// Basic Pact values (literals, objects, lists, etc.)
    VPactValue(PactValue),
    /// Callable entities (functions, builtins, partial applications)
    VClosure(CanApply),
}

/// Callable entities - matches Haskell CanApply
#[derive(Debug, Clone, PartialEq)]
pub enum CanApply {
    /// User-defined function closure
    C(Closure),
    /// Native builtin function
    N(NativeFn),
    /// Capability token closure
    CT(CapTokenClosure),
    /// Lambda closure
    LC(LamClosure), 
    /// Partially applied function
    PC(PartialClosure),
    /// Partially applied builtin
    PN(PartialNativeFn),
    /// DefPact closure
    DPC(DefPactClosure),
}
```

#### 1.2 Environment (src/env.rs)
```rust
/// CEK Environment - matches Haskell CEKEnv exactly
#[derive(Debug, Clone)]
pub struct CEKEnv {
    /// Local variable bindings (DeBruijn indices)
    pub local: RAList<CEKValue>,
    /// Database interface for persistence operations
    pub pact_db: Arc<dyn PactDb>,
    /// Builtin function resolver
    pub builtins: BuiltinEnv,
    /// DefPact execution state
    pub defpact_step: Option<DefPactStep>,
    /// Whether currently in capability context
    pub in_cap: bool,
}

/// Random Access List for efficient variable lookup
pub struct RAList<T> {
    items: im::Vector<T>,
}

impl<T> RAList<T> {
    pub fn new() -> Self { /* ... */ }
    pub fn cons(&self, item: T) -> Self { /* ... */ }
    pub fn lookup(&self, index: usize) -> Option<&T> { /* ... */ }
}
```

#### 1.3 Continuations (src/cont.rs)  
```rust
/// Continuation stack - matches Haskell Cont exactly
#[derive(Debug, Clone)]
pub enum Cont {
    /// Empty continuation (terminal state)
    Mt,
    
    /// Function argument evaluation
    Args {
        env: CEKEnv,
        info: SpanInfo,
        args: Vec<CoreTerm>,
        cont: Box<Cont>,
    },
    
    /// Function application with accumulated arguments
    Fn {
        function: CanApply,
        env: CEKEnv,
        args: Vec<CoreTerm>,
        values: Vec<CEKValue>,
        cont: Box<Cont>,
    },
    
    /// Let binding continuation
    LetC {
        env: CEKEnv,
        info: SpanInfo,
        arg: Arg,
        body: CoreTerm,
        cont: Box<Cont>,
    },
    
    /// Sequence evaluation
    SeqC {
        env: CEKEnv,
        info: SpanInfo,
        expr: CoreTerm,
        cont: Box<Cont>,
    },
    
    /// List literal construction
    ListC {
        env: CEKEnv,
        info: SpanInfo,
        exprs: Vec<CoreTerm>,
        values: Vec<PactValue>,
        cont: Box<Cont>,
    },
    
    /// Conditional evaluation (if/and/or/enforce)
    CondC {
        env: CEKEnv,
        info: SpanInfo,
        cond_cont: CondCont,
        cont: Box<Cont>,
    },
    
    /// Higher-order builtin function continuation
    BuiltinC {
        env: CEKEnv,
        info: SpanInfo,
        builtin_cont: BuiltinCont,
        cont: Box<Cont>,
    },
    
    /// Capability invocation
    CapInvokeC {
        env: CEKEnv,
        info: SpanInfo,
        cap_cont: CapCont,
        cont: Box<Cont>,
    },
    
    /// Capability body execution
    CapBodyC {
        env: CEKEnv,
        info: SpanInfo,
        body_state: CapBodyState,
        cont: Box<Cont>,
    },
    
    /// Capability pop from stack
    CapPopC {
        pop_state: CapPopState,
        info: SpanInfo,
        cont: Box<Cont>,
    },
    
    /// DefPact step execution
    DefPactStepC {
        env: CEKEnv,
        info: SpanInfo,
        cont: Box<Cont>,
    },
    
    /// Nested DefPact step
    NestedDefPactStepC {
        env: CEKEnv,
        info: SpanInfo,
        cont: Box<Cont>,
        exec: DefPactExec,
    },
    
    /// Ignore value continuation
    IgnoreValueC {
        value: PactValue,
        cont: Box<Cont>,
    },
    
    /// Enforce boolean type
    EnforceBoolC {
        info: SpanInfo,
        cont: Box<Cont>,
    },
    
    /// Enforce PactValue type
    EnforcePactValueC {
        info: SpanInfo,
        cont: Box<Cont>,
    },
    
    /// Module admin continuation
    ModuleAdminC {
        module: ModuleName,
        cont: Box<Cont>,
    },
    
    /// Stack frame pop
    StackPopC {
        info: SpanInfo,
        ty: Option<Type>,
        cont: Box<Cont>,
    },
    
    /// Enforce error continuation
    EnforceErrorC {
        info: SpanInfo,
        cont: Box<Cont>,
    },
}
```

### Phase 2: Evaluation Monad

#### 2.1 EvalM Monad (src/monad.rs)
```rust
/// Evaluation monad - equivalent to Haskell EvalM
pub struct EvalM<T> {
    inner: Box<dyn FnOnce(EvalMEnv) -> Result<(T, EvalState), PactError> + Send>,
}

impl<T> EvalM<T> {
    pub fn new<F>(f: F) -> Self 
    where F: FnOnce(EvalMEnv) -> Result<(T, EvalState), PactError> + Send + 'static
    {
        EvalM { inner: Box::new(f) }
    }
    
    pub fn run(self, env: EvalMEnv, state: EvalState) -> Result<(T, EvalState), PactError> {
        (self.inner)(env)
    }
    
    pub fn pure_value(value: T) -> Self { /* ... */ }
    pub fn throw_error(error: PactError) -> Self { /* ... */ }
    pub fn get_state() -> EvalM<EvalState> { /* ... */ }
    pub fn put_state(state: EvalState) -> EvalM<()> { /* ... */ }
    pub fn ask_env() -> EvalM<EvalMEnv> { /* ... */ }
}

impl<T> EvalM<T> {
    pub fn bind<U, F>(self, f: F) -> EvalM<U>
    where F: FnOnce(T) -> EvalM<U> + Send + 'static
    {
        // Monadic bind implementation
    }
    
    pub fn map<U, F>(self, f: F) -> EvalM<U>
    where F: FnOnce(T) -> U + Send + 'static
    {
        // Functor map implementation
    }
}

/// Environment for the evaluation monad
#[derive(Debug, Clone)]
pub struct EvalMEnv {
    pub gas_env: GasEnv,
    pub purity_env: PurityEnv,
    pub tx_env: TxEnv,
}

/// State for the evaluation monad
#[derive(Debug, Clone)]
pub struct EvalState {
    pub gas_state: GasState,
    pub eval_stack: Vec<StackFrame>,
    pub call_stack: Vec<CallFrame>,
}
```

#### 2.2 Error Handling (src/error.rs)
```rust
/// CEK Error Handler - matches Haskell CEKErrorHandler
#[derive(Debug, Clone)]
pub enum CEKErrorHandler {
    /// No error handler
    CEKNoHandler,
    
    /// Try-catch style error handler
    CEKHandler {
        env: CEKEnv,
        recovery: CoreTerm,
        cont: Cont,
        error_state: ErrorState,
        next_handler: Box<CEKErrorHandler>,
    },
    
    /// Enforce-one error handler (special case)
    CEKEnforceOne {
        env: CEKEnv,
        info: SpanInfo,
        remaining: Vec<CoreTerm>,
        cont: Cont,
        error_state: ErrorState,
        next_handler: Box<CEKErrorHandler>,
    },
}

#[derive(Debug, Clone)]
pub struct ErrorState {
    pub source_info: SpanInfo,
    pub call_stack: Vec<CallFrame>,
}
```

### Phase 3: Core Evaluation

#### 3.1 Main Evaluator (src/evaluator.rs)
```rust
/// Main CEK evaluation function - matches Haskell evalCEK exactly
pub fn eval_cek(
    cont: Cont,
    handler: CEKErrorHandler,
    env: CEKEnv,
    term: CoreTerm,
) -> EvalM<EvalResult> {
    use CoreTerm::*;
    
    match term {
        // Variable lookup with DeBruijn indices
        Var(name, info) => eval_var(cont, handler, env, name, info),
        
        // Constants return directly
        Constant(literal, info) => {
            let value = CEKValue::VPactValue(literal_to_pact_value(literal)?);
            return_cek_value(cont, handler, value)
        },
        
        // Application starts argument evaluation
        App { func, args, info } => {
            if args.is_empty() {
                eval_cek(cont, handler, env, *func)
            } else {
                let args_cont = Cont::Args {
                    env: env.clone(),
                    info,
                    args,
                    cont: Box::new(cont),
                };
                eval_cek(args_cont, handler, env, *func)
            }
        },
        
        // Lambda creates closure
        Lam { args, body, info } => {
            let closure = CanApply::LC(LamClosure {
                args,
                body: *body,
                env,
                info,
            });
            let value = CEKValue::VClosure(closure);
            return_cek_value(cont, handler, value)
        },
        
        // Builtin lookup from environment
        Builtin(builtin, info) => {
            let native_fn = env.builtins.lookup(builtin, info)?;
            let closure = CanApply::N(native_fn);
            let value = CEKValue::VClosure(closure);
            return_cek_value(cont, handler, value)
        },
        
        // Let binding
        Let { arg, expr, body, info } => {
            let let_cont = Cont::LetC {
                env: env.clone(),
                info,
                arg,
                body: *body,
                cont: Box::new(cont),
            };
            eval_cek(let_cont, handler, env, *expr)
        },
        
        // Sequence evaluation
        Sequence { first, second, info } => {
            let seq_cont = Cont::SeqC {
                env: env.clone(),
                info,
                expr: *second,
                cont: Box::new(cont),
            };
            eval_cek(seq_cont, handler, env, *first)
        },
        
        // List literal construction
        ListLit { elements, info } => {
            eval_list_literal(cont, handler, env, elements, info)
        },
        
        // Other cases...
        _ => {
            EvalM::throw_error(PactError::Runtime(RuntimeError::UnimplementedBuiltin {
                name: "Term evaluation".to_string(),
                reason: "Not yet implemented".to_string(),
            }))
        }
    }
}

/// Return value to continuation - matches Haskell returnCEKValue
pub fn return_cek_value(
    cont: Cont,
    handler: CEKErrorHandler,
    value: CEKValue,
) -> EvalM<EvalResult> {
    apply_cont_to_value(cont, handler, value)
}

/// Apply continuation to value - matches Haskell applyContToValue
pub fn apply_cont_to_value(
    cont: Cont,
    handler: CEKErrorHandler,
    value: CEKValue,
) -> EvalM<EvalResult> {
    match cont {
        Cont::Mt => apply_cont(handler, value),
        
        Cont::Args { env, info, args, cont } => {
            match value {
                CEKValue::VClosure(can_apply) => {
                    let fn_cont = Cont::Fn {
                        function: can_apply,
                        env,
                        args,
                        values: Vec::new(),
                        cont,
                    };
                    apply_cont_to_value(fn_cont, handler, value)
                },
                _ => {
                    EvalM::throw_error(PactError::Runtime(RuntimeError::TypeMismatch {
                        expected: "function".to_string(),
                        found: "non-function".to_string(),
                        context: "application".to_string(),
                    }))
                }
            }
        },
        
        Cont::Fn { function, env, mut args, mut values, cont } => {
            values.push(value);
            if args.is_empty() {
                // All arguments collected, apply function
                apply_lambda(function, values, *cont, handler)
            } else {
                // Evaluate next argument
                let next_arg = args.remove(0);
                let fn_cont = Cont::Fn {
                    function,
                    env: env.clone(),
                    args,
                    values,
                    cont,
                };
                eval_cek(fn_cont, handler, env, next_arg)
            }
        },
        
        Cont::LetC { env, info, arg, body, cont } => {
            let extended_env = env.extend_local(value);
            eval_cek(*cont, handler, extended_env, body)
        },
        
        Cont::SeqC { env, info, expr, cont } => {
            // Discard current value and evaluate next expression
            eval_cek(*cont, handler, env, expr)
        },
        
        // Handle all other continuation types...
        _ => {
            EvalM::throw_error(PactError::Runtime(RuntimeError::UnimplementedBuiltin {
                name: "Continuation".to_string(),
                reason: "Not yet implemented".to_string(),
            }))
        }
    }
}
```

### Phase 4: Builtin Integration

#### 4.1 Builtin Environment (src/builtin/env.rs)
```rust
/// Builtin environment for function resolution
pub struct BuiltinEnv {
    functions: HashMap<CoreBuiltin, NativeFunction>,
}

impl BuiltinEnv {
    pub fn lookup(&self, builtin: CoreBuiltin, info: SpanInfo) -> Result<NativeFn, PactError> {
        self.functions.get(&builtin)
            .map(|f| NativeFn {
                builtin,
                function: f.clone(),
                info,
            })
            .ok_or_else(|| PactError::Runtime(RuntimeError::UnknownBuiltin {
                name: format!("{:?}", builtin),
            }))
    }
}

/// Native function type - matches Haskell NativeFunction exactly
pub type NativeFunction = fn(
    info: SpanInfo,
    builtin: CoreBuiltin,
    cont: Cont,
    handler: CEKErrorHandler,
    env: CEKEnv,
    args: Vec<CEKValue>,
) -> EvalM<EvalResult>;

/// Native function closure
#[derive(Debug, Clone, PartialEq)]
pub struct NativeFn {
    pub builtin: CoreBuiltin,
    pub function: NativeFunction,
    pub info: SpanInfo,
}
```

#### 4.2 Builtin Implementation Pattern (src/builtin/arithmetic.rs)
```rust
/// Addition builtin - proper CEK pattern
pub fn core_add(
    info: SpanInfo,
    _builtin: CoreBuiltin,
    cont: Cont,
    handler: CEKErrorHandler,
    _env: CEKEnv,
    args: Vec<CEKValue>,
) -> EvalM<EvalResult> {
    // Charge gas for operation
    charge_gas("add", GasArgs::Constant(MilliGas(10)))?;
    
    match args.as_slice() {
        [CEKValue::VPactValue(PactValue::Integer(a)), CEKValue::VPactValue(PactValue::Integer(b))] => {
            let result = a + b;
            let value = CEKValue::VPactValue(PactValue::Integer(result));
            return_cek_value(cont, handler, value)
        },
        [CEKValue::VPactValue(PactValue::Decimal(a)), CEKValue::VPactValue(PactValue::Decimal(b))] => {
            let result = a + b;
            let value = CEKValue::VPactValue(PactValue::Decimal(result));
            return_cek_value(cont, handler, value)
        },
        _ => {
            EvalM::throw_error(PactError::Runtime(RuntimeError::TypeMismatch {
                expected: "numeric types".to_string(),
                found: "other".to_string(),
                context: "addition".to_string(),
            }))
        }
    }
}
```

## Implementation Plan

### Phase 1: Foundation (Week 1)
1. Implement new type system (CEKValue, CanApply, CEKEnv, Cont)
2. Create EvalM monad with proper state/error handling
3. Build RAList and core data structures

### Phase 2: Core Evaluation (Week 2)  
1. Implement evalCEK and returnCEKValue functions
2. Handle basic continuation cases (Mt, Args, Fn, Let, Seq)
3. Add variable lookup and constant evaluation

### Phase 3: Extended Features (Week 3)
1. Add remaining continuation types
2. Implement capability system integration  
3. Add DefPact support hooks

### Phase 4: Builtin System (Week 4)
1. Create proper builtin environment
2. Implement core arithmetic/comparison builtins
3. Add higher-order function support (map, filter, fold)
4. Test with database operations

### Phase 5: Testing & Integration (Week 5)
1. Comprehensive test suite
2. Performance benchmarking
3. Integration with existing Pact infrastructure

This redesign provides a solid foundation that can implement ALL Pact builtins correctly without workarounds, following the Haskell architecture exactly.
