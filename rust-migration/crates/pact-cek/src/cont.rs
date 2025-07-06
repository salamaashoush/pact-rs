//! CEK Continuations
//!
//! This module implements the continuation stack (the 'K' in CEK) following
//! the Haskell implementation exactly. All continuation types match their
//! Haskell counterparts in structure and semantics.

use crate::types::{CEKValue, CEKEnv, CanApply};
use pact_ir::{CoreTerm, ModuleName, Arg, Type, Field};
use pact_core::values::PactValue;
use pact_core::shared::SpanInfo;

/// Continuation stack - matches Haskell Cont exactly
///
/// ```haskell
/// data Cont e b i
///   = Mt
///   | Args !(CEKEnv e b i) i ![EvalTerm b i] !(Cont e b i)
///   | Fn !(CanApply e b i) !(CEKEnv e b i) ![EvalTerm b i] ![CEKValue e b i] !(Cont e b i)
///   | LetC !(CEKEnv e b i) i (Arg Type i) !(EvalTerm b i) !(Cont e b i)
///   | SeqC (CEKEnv e b i) i (EvalTerm b i) (Cont e b i)
///   | ListC (CEKEnv e b i) i [EvalTerm b i] [PactValue] (Cont e b i)
///   | CondC (CEKEnv e b i) i (CondCont e b i) (Cont e b i)
///   | BuiltinC (CEKEnv e b i) i (BuiltinCont e b i) (Cont e b i)
///   | CapInvokeC (CEKEnv e b i) i (CapCont e b i) (Cont e b i)
///   | ... and 11 more variants
/// ```
#[derive(Debug, Clone)]
pub enum Cont {
    /// Empty continuation (terminal state)
    Mt,

    /// Function argument evaluation
    /// After evaluating function, evaluate arguments
    Args {
        env: CEKEnv,
        info: SpanInfo,
        args: Vec<CoreTerm>,
        cont: Box<Cont>,
    },

    /// Function application with accumulated arguments
    /// Apply function when all arguments are evaluated
    Fn {
        function: CanApply,
        env: CEKEnv,
        args: Vec<CoreTerm>,
        values: Vec<CEKValue>,
        cont: Box<Cont>,
    },

    /// Let binding continuation
    /// Bind variable and continue with body
    LetC {
        env: CEKEnv,
        info: SpanInfo,
        arg: Arg<Type, SpanInfo>,
        body: CoreTerm,
        cont: Box<Cont>,
    },

    /// Sequence evaluation
    /// Discard value and evaluate next expression
    SeqC {
        env: CEKEnv,
        info: SpanInfo,
        expr: CoreTerm,
        cont: Box<Cont>,
    },

    /// List literal construction
    /// Accumulate evaluated values into list
    ListC {
        env: CEKEnv,
        info: SpanInfo,
        exprs: Vec<CoreTerm>,
        values: Vec<PactValue>,
        cont: Box<Cont>,
    },

    /// Conditional evaluation (if/and/or/enforce)
    /// Handle control flow based on condition result
    CondC {
        env: CEKEnv,
        info: SpanInfo,
        cond_cont: CondCont,
        cont: Box<Cont>,
    },

    /// Higher-order builtin function continuation
    /// For builtins like map, filter, fold that take functions
    BuiltinC {
        env: CEKEnv,
        info: SpanInfo,
        builtin_cont: BuiltinCont,
        cont: Box<Cont>,
    },

    /// Object literal construction
    /// Accumulate evaluated fields into object
    ObjC {
        env: CEKEnv,
        info: SpanInfo,
        field: Field,
        remaining_fields: Vec<(Field, CoreTerm)>,
        evaluated_fields: Vec<(Field, PactValue)>,
        cont: Box<Cont>,
    },

    /// Capability invocation
    /// Handle capability acquisition and enforcement
    CapInvokeC {
        env: CEKEnv,
        info: SpanInfo,
        cap_cont: CapCont,
        cont: Box<Cont>,
    },

    /// Capability body execution
    /// Execute code within capability context
    CapBodyC {
        env: CEKEnv,
        info: SpanInfo,
        cap_body: CapBodyState,
        cont: Box<Cont>,
    },

    /// Map list continuation - evaluates list for map operation
    MapListC {
        env: CEKEnv,
        info: SpanInfo,
        func: CanApply,
        cont: Box<Cont>,
    },

    /// Filter list continuation - evaluates list for filter operation
    FilterListC {
        env: CEKEnv,
        info: SpanInfo,
        func: CanApply,
        cont: Box<Cont>,
    },

    /// Fold init continuation - evaluates initial value for fold
    FoldInitC {
        env: CEKEnv,
        info: SpanInfo,
        func: CanApply,
        list_expr: CoreTerm,
        cont: Box<Cont>,
    },

    /// Fold list continuation - evaluates list for fold operation
    FoldListC {
        env: CEKEnv,
        info: SpanInfo,
        func: CanApply,
        init_val: CEKValue,
        cont: Box<Cont>,
    },

    /// Zip list1 continuation - evaluates first list for zip
    ZipList1C {
        env: CEKEnv,
        info: SpanInfo,
        func: CanApply,
        list2_expr: CoreTerm,
        cont: Box<Cont>,
    },

    /// Zip list2 continuation - evaluates second list for zip
    ZipList2C {
        env: CEKEnv,
        info: SpanInfo,
        func: CanApply,
        list1: Vec<PactValue>,
        cont: Box<Cont>,
    },

    /// Capability pop from stack
    /// Remove capability from active stack
    CapPopC {
        pop_state: CapPopState,
        info: SpanInfo,
        cont: Box<Cont>,
    },

    /// DefPact step execution
    /// Execute DefPact step within transaction
    DefPactStepC {
        env: CEKEnv,
        info: SpanInfo,
        cont: Box<Cont>,
    },

    /// Nested DefPact step
    /// Handle DefPact steps within other DefPacts
    NestedDefPactStepC {
        env: CEKEnv,
        info: SpanInfo,
        cont: Box<Cont>,
        exec: DefPactExec,
    },

    /// Ignore value continuation
    /// Continue with predetermined value
    IgnoreValueC {
        value: PactValue,
        cont: Box<Cont>,
    },

    /// Enforce boolean type
    /// Ensure value is boolean for conditionals
    EnforceBoolC {
        info: SpanInfo,
        cont: Box<Cont>,
    },

    /// Enforce PactValue type
    /// Ensure value is proper PactValue
    EnforcePactValueC {
        info: SpanInfo,
        cont: Box<Cont>,
    },

    /// Module admin continuation
    /// Handle module administrative operations
    ModuleAdminC {
        module: ModuleName,
        cont: Box<Cont>,
    },

    /// Stack frame pop
    /// Remove frame from call stack for debugging
    StackPopC {
        info: SpanInfo,
        ty: Option<Type>,
        cont: Box<Cont>,
    },

    /// Enforce error continuation
    /// Handle enforce statement failures
    EnforceErrorC {
        info: SpanInfo,
        cont: Box<Cont>,
    },
}

/// Conditional continuation types
#[derive(Debug, Clone)]
pub enum CondCont {
    /// If-then-else conditional
    IfCont {
        then_expr: CoreTerm,
        else_expr: Option<CoreTerm>,
    },

    /// Logical AND operation
    AndCont {
        right_expr: CoreTerm,
    },

    /// Logical OR operation
    OrCont {
        right_expr: CoreTerm,
    },

    /// Enforce statement with message
    EnforceCont {
        message: CoreTerm,
    },

    /// Enforce-one statement (try multiple conditions)
    EnforceOneCont {
        remaining: Vec<CoreTerm>,
        tried: Vec<CoreTerm>,
    },

    /// NOT query operation (not?)
    /// Negates the boolean result from closure application
    NotQC,

    /// AND query operation (and?)
    /// If left closure returns true, apply right closure; else short-circuit
    AndQC {
        right_closure: CanApply,
        value: PactValue,
    },

    /// OR query operation (or?)
    /// If left closure returns false, apply right closure; else short-circuit
    OrQC {
        right_closure: CanApply,
        value: PactValue,
    },
}

/// Builtin continuation for higher-order functions
#[derive(Debug, Clone)]
pub enum BuiltinCont {
    /// Map operation continuation
    MapCont {
        func: CanApply,
        remaining: Vec<PactValue>,
        accumulated: Vec<PactValue>,
    },

    /// Filter operation continuation
    FilterCont {
        func: CanApply,
        remaining: Vec<PactValue>,
        accumulated: Vec<PactValue>,
    },

    /// Fold operation continuation
    FoldCont {
        func: CanApply,
        remaining: Vec<PactValue>,
        accumulator: PactValue,
    },

    /// Zip operation continuation
    ZipCont {
        func: CanApply,
        list2: Vec<PactValue>,
        remaining1: Vec<PactValue>,
        remaining2: Vec<PactValue>,
        accumulated: Vec<PactValue>,
    },

    /// Select operation continuation (for database queries)
    SelectCont {
        table: String,
        filter_func: Option<CanApply>,
        remaining: Vec<(String, PactValue)>,
        accumulated: Vec<PactValue>,
    },

    /// Create user guard continuation
    CreateUserGuardCont {
        args: Vec<CoreTerm>,
        evaluated_args: Vec<PactValue>,
    },

    /// Map builtin form continuation - evaluates list after function
    MapBuiltinC {
        list_expr: CoreTerm,
    },

    /// Filter builtin form continuation - evaluates list after function
    FilterBuiltinC {
        list_expr: CoreTerm,
    },

    /// Fold builtin form continuation - evaluates init and list after function
    FoldBuiltinC {
        init_expr: CoreTerm,
        list_expr: CoreTerm,
    },

    /// Zip builtin form continuation - evaluates both lists after function
    ZipBuiltinC {
        list1_expr: CoreTerm,
        list2_expr: CoreTerm,
    },

    /// Cond form continuation - evaluates conditions sequentially
    CondC {
        expr: CoreTerm,
        remaining_conds: Vec<(CoreTerm, CoreTerm)>,
    },
}

/// Capability continuation
#[derive(Debug, Clone)]
pub struct CapCont {
    /// Capability term to evaluate
    pub cap_term: CoreTerm,
    /// Body state containing forms to execute after capability
    pub body_state: CapBodyState,
}

/// Capability body execution state
#[derive(Debug, Clone)]
pub struct CapBodyState {
    /// Body forms to execute
    pub body_forms: Vec<CoreTerm>,
}

/// Capability pop state
#[derive(Debug, Clone)]
pub struct CapPopState {
    /// Capability to remove from stack
    pub cap_name: String,
    /// Capability arguments
    pub cap_args: Vec<PactValue>,
}

/// DefPact execution state
#[derive(Debug, Clone)]
pub struct DefPactExec {
    /// DefPact ID
    pub pact_id: String,
    /// Current step number
    pub step: u32,
    /// Total number of steps
    pub total_steps: u32,
    /// Step continuation
    pub step_cont: Option<CoreTerm>,
    /// Rollback information
    pub rollback: Option<DefPactRollback>,
}

/// DefPact rollback information
#[derive(Debug, Clone)]
pub struct DefPactRollback {
    /// Steps to rollback
    pub steps: Vec<CoreTerm>,
    /// Current rollback step
    pub current: u32,
}

impl Cont {
    /// Check if this is the empty continuation
    pub fn is_mt(&self) -> bool {
        matches!(self, Cont::Mt)
    }

    /// Create empty continuation
    pub fn mt() -> Self {
        Cont::Mt
    }

    /// Create argument evaluation continuation
    pub fn args(env: CEKEnv, info: SpanInfo, args: Vec<CoreTerm>, cont: Cont) -> Self {
        Cont::Args {
            env,
            info,
            args,
            cont: Box::new(cont),
        }
    }

    /// Create function application continuation
    pub fn fn_cont(
        function: CanApply,
        env: CEKEnv,
        args: Vec<CoreTerm>,
        values: Vec<CEKValue>,
        cont: Cont,
    ) -> Self {
        Cont::Fn {
            function,
            env,
            args,
            values,
            cont: Box::new(cont),
        }
    }

    /// Create let binding continuation
    pub fn let_cont(
        env: CEKEnv,
        info: SpanInfo,
        arg: Arg<Type, SpanInfo>,
        body: CoreTerm,
        cont: Cont,
    ) -> Self {
        Cont::LetC {
            env,
            info,
            arg,
            body,
            cont: Box::new(cont),
        }
    }

    /// Create sequence continuation
    pub fn seq_cont(env: CEKEnv, info: SpanInfo, expr: CoreTerm, cont: Cont) -> Self {
        Cont::SeqC {
            env,
            info,
            expr,
            cont: Box::new(cont),
        }
    }

    /// Create list construction continuation
    pub fn list_cont(
        env: CEKEnv,
        info: SpanInfo,
        exprs: Vec<CoreTerm>,
        values: Vec<PactValue>,
        cont: Cont,
    ) -> Self {
        Cont::ListC {
            env,
            info,
            exprs,
            values,
            cont: Box::new(cont),
        }
    }

    /// Create conditional continuation
    pub fn cond_cont(
        env: CEKEnv,
        info: SpanInfo,
        cond_cont: CondCont,
        cont: Cont,
    ) -> Self {
        Cont::CondC {
            env,
            info,
            cond_cont,
            cont: Box::new(cont),
        }
    }

    /// Create builtin continuation
    pub fn builtin_cont(
        env: CEKEnv,
        info: SpanInfo,
        builtin_cont: BuiltinCont,
        cont: Cont,
    ) -> Self {
        Cont::BuiltinC {
            env,
            info,
            builtin_cont,
            cont: Box::new(cont),
        }
    }

    /// Create capability invocation continuation
    pub fn cap_invoke_cont(
        env: CEKEnv,
        info: SpanInfo,
        cap_cont: CapCont,
        cont: Cont,
    ) -> Self {
        Cont::CapInvokeC {
            env,
            info,
            cap_cont,
            cont: Box::new(cont),
        }
    }

    /// Get the next continuation (unwrap one level)
    pub fn next(&self) -> Option<&Cont> {
        match self {
            Cont::Mt => None,
            Cont::Args { cont, .. } => Some(cont),
            Cont::Fn { cont, .. } => Some(cont),
            Cont::LetC { cont, .. } => Some(cont),
            Cont::SeqC { cont, .. } => Some(cont),
            Cont::ListC { cont, .. } => Some(cont),
            Cont::CondC { cont, .. } => Some(cont),
            Cont::BuiltinC { cont, .. } => Some(cont),
            Cont::ObjC { cont, .. } => Some(cont),
            Cont::CapInvokeC { cont, .. } => Some(cont),
            Cont::CapBodyC { cont, .. } => Some(cont),
            Cont::CapPopC { cont, .. } => Some(cont),
            Cont::DefPactStepC { cont, .. } => Some(cont),
            Cont::NestedDefPactStepC { cont, .. } => Some(cont),
            Cont::IgnoreValueC { cont, .. } => Some(cont),
            Cont::EnforceBoolC { cont, .. } => Some(cont),
            Cont::EnforcePactValueC { cont, .. } => Some(cont),
            Cont::ModuleAdminC { cont, .. } => Some(cont),
            Cont::StackPopC { cont, .. } => Some(cont),
            Cont::EnforceErrorC { cont, .. } => Some(cont),
            Cont::MapListC { cont, .. } => Some(cont),
            Cont::FilterListC { cont, .. } => Some(cont),
            Cont::FoldInitC { cont, .. } => Some(cont),
            Cont::FoldListC { cont, .. } => Some(cont),
            Cont::ZipList1C { cont, .. } => Some(cont),
            Cont::ZipList2C { cont, .. } => Some(cont),
        }
    }

    /// Get source information from continuation
    pub fn source_info(&self) -> Option<&SpanInfo> {
        match self {
            Cont::Mt => None,
            Cont::Args { info, .. } => Some(info),
            Cont::Fn { .. } => None, // Function application doesn't have direct info
            Cont::LetC { info, .. } => Some(info),
            Cont::SeqC { info, .. } => Some(info),
            Cont::ListC { info, .. } => Some(info),
            Cont::CondC { info, .. } => Some(info),
            Cont::BuiltinC { info, .. } => Some(info),
            Cont::ObjC { info, .. } => Some(info),
            Cont::CapInvokeC { info, .. } => Some(info),
            Cont::CapBodyC { info, .. } => Some(info),
            Cont::CapPopC { info, .. } => Some(info),
            Cont::DefPactStepC { info, .. } => Some(info),
            Cont::NestedDefPactStepC { info, .. } => Some(info),
            Cont::IgnoreValueC { .. } => None,
            Cont::EnforceBoolC { info, .. } => Some(info),
            Cont::EnforcePactValueC { info, .. } => Some(info),
            Cont::ModuleAdminC { .. } => None,
            Cont::StackPopC { info, .. } => Some(info),
            Cont::EnforceErrorC { info, .. } => Some(info),
            Cont::MapListC { info, .. } => Some(info),
            Cont::FilterListC { info, .. } => Some(info),
            Cont::FoldInitC { info, .. } => Some(info),
            Cont::FoldListC { info, .. } => Some(info),
            Cont::ZipList1C { info, .. } => Some(info),
            Cont::ZipList2C { info, .. } => Some(info),
        }
    }
}

/// Conditional continuation constructors
impl CondCont {
    /// Create if-then-else continuation
    pub fn if_cont(then_expr: CoreTerm, else_expr: Option<CoreTerm>) -> Self {
        CondCont::IfCont { then_expr, else_expr }
    }

    /// Create logical AND continuation
    pub fn and_cont(right_expr: CoreTerm) -> Self {
        CondCont::AndCont { right_expr }
    }

    /// Create logical OR continuation
    pub fn or_cont(right_expr: CoreTerm) -> Self {
        CondCont::OrCont { right_expr }
    }

    /// Create enforce continuation
    pub fn enforce_cont(message: CoreTerm) -> Self {
        CondCont::EnforceCont { message }
    }

    /// Create enforce-one continuation
    pub fn enforce_one_cont(remaining: Vec<CoreTerm>, tried: Vec<CoreTerm>) -> Self {
        CondCont::EnforceOneCont { remaining, tried }
    }
}

/// Builtin continuation constructors
impl BuiltinCont {
    /// Create map continuation
    pub fn map_cont(func: CanApply, remaining: Vec<PactValue>, accumulated: Vec<PactValue>) -> Self {
        BuiltinCont::MapCont { func, remaining, accumulated }
    }

    /// Create filter continuation
    pub fn filter_cont(func: CanApply, remaining: Vec<PactValue>, accumulated: Vec<PactValue>) -> Self {
        BuiltinCont::FilterCont { func, remaining, accumulated }
    }

    /// Create fold continuation
    pub fn fold_cont(func: CanApply, remaining: Vec<PactValue>, accumulator: PactValue) -> Self {
        BuiltinCont::FoldCont { func, remaining, accumulator }
    }

    /// Create user guard continuation
    pub fn create_user_guard_cont(_env: CEKEnv, _info: SpanInfo, args: Vec<CoreTerm>, _cont: Cont) -> Self {
        BuiltinCont::CreateUserGuardCont { 
            args, 
            evaluated_args: vec![] 
        }
    }
}

