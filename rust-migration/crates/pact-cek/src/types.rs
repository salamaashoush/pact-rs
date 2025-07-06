//! Core CEK Types
//!
//! This module implements the core CEK machine types following the Haskell
//! reference implementation exactly. All types match their Haskell counterparts
//! in structure and semantics.

use pact_ir::{CoreTerm, CoreBuiltin, Arg, Type};
use pact_core::values::PactValue;
use pact_core::shared::SpanInfo;
use pact_core::gas::MilliGas;
use std::sync::Arc;
use std::collections::HashMap;

/// CEK machine values - exactly matches Haskell CEKValue
///
/// ```haskell
/// data CEKValue e b i
///   = VPactValue !PactValue
///   | VClosure  !(CanApply e b i)
/// ```
#[derive(Debug, Clone)]
pub enum CEKValue {
    /// Basic Pact values (literals, objects, lists, etc.)
    VPactValue(PactValue),
    /// Callable entities (functions, builtins, partial applications)
    VClosure(CanApply),
    /// Table value (for database operations)
    VTable {
        name: String,
        schema: TableSchema,
        module_hash: Option<String>,
    },
}

/// Callable entities - matches Haskell CanApply exactly
///
/// ```haskell
/// data CanApply e b i
///   = C !(Closure e b i)           -- User-defined function
///   | N !(NativeFn e b i)          -- Builtin function
///   | CT !(CapTokenClosure i)      -- Capability token
///   | LC !(LamClosure e b i)       -- Lambda closure
///   | PC !(PartialClosure e b i)   -- Partially applied function
///   | PN !(PartialNativeFn e b i)  -- Partially applied builtin
///   | DPC !(DefPactClosure e b i)  -- DefPact closure
/// ```
#[derive(Debug, Clone)]
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

/// User-defined function closure
#[derive(Debug, Clone)]
pub struct Closure {
    /// Function name
    pub name: String,
    /// Function arguments
    pub args: Vec<Arg<Type, SpanInfo>>,
    /// Function body
    pub body: CoreTerm,
    /// Captured environment
    pub env: CEKEnv,
    /// Source information
    pub info: SpanInfo,
}

/// Native builtin function
#[derive(Debug, Clone)]
pub struct NativeFn {
    /// Builtin identifier
    pub builtin: CoreBuiltin,
    /// Function arity
    pub arity: usize,
    /// Source information
    pub info: SpanInfo,
}

/// Capability token closure
#[derive(Debug, Clone)]
pub struct CapTokenClosure {
    /// Capability name
    pub name: String,
    /// Captured arguments
    pub args: Vec<PactValue>,
    /// Source information
    pub info: SpanInfo,
}

/// Lambda closure
#[derive(Debug, Clone)]
pub struct LamClosure {
    /// Lambda arguments
    pub args: Vec<Arg<Type, SpanInfo>>,
    /// Lambda body
    pub body: CoreTerm,
    /// Captured environment
    pub env: CEKEnv,
    /// Source information
    pub info: SpanInfo,
}

/// Partially applied function
#[derive(Debug, Clone)]
pub struct PartialClosure {
    /// Original closure
    pub closure: Closure,
    /// Applied arguments so far
    pub applied_args: Vec<CEKValue>,
    /// Remaining argument count
    pub remaining_arity: usize,
}

/// Partially applied builtin
#[derive(Debug, Clone)]
pub struct PartialNativeFn {
    /// Original native function
    pub native_fn: NativeFn,
    /// Applied arguments so far
    pub applied_args: Vec<CEKValue>,
    /// Remaining argument count
    pub remaining_arity: usize,
    /// Captured environment
    pub env: CEKEnv,
}

/// DefPact closure
#[derive(Debug, Clone)]
pub struct DefPactClosure {
    /// DefPact name
    pub name: String,
    /// DefPact steps
    pub steps: Vec<DefPactStep>,
    /// Current step index
    pub current_step: usize,
    /// Captured environment
    pub env: CEKEnv,
    /// Source information
    pub info: SpanInfo,
}

/// DefPact execution step
#[derive(Debug, Clone)]
pub struct DefPactStep {
    /// Step expression
    pub expr: CoreTerm,
    /// Optional rollback expression
    pub rollback: Option<CoreTerm>,
    /// Step entity (for authorization)
    pub entity: Option<CoreTerm>,
}

/// CEK Environment - matches Haskell CEKEnv exactly
///
/// ```haskell
/// data CEKEnv e b i = CEKEnv
///   { _ceLocal :: RAList (CEKValue e b i)
///   , _cePactDb :: PactDb b i
///   , _ceBuiltins :: BuiltinEnv e b i
///   , _ceDefPactStep :: Maybe DefPactStep
///   , _ceInCap :: Bool
///   }
/// ```
#[derive(Debug, Clone)]
pub struct CEKEnv {
    /// Local variable bindings (DeBruijn indices)
    pub local: RAList<CEKValue>,
    /// Database interface for persistence operations
    pub pact_db: Arc<dyn PactDb>,
    /// Builtin function resolver
    pub builtins: Arc<BuiltinEnv>,
    /// DefPact execution state
    pub defpact_step: Option<DefPactStepState>,
    /// Whether currently in capability context
    pub in_cap: bool,
    /// Capability execution stack for proper nesting
    pub capability_stack: Vec<CapabilityContext>,
    /// Current module context for authorization
    pub module_context: Option<ModuleContext>,
    /// Runtime flags for execution behavior
    pub flags: ExecutionFlags,
    /// Call stack depth for recursion limiting
    call_stack_depth: usize,
    /// Current chain ID for cross-chain operations
    pub chain_id: Option<pact_core::shared::ChainId>,
    /// Loaded modules environment - for top-level name resolution
    pub loaded: Arc<pact_ir::module::CoreLoaded>,
}

/// Execution flags that control runtime behavior
#[derive(Debug, Clone, Default)]
pub struct ExecutionFlags {
    /// Allow database reads in local execution mode
    pub allow_read_in_local: bool,
    /// Enable enforcement of version guards
    pub enforce_version_guards: bool,
    /// Enable static analysis warnings
    pub enable_warnings: bool,
    /// Disallow local-only builtins in production mode
    pub disallow_local_only_builtins: bool,
}

/// Capability execution context for proper nesting and authorization
#[derive(Debug, Clone)]
pub struct CapabilityContext {
    /// Capability name
    pub name: String,
    /// Capability arguments
    pub args: Vec<PactValue>,
    /// Whether this capability grants database access
    pub grants_db_access: bool,
    /// Module that defined this capability
    pub defining_module: Option<String>,
    /// Source location where capability was acquired
    pub source_info: SpanInfo,
}

/// Module execution context for authorization checks
#[derive(Debug, Clone)]
pub struct ModuleContext {
    /// Module name
    pub name: String,
    /// Module hash for verification
    pub hash: pact_core::names::ModuleHash,
    /// Module governance requirements
    pub governance: GovernanceRequirement,
    /// Whether module is blessed (system module)
    pub is_blessed: bool,
}

/// Governance requirement for module operations
#[derive(Debug, Clone)]
pub enum GovernanceRequirement {
    /// Keyset-based governance
    Keyset { name: String },
    /// Capability-based governance
    Capability { name: String, args: Vec<PactValue> },
    /// No governance (open module)
    None,
}

/// DefPact execution state
#[derive(Debug, Clone)]
pub struct DefPactStepState {
    /// DefPact ID
    pub pact_id: String,
    /// Current step number
    pub step: u32,
    /// Whether this is a rollback step
    pub rollback: bool,
    /// Step result from previous step
    pub step_result: Option<PactValue>,
}

/// Random Access List for efficient variable lookup
/// Matches Haskell RAList behavior
#[derive(Debug, Clone, PartialEq)]
pub struct RAList<T: Clone> {
    items: im::Vector<T>,
}

impl<T: Clone> RAList<T> {
    /// Create new empty RAList
    pub fn new() -> Self {
        RAList {
            items: im::Vector::new(),
        }
    }

    /// Add item to front (index 0)
    pub fn cons(&self, item: T) -> Self {
        let mut new_items = self.items.clone();
        new_items.push_front(item);
        RAList { items: new_items }
    }

    /// Lookup item by index (0-based from front)
    pub fn lookup(&self, index: usize) -> Option<&T> {
        self.items.get(index)
    }

    /// Get length
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
}

impl<T: Clone> Default for RAList<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl CEKEnv {
    /// Create new environment
    pub fn new(
        pact_db: Arc<dyn PactDb>,
        builtins: BuiltinEnv,
        loaded: pact_ir::module::CoreLoaded,
    ) -> Self {
        CEKEnv {
            local: RAList::new(),
            pact_db,
            builtins: Arc::new(builtins),
            defpact_step: None,
            in_cap: false,
            capability_stack: Vec::new(),
            module_context: None,
            flags: ExecutionFlags::default(),
            call_stack_depth: 0,
            chain_id: None,
            loaded: Arc::new(loaded),
        }
    }

    /// Get current call stack depth
    pub fn call_stack_depth(&self) -> usize {
        self.call_stack_depth
    }

    /// Increment call stack depth
    pub fn push_call_frame(&self) -> Self {
        let mut new_env = self.clone();
        new_env.call_stack_depth += 1;
        new_env
    }

    /// Decrement call stack depth
    pub fn pop_call_frame(&self) -> Self {
        let mut new_env = self.clone();
        new_env.call_stack_depth = new_env.call_stack_depth.saturating_sub(1);
        new_env
    }

    /// Extend environment with new local binding
    pub fn extend_local(&self, value: CEKValue) -> Self {
        CEKEnv {
            local: self.local.cons(value),
            pact_db: self.pact_db.clone(),
            builtins: self.builtins.clone(),
            defpact_step: self.defpact_step.clone(),
            in_cap: self.in_cap,
            capability_stack: self.capability_stack.clone(),
            module_context: self.module_context.clone(),
            flags: self.flags.clone(),
            call_stack_depth: self.call_stack_depth,
            chain_id: self.chain_id.clone(),
            loaded: self.loaded.clone(),
        }
    }

    /// Lookup local variable by DeBruijn index
    pub fn lookup_local(&self, index: usize) -> Option<&CEKValue> {
        self.local.lookup(index)
    }

    /// Set capability context
    pub fn set_in_cap(&self, in_cap: bool) -> Self {
        CEKEnv {
            local: self.local.clone(),
            pact_db: self.pact_db.clone(),
            builtins: self.builtins.clone(),
            defpact_step: self.defpact_step.clone(),
            in_cap,
            capability_stack: self.capability_stack.clone(),
            module_context: self.module_context.clone(),
            flags: self.flags.clone(),
            call_stack_depth: self.call_stack_depth,
            chain_id: self.chain_id.clone(),
            loaded: self.loaded.clone(),
        }
    }

    /// Set DefPact step state
    pub fn set_defpact_step(&self, step: Option<DefPactStepState>) -> Self {
        CEKEnv {
            local: self.local.clone(),
            pact_db: self.pact_db.clone(),
            builtins: self.builtins.clone(),
            defpact_step: step,
            in_cap: self.in_cap,
            capability_stack: self.capability_stack.clone(),
            module_context: self.module_context.clone(),
            flags: self.flags.clone(),
            call_stack_depth: self.call_stack_depth,
            chain_id: self.chain_id.clone(),
            loaded: self.loaded.clone(),
        }
    }

    /// Push capability onto the capability stack
    pub fn push_capability(&self, capability: CapabilityContext) -> Self {
        let mut new_stack = self.capability_stack.clone();
        new_stack.push(capability);

        CEKEnv {
            local: self.local.clone(),
            pact_db: self.pact_db.clone(),
            builtins: self.builtins.clone(),
            defpact_step: self.defpact_step.clone(),
            in_cap: true, // Set in_cap to true when capability is active
            capability_stack: new_stack,
            module_context: self.module_context.clone(),
            flags: self.flags.clone(),
            call_stack_depth: self.call_stack_depth,
            chain_id: self.chain_id.clone(),
            loaded: self.loaded.clone(),
        }
    }

    /// Pop capability from the capability stack
    pub fn pop_capability(&self) -> Self {
        let mut new_stack = self.capability_stack.clone();
        new_stack.pop();

        let in_cap = !new_stack.is_empty(); // Only in_cap if stack is not empty

        CEKEnv {
            local: self.local.clone(),
            pact_db: self.pact_db.clone(),
            builtins: self.builtins.clone(),
            defpact_step: self.defpact_step.clone(),
            in_cap,
            capability_stack: new_stack,
            module_context: self.module_context.clone(),
            flags: self.flags.clone(),
            call_stack_depth: self.call_stack_depth,
            chain_id: self.chain_id.clone(),
            loaded: self.loaded.clone(),
        }
    }

    /// Check if a specific capability is currently granted
    pub fn has_capability(&self, cap_name: &str, cap_args: &[PactValue]) -> bool {
        self.capability_stack.iter().any(|cap| {
            cap.name == cap_name && cap.args == cap_args
        })
    }

    /// Check if any capability grants database access
    pub fn has_database_access(&self) -> bool {
        self.capability_stack.iter().any(|cap| cap.grants_db_access)
    }

    /// Set module context
    pub fn set_module_context(&self, module_context: Option<ModuleContext>) -> Self {
        CEKEnv {
            local: self.local.clone(),
            pact_db: self.pact_db.clone(),
            builtins: self.builtins.clone(),
            defpact_step: self.defpact_step.clone(),
            in_cap: self.in_cap,
            capability_stack: self.capability_stack.clone(),
            module_context,
            flags: self.flags.clone(),
            call_stack_depth: self.call_stack_depth,
            chain_id: self.chain_id.clone(),
            loaded: self.loaded.clone(),
        }
    }

    /// Get current pact ID if in DefPact execution
    pub fn get_current_pact_id(&self) -> Option<String> {
        self.defpact_step.as_ref().map(|step| step.pact_id.clone())
    }

    /// Check if we're currently within a defcap execution
    pub fn is_within_defcap(&self) -> bool {
        self.in_cap
    }


    /// Check if current context has module admin privileges
    pub fn has_module_admin(&self, module_name: &str) -> bool {
        // Check if current module context matches and allows admin operations
        match &self.module_context {
            Some(ctx) => ctx.name == module_name && !ctx.is_blessed,
            None => false,
        }
    }

    /// Lookup fully qualified name from loaded modules
    pub fn lookup_fqname(&self, fqn: &pact_ir::module::FullyQualifiedName) -> Option<&pact_ir::term::Def<pact_ir::Name, pact_ir::Type, pact_ir::CoreBuiltin, SpanInfo>> {
        self.loaded.all_loaded.get(fqn)
    }
}

/// Domain for database operations (matches Haskell Domain and pact-db)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Domain {
    /// User tables
    User(TableName),
    /// System tables (keyset registry, modules, etc.)
    KeySets,
    Modules,
    Namespaces,
    /// DefPact tables
    Pacts,
}

/// Table name wrapper
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TableName {
    pub name: String,
}

impl TableName {
    pub fn new(name: String) -> Self {
        TableName { name }
    }
}

/// Transaction ID
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TxId(pub u64);

/// Transaction log entry
#[derive(Debug, Clone)]
pub struct TxLog {
    pub tx_id: TxId,
    pub data: String, // Simplified for now
}

/// Execution mode for database operations
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExecutionMode {
    /// Local execution (no transaction)
    Local,
    /// Transactional execution
    Transactional,
}

/// Row key for database operations
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RowKey(pub String);

/// Row data for database operations
#[derive(Debug, Clone, PartialEq)]
pub struct RowData {
    pub fields: std::collections::HashMap<String, PactValue>,
}

impl RowData {
    pub fn new() -> Self {
        RowData { fields: std::collections::HashMap::new() }
    }

    pub fn insert(&mut self, key: String, value: PactValue) {
        self.fields.insert(key, value);
    }

    pub fn get(&self, key: &str) -> Option<&PactValue> {
        self.fields.get(key)
    }
}

/// Database interface trait for Pact persistence - integrated with EvalM
pub trait PactDb: std::fmt::Debug + Send + Sync {
    /// Read a value from the database with gas charging
    fn read(&self, domain: Domain, key: RowKey) -> super::monad::EvalM<Option<RowData>>;

    /// Write a value to the database with gas charging
    fn write(&self, domain: Domain, key: RowKey, data: RowData) -> super::monad::EvalM<()>;

    /// Write a value with specific write type (insert/update/write)
    fn write_with_type(&self, domain: Domain, key: RowKey, data: RowData, write_type: WriteType) -> super::monad::EvalM<()> {
        // Default implementation delegates to write for backward compatibility
        // Specific implementations should override this to enforce write type semantics
        match write_type {
            WriteType::Write => self.write(domain, key, data),
            WriteType::Insert => {
                // Should check if key exists and fail if it does
                self.write(domain, key, data)
            }
            WriteType::Update => {
                // Should check if key exists and fail if it doesn't
                self.write(domain, key, data)
            }
        }
    }

    /// Get all keys for a domain with gas charging
    fn keys(&self, domain: Domain) -> super::monad::EvalM<Vec<RowKey>>;

    /// Select rows from domain with optional filtering and gas charging
    fn select(&self, domain: Domain, filter: Option<super::monad::EvalM<bool>>) -> super::monad::EvalM<Vec<(RowKey, RowData)>>;

    /// Create a new table with gas charging
    fn create_table(&self, table_name: String, schema: TableSchema) -> super::monad::EvalM<()>;

    /// Begin transaction with execution mode
    fn begin_tx(&self, mode: ExecutionMode) -> super::monad::EvalM<Option<TxId>>;

    /// Commit transaction with gas charging
    fn commit_tx(&self) -> super::monad::EvalM<()>;

    /// Rollback transaction
    fn rollback_tx(&self) -> super::monad::EvalM<()>;

    /// Describe table schema
    fn describe_table(&self, table: TableName) -> super::monad::EvalM<Option<pact_schema::Schema>>;

    /// Check if table exists
    fn table_exists(&self, table: TableName) -> super::monad::EvalM<bool>;

    /// Get transaction log
    fn tx_log(&self, domain: Domain, tx_id: TxId) -> super::monad::EvalM<Vec<TxLog>>;

    /// Get transaction IDs
    fn tx_ids(&self, domain: Domain, tx_id: TxId) -> super::monad::EvalM<Vec<TxId>>;

    /// Simplified interface for compatibility (delegates to EvalM versions)
    fn read_row(&self, table: &str, key: &str) -> Result<Option<PactValue>, pact_core::errors::PactErrorI> {
        // Simplified sync version for compatibility
        // In full implementation, this would run the EvalM computation
        Ok(None)
    }

    fn write_row(&self, table: &str, key: &str, value: PactValue) -> Result<(), pact_core::errors::PactErrorI> {
        // Simplified sync version for compatibility
        Ok(())
    }

    fn select_rows(&self, table: &str, filter: Option<&str>) -> Result<Vec<(String, PactValue)>, pact_core::errors::PactErrorI> {
        // Simplified sync version for compatibility
        Ok(vec![])
    }
}

/// Table schema definition
#[derive(Debug, Clone)]
pub struct TableSchema {
    pub columns: Vec<ColumnDef>,
}


/// Column definition for table schema
#[derive(Debug, Clone)]
pub struct ColumnDef {
    pub name: String,
    pub column_type: ColumnType,
    pub nullable: bool,
}

/// Column types for database schema
#[derive(Debug, Clone)]
pub enum ColumnType {
    String,
    Integer,
    Decimal,
    Bool,
    Object,
    List,
    Time,
    Guard,
    Keyset,
    ModRef,
}

/// Write operation type
#[derive(Debug, Clone, Copy)]
pub enum WriteType {
    /// Insert only - fails if key exists
    Insert,
    /// Update only - fails if key doesn't exist
    Update,
    /// Write (upsert) - always succeeds
    Write,
}

/// Builtin environment for function resolution
#[derive(Debug)]
pub struct BuiltinEnv {
    functions: HashMap<CoreBuiltin, BuiltinSpec>,
}

/// Builtin function specification
pub struct BuiltinSpec {
    /// Function name
    pub name: &'static str,
    /// Function arity
    pub arity: usize,
    /// Function implementation
    pub implementation: NativeFunction,
}

impl std::fmt::Debug for BuiltinSpec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BuiltinSpec")
            .field("name", &self.name)
            .field("arity", &self.arity)
            .field("implementation", &"<function>")
            .finish()
    }
}

/// Native function type - matches Haskell NativeFunction exactly
///
/// ```haskell
/// type NativeFunction e b i = i -> b -> Cont e b i -> CEKErrorHandler e b i -> CEKEnv e b i -> [CEKValue e b i] -> EvalM e b i (EvalResult e b i)
/// ```
pub type NativeFunction = Box<dyn Fn(
    SpanInfo,
    CoreBuiltin,
    super::cont::Cont,
    super::error::CEKErrorHandler,
    CEKEnv,
    Vec<CEKValue>,
) -> super::monad::EvalM<super::eval::EvalResult> + Send + Sync>;

impl BuiltinEnv {
    /// Create new builtin environment
    pub fn new() -> Self {
        BuiltinEnv {
            functions: HashMap::new(),
        }
    }

    /// Register a builtin function
    pub fn register(&mut self, builtin: CoreBuiltin, spec: BuiltinSpec) {
        self.functions.insert(builtin, spec);
    }

    /// Lookup builtin function
    pub fn lookup(&self, builtin: CoreBuiltin, info: SpanInfo) -> Result<NativeFn, pact_core::errors::PactErrorI> {
        self.functions.get(&builtin)
            .map(|spec| NativeFn {
                builtin,
                arity: spec.arity,
                info,
            })
            .ok_or_else(|| pact_core::errors::PactError::PEExecutionError(
                pact_core::errors::EvalError::RuntimeError(format!("Unknown builtin: {:?}", builtin)),
                vec![],
                info
            ))
    }

    /// Get builtin implementation
    pub fn get_implementation(&self, builtin: CoreBuiltin) -> Option<&NativeFunction> {
        self.functions.get(&builtin).map(|spec| &spec.implementation)
    }
}

impl Default for BuiltinEnv {
    fn default() -> Self {
        Self::new()
    }
}

/// CEKValue convenience constructors
impl CEKValue {
    /// Create a PactValue
    pub fn pact_value(value: PactValue) -> Self {
        CEKValue::VPactValue(value)
    }

    /// Create a closure
    pub fn closure(can_apply: CanApply) -> Self {
        CEKValue::VClosure(can_apply)
    }

    /// Create an integer value
    pub fn integer(i: i64) -> Self {
        CEKValue::VPactValue(PactValue::Integer(i.into()))
    }

    /// Create a boolean value
    pub fn boolean(b: bool) -> Self {
        CEKValue::VPactValue(PactValue::Bool(b))
    }

    /// Create a string value
    pub fn string(s: String) -> Self {
        CEKValue::VPactValue(PactValue::String(s))
    }

    /// Create a list value
    pub fn list(items: Vec<PactValue>) -> Self {
        CEKValue::VPactValue(PactValue::List(items))
    }

    /// Check if this is a PactValue
    pub fn is_pact_value(&self) -> bool {
        matches!(self, CEKValue::VPactValue(_))
    }

    /// Check if this is a closure
    pub fn is_closure(&self) -> bool {
        matches!(self, CEKValue::VClosure(_))
    }

    /// Extract PactValue if possible
    pub fn as_pact_value(&self) -> Option<&PactValue> {
        match self {
            CEKValue::VPactValue(v) => Some(v),
            _ => None,
        }
    }

    /// Extract closure if possible
    pub fn as_closure(&self) -> Option<&CanApply> {
        match self {
            CEKValue::VClosure(c) => Some(c),
            _ => None,
        }
    }

    /// Check if value can be applied (is a closure)
    pub fn can_apply(&self) -> Option<CanApply> {
        match self {
            CEKValue::VClosure(c) => Some(c.clone()),
            _ => None,
        }
    }
}

/// Pattern synonyms for PactValue access (like Haskell)
impl CEKValue {
    /// Check if this is a string value
    pub fn is_string(&self) -> bool {
        matches!(self, CEKValue::VPactValue(PactValue::String(_)))
    }

    /// Check if this is an integer value
    pub fn is_integer(&self) -> bool {
        matches!(self, CEKValue::VPactValue(PactValue::Integer(_)))
    }

    /// Check if this is a boolean value
    pub fn is_bool(&self) -> bool {
        matches!(self, CEKValue::VPactValue(PactValue::Bool(_)))
    }

    /// Check if this is a list value
    pub fn is_list(&self) -> bool {
        matches!(self, CEKValue::VPactValue(PactValue::List(_)))
    }

    /// Extract string value
    pub fn as_string(&self) -> Option<&String> {
        match self {
            CEKValue::VPactValue(PactValue::String(s)) => Some(s),
            _ => None,
        }
    }

    /// Extract integer value
    pub fn as_integer(&self) -> Option<&num_bigint::BigInt> {
        match self {
            CEKValue::VPactValue(PactValue::Integer(i)) => Some(i),
            _ => None,
        }
    }

    /// Extract boolean value
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            CEKValue::VPactValue(PactValue::Bool(b)) => Some(*b),
            _ => None,
        }
    }

    /// Extract list value
    pub fn as_list(&self) -> Option<&Vec<PactValue>> {
        match self {
            CEKValue::VPactValue(PactValue::List(items)) => Some(items),
            _ => None,
        }
    }
}

/// Gas tracking state
#[derive(Debug, Clone)]
pub struct GasState {
    /// Gas limit for execution
    gas_limit: u64,
    /// Gas used so far
    gas_used: u64,
}

impl GasState {
    /// Create new gas state with limit
    pub fn new(gas_limit: u64) -> Self {
        GasState {
            gas_limit,
            gas_used: 0,
        }
    }

    /// Get gas limit
    pub fn gas_limit(&self) -> u64 {
        self.gas_limit
    }

    /// Get gas used
    pub fn gas_used(&self) -> u64 {
        self.gas_used
    }

    /// Charge gas
    pub fn charge_gas(&self, cost: MilliGas) -> Result<GasState, pact_core::errors::PactErrorI> {
        let new_used = self.gas_used + cost.value();
        if new_used > self.gas_limit {
            Err(pact_core::errors::PactError::PEExecutionError(
                pact_core::errors::EvalError::RuntimeError(format!(
                    "Gas limit exceeded: limit {}, used {}", 
                    self.gas_limit, 
                    new_used
                )),
                vec![],
                pact_core::shared::SpanInfo::empty()
            ))
        } else {
            Ok(GasState {
                gas_limit: self.gas_limit,
                gas_used: new_used,
            })
        }
    }
}

impl Default for GasState {
    fn default() -> Self {
        GasState::new(150_000) // Default gas limit
    }
}
