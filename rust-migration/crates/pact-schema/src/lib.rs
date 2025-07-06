//! Type system and schema for Pact
//!
//! This crate provides the complete type system including inference,
//! row types, type checking, and type classes.

pub mod inference;

pub use inference::{ConstraintSolver, Substitution, TypeInferencer, TypeVarGen};

/// Type alias for type class predicates
pub type Pred = TypeClass;

use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::fmt;

// Re-export from pact-names
pub use pact_core::names::{ModuleName, QualifiedName};

/// Field name
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Field(pub String);

impl std::fmt::Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Primitive types in Pact (matching Haskell implementation)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PrimType {
    /// Integer type
    Integer,
    /// Decimal type (arbitrary precision)
    Decimal,
    /// String type
    String,
    /// Boolean type
    Bool,
    /// Time type (UTC timestamps)
    Time,
    /// Guard type (authorization guards)
    Guard,
    /// Keyset type (key sets for authorization)
    Keyset,
    /// Unit type (for side effects)
    Unit,
}

/// Kind system for Pact types
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PactKind {
    /// Regular types
    TyKind,
    /// Row types (for structural typing)
    RowKind,
    /// Module reference types
    ModRefKind,
}

/// Type variable for inference (generic over name type)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TypeVar<N> {
    pub var: N,
    pub kind: PactKind,
}

/// Row types for structural typing
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum RowTy<N>
where
    N: Clone + PartialEq + Eq + std::hash::Hash,
{
    /// Row variable (for inference)
    RowVar(N),
    /// Concrete row with field mappings
    RowConcrete(HashMap<Field, Type<N>>),
}

/// Module references
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum MRef<N>
where
    N: Clone + PartialEq + Eq + std::hash::Hash,
{
    /// Module reference variable
    MRefVar(N),
    /// Concrete set of module names
    MConcrete(HashSet<ModuleName>),
}

/// Rose row types for advanced row polymorphism
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum RoseRow<N>
where
    N: Clone + PartialEq + Eq + std::hash::Hash,
{
    /// Basic row type
    RoseRowTy(RowTy<N>),
    /// Row concatenation (for advanced row operations)
    RoseRowConcat(Box<RoseRow<N>>, Box<RoseRow<N>>),
}

/// Type constraints for constraint solving
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeConstraint<N>
where
    N: Clone + PartialEq + Eq + std::hash::Hash,
{
    /// Type equality constraint
    Equal(Type<N>, Type<N>),
    /// Subtype constraint
    Subtype(Type<N>, Type<N>),
    /// Object has field constraint
    HasField(Type<N>, Field, Type<N>),
    /// Type class constraint
    Class(TypeClass),
}

/// Type environment for name resolution and inference
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypeEnv<N>
where
    N: Clone + PartialEq + Eq + std::hash::Hash,
{
    /// Variable to type scheme mappings
    pub vars: HashMap<String, TypeScheme<N>>,
}

impl<N> TypeEnv<N>
where
    N: Clone + PartialEq + Eq + std::hash::Hash,
{
    /// Create a new empty type environment
    pub fn new() -> Self {
        TypeEnv {
            vars: HashMap::new(),
        }
    }

    /// Bind a variable to a type scheme
    pub fn bind_var(&mut self, name: String, scheme: TypeScheme<N>) {
        self.vars.insert(name, scheme);
    }

    /// Lookup a variable's type scheme
    pub fn lookup_var(&self, name: &str) -> Option<&TypeScheme<N>> {
        self.vars.get(name)
    }

    /// Extend environment with another environment
    pub fn extend(&mut self, other: TypeEnv<N>) {
        self.vars.extend(other.vars);
    }
}

/// Void type for types with no variables
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Void {}

/// Object schema definition
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Schema {
    /// Schema name
    pub name: QualifiedName,
    /// Fields in the schema (no type vars in schema)
    pub fields: HashMap<Field, Type<Void>>,
}

/// Definition kinds in Pact
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum DefKind {
    /// Function definition
    DKDefun,
    /// Constant definition
    DKDefConst,
    /// Capability definition
    DKDefCap,
    /// Pact definition (multi-step transaction)
    DKDefPact,
    /// Schema definition
    DKDefSchema(Schema),
    /// Table definition
    DKDefTable,
}

/// The main Pact type enum (generic over type variable names)
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Type<N>
where
    N: Clone + PartialEq + Eq + std::hash::Hash,
{
    /// Primitive type
    Prim(PrimType),

    /// Type variable for inference
    Var(N),

    /// Function type: arg -> ret
    Fun(Box<Type<N>>, Box<Type<N>>),

    /// Nullary function: () -> ret
    Nullary(Box<Type<N>>),

    /// List type: [element_type]
    List(Box<Type<N>>),

    /// Object type with row type (structural typing)
    Object(RowTy<N>),

    /// Table type with row type
    Table(RowTy<N>),

    /// Module reference
    Module(MRef<N>),

    /// Capability token
    Cap,

    /// Any type (top type)
    Any,
}

/// Type scheme for polymorphic types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypeScheme<TV>
where
    TV: Clone + PartialEq + Eq + std::hash::Hash,
{
    /// Type variables
    pub type_vars: Vec<TV>,
    /// Type class predicates
    pub predicates: Vec<TypeClass>,
    /// Body type
    pub body: Type<TV>,
}

/// Built-in type classes in Pact (matches Haskell BuiltinTC)
/// Uses concrete String types for simplicity
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TypeClass {
    /// Equality type class - supports == and !=
    Eq(Type<String>),

    /// Ordering type class - supports <, >, <=, >=
    /// Requires Eq as superclass
    Ord(Type<String>),

    /// Show type class - supports conversion to string representation
    Show(Type<String>),

    /// Addition type class - supports + operation (numbers and strings)
    Add(Type<String>),

    /// Numeric type class - supports basic arithmetic
    /// Includes integers and decimals
    Num(Type<String>),

    /// List-like type class - supports length, at, etc.
    /// Includes lists, strings, and objects
    ListLike(Type<String>),

    /// Fractional type class - supports division
    /// Only decimals in Pact
    Fractional(Type<String>),

    /// EnforceRead type class - for database operations
    EnforceRead(Type<String>),

    /// IsValue type class - for runtime value checking
    IsValue(Type<String>),

    /// Row subtyping constraint - for structural typing
    /// First row is subtype of second
    RoseSubRow(RoseRow<String>, RoseRow<String>),

    /// Row equality constraint - for unification
    RoseRowEq(RoseRow<String>, RoseRow<String>),
}

/// Type aliases for concrete types
pub type ConcreteType = Type<String>;
pub type ConcreteTypeVar = TypeVar<String>;
pub type ConcreteTypeScheme = TypeScheme<String>;
pub type ConcreteTypeEnv = TypeEnv<String>;

/// Type error for type checking
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum TypeError {
    #[error("Type mismatch: expected {expected}, got {actual}")]
    TypeMismatch { expected: String, actual: String },

    #[error("Unification failed: {left} and {right}")]
    UnificationFailure { left: String, right: String },

    #[error("Occurs check failed for variable {var}")]
    OccursCheck { var: String },

    #[error("Infinite type: variable {var} occurs in type {ty}")]
    InfiniteType { var: String, ty: String },

    #[error("Row mismatch: fields {fields:?}")]
    RowMismatchFields { fields: Vec<String> },

    #[error("Missing field: {field}")]
    MissingField { field: String },

    #[error("No instance for {class_name} with type {ty}")]
    NoInstance { class_name: String, ty: String },

    #[error("Unknown variable: {name}")]
    UnknownVariable { name: String },

    #[error("Undefined variable: {name}")]
    UndefinedVariable { name: String },

    #[error("Type class error: {message}")]
    TypeClassError { message: String },
}

/// Type class environment for storing type class instances
#[derive(Debug, Clone, PartialEq, Default)]
pub struct TypeClassEnv {
    /// Type class instances
    pub instances: HashMap<String, Vec<TypeClass>>,
}

impl TypeClassEnv {
    /// Create a new type class environment
    pub fn new() -> Self {
        TypeClassEnv::default()
    }
}

/// Overload resolution context
#[derive(Debug, Clone, PartialEq, Default)]
pub struct OverloadContext {
    /// Current overloads being resolved
    pub overloads: HashMap<String, Vec<ConcreteType>>,
}

impl OverloadContext {
    /// Create a new overload context
    pub fn new() -> Self {
        OverloadContext::default()
    }
}

impl fmt::Display for PrimType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrimType::Integer => write!(f, "integer"),
            PrimType::Decimal => write!(f, "decimal"),
            PrimType::String => write!(f, "string"),
            PrimType::Bool => write!(f, "bool"),
            PrimType::Time => write!(f, "time"),
            PrimType::Guard => write!(f, "guard"),
            PrimType::Keyset => write!(f, "keyset"),
            PrimType::Unit => write!(f, "unit"),
        }
    }
}

impl<N> fmt::Display for Type<N>
where
    N: Clone + PartialEq + Eq + std::hash::Hash + fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Prim(p) => write!(f, "{}", p),
            Type::Var(v) => write!(f, "{}", v),
            Type::Fun(arg, ret) => write!(f, "({} -> {})", arg, ret),
            Type::Nullary(ret) => write!(f, "(() -> {})", ret),
            Type::List(elem) => write!(f, "[{}]", elem),
            Type::Object(_) => write!(f, "object"),
            Type::Table(_) => write!(f, "table"),
            Type::Module(_) => write!(f, "module"),
            Type::Cap => write!(f, "capability"),
            Type::Any => write!(f, "any"),
        }
    }
}

// Convert local TypeError to PactError
impl From<TypeError> for pact_core::errors::PactError<pact_core::shared::SpanInfo> {
    fn from(error: TypeError) -> Self {
        use pact_core::errors::{PactError, EvalError};
        use pact_core::shared::SpanInfo;
        
        let eval_error = match error {
            TypeError::TypeMismatch { expected, actual } => {
                EvalError::TypeMismatch { expected, found: actual, context: "type checking".to_string() }
            }
            TypeError::InfiniteType { var, ty } => {
                EvalError::InvalidArgument(format!("Infinite type: variable {} occurs in type {}", var, ty))
            }
            TypeError::MissingField { field } => {
                EvalError::InvalidArgument(format!("Missing field: {}", field))
            }
            TypeError::NoInstance { class_name, ty } => {
                EvalError::InvalidArgument(format!("No instance for {} with type {}", class_name, ty))
            }
            other => {
                EvalError::InvalidArgument(format!("{}", other))
            }
        };
        
        PactError::PEExecutionError(
            eval_error,
            vec![], // No stack frames for type errors
            SpanInfo::empty(), // Default span
        )
    }
}

#[cfg(test)]
mod tests;
