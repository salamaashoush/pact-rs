//! Core IR Term representation
//!
//! This module defines the core Term type that represents Pact expressions
//! after desugaring and name resolution, closely matching the Haskell implementation.

use compact_str::CompactString;
use serde::{Deserialize, Serialize};
use pact_core::values::PactValue;

// Re-export SpanInfo from shared types
pub use pact_core::shared::SpanInfo;

/// Names in the IR - either parsed names or resolved names
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Name {
    /// Parsed bare name
    Parsed(ParsedName),
    /// Fully resolved name with module context
    Resolved(ResolvedName),
    /// DeBruijn index for locally bound variables
    DeBruijn(DeBruijnIndex),
}

/// Parsed names from the surface syntax
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ParsedName {
    /// Bare name
    BN(BareName),
    /// Qualified name with module
    QN(QualifiedName),
    /// Dynamic access name
    DN(DynamicName),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BareName(pub CompactString);

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct QualifiedName {
    pub name: CompactString,
    pub module: ModuleName,
}

impl std::fmt::Display for QualifiedName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.module, self.name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DynamicName {
    pub name: CompactString,
    pub field: CompactString,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ModuleName {
    pub name: CompactString,
    pub namespace: Option<NamespaceName>,
}

impl ModuleName {
    /// Render module name as string
    pub fn render(&self) -> String {
        match &self.namespace {
            Some(ns) => format!("{}.{}", ns.0, self.name),
            None => self.name.to_string(),
        }
    }
}

impl std::fmt::Display for ModuleName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.namespace {
            Some(ns) => write!(f, "{}.{}", ns.0, self.name),
            None => write!(f, "{}", self.name),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct NamespaceName(pub CompactString);

/// DefPact identifier
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct DefPactId(pub CompactString);

/// Resolved names after name resolution
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ResolvedName {
    pub name: CompactString,
    pub module: ModuleName,
    pub hash: Option<CompactString>,
}

/// DeBruijn index for lambda-bound variables
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DeBruijnIndex(pub usize);

/// Core IR Term type - matches Haskell implementation exactly
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Term<N, T, B, I> {
    /// Variable reference
    Var(N, I),

    /// Lambda abstraction
    Lam {
        args: Vec<Arg<T, I>>,
        body: Box<Term<N, T, B, I>>,
        info: I,
    },

    /// Let binding
    Let {
        arg: Arg<T, I>,
        expr: Box<Term<N, T, B, I>>,
        body: Box<Term<N, T, B, I>>,
        info: I,
    },

    /// Function application
    App {
        func: Box<Term<N, T, B, I>>,
        args: Vec<Term<N, T, B, I>>,
        info: I,
    },

    /// Builtin form (special syntax that gets special evaluation)
    BuiltinForm {
        form: BuiltinForm<Box<Term<N, T, B, I>>>,
        info: I,
    },

    /// Builtin function
    Builtin(B, I),

    /// Constant literal
    Constant(Literal, I),

    /// Sequence (do-like)
    Sequence {
        first: Box<Term<N, T, B, I>>,
        second: Box<Term<N, T, B, I>>,
        info: I,
    },

    /// Nullary expression (lazy evaluation)
    Nullary {
        expr: Box<Term<N, T, B, I>>,
        info: I,
    },

    /// List literal
    ListLit {
        elements: Vec<Term<N, T, B, I>>,
        info: I,
    },

    /// Object literal
    ObjectLit {
        fields: Vec<(Field, Term<N, T, B, I>)>,
        info: I,
    },

    /// Inline value (pre-evaluated constants)
    InlineValue {
        value: PactValue,
        info: I,
    },
}

/// Function argument with optional type
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Arg<T, I> {
    pub name: CompactString,
    pub ty: Option<T>,
    pub info: I,
}

/// Object field
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Field(pub CompactString);

/// Literal values
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    LString(CompactString),
    LInteger(i64),
    LDecimal(Decimal),
    LBool(bool),
    LUnit,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Decimal {
    pub precision: u8,
    pub mantissa: i128,
}

/// Type system
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Type {
    /// Primitive types
    TyPrim(PrimType),
    /// List type
    TyList(Box<Type>),
    /// Polymorphic list
    TyPolyList,
    /// Module reference
    TyModRef(Vec<ModuleName>),
    /// Keyset type
    TyKeyset,
    /// Object type with schema
    TyObject(TypedName),
    /// Table type with schema
    TyTable(TypedName),
    /// Polymorphic object
    TyPolyObject,
    /// Any type (top type)
    TyAny,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum PrimType {
    PrimInt,
    PrimDecimal,
    PrimBool,
    PrimString,
    PrimTime,
    PrimUnit,
    PrimGuard,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TypedName {
    TBN(BareName),
    TQN(QualifiedName),
}

/// Builtin forms representing special syntax
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum BuiltinForm<T> {
    /// Conditional: and
    CAnd(T, T),
    /// Conditional: or
    COr(T, T),
    /// Conditional: if
    CIf {
        cond: T,
        then_expr: T,
        else_expr: Option<T>,
    },
    /// Enforce with error message
    CEnforce {
        cond: T,
        msg: T,
    },
    /// Enforce-one (try multiple conditions)
    CEnforceOne {
        conditions: Vec<T>,
    },
    /// With-capability
    CWithCapability {
        cap: T,
        body: Vec<T>,
    },
    /// Create user guard
    CCreateUserGuard {
        name: T,
        args: Vec<T>,
    },
    /// Try-catch error handling
    CTry {
        expr: T,
        handler: T,
    },
    /// Map operation
    CMap {
        func: T,
        list: T,
    },
    /// Filter operation
    CFilter {
        func: T,
        list: T,
    },
    /// Fold operation
    CFold {
        func: T,
        init: T,
        list: T,
    },
    /// Zip operation
    CZip {
        func: T,
        list1: T,
        list2: T,
    },
    /// Cond (pattern matching)
    CCond {
        conditions: Vec<(T, T)>,
    },
    /// Suspend (for defpacts)
    CSuspend(T),
}

/// Builtin functions - matches Haskell CoreBuiltin
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum CoreBuiltin {
    // Arithmetic
    CoreAdd,
    CoreSub,
    CoreMultiply,
    CoreDivide,
    CoreMod,
    CorePow,
    CoreNegate,
    CoreAbs,
    CoreSqrt,
    CoreLn,
    CoreExp,
    CoreRound,
    CoreCeiling,
    CoreFloor,
    CoreCeilingPrec,
    CoreFloorPrec,
    CoreRoundPrec,
    CoreDec,
    CoreLogBase,

    // Comparison
    CoreEq,
    CoreNeq,
    CoreLT,
    CoreGT,
    CoreLEQ,
    CoreGEQ,

    // Boolean operations
    CoreAndQ,
    CoreOrQ,
    CoreNotQ,
    CoreNot,

    // String operations
    CoreLength,
    CoreTake,
    CoreDrop,
    CoreConcat,
    CoreReverse,
    CoreContains,
    CoreFormat,
    CoreShow,
    CoreStrToInt,
    CoreStrToIntBase,
    CoreIntToStr,
    CoreStrToList,

    // List operations
    CoreMap,
    CoreFilter,
    CoreFold,
    CoreZip,
    CoreAt,
    CoreSort,
    CoreReverse2,
    CoreDistinct,
    CoreRemove,
    CoreSortObject,
    CoreMakeList,
    CoreEnumerate,
    CoreEnumerateStepN,
    CoreWhere,

    // Database operations
    CoreInsert,
    CoreUpdate,
    CoreWithRead,
    CoreWithDefaultRead,
    CoreRead,
    CoreSelect,
    CoreSelectWithFields,
    CoreKeys,
    CoreWrite,
    CoreCreateTable,
    CoreDescribeTable,
    CoreFoldDb,
    CoreDefineKeySet,
    CoreDefineKeysetData,
    CoreTxLog,
    CoreTxHash,

    // Capability operations
    CoreEnforceKeyset,
    CoreEnforceKeysetName,
    CoreKeyLog,
    CoreRequireCapability,
    CoreInstallCapability,
    CoreComposeCapability,
    CoreEmitEvent,
    CoreCreateCapabilityGuard,
    CoreCreateCapabilityPactGuard,

    // Guard operations
    CoreKeysetRefGuard,
    CoreCreateDefPactGuard,
    CoreCreateModuleGuard,

    // Cryptographic operations
    CoreHash,
    CoreVerifySignature,
    CoreHashKeccak256,
    CorePoseidonHashHackachain,
    CoreVerifySPV,
    CoreZkPairingCheck,
    CoreZkPointAdd,
    CoreZKScalarMult,
    CoreHashPoseidon,
    CoreHyperlaneMessageId,
    CoreHyperlaneDecodeMessage,
    CoreHyperlaneEncodeMessage,

    // Time operations
    CoreParseTime,
    CoreFormatTime,
    CoreTime,
    CoreAddTime,
    CoreDiffTime,
    CoreDays,
    CoreHours,
    CoreMinutes,

    // Type operations
    CoreTypeOf,
    CoreEnforceGuard,
    CoreTypeOfPrincipal,

    // Module operations
    CoreDescribeModule,
    CoreDescribeKeyset,
    CoreListModules,
    CoreAcquireModuleAdmin,
    CoreStaticRedeploy,

    // Namespace operations
    CoreNamespace,
    CoreDefineNamespace,
    CoreDescribeNamespace,

    // Principal operations
    CoreCreatePrincipal,
    CoreIsPrincipal,
    CoreValidatePrincipal,

    // DefPact operations
    CorePactId,
    CoreYield,
    CoreYieldToChain,
    CoreResume,
    CoreContinue,

    // Utility operations
    CoreIdentity,
    CoreCond,
    CoreB64Encode,
    CoreB64Decode,
    CoreBind,
    CoreChainData,
    CoreIsCharset,
    CoreEnforceVerifier,
    CoreCompose,

    // IO/Message operations
    CoreReadMsg,
    CoreReadMsgDefault,
    CoreReadKeyset,
    CoreReadDecimal,
    CoreReadInteger,
    CoreReadString,

    // Bitwise operations
    CoreBitwiseAnd,
    CoreBitwiseOr,
    CoreBitwiseXor,
    CoreBitwiseFlip,
    CoreBitShift,

    // REPL operations (for testing)
    CoreReplOnlyBeginTx,
    CoreReplOnlyCommitTx,
    CoreReplOnlyRollbackTx,
    CoreReplOnlyPrintLn,
}

/// Type alias for the main Term type used after desugaring
pub type CoreTerm = Term<Name, Type, CoreBuiltin, SpanInfo>;

/// Top-level IR items
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TopLevel<N, T, B, I> {
    TLModule(Module<N, T, B, I>),
    TLInterface(Interface<N, T, B, I>),
    TLTerm(Term<N, T, B, I>),
    TLUse(Import<I>),
}

/// Module definition in IR
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Module<N, T, B, I> {
    pub name: CompactString,
    pub governance: Governance,
    pub definitions: Vec<Def<N, T, B, I>>,
    pub imports: Vec<ExtDecl<I>>,
    pub annotations: Vec<PactAnn<I>>,
    pub info: I,
}

/// Interface definition in IR
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Interface<N, T, B, I> {
    pub name: CompactString,
    pub imports: Vec<Import<I>>,
    pub definitions: Vec<IfDef<N, T, B, I>>,
    pub annotations: Vec<PactAnn<I>>,
    pub info: I,
}

/// Module governance
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Governance {
    KeyGov(CompactString),
    CapGov(CompactString),
}

/// External declarations
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ExtDecl<I> {
    ExtImport(Import<I>),
    ExtImplements { module: ModuleName, info: I },
    ExtBless { hash: CompactString, info: I },
}

/// Import statement
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Import<I> {
    pub module: ModuleName,
    pub hash: Option<CompactString>,
    pub imports: Option<Vec<CompactString>>,
    pub info: I,
}

/// Definition types
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Def<N, T, B, I> {
    Dfun(Defun<N, T, B, I>),
    DConst(DefConst<N, T, B, I>),
    DCap(DefCap<N, T, B, I>),
    DSchema(DefSchema<T, I>),
    DTable(DefTable<I>),
    DPact(DefPact<N, T, B, I>),
}

/// Interface definition types
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IfDef<N, T, B, I> {
    IfDfun(IfDefun<T, I>),
    IfDConst(DefConst<N, T, B, I>),
    IfDCap(IfDefCap<T, I>),
    IfDSchema(DefSchema<T, I>),
    IfDPact(IfDefPact<T, I>),
}

/// Function definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Defun<N, T, B, I> {
    pub name: Arg<T, I>,
    pub args: Vec<Arg<T, I>>,
    pub body: Term<N, T, B, I>,
    pub annotations: Vec<PactAnn<I>>,
    pub info: I,
}

/// Constant definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DefConst<N, T, B, I> {
    pub name: Arg<T, I>,
    pub value: Term<N, T, B, I>,
    pub doc: Option<(CompactString, PactDocType)>,
    pub info: I,
}

/// Capability definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DefCap<N, T, B, I> {
    pub name: Arg<T, I>,
    pub args: Vec<Arg<T, I>>,
    pub body: Term<N, T, B, I>,
    pub annotations: Vec<PactAnn<I>>,
    pub meta: Option<DCapMeta>,
    pub info: I,
}

/// Schema definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DefSchema<T, I> {
    pub name: CompactString,
    pub fields: Vec<Arg<T, I>>,
    pub annotations: Vec<PactAnn<I>>,
    pub info: I,
}

/// Table definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DefTable<I> {
    pub name: CompactString,
    pub schema: ParsedName,
    pub doc: Option<(CompactString, PactDocType)>,
    pub info: I,
}

/// Pact definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DefPact<N, T, B, I> {
    pub name: Arg<T, I>,
    pub args: Vec<Arg<T, I>>,
    pub steps: Vec<PactStep<N, T, B, I>>,
    pub annotations: Vec<PactAnn<I>>,
    pub info: I,
}

/// Interface function definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IfDefun<T, I> {
    pub name: Arg<T, I>,
    pub args: Vec<Arg<T, I>>,
    pub annotations: Vec<PactAnn<I>>,
    pub info: I,
}

/// Interface capability definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IfDefCap<T, I> {
    pub name: Arg<T, I>,
    pub args: Vec<Arg<T, I>>,
    pub annotations: Vec<PactAnn<I>>,
    pub meta: Option<DCapMeta>,
    pub info: I,
}

/// Interface pact definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IfDefPact<T, I> {
    pub name: Arg<T, I>,
    pub args: Vec<Arg<T, I>>,
    pub annotations: Vec<PactAnn<I>>,
    pub info: I,
}

/// Capability metadata
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DCapMeta {
    DefManaged(Option<(CompactString, ParsedName)>),
    DefEvent,
}

/// Pact step
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum PactStep<N, T, B, I> {
    Step {
        entity: Option<Term<N, T, B, I>>,
        expr: Term<N, T, B, I>,
    },
    StepWithRollback {
        entity: Option<Term<N, T, B, I>>,
        expr: Term<N, T, B, I>,
        rollback: Term<N, T, B, I>,
    },
}

/// Annotations
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum PactAnn<I> {
    PactDoc { ty: PactDocType, doc: CompactString },
    PactModel(Vec<PropertyExpr<I>>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PactDocType {
    PactDocAnn,
    PactDocString,
}

/// Property expressions for @model
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum PropertyExpr<I> {
    PropAtom(ParsedName, I),
    PropConstant(Literal, I),
    PropKeyword(PropKeyword, I),
    PropDelim(PropDelim, I),
    PropSequence(Vec<PropertyExpr<I>>, I),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PropKeyword {
    KwLet,
    KwLambda,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PropDelim {
    DelimLBrace,
    DelimRBrace,
    DelimLBracket,
    DelimRBracket,
    DelimComma,
    DelimColon,
}

/// Type alias for the main TopLevel type used after desugaring
pub type CoreTopLevel = TopLevel<Name, Type, CoreBuiltin, SpanInfo>;
