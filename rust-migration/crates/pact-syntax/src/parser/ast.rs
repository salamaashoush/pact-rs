//! AST for Pact following Haskell ParseTree structure
//!
//! This is the single AST used throughout the codebase.

use compact_str::CompactString;
use serde::{Deserialize, Serialize};

// Re-export SpanInfo from shared types
pub use pact_core::shared::SpanInfo;

/// Module names with optional namespace
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ModuleName {
    pub name: CompactString,
    pub namespace: Option<NamespaceName>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct NamespaceName(pub CompactString);

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BareName(pub CompactString);

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct QualifiedName {
    pub name: CompactString,
    pub module: ModuleName,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DynamicName {
    pub name: CompactString,
    pub field: CompactString,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ParsedName {
    BN(BareName),
    QN(QualifiedName),
    DN(DynamicName),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ParsedTyName {
    TBN(BareName),
    TQN(QualifiedName),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Field(pub CompactString);

/// Literals
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

/// Types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Type {
    TyPrim(PrimType),
    TyList(Box<Type>),
    TyPolyList,
    TyModRef(Vec<ModuleName>),
    TyKeyset,
    TyObject(ParsedTyName),
    TyTable(ParsedTyName),
    TyPolyObject,
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

/// Arguments with optional type annotations
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MArg<I> {
    pub name: CompactString,
    pub ty: Option<Type>,
    pub info: I,
}

/// Typed arguments (always have type)
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Arg<I> {
    pub name: CompactString,
    pub ty: Type,
    pub info: I,
}

/// Let binder
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Binder<I> {
    pub arg: MArg<I>,
    pub expr: ParsedExpr<I>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum LetForm {
    LFLetNormal,
    LFLetStar,
}

/// Top-level program
pub type Program<I> = Vec<ParsedTopLevel<I>>;

/// Top-level items
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ParsedTopLevel<I> {
    TLModule(ParsedModule<I>),
    TLInterface(ParsedInterface<I>),
    TLTerm(ParsedExpr<I>),
    TLUse(Import<I>),
}

impl ParsedTopLevel<SpanInfo> {
    pub fn span(&self) -> SpanInfo {
        match self {
            ParsedTopLevel::TLModule(m) => m.info,
            ParsedTopLevel::TLInterface(i) => i.info,
            ParsedTopLevel::TLTerm(expr) => expr.span(),
            ParsedTopLevel::TLUse(import) => import.info,
        }
    }
}

/// REPL top-level items
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ReplTopLevel<I> {
    RTLTopLevel(ParsedTopLevel<I>),
    RTLDefun(ParsedDefun<I>),
    RTLDefConst(ParsedDefConst<I>),
}

/// Module governance
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Governance {
    KeyGov(CompactString),
    CapGov(CompactString),
}

/// Module definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ParsedModule<I> {
    pub name: CompactString,
    pub governance: Governance,
    pub definitions: Vec<ParsedDef<I>>,
    pub imports: Vec<ExtDecl<I>>,
    pub annotations: Vec<PactAnn<I>>,
    pub info: I,
}

/// Interface definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ParsedInterface<I> {
    pub name: CompactString,
    pub imports: Vec<Import<I>>,
    pub definitions: Vec<ParsedIfDef<I>>,
    pub annotations: Vec<PactAnn<I>>,
    pub info: I,
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

/// Module definitions
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ParsedDef<I> {
    Dfun(ParsedDefun<I>),
    DConst(ParsedDefConst<I>),
    DCap(ParsedDefCap<I>),
    DSchema(DefSchema<I>),
    DTable(DefTable<I>),
    DPact(DefPact<I>),
}

/// Interface definitions
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ParsedIfDef<I> {
    IfDfun(IfDefun<I>),
    IfDConst(ParsedDefConst<I>),
    IfDCap(IfDefCap<I>),
    IfDSchema(DefSchema<I>),
    IfDPact(IfDefPact<I>),
}

/// Function definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ParsedDefun<I> {
    pub name: MArg<I>,
    pub args: Vec<MArg<I>>,
    pub body: Vec<ParsedExpr<I>>,
    pub annotations: Vec<PactAnn<I>>,
    pub info: I,
}

/// Interface function definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IfDefun<I> {
    pub name: MArg<I>,
    pub args: Vec<MArg<I>>,
    pub annotations: Vec<PactAnn<I>>,
    pub info: I,
}

/// Constant definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ParsedDefConst<I> {
    pub name: MArg<I>,
    pub value: ParsedExpr<I>,
    pub doc: Option<(CompactString, PactDocType)>,
    pub info: I,
}

/// Capability definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ParsedDefCap<I> {
    pub name: MArg<I>,
    pub args: Vec<MArg<I>>,
    pub body: Vec<ParsedExpr<I>>,
    pub annotations: Vec<PactAnn<I>>,
    pub meta: Option<DCapMeta>,
    pub info: I,
}

/// Interface capability definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IfDefCap<I> {
    pub name: MArg<I>,
    pub args: Vec<MArg<I>>,
    pub annotations: Vec<PactAnn<I>>,
    pub meta: Option<DCapMeta>,
    pub info: I,
}

/// Capability metadata
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DCapMeta {
    DefManaged(Option<(CompactString, ParsedName)>),
    DefEvent,
}

/// Schema definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DefSchema<I> {
    pub name: CompactString,
    pub fields: Vec<Arg<I>>,
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
pub struct DefPact<I> {
    pub name: MArg<I>,
    pub args: Vec<MArg<I>>,
    pub steps: Vec<PactStep<I>>,
    pub annotations: Vec<PactAnn<I>>,
    pub info: I,
}

/// Interface pact definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IfDefPact<I> {
    pub name: MArg<I>,
    pub args: Vec<MArg<I>>,
    pub annotations: Vec<PactAnn<I>>,
    pub info: I,
}

/// Pact step
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum PactStep<I> {
    Step {
        entity: Option<ParsedExpr<I>>,
        expr: ParsedExpr<I>,
        model: Option<Vec<PropertyExpr<I>>>,
    },
    StepWithRollback {
        entity: Option<ParsedExpr<I>>,
        expr: ParsedExpr<I>,
        rollback: ParsedExpr<I>,
        model: Option<Vec<PropertyExpr<I>>>,
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

/// Expressions - Complete Haskell-conformant AST
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ParsedExpr<I> {
    // Variables and constants
    Var(ParsedName, I),
    Constant(Literal, I),

    // Compound structures
    List(Vec<ParsedExpr<I>>, I),
    Object(Vec<(Field, ParsedExpr<I>)>, I),

    // Application - simplified to just positional args for performance
    App {
        func: Box<ParsedExpr<I>>,
        args: Vec<ParsedExpr<I>>,
        info: I,
    },

    // Binding forms - object destructuring
    Binding {
        bindings: Vec<(Field, MArg<I>)>,
        body: Vec<ParsedExpr<I>>,
        info: I,
    },

    // Special forms
    Let {
        form: LetForm,
        bindings: Vec<Binder<I>>,
        body: Vec<ParsedExpr<I>>,
        info: I,
    },

    Lam {
        args: Vec<MArg<I>>,
        body: Vec<ParsedExpr<I>>,
        info: I,
    },

    // Control flow
    If {
        cond: Box<ParsedExpr<I>>,
        then_expr: Box<ParsedExpr<I>>,
        else_expr: Option<Box<ParsedExpr<I>>>,
        info: I,
    },

    And {
        left: Box<ParsedExpr<I>>,
        right: Box<ParsedExpr<I>>,
        info: I,
    },

    Or {
        left: Box<ParsedExpr<I>>,
        right: Box<ParsedExpr<I>>,
        info: I,
    },

    Cond {
        branches: Vec<(ParsedExpr<I>, ParsedExpr<I>)>,
        info: I,
    },

    // Enforcement
    Enforce {
        cond: Box<ParsedExpr<I>>,
        msg: Box<ParsedExpr<I>>,
        info: I,
    },

    EnforceOne {
        msg: Box<ParsedExpr<I>>,
        conds: Vec<ParsedExpr<I>>,
        info: I,
    },

    // Capabilities
    WithCapability {
        cap: Box<ParsedExpr<I>>,
        body: Vec<ParsedExpr<I>>,
        info: I,
    },

    RequireCapability {
        cap: Box<ParsedExpr<I>>,
        info: I,
    },

    ComposeCapability {
        cap: Box<ParsedExpr<I>>,
        info: I,
    },

    InstallCapability {
        cap: Box<ParsedExpr<I>>,
        info: I,
    },

    EmitEvent {
        cap: Box<ParsedExpr<I>>,
        info: I,
    },

    CreateUserGuard {
        name: ParsedName,
        args: Vec<ParsedExpr<I>>,
        info: I,
    },

    // Try/catch
    Try {
        expr: Box<ParsedExpr<I>>,
        catch: Box<ParsedExpr<I>>,
        info: I,
    },

    // Type operations
    TypeAnn {
        expr: Box<ParsedExpr<I>>,
        ty: Type,
        info: I,
    },

    // Suspend/resume for defpacts
    Suspend {
        expr: Box<ParsedExpr<I>>,
        info: I,
    },

    Yield {
        data: Box<ParsedExpr<I>>,
        target: Option<Box<ParsedExpr<I>>>,
        info: I,
    },

    Resume {
        bindings: Vec<Binder<I>>,
        body: Box<ParsedExpr<I>>,
        info: I,
    },

    // Definition expressions (for top-level usage)
    DefConst(Box<ParsedDefConst<I>>),
    DefFun(Box<ParsedDefun<I>>),
    DefCap(Box<ParsedDefCap<I>>),
    DefSchema(Box<DefSchema<I>>),
    DefTable(Box<DefTable<I>>),
    DefPact(Box<DefPact<I>>),
}

impl ParsedExpr<SpanInfo> {
    pub fn span(&self) -> SpanInfo {
        match self {
            ParsedExpr::Var(_, info)
            | ParsedExpr::Constant(_, info)
            | ParsedExpr::List(_, info)
            | ParsedExpr::Object(_, info) => *info,

            ParsedExpr::App { info, .. }
            | ParsedExpr::Binding { info, .. }
            | ParsedExpr::Let { info, .. }
            | ParsedExpr::Lam { info, .. }
            | ParsedExpr::If { info, .. }
            | ParsedExpr::And { info, .. }
            | ParsedExpr::Or { info, .. }
            | ParsedExpr::Cond { info, .. }
            | ParsedExpr::Enforce { info, .. }
            | ParsedExpr::EnforceOne { info, .. }
            | ParsedExpr::WithCapability { info, .. }
            | ParsedExpr::RequireCapability { info, .. }
            | ParsedExpr::ComposeCapability { info, .. }
            | ParsedExpr::InstallCapability { info, .. }
            | ParsedExpr::EmitEvent { info, .. }
            | ParsedExpr::CreateUserGuard { info, .. }
            | ParsedExpr::Try { info, .. }
            | ParsedExpr::TypeAnn { info, .. }
            | ParsedExpr::Suspend { info, .. }
            | ParsedExpr::Yield { info, .. }
            | ParsedExpr::Resume { info, .. } => *info,
            
            ParsedExpr::DefConst(def) => def.info,
            ParsedExpr::DefFun(def) => def.info,
            ParsedExpr::DefCap(def) => def.info,
            ParsedExpr::DefSchema(def) => def.info,
            ParsedExpr::DefTable(def) => def.info,
            ParsedExpr::DefPact(def) => def.info,
        }
    }
}
