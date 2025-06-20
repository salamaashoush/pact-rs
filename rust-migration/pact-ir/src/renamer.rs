//! Name resolution and DeBruijn index assignment
//!
//! This module implements the renaming phase that converts parsed names
//! to their final resolved form with proper variable scoping.

use crate::term::*;
use pact_errors::{PactError, EvalError};
use std::collections::HashMap;
use compact_str::CompactString;

/// Variable depth tracking for DeBruijn index calculation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VarDepth(pub usize);

impl VarDepth {
    pub fn new() -> Self {
        Self(0)
    }
    
    pub fn increment(self) -> Self {
        Self(self.0 + 1)
    }
    
    pub fn add(self, n: usize) -> Self {
        Self(self.0 + n)
    }
}

/// Name kind - different types of resolved names
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NameKind {
    /// Locally bound variable (lambda/let) with DeBruijn index
    NBound(DeBruijnIndex),
    /// Top-level definition in a module
    NTopLevel { module: ModuleName, hash: Option<CompactString> },
    /// Module reference
    NModRef(Vec<ModuleName>),
    /// Dynamic reference (::)
    NDynRef { base: CompactString, field: CompactString },
}

/// Definition kind - what type of definition this name refers to
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefKind {
    DKDefun,
    DKDefConst,
    DKDefCap,
    DKDefSchema,
    DKDefTable,
    DKDefPact,
}

/// Resolved name with kind information
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedNameInfo {
    pub name: CompactString,
    pub kind: NameKind,
    pub def_kind: Option<DefKind>,
}

/// Renamer environment for name resolution
#[derive(Debug, Clone)]
pub struct RenamerEnv {
    /// Local variable bindings (lambda/let parameters)
    pub local_binds: HashMap<CompactString, (NameKind, Option<DefKind>)>,
    /// Current module top-level bindings
    pub module_binds: HashMap<CompactString, (NameKind, DefKind)>,
    /// Current variable depth for DeBruijn indices
    pub var_depth: VarDepth,
    /// Current module context
    pub current_module: Option<ModuleName>,
    /// Loaded modules and their exports
    pub loaded_modules: HashMap<ModuleName, ModuleData>,
    /// Module dependencies discovered during renaming
    pub dependencies: std::collections::HashSet<ModuleName>,
    /// Namespace context
    pub current_namespace: Option<NamespaceName>,
}

/// Module data for name resolution
#[derive(Debug, Clone)]
pub struct ModuleData {
    pub name: ModuleName,
    pub exports: HashMap<CompactString, (NameKind, DefKind)>,
    pub interfaces: Vec<ModuleName>,
    pub hash: Option<CompactString>,
}

impl RenamerEnv {
    pub fn new() -> Self {
        Self {
            local_binds: HashMap::new(),
            module_binds: HashMap::new(),
            var_depth: VarDepth::new(),
            current_module: None,
            loaded_modules: HashMap::new(),
            dependencies: std::collections::HashSet::new(),
            current_namespace: None,
        }
    }

    /// Enter a new lambda scope with parameter bindings
    pub fn enter_lambda_scope(&mut self, args: &[Arg<Type, SpanInfo>]) {
        let base_depth = self.var_depth.0;
        
        // Assign consecutive DeBruijn indices to arguments
        for (i, arg) in args.iter().enumerate() {
            let index = DeBruijnIndex(base_depth + i);
            self.local_binds.insert(
                arg.name.clone(),
                (NameKind::NBound(index), None)
            );
        }
        
        // Update depth
        self.var_depth = self.var_depth.add(args.len());
    }

    /// Enter a let binding scope
    pub fn enter_let_scope(&mut self, arg: &Arg<Type, SpanInfo>) {
        let index = DeBruijnIndex(self.var_depth.0);
        self.local_binds.insert(
            arg.name.clone(),
            (NameKind::NBound(index), None)
        );
        self.var_depth = self.var_depth.increment();
    }

    /// Exit a scope by removing the specified number of bindings
    pub fn exit_scope(&mut self, original_depth: VarDepth) {
        // Remove bindings that were added at depths >= original_depth
        self.local_binds.retain(|_, (kind, _)| {
            match kind {
                NameKind::NBound(DeBruijnIndex(idx)) => *idx < original_depth.0,
                _ => true,
            }
        });
        self.var_depth = original_depth;
    }

    /// Resolve a parsed name to its final form
    pub fn resolve_name(&mut self, name: &ParsedName) -> Result<ResolvedNameInfo, PactError<SpanInfo>> {
        match name {
            ParsedName::BN(bare_name) => self.resolve_bare_name(&bare_name.0),
            ParsedName::QN(qual_name) => self.resolve_qualified_name(qual_name),
            ParsedName::DN(dyn_name) => self.resolve_dynamic_name(dyn_name),
        }
    }

    /// Resolve a bare name (simple identifier)
    fn resolve_bare_name(&mut self, name: &str) -> Result<ResolvedNameInfo, PactError<SpanInfo>> {
        let name_str = CompactString::from(name);
        
        // First check local bindings (lambda/let variables)
        if let Some((kind, def_kind)) = self.local_binds.get(&name_str) {
            // Convert DeBruijn index relative to current depth
            let resolved_kind = match kind {
                NameKind::NBound(DeBruijnIndex(original_idx)) => {
                    let relative_idx = self.var_depth.0 - original_idx - 1;
                    NameKind::NBound(DeBruijnIndex(relative_idx))
                }
                other => other.clone(),
            };
            
            return Ok(ResolvedNameInfo {
                name: name_str,
                kind: resolved_kind,
                def_kind: *def_kind,
            });
        }

        // Check current module bindings
        if let Some((kind, def_kind)) = self.module_binds.get(&name_str) {
            return Ok(ResolvedNameInfo {
                name: name_str,
                kind: kind.clone(),
                def_kind: Some(*def_kind),
            });
        }

        // Check loaded modules for exported names
        for (module_name, module_data) in &self.loaded_modules {
            if let Some((kind, def_kind)) = module_data.exports.get(&name_str) {
                // Add dependency if from different module
                if Some(module_name) != self.current_module.as_ref() {
                    self.dependencies.insert(module_name.clone());
                }
                
                return Ok(ResolvedNameInfo {
                    name: name_str,
                    kind: kind.clone(),
                    def_kind: Some(*def_kind),
                });
            }
        }

        // Try as module reference
        let module_name = ModuleName {
            name: name_str.clone(),
            namespace: self.current_namespace.clone(),
        };
        
        if self.loaded_modules.contains_key(&module_name) {
            return Ok(ResolvedNameInfo {
                name: name_str,
                kind: NameKind::NModRef(vec![module_name]),
                def_kind: None,
            });
        }

        // Unresolved name
        Err(PactError::PEExecutionError(
            EvalError::RuntimeError(format!("Unbound variable: {}", name)),
            vec![],
            SpanInfo { start: 0, end: 0 }
        ))
    }

    /// Resolve a qualified name (module.member)
    fn resolve_qualified_name(&mut self, qual_name: &QualifiedName) -> Result<ResolvedNameInfo, PactError<SpanInfo>> {
        let module_name = &qual_name.module;
        let member_name = &qual_name.name;

        // Check if module is loaded
        if let Some(module_data) = self.loaded_modules.get(module_name) {
            // Look for member in module exports
            if let Some((kind, def_kind)) = module_data.exports.get(member_name) {
                // Add dependency
                if Some(module_name) != self.current_module.as_ref() {
                    self.dependencies.insert(module_name.clone());
                }
                
                return Ok(ResolvedNameInfo {
                    name: member_name.clone(),
                    kind: kind.clone(),
                    def_kind: Some(*def_kind),
                });
            }
        }

        // Module not found or member not exported
        Err(PactError::PEExecutionError(
            EvalError::RuntimeError(format!(
                "Cannot resolve qualified name: {}.{}",
                module_name.name, member_name
            )),
            vec![],
            SpanInfo { start: 0, end: 0 }
        ))
    }

    /// Resolve a dynamic name (base::field)
    fn resolve_dynamic_name(&mut self, dyn_name: &DynamicName) -> Result<ResolvedNameInfo, PactError<SpanInfo>> {
        Ok(ResolvedNameInfo {
            name: dyn_name.name.clone(),
            kind: NameKind::NDynRef {
                base: dyn_name.name.clone(),
                field: dyn_name.field.clone(),
            },
            def_kind: None,
        })
    }

    /// Load a module's exports for name resolution
    pub fn load_module(&mut self, module: &Module<Name, Type, CoreBuiltin, SpanInfo>) {
        let mut exports = HashMap::new();
        
        // Extract exports from module definitions
        for def in &module.definitions {
            let (name, def_kind) = match def {
                Def::Dfun(defun) => (defun.name.name.clone(), DefKind::DKDefun),
                Def::DConst(defconst) => (defconst.name.name.clone(), DefKind::DKDefConst),
                Def::DCap(defcap) => (defcap.name.name.clone(), DefKind::DKDefCap),
                Def::DSchema(defschema) => (defschema.name.clone(), DefKind::DKDefSchema),
                Def::DTable(deftable) => (deftable.name.clone(), DefKind::DKDefTable),
                Def::DPact(defpact) => (defpact.name.name.clone(), DefKind::DKDefPact),
            };

            let name_kind = NameKind::NTopLevel {
                module: ModuleName {
                    name: module.name.clone(),
                    namespace: None, // TODO: handle namespaces
                },
                hash: None, // TODO: handle module hashes
            };

            exports.insert(name, (name_kind, def_kind));
        }

        let module_name = ModuleName {
            name: module.name.clone(),
            namespace: None,
        };

        let module_data = ModuleData {
            name: module_name.clone(),
            exports,
            interfaces: Vec::new(), // TODO: extract from imports
            hash: None,
        };

        self.loaded_modules.insert(module_name, module_data);
    }

    /// Set current module context
    pub fn set_current_module(&mut self, module_name: ModuleName) {
        self.current_module = Some(module_name);
    }
}

impl Default for RenamerEnv {
    fn default() -> Self {
        Self::new()
    }
}

/// Main renaming function for terms
pub fn rename_term(
    term: Term<ParsedName, Type, CoreBuiltin, SpanInfo>,
    env: &mut RenamerEnv,
) -> Result<CoreTerm, PactError<SpanInfo>> {
    match term {
        Term::Var(name, info) => {
            let resolved = env.resolve_name(&name)?;
            Ok(Term::Var(
                Name::Resolved(ResolvedName {
                    name: resolved.name,
                    module: match &resolved.kind {
                        NameKind::NTopLevel { module, .. } => module.clone(),
                        _ => ModuleName {
                            name: "".into(),
                            namespace: None,
                        },
                    },
                    hash: match &resolved.kind {
                        NameKind::NTopLevel { hash, .. } => hash.clone(),
                        _ => None,
                    },
                }),
                info,
            ))
        }

        Term::Lam { args, body, info } => {
            let original_depth = env.var_depth;
            
            // Enter lambda scope
            env.enter_lambda_scope(&args);
            
            // Rename body
            let renamed_body = rename_term(*body, env)?;
            
            // Exit scope
            env.exit_scope(original_depth);
            
            Ok(Term::Lam {
                args,
                body: Box::new(renamed_body),
                info,
            })
        }

        Term::Let { arg, expr, body, info } => {
            // Rename the binding expression first (outside the binding scope)
            let renamed_expr = rename_term(*expr, env)?;
            
            let original_depth = env.var_depth;
            
            // Enter let scope
            env.enter_let_scope(&arg);
            
            // Rename body
            let renamed_body = rename_term(*body, env)?;
            
            // Exit scope
            env.exit_scope(original_depth);
            
            Ok(Term::Let {
                arg,
                expr: Box::new(renamed_expr),
                body: Box::new(renamed_body),
                info,
            })
        }

        Term::App { func, args, info } => {
            let renamed_func = rename_term(*func, env)?;
            let mut renamed_args = Vec::new();
            
            for arg in args {
                renamed_args.push(rename_term(arg, env)?);
            }
            
            Ok(Term::App {
                func: Box::new(renamed_func),
                args: renamed_args,
                info,
            })
        }

        Term::BuiltinForm { form, info } => {
            let renamed_form = rename_builtin_form(form, env)?;
            Ok(Term::BuiltinForm {
                form: renamed_form,
                info,
            })
        }

        Term::Builtin(builtin, info) => {
            Ok(Term::Builtin(builtin, info))
        }

        Term::Constant(literal, info) => {
            Ok(Term::Constant(literal, info))
        }

        Term::Sequence { first, second, info } => {
            let renamed_first = rename_term(*first, env)?;
            let renamed_second = rename_term(*second, env)?;
            
            Ok(Term::Sequence {
                first: Box::new(renamed_first),
                second: Box::new(renamed_second),
                info,
            })
        }

        Term::Nullary { expr, info } => {
            let renamed_expr = rename_term(*expr, env)?;
            Ok(Term::Nullary {
                expr: Box::new(renamed_expr),
                info,
            })
        }

        Term::ListLit { elements, info } => {
            let mut renamed_elements = Vec::new();
            for element in elements {
                renamed_elements.push(rename_term(element, env)?);
            }
            
            Ok(Term::ListLit {
                elements: renamed_elements,
                info,
            })
        }

        Term::ObjectLit { fields, info } => {
            let mut renamed_fields = Vec::new();
            for (field, term) in fields {
                renamed_fields.push((field, rename_term(term, env)?));
            }
            
            Ok(Term::ObjectLit {
                fields: renamed_fields,
                info,
            })
        }

        Term::InlineValue { value, info } => {
            Ok(Term::InlineValue { value, info })
        }
    }
}

/// Rename builtin forms
fn rename_builtin_form(
    form: BuiltinForm<Box<Term<ParsedName, Type, CoreBuiltin, SpanInfo>>>,
    env: &mut RenamerEnv,
) -> Result<BuiltinForm<Box<CoreTerm>>, PactError<SpanInfo>> {
    match form {
        BuiltinForm::CAnd(left, right) => {
            let renamed_left = rename_term(*left, env)?;
            let renamed_right = rename_term(*right, env)?;
            Ok(BuiltinForm::CAnd(Box::new(renamed_left), Box::new(renamed_right)))
        }

        BuiltinForm::COr(left, right) => {
            let renamed_left = rename_term(*left, env)?;
            let renamed_right = rename_term(*right, env)?;
            Ok(BuiltinForm::COr(Box::new(renamed_left), Box::new(renamed_right)))
        }

        BuiltinForm::CIf { cond, then_expr, else_expr } => {
            let renamed_cond = rename_term(*cond, env)?;
            let renamed_then = rename_term(*then_expr, env)?;
            let renamed_else = match else_expr {
                Some(else_term) => Some(Box::new(rename_term(*else_term, env)?)),
                None => None,
            };
            
            Ok(BuiltinForm::CIf {
                cond: Box::new(renamed_cond),
                then_expr: Box::new(renamed_then),
                else_expr: renamed_else,
            })
        }

        BuiltinForm::CEnforce { cond, msg } => {
            let renamed_cond = rename_term(*cond, env)?;
            let renamed_msg = rename_term(*msg, env)?;
            Ok(BuiltinForm::CEnforce {
                cond: Box::new(renamed_cond),
                msg: Box::new(renamed_msg),
            })
        }

        BuiltinForm::CWithCapability { cap, body } => {
            let renamed_cap = rename_term(*cap, env)?;
            let mut renamed_body = Vec::new();
            for term in body {
                renamed_body.push(Box::new(rename_term(*term, env)?));
            }
            
            Ok(BuiltinForm::CWithCapability {
                cap: Box::new(renamed_cap),
                body: renamed_body,
            })
        }

        BuiltinForm::CSuspend(expr) => {
            let renamed_expr = rename_term(*expr, env)?;
            Ok(BuiltinForm::CSuspend(Box::new(renamed_expr)))
        }

        BuiltinForm::CEnforceOne { conditions } => {
            let mut renamed_conditions = Vec::new();
            for cond in conditions {
                renamed_conditions.push(Box::new(rename_term(*cond, env)?));
            }
            Ok(BuiltinForm::CEnforceOne {
                conditions: renamed_conditions,
            })
        }

        BuiltinForm::CCreateUserGuard { name, args } => {
            let renamed_name = rename_term(*name, env)?;
            let mut renamed_args = Vec::new();
            for arg in args {
                renamed_args.push(Box::new(rename_term(*arg, env)?));
            }
            Ok(BuiltinForm::CCreateUserGuard {
                name: Box::new(renamed_name),
                args: renamed_args,
            })
        }

        BuiltinForm::CTry { expr, handler } => {
            let renamed_expr = rename_term(*expr, env)?;
            let renamed_handler = rename_term(*handler, env)?;
            Ok(BuiltinForm::CTry {
                expr: Box::new(renamed_expr),
                handler: Box::new(renamed_handler),
            })
        }

        BuiltinForm::CMap { func, list } => {
            let renamed_func = rename_term(*func, env)?;
            let renamed_list = rename_term(*list, env)?;
            Ok(BuiltinForm::CMap {
                func: Box::new(renamed_func),
                list: Box::new(renamed_list),
            })
        }

        BuiltinForm::CFilter { func, list } => {
            let renamed_func = rename_term(*func, env)?;
            let renamed_list = rename_term(*list, env)?;
            Ok(BuiltinForm::CFilter {
                func: Box::new(renamed_func),
                list: Box::new(renamed_list),
            })
        }

        BuiltinForm::CFold { func, init, list } => {
            let renamed_func = rename_term(*func, env)?;
            let renamed_init = rename_term(*init, env)?;
            let renamed_list = rename_term(*list, env)?;
            Ok(BuiltinForm::CFold {
                func: Box::new(renamed_func),
                init: Box::new(renamed_init),
                list: Box::new(renamed_list),
            })
        }

        BuiltinForm::CZip { func, list1, list2 } => {
            let renamed_func = rename_term(*func, env)?;
            let renamed_list1 = rename_term(*list1, env)?;
            let renamed_list2 = rename_term(*list2, env)?;
            Ok(BuiltinForm::CZip {
                func: Box::new(renamed_func),
                list1: Box::new(renamed_list1),
                list2: Box::new(renamed_list2),
            })
        }

        BuiltinForm::CCond { conditions } => {
            let mut renamed_conditions = Vec::new();
            for (cond, expr) in conditions {
                let renamed_cond = rename_term(*cond, env)?;
                let renamed_expr = rename_term(*expr, env)?;
                renamed_conditions.push((Box::new(renamed_cond), Box::new(renamed_expr)));
            }
            Ok(BuiltinForm::CCond {
                conditions: renamed_conditions,
            })
        }
    }
}

/// Rename a top-level item
pub fn rename_top_level(
    top_level: TopLevel<ParsedName, Type, CoreBuiltin, SpanInfo>,
    env: &mut RenamerEnv,
) -> Result<CoreTopLevel, PactError<SpanInfo>> {
    match top_level {
        TopLevel::TLModule(module) => {
            let renamed_module = rename_module(module, env)?;
            Ok(TopLevel::TLModule(renamed_module))
        }
        TopLevel::TLInterface(interface) => {
            let renamed_interface = rename_interface(interface, env)?;
            Ok(TopLevel::TLInterface(renamed_interface))
        }
        TopLevel::TLTerm(term) => {
            let renamed_term = rename_term(term, env)?;
            Ok(TopLevel::TLTerm(renamed_term))
        }
        TopLevel::TLUse(import) => {
            Ok(TopLevel::TLUse(import))
        }
    }
}

/// Rename a module
fn rename_module(
    module: Module<ParsedName, Type, CoreBuiltin, SpanInfo>,
    env: &mut RenamerEnv,
) -> Result<Module<Name, Type, CoreBuiltin, SpanInfo>, PactError<SpanInfo>> {
    // Set module context
    let module_name = ModuleName {
        name: module.name.clone(),
        namespace: None,
    };
    env.set_current_module(module_name);

    // Rename definitions
    let mut renamed_definitions = Vec::new();
    for def in module.definitions {
        renamed_definitions.push(rename_def(def, env)?);
    }

    Ok(Module {
        name: module.name,
        governance: module.governance,
        definitions: renamed_definitions,
        imports: module.imports,
        annotations: module.annotations,
        info: module.info,
    })
}

/// Rename a definition
fn rename_def(
    def: Def<ParsedName, Type, CoreBuiltin, SpanInfo>,
    env: &mut RenamerEnv,
) -> Result<Def<Name, Type, CoreBuiltin, SpanInfo>, PactError<SpanInfo>> {
    match def {
        Def::Dfun(defun) => {
            let renamed_defun = rename_defun(defun, env)?;
            Ok(Def::Dfun(renamed_defun))
        }
        Def::DConst(defconst) => {
            let renamed_defconst = rename_defconst(defconst, env)?;
            Ok(Def::DConst(renamed_defconst))
        }
        Def::DCap(defcap) => {
            let renamed_defcap = rename_defcap(defcap, env)?;
            Ok(Def::DCap(renamed_defcap))
        }
        Def::DSchema(defschema) => {
            Ok(Def::DSchema(defschema)) // No renaming needed for schemas
        }
        Def::DTable(deftable) => {
            Ok(Def::DTable(deftable)) // No renaming needed for tables
        }
        Def::DPact(defpact) => {
            let renamed_defpact = rename_defpact(defpact, env)?;
            Ok(Def::DPact(renamed_defpact))
        }
    }
}

/// Rename a function definition
fn rename_defun(
    defun: Defun<ParsedName, Type, CoreBuiltin, SpanInfo>,
    env: &mut RenamerEnv,
) -> Result<Defun<Name, Type, CoreBuiltin, SpanInfo>, PactError<SpanInfo>> {
    let original_depth = env.var_depth;
    
    // Enter function parameter scope
    env.enter_lambda_scope(&defun.args);
    
    // Rename function body
    let renamed_body = rename_term(defun.body, env)?;
    
    // Exit scope
    env.exit_scope(original_depth);
    
    Ok(Defun {
        name: defun.name,
        args: defun.args,
        body: renamed_body,
        annotations: defun.annotations,
        info: defun.info,
    })
}

/// Rename a constant definition
fn rename_defconst(
    defconst: DefConst<ParsedName, Type, CoreBuiltin, SpanInfo>,
    env: &mut RenamerEnv,
) -> Result<DefConst<Name, Type, CoreBuiltin, SpanInfo>, PactError<SpanInfo>> {
    let renamed_value = rename_term(defconst.value, env)?;
    
    Ok(DefConst {
        name: defconst.name,
        value: renamed_value,
        doc: defconst.doc,
        info: defconst.info,
    })
}

/// Rename a capability definition
fn rename_defcap(
    defcap: DefCap<ParsedName, Type, CoreBuiltin, SpanInfo>,
    env: &mut RenamerEnv,
) -> Result<DefCap<Name, Type, CoreBuiltin, SpanInfo>, PactError<SpanInfo>> {
    let original_depth = env.var_depth;
    
    // Enter capability parameter scope
    env.enter_lambda_scope(&defcap.args);
    
    // Rename capability body
    let renamed_body = rename_term(defcap.body, env)?;
    
    // Exit scope
    env.exit_scope(original_depth);
    
    Ok(DefCap {
        name: defcap.name,
        args: defcap.args,
        body: renamed_body,
        meta: defcap.meta,
        annotations: defcap.annotations,
        info: defcap.info,
    })
}

/// Rename a defpact definition
fn rename_defpact(
    defpact: DefPact<ParsedName, Type, CoreBuiltin, SpanInfo>,
    env: &mut RenamerEnv,
) -> Result<DefPact<Name, Type, CoreBuiltin, SpanInfo>, PactError<SpanInfo>> {
    let original_depth = env.var_depth;
    
    // Enter defpact parameter scope
    env.enter_lambda_scope(&defpact.args);
    
    // Rename defpact steps
    let mut renamed_steps = Vec::new();
    for step in defpact.steps {
        let renamed_step = rename_pact_step(step, env)?;
        renamed_steps.push(renamed_step);
    }
    
    // Exit scope
    env.exit_scope(original_depth);
    
    Ok(DefPact {
        name: defpact.name,
        args: defpact.args,
        steps: renamed_steps,
        annotations: defpact.annotations,
        info: defpact.info,
    })
}

/// Rename a defpact step
fn rename_pact_step(
    step: PactStep<ParsedName, Type, CoreBuiltin, SpanInfo>,
    env: &mut RenamerEnv,
) -> Result<PactStep<Name, Type, CoreBuiltin, SpanInfo>, PactError<SpanInfo>> {
    match step {
        PactStep::Step { entity, expr } => {
            let renamed_entity = match entity {
                Some(e) => Some(rename_term(e, env)?),
                None => None,
            };
            let renamed_expr = rename_term(expr, env)?;
            Ok(PactStep::Step {
                entity: renamed_entity,
                expr: renamed_expr,
            })
        }
        PactStep::StepWithRollback { entity, expr, rollback } => {
            let renamed_entity = match entity {
                Some(e) => Some(rename_term(e, env)?),
                None => None,
            };
            let renamed_expr = rename_term(expr, env)?;
            let renamed_rollback = rename_term(rollback, env)?;
            Ok(PactStep::StepWithRollback {
                entity: renamed_entity,
                expr: renamed_expr,
                rollback: renamed_rollback,
            })
        }
    }
}

/// Rename an interface (stub implementation)
fn rename_interface(
    _interface: Interface<ParsedName, Type, CoreBuiltin, SpanInfo>,
    _env: &mut RenamerEnv,
) -> Result<Interface<Name, Type, CoreBuiltin, SpanInfo>, PactError<SpanInfo>> {
    // TODO: Implement interface renaming
    Err(PactError::PEExecutionError(
        EvalError::RuntimeError("Interface renaming not yet implemented".to_string()),
        vec![],
        SpanInfo { start: 0, end: 0 }
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_debruijn_index_calculation() {
        let mut env = RenamerEnv::new();
        
        // Test basic lambda binding
        let args = vec![Arg {
            name: "x".into(),
            ty: None,
            info: SpanInfo { start: 0, end: 0 },
        }];
        
        env.enter_lambda_scope(&args);
        
        // Should find 'x' at index 0
        let resolved = env.resolve_bare_name("x").unwrap();
        assert!(matches!(resolved.kind, NameKind::NBound(DeBruijnIndex(0))));
    }

    #[test]
    fn test_nested_lambda_scoping() {
        let mut env = RenamerEnv::new();
        
        // Outer lambda: (lambda (x) ...)
        let outer_args = vec![Arg {
            name: "x".into(),
            ty: None,
            info: SpanInfo { start: 0, end: 0 },
        }];
        env.enter_lambda_scope(&outer_args);
        
        // Inner lambda: (lambda (y) ...)
        let inner_args = vec![Arg {
            name: "y".into(),
            ty: None,
            info: SpanInfo { start: 0, end: 0 },
        }];
        env.enter_lambda_scope(&inner_args);
        
        // In inner scope: x should be at relative index 1, y at index 0
        let x_resolved = env.resolve_bare_name("x").unwrap();
        let y_resolved = env.resolve_bare_name("y").unwrap();
        
        assert!(matches!(x_resolved.kind, NameKind::NBound(DeBruijnIndex(1))));
        assert!(matches!(y_resolved.kind, NameKind::NBound(DeBruijnIndex(0))));
    }
}