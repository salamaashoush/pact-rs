//! Desugaring transformation from ParseTree to Core IR
//!
//! This module implements the critical transformation from parsed AST
//! to the Core IR Term representation, handling:
//! - Special form recognition and transformation
//! - Builtin function resolution and arity checking
//! - Name resolution and scoping
//! - DeBruijn index assignment for lambda-bound variables

use crate::term::*;
use crate::special_forms::SpecialForm;
use crate::builtin_forms::resolve_builtin_arity;
use pact_parser::ast::{
    ParsedExpr, ParsedTopLevel, ParsedModule, ParsedDef, ParsedDefun,
    ParsedDefConst, ParsedDefCap, DefSchema, DefTable, DefPact,
    Binder, LetForm, MArg, Literal as ParsedLiteral,
    Type as ParsedType, SpanInfo as ParsedSpanInfo,
    ParsedName as AstParsedName,
    ModuleName as AstModuleName,
    Field as AstField, PrimType as AstPrimType,
    Governance as AstGovernance, ExtDecl as AstExtDecl,
    Import as AstImport, PactAnn as AstPactAnn,
    PactDocType as AstPactDocType, PropertyExpr as AstPropertyExpr,
    PropKeyword as AstPropKeyword, PropDelim as AstPropDelim,
    ParsedTyName,
};
use pact_errors::{PactError, EvalError};
use std::collections::HashMap;
use compact_str::CompactString;

/// Desugaring result with dependency tracking
#[derive(Debug, Clone)]
pub struct DesugarOutput<T> {
    pub result: T,
    pub dependencies: std::collections::HashSet<ModuleName>,
}

impl<T> DesugarOutput<T> {
    pub fn new(result: T) -> Self {
        Self {
            result,
            dependencies: std::collections::HashSet::new(),
        }
    }

    pub fn with_dependency(mut self, dep: ModuleName) -> Self {
        self.dependencies.insert(dep);
        self
    }
}

/// Desugaring statistics
#[derive(Debug, Clone, Default)]
pub struct DesugarStats {
    pub terms_desugared: usize,
    pub special_forms_processed: usize,
    pub modules_processed: usize,
    pub interfaces_processed: usize,
    pub functions_desugared: usize,
    pub constants_desugared: usize,
    pub capabilities_desugared: usize,
}

/// Desugaring context for name resolution
#[derive(Debug, Clone)]
pub struct DesugarContext {
    /// Local variable scope for DeBruijn indices
    pub local_scope: Vec<CompactString>,
    /// Module imports and their aliases
    pub imports: HashMap<CompactString, ModuleName>,
    /// Native builtins (from the runtime environment)
    pub natives: HashMap<CompactString, CoreBuiltin>,
    /// Current module context
    pub current_module: Option<ModuleName>,
    /// Statistics tracking
    pub stats: DesugarStats,
}

impl DesugarContext {
    pub fn new() -> Self {
        Self {
            local_scope: Vec::new(),
            imports: HashMap::new(),
            natives: Self::default_natives(),
            current_module: None,
            stats: DesugarStats::default(),
        }
    }

    /// Default native builtins available in all contexts
    fn default_natives() -> HashMap<CompactString, CoreBuiltin> {
        let mut natives = HashMap::new();
        
        // Add core arithmetic
        natives.insert("+".into(), CoreBuiltin::CoreAdd);
        natives.insert("-".into(), CoreBuiltin::CoreSub);
        natives.insert("*".into(), CoreBuiltin::CoreMultiply);
        natives.insert("/".into(), CoreBuiltin::CoreDivide);
        natives.insert("=".into(), CoreBuiltin::CoreEq);
        natives.insert("<".into(), CoreBuiltin::CoreLT);
        natives.insert(">".into(), CoreBuiltin::CoreGT);
        natives.insert("<=".into(), CoreBuiltin::CoreLEQ);
        natives.insert(">=".into(), CoreBuiltin::CoreGEQ);
        
        // Add core list operations
        natives.insert("map".into(), CoreBuiltin::CoreMap);
        natives.insert("filter".into(), CoreBuiltin::CoreFilter);
        natives.insert("fold".into(), CoreBuiltin::CoreFold);
        natives.insert("length".into(), CoreBuiltin::CoreLength);
        
        // Add core database operations
        natives.insert("read".into(), CoreBuiltin::CoreRead);
        natives.insert("insert".into(), CoreBuiltin::CoreInsert);
        natives.insert("update".into(), CoreBuiltin::CoreUpdate);
        natives.insert("select".into(), CoreBuiltin::CoreSelect);
        
        natives
    }

    /// Push a new variable to the local scope
    pub fn push_var(&mut self, name: CompactString) {
        self.local_scope.push(name);
    }

    /// Pop a variable from the local scope
    pub fn pop_var(&mut self) -> Option<CompactString> {
        self.local_scope.pop()
    }

    /// Look up a variable in the local scope, returning DeBruijn index
    pub fn lookup_local(&self, name: &str) -> Option<usize> {
        self.local_scope.iter().rposition(|n| n == name)
    }

    /// Look up a native builtin
    pub fn lookup_native(&self, name: &str) -> Option<CoreBuiltin> {
        self.natives.get(name).copied()
    }

    /// Add an import to the context
    pub fn add_import(&mut self, alias: CompactString, module: ModuleName) {
        self.imports.insert(alias, module);
    }

    /// Track a desugared term
    pub fn track_term(&mut self) {
        self.stats.terms_desugared += 1;
    }

    /// Track a processed special form
    pub fn track_special_form(&mut self) {
        self.stats.special_forms_processed += 1;
    }

    /// Track a processed module
    pub fn track_module(&mut self) {
        self.stats.modules_processed += 1;
    }

    /// Track a processed interface
    pub fn track_interface(&mut self) {
        self.stats.interfaces_processed += 1;
    }

    /// Track a desugared function
    pub fn track_function(&mut self) {
        self.stats.functions_desugared += 1;
    }

    /// Track a desugared constant
    pub fn track_constant(&mut self) {
        self.stats.constants_desugared += 1;
    }

    /// Track a desugared capability
    pub fn track_capability(&mut self) {
        self.stats.capabilities_desugared += 1;
    }

    /// Get current statistics
    pub fn get_stats(&self) -> &DesugarStats {
        &self.stats
    }
}

impl Default for DesugarContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Main desugaring function for top-level items
pub fn desugar_top_level(
    top_level: ParsedTopLevel<ParsedSpanInfo>,
    ctx: &mut DesugarContext,
) -> Result<DesugarOutput<CoreTopLevel>, PactError<SpanInfo>> {
    match top_level {
        ParsedTopLevel::TLModule(module) => {
            let result = desugar_module(module, ctx)?;
            ctx.track_module();
            Ok(DesugarOutput {
                result: TopLevel::TLModule(result.result),
                dependencies: result.dependencies,
            })
        }
        ParsedTopLevel::TLInterface(interface) => {
            let result = desugar_interface(interface, ctx)?;
            ctx.track_interface();
            Ok(DesugarOutput {
                result: TopLevel::TLInterface(result.result),
                dependencies: result.dependencies,
            })
        }
        ParsedTopLevel::TLTerm(expr) => {
            let result = desugar_expr(expr, ctx)?;
            ctx.track_term();
            Ok(DesugarOutput {
                result: TopLevel::TLTerm(result.result),
                dependencies: result.dependencies,
            })
        }
        ParsedTopLevel::TLUse(import) => {
            let result = desugar_import(import, ctx)?;
            Ok(DesugarOutput {
                result: TopLevel::TLUse(result.result),
                dependencies: result.dependencies,
            })
        }
    }
}

/// Desugar a module
pub fn desugar_module(
    module: ParsedModule<ParsedSpanInfo>,
    ctx: &mut DesugarContext,
) -> Result<DesugarOutput<Module<Name, Type, CoreBuiltin, SpanInfo>>, PactError<SpanInfo>> {
    // Set current module context
    let module_name = ModuleName {
        name: module.name.clone(),
        namespace: None, // TODO: handle namespaces
    };
    ctx.current_module = Some(module_name.clone());

    // Process imports first
    let mut imports = Vec::new();
    let mut dependencies = std::collections::HashSet::new();
    
    for import in module.imports {
        let import_result = desugar_ext_decl(import, ctx)?;
        if let ExtDecl::ExtImport(imp) = &import_result.result {
            dependencies.insert(imp.module.clone());
        }
        imports.push(import_result.result);
        dependencies.extend(import_result.dependencies);
    }

    // Process definitions
    let mut definitions = Vec::new();
    for def in module.definitions {
        let def_result = desugar_def(def, ctx)?;
        definitions.push(def_result.result);
        dependencies.extend(def_result.dependencies);
    }

    // Convert annotations
    let annotations = module.annotations.into_iter()
        .map(|ann| desugar_pact_ann(ann))
        .collect::<Result<Vec<_>, _>>()?;

    let result = Module {
        name: module.name,
        governance: desugar_governance(module.governance),
        definitions,
        imports,
        annotations,
        info: desugar_span_info(module.info),
    };

    Ok(DesugarOutput {
        result,
        dependencies,
    })
}

/// Desugar an interface
/// 
/// Following Haskell implementation exactly:
/// 1. Convert interface name to ModuleName 
/// 2. Process imports and track dependencies
/// 3. Desugar interface definitions
/// 4. Create Interface with placeholder hashes
pub fn desugar_interface(
    interface: pact_parser::ast::ParsedInterface<ParsedSpanInfo>,
    ctx: &mut DesugarContext,
) -> Result<DesugarOutput<Interface<Name, Type, CoreBuiltin, SpanInfo>>, PactError<SpanInfo>> {
    // Set current module context
    let interface_name = ModuleName {
        name: interface.name.clone(),
        namespace: None, // TODO: handle namespaces
    };
    ctx.current_module = Some(interface_name.clone());

    // Process imports first  
    let mut imports = Vec::new();
    let mut dependencies = std::collections::HashSet::new();
    
    for import in interface.imports {
        let import_result = desugar_import(import, ctx)?;
        dependencies.insert(import_result.result.module.clone());
        imports.push(import_result.result);
        dependencies.extend(import_result.dependencies);
    }

    // Process interface definitions
    let mut definitions = Vec::new();
    for def in interface.definitions {
        let def_result = desugar_if_def(def, ctx)?;
        definitions.push(def_result.result);
        dependencies.extend(def_result.dependencies);
    }

    // Convert annotations
    let annotations = interface.annotations.into_iter()
        .map(|ann| desugar_pact_ann(ann))
        .collect::<Result<Vec<_>, _>>()?;

    // Create interface with placeholder hashes (matching Haskell)
    let result = Interface {
        name: interface.name,
        definitions,
        imports,
        annotations,
        info: desugar_span_info(interface.info),
    };

    Ok(DesugarOutput {
        result,
        dependencies,
    })
}

/// Desugar interface definitions
/// 
/// Following Haskell desugarIfDef implementation
fn desugar_if_def(
    def: pact_parser::ast::ParsedIfDef<ParsedSpanInfo>,
    ctx: &mut DesugarContext,
) -> Result<DesugarOutput<IfDef<Name, Type, CoreBuiltin, SpanInfo>>, PactError<SpanInfo>> {
    use pact_parser::ast::ParsedIfDef;
    
    match def {
        ParsedIfDef::IfDfun(if_defun) => {
            let result = IfDef::IfDfun(IfDefun {
                name: desugar_arg(if_defun.name),
                args: if_defun.args.into_iter().map(desugar_arg).collect(),
                annotations: if_defun.annotations.into_iter()
                    .map(desugar_pact_ann)
                    .collect::<Result<Vec<_>, _>>()?,
                info: desugar_span_info(if_defun.info),
            });
            Ok(DesugarOutput::new(result))
        }
        ParsedIfDef::IfDConst(def_const) => {
            let const_result = desugar_defconst(def_const, ctx)?;
            Ok(DesugarOutput {
                result: IfDef::IfDConst(const_result.result),
                dependencies: const_result.dependencies,
            })
        }
        ParsedIfDef::IfDCap(if_defcap) => {
            // For capabilities, we need to handle meta (managed/unmanaged)
            let meta = if_defcap.meta.map(|m| desugar_dcap_meta(m)).transpose()?;
            
            let result = IfDef::IfDCap(IfDefCap {
                name: desugar_arg(if_defcap.name),
                args: if_defcap.args.into_iter().map(desugar_arg).collect(),
                meta,
                annotations: if_defcap.annotations.into_iter()
                    .map(desugar_pact_ann)
                    .collect::<Result<Vec<_>, _>>()?,
                info: desugar_span_info(if_defcap.info),
            });
            Ok(DesugarOutput::new(result))
        }
        ParsedIfDef::IfDSchema(def_schema) => {
            let schema_result = desugar_defschema(def_schema, ctx)?;
            Ok(DesugarOutput {
                result: IfDef::IfDSchema(schema_result.result),
                dependencies: schema_result.dependencies,
            })
        }
        ParsedIfDef::IfDPact(if_defpact) => {
            let result = IfDef::IfDPact(IfDefPact {
                name: desugar_arg(if_defpact.name),
                args: if_defpact.args.into_iter().map(desugar_arg).collect(),
                annotations: if_defpact.annotations.into_iter()
                    .map(desugar_pact_ann)
                    .collect::<Result<Vec<_>, _>>()?,
                info: desugar_span_info(if_defpact.info),
            });
            Ok(DesugarOutput::new(result))
        }
    }
}

/// Desugar an import
pub fn desugar_import(
    import: AstImport<ParsedSpanInfo>,
    ctx: &mut DesugarContext,
) -> Result<DesugarOutput<Import<SpanInfo>>, PactError<SpanInfo>> {
    let module_name = desugar_module_name(import.module);
    
    // Add to context
    ctx.add_import(module_name.name.clone(), module_name.clone());
    
    let result = Import {
        module: module_name.clone(),
        hash: import.hash,
        imports: import.imports,
        info: desugar_span_info(import.info),
    };

    Ok(DesugarOutput::new(result).with_dependency(module_name))
}

/// Desugar a definition
pub fn desugar_def(
    def: ParsedDef<ParsedSpanInfo>,
    ctx: &mut DesugarContext,
) -> Result<DesugarOutput<Def<Name, Type, CoreBuiltin, SpanInfo>>, PactError<SpanInfo>> {
    match def {
        ParsedDef::Dfun(defun) => {
            let result = desugar_defun(defun, ctx)?;
            Ok(DesugarOutput {
                result: Def::Dfun(result.result),
                dependencies: result.dependencies,
            })
        }
        ParsedDef::DConst(defconst) => {
            let result = desugar_defconst(defconst, ctx)?;
            Ok(DesugarOutput {
                result: Def::DConst(result.result),
                dependencies: result.dependencies,
            })
        }
        ParsedDef::DCap(defcap) => {
            let result = desugar_defcap(defcap, ctx)?;
            Ok(DesugarOutput {
                result: Def::DCap(result.result),
                dependencies: result.dependencies,
            })
        }
        ParsedDef::DSchema(defschema) => {
            let result = desugar_defschema(defschema, ctx)?;
            Ok(DesugarOutput {
                result: Def::DSchema(result.result),
                dependencies: result.dependencies,
            })
        }
        ParsedDef::DTable(deftable) => {
            let result = desugar_deftable(deftable, ctx)?;
            Ok(DesugarOutput {
                result: Def::DTable(result.result),
                dependencies: result.dependencies,
            })
        }
        ParsedDef::DPact(defpact) => {
            let result = desugar_defpact(defpact, ctx)?;
            Ok(DesugarOutput {
                result: Def::DPact(result.result),
                dependencies: result.dependencies,
            })
        }
    }
}

/// Desugar a function definition
pub fn desugar_defun(
    defun: ParsedDefun<ParsedSpanInfo>,
    ctx: &mut DesugarContext,
) -> Result<DesugarOutput<Defun<Name, Type, CoreBuiltin, SpanInfo>>, PactError<SpanInfo>> {
    // Save current scope
    let saved_scope_len = ctx.local_scope.len();
    
    // Add function arguments to scope
    for arg in &defun.args {
        ctx.push_var(arg.name.clone());
    }

    // Desugar function body
    let body = if defun.body.len() == 1 {
        let body_result = desugar_expr(defun.body.into_iter().next().unwrap(), ctx)?;
        body_result.result
    } else {
        // Multiple expressions - create a sequence
        let mut terms = Vec::new();
        let mut deps = std::collections::HashSet::new();
        for expr in defun.body {
            let expr_result = desugar_expr(expr, ctx)?;
            terms.push(expr_result.result);
            deps.extend(expr_result.dependencies);
        }
        
        // Build right-associative sequence
        let mut result = terms.pop().unwrap();
        while let Some(term) = terms.pop() {
            result = Term::Sequence {
                first: Box::new(term),
                second: Box::new(result),
                info: SpanInfo { start: 0, end: 0 }, // TODO: proper span info
            };
        }
        result
    };

    // Restore scope
    ctx.local_scope.truncate(saved_scope_len);

    let result = Defun {
        name: desugar_arg(defun.name),
        args: defun.args.into_iter().map(desugar_arg).collect(),
        body,
        annotations: defun.annotations.into_iter()
            .map(desugar_pact_ann)
            .collect::<Result<Vec<_>, _>>()?,
        info: desugar_span_info(defun.info),
    };

    ctx.track_function();
    Ok(DesugarOutput::new(result))
}

/// Desugar a constant definition
pub fn desugar_defconst(
    defconst: ParsedDefConst<ParsedSpanInfo>,
    ctx: &mut DesugarContext,
) -> Result<DesugarOutput<DefConst<Name, Type, CoreBuiltin, SpanInfo>>, PactError<SpanInfo>> {
    let value_result = desugar_expr(defconst.value, ctx)?;
    
    let result = DefConst {
        name: desugar_arg(defconst.name),
        value: value_result.result,
        doc: defconst.doc.map(|(s, t)| (s, desugar_pact_doc_type(t))),
        info: desugar_span_info(defconst.info),
    };

    ctx.track_constant();
    Ok(DesugarOutput {
        result,
        dependencies: value_result.dependencies,
    })
}

/// Desugar expressions - the core transformation logic
pub fn desugar_expr(
    expr: ParsedExpr<ParsedSpanInfo>,
    ctx: &mut DesugarContext,
) -> Result<DesugarOutput<CoreTerm>, PactError<SpanInfo>> {
    // Track that we're desugaring a term
    ctx.track_term();
    
    match expr {
        ParsedExpr::Var(name, info) => {
            desugar_var(name, info, ctx)
        }
        ParsedExpr::Constant(lit, info) => {
            Ok(DesugarOutput::new(Term::Constant(
                desugar_literal(lit),
                desugar_span_info(info)
            )))
        }
        ParsedExpr::List(elements, info) => {
            desugar_list(elements, info, ctx)
        }
        ParsedExpr::Object(fields, info) => {
            desugar_object(fields, info, ctx)
        }
        ParsedExpr::App { func, args, info } => {
            desugar_app(*func, args, info, ctx)
        }
        ParsedExpr::Let { form, bindings, body, info } => {
            desugar_let(form, bindings, body, info, ctx)
        }
        ParsedExpr::Lam { args, body, info } => {
            desugar_lambda(args, body, info, ctx)
        }
        ParsedExpr::If { cond, then_expr, else_expr, info } => {
            desugar_if(*cond, *then_expr, else_expr.map(|boxed| *boxed), info, ctx)
        }
        ParsedExpr::WithCapability { cap, body, info } => {
            desugar_with_capability(*cap, body, info, ctx)
        }
        ParsedExpr::TypeAnn { expr, ty: _, info: _ } => {
            // For now, just desugar the expression and ignore the type annotation
            desugar_expr(*expr, ctx)
        }
        ParsedExpr::Suspend { expr, info } => {
            let expr_result = desugar_expr(*expr, ctx)?;
            Ok(DesugarOutput {
                result: Term::BuiltinForm {
                    form: BuiltinForm::CSuspend(Box::new(expr_result.result)),
                    info: desugar_span_info(info),
                },
                dependencies: expr_result.dependencies,
            })
        }
        // TODO: Implement remaining expression types
        ParsedExpr::Binding { info, .. } => {
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError("Binding expressions not yet implemented".to_string()),
                vec![],
                desugar_span_info(info)
            ))
        }
        ParsedExpr::And { left, right, info } => {
            let left_result = desugar_expr(*left, ctx)?;
            let right_result = desugar_expr(*right, ctx)?;
            let mut deps = left_result.dependencies;
            deps.extend(right_result.dependencies);
            
            Ok(DesugarOutput {
                result: Term::BuiltinForm {
                    form: BuiltinForm::CAnd(Box::new(left_result.result), Box::new(right_result.result)),
                    info: desugar_span_info(info),
                },
                dependencies: deps,
            })
        }
        ParsedExpr::Or { left, right, info } => {
            let left_result = desugar_expr(*left, ctx)?;
            let right_result = desugar_expr(*right, ctx)?;
            let mut deps = left_result.dependencies;
            deps.extend(right_result.dependencies);
            
            Ok(DesugarOutput {
                result: Term::BuiltinForm {
                    form: BuiltinForm::COr(Box::new(left_result.result), Box::new(right_result.result)),
                    info: desugar_span_info(info),
                },
                dependencies: deps,
            })
        }
        ParsedExpr::Cond { info, .. } => {
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError("Cond expressions not yet implemented".to_string()),
                vec![],
                desugar_span_info(info)
            ))
        }
        ParsedExpr::Enforce { cond, msg, info } => {
            let cond_result = desugar_expr(*cond, ctx)?;
            let msg_result = desugar_expr(*msg, ctx)?;
            let mut deps = cond_result.dependencies;
            deps.extend(msg_result.dependencies);
            
            Ok(DesugarOutput {
                result: Term::BuiltinForm {
                    form: BuiltinForm::CEnforce {
                        cond: Box::new(cond_result.result),
                        msg: Box::new(msg_result.result),
                    },
                    info: desugar_span_info(info),
                },
                dependencies: deps,
            })
        }
        ParsedExpr::EnforceOne { info, .. } => {
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError("EnforceOne expressions not yet implemented".to_string()),
                vec![],
                desugar_span_info(info)
            ))
        }
        ParsedExpr::RequireCapability { info, .. } => {
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError("RequireCapability expressions not yet implemented".to_string()),
                vec![],
                desugar_span_info(info)
            ))
        }
        ParsedExpr::ComposeCapability { info, .. } => {
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError("ComposeCapability expressions not yet implemented".to_string()),
                vec![],
                desugar_span_info(info)
            ))
        }
        ParsedExpr::InstallCapability { info, .. } => {
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError("InstallCapability expressions not yet implemented".to_string()),
                vec![],
                desugar_span_info(info)
            ))
        }
        ParsedExpr::EmitEvent { info, .. } => {
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError("EmitEvent expressions not yet implemented".to_string()),
                vec![],
                desugar_span_info(info)
            ))
        }
        ParsedExpr::CreateUserGuard { info, .. } => {
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError("CreateUserGuard expressions not yet implemented".to_string()),
                vec![],
                desugar_span_info(info)
            ))
        }
        ParsedExpr::Try { info, .. } => {
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError("Try expressions not yet implemented".to_string()),
                vec![],
                desugar_span_info(info)
            ))
        }
        ParsedExpr::Yield { info, .. } => {
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError("Yield expressions not yet implemented".to_string()),
                vec![],
                desugar_span_info(info)
            ))
        }
        ParsedExpr::Resume { info, .. } => {
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError("Resume expressions not yet implemented".to_string()),
                vec![],
                desugar_span_info(info)
            ))
        }
        
        // Definition expressions are handled as top-level terms
        ParsedExpr::DefConst(boxed_def) => {
            let info = boxed_def.info;
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError("DefConst expressions should be handled as top-level terms".to_string()),
                vec![],
                desugar_span_info(info)
            ))
        }
        ParsedExpr::DefFun(boxed_def) => {
            let info = boxed_def.info;
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError("DefFun expressions should be handled as top-level terms".to_string()),
                vec![],
                desugar_span_info(info)
            ))
        }
        ParsedExpr::DefCap(boxed_def) => {
            let info = boxed_def.info;
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError("DefCap expressions should be handled as top-level terms".to_string()),
                vec![],
                desugar_span_info(info)
            ))
        }
        ParsedExpr::DefSchema(boxed_def) => {
            let info = boxed_def.info;
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError("DefSchema expressions should be handled as top-level terms".to_string()),
                vec![],
                desugar_span_info(info)
            ))
        }
        ParsedExpr::DefTable(boxed_def) => {
            let info = boxed_def.info;
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError("DefTable expressions should be handled as top-level terms".to_string()),
                vec![],
                desugar_span_info(info)
            ))
        }
        ParsedExpr::DefPact(boxed_def) => {
            let info = boxed_def.info;
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError("DefPact expressions should be handled as top-level terms".to_string()),
                vec![],
                desugar_span_info(info)
            ))
        }
    }
}

/// Desugar variable references
fn desugar_var(
    name: AstParsedName,
    info: ParsedSpanInfo,
    ctx: &mut DesugarContext,
) -> Result<DesugarOutput<CoreTerm>, PactError<SpanInfo>> {
    let span_info = desugar_span_info(info);
    
    match &name {
        AstParsedName::BN(bare_name) => {
            let name_str = &bare_name.0;
            
            // Check if it's a locally bound variable (DeBruijn index)
            if let Some(index) = ctx.lookup_local(name_str) {
                return Ok(DesugarOutput::new(Term::Var(
                    Name::DeBruijn(DeBruijnIndex(index)),
                    span_info
                )));
            }
            
            // Check if it's a native builtin
            if let Some(builtin) = ctx.lookup_native(name_str) {
                return Ok(DesugarOutput::new(Term::Builtin(builtin, span_info)));
            }
            
            // Regular variable reference
            Ok(DesugarOutput::new(Term::Var(
                Name::Parsed(desugar_parsed_name(name)),
                span_info
            )))
        }
        _ => {
            // Qualified names, dynamic names, etc.
            Ok(DesugarOutput::new(Term::Var(
                Name::Parsed(desugar_parsed_name(name)),
                span_info
            )))
        }
    }
}

/// Desugar function application with special form handling
fn desugar_app(
    func: ParsedExpr<ParsedSpanInfo>,
    args: Vec<ParsedExpr<ParsedSpanInfo>>,
    info: ParsedSpanInfo,
    ctx: &mut DesugarContext,
) -> Result<DesugarOutput<CoreTerm>, PactError<SpanInfo>> {
    // Check if this is a special form
    if let ParsedExpr::Var(AstParsedName::BN(bare_name), _) = &func {
        let func_name = &bare_name.0;
        
        // Check for special forms
        if let Some(special_form) = SpecialForm::from_str(func_name) {
            return desugar_special_form(special_form, args, info, ctx);
        }
        
        // Check for builtin functions with arity resolution
        if let Some(builtin) = resolve_builtin_arity(func_name, args.len()) {
            let mut desugar_args = Vec::new();
            let mut deps = std::collections::HashSet::new();
            
            for arg_expr in args {
                let arg_result = desugar_expr(arg_expr, ctx)?;
                desugar_args.push(arg_result.result);
                deps.extend(arg_result.dependencies);
            }
            
            return Ok(DesugarOutput {
                result: Term::App {
                    func: Box::new(Term::Builtin(builtin, desugar_span_info(info))),
                    args: desugar_args,
                    info: desugar_span_info(info),
                },
                dependencies: deps,
            });
        }
    }
    
    // Regular function application
    let func_result = desugar_expr(func, ctx)?;
    let mut desugar_args = Vec::new();
    let mut deps = func_result.dependencies;
    
    for arg_expr in args {
        let arg_result = desugar_expr(arg_expr, ctx)?;
        desugar_args.push(arg_result.result);
        deps.extend(arg_result.dependencies);
    }
    
    Ok(DesugarOutput {
        result: Term::App {
            func: Box::new(func_result.result),
            args: desugar_args,
            info: desugar_span_info(info),
        },
        dependencies: deps,
    })
}

/// Desugar special forms into BuiltinForm
fn desugar_special_form(
    form: SpecialForm,
    args: Vec<ParsedExpr<ParsedSpanInfo>>,
    info: ParsedSpanInfo,
    ctx: &mut DesugarContext,
) -> Result<DesugarOutput<CoreTerm>, PactError<SpanInfo>> {
    let span_info = desugar_span_info(info);
    
    match form {
        SpecialForm::SFAnd => {
            if args.len() != 2 {
                return Err(PactError::PEExecutionError(
                    EvalError::ArgumentCountMismatch { 
                        function: "and".to_string(),
                        expected: 2, 
                        received: args.len() 
                    },
                    vec![],
                    span_info
                ));
            }
            let left_result = desugar_expr(args[0].clone(), ctx)?;
            let right_result = desugar_expr(args[1].clone(), ctx)?;
            let mut deps = left_result.dependencies;
            deps.extend(right_result.dependencies);
            
            Ok(DesugarOutput {
                result: Term::BuiltinForm {
                    form: BuiltinForm::CAnd(Box::new(left_result.result), Box::new(right_result.result)),
                    info: span_info,
                },
                dependencies: deps,
            })
        }
        SpecialForm::SFOr => {
            if args.len() != 2 {
                return Err(PactError::PEExecutionError(
                    EvalError::ArgumentCountMismatch { 
                        function: "or".to_string(),
                        expected: 2, 
                        received: args.len() 
                    },
                    vec![],
                    span_info
                ));
            }
            let left_result = desugar_expr(args[0].clone(), ctx)?;
            let right_result = desugar_expr(args[1].clone(), ctx)?;
            let mut deps = left_result.dependencies;
            deps.extend(right_result.dependencies);
            
            Ok(DesugarOutput {
                result: Term::BuiltinForm {
                    form: BuiltinForm::COr(Box::new(left_result.result), Box::new(right_result.result)),
                    info: span_info,
                },
                dependencies: deps,
            })
        }
        SpecialForm::SFIf => {
            if args.len() < 2 || args.len() > 3 {
                return Err(PactError::PEExecutionError(
                    EvalError::ArgumentCountMismatch { 
                        function: "if".to_string(),
                        expected: 3, 
                        received: args.len() 
                    },
                    vec![],
                    span_info
                ));
            }
            let cond_result = desugar_expr(args[0].clone(), ctx)?;
            let then_result = desugar_expr(args[1].clone(), ctx)?;
            let else_result = if args.len() == 3 {
                Some(desugar_expr(args[2].clone(), ctx)?)
            } else {
                None
            };
            
            let mut deps = cond_result.dependencies;
            deps.extend(then_result.dependencies);
            if let Some(ref else_res) = else_result {
                deps.extend(else_res.dependencies.clone());
            }
            
            Ok(DesugarOutput {
                result: Term::BuiltinForm {
                    form: BuiltinForm::CIf {
                        cond: Box::new(cond_result.result),
                        then_expr: Box::new(then_result.result),
                        else_expr: else_result.map(|r| Box::new(r.result)),
                    },
                    info: span_info,
                },
                dependencies: deps,
            })
        }
        SpecialForm::SFEnforce => {
            if args.len() != 2 {
                return Err(PactError::PEExecutionError(
                    EvalError::ArgumentCountMismatch { 
                        function: "enforce".to_string(),
                        expected: 2, 
                        received: args.len() 
                    },
                    vec![],
                    span_info
                ));
            }
            let cond_result = desugar_expr(args[0].clone(), ctx)?;
            let msg_result = desugar_expr(args[1].clone(), ctx)?;
            let mut deps = cond_result.dependencies;
            deps.extend(msg_result.dependencies);
            
            Ok(DesugarOutput {
                result: Term::BuiltinForm {
                    form: BuiltinForm::CEnforce {
                        cond: Box::new(cond_result.result),
                        msg: Box::new(msg_result.result),
                    },
                    info: span_info,
                },
                dependencies: deps,
            })
        }
        SpecialForm::SFWithCapability => {
            if args.len() < 2 {
                return Err(PactError::PEExecutionError(
                    EvalError::ArgumentCountMismatch { 
                        function: "with-capability".to_string(),
                        expected: 2, 
                        received: args.len() 
                    },
                    vec![],
                    span_info
                ));
            }
            let cap_result = desugar_expr(args[0].clone(), ctx)?;
            let mut body_results = Vec::new();
            let mut deps = cap_result.dependencies;
            
            for body_expr in args.into_iter().skip(1) {
                let body_result = desugar_expr(body_expr, ctx)?;
                deps.extend(body_result.dependencies);
                body_results.push(Box::new(body_result.result));
            }
            
            Ok(DesugarOutput {
                result: Term::BuiltinForm {
                    form: BuiltinForm::CWithCapability {
                        cap: Box::new(cap_result.result),
                        body: body_results,
                    },
                    info: span_info,
                },
                dependencies: deps,
            })
        }
        SpecialForm::SFDo => {
            // 'do' creates a sequence of expressions
            if args.is_empty() {
                return Err(PactError::PEExecutionError(
                    EvalError::ArgumentCountMismatch { 
                        function: "do".to_string(),
                        expected: 1, 
                        received: 0 
                    },
                    vec![],
                    span_info
                ));
            }
            
            let mut terms = Vec::new();
            let mut deps = std::collections::HashSet::new();
            for arg in args {
                let result = desugar_expr(arg, ctx)?;
                terms.push(result.result);
                deps.extend(result.dependencies);
            }
            
            // Build right-associative sequence
            let mut result = terms.pop().unwrap();
            while let Some(term) = terms.pop() {
                result = Term::Sequence {
                    first: Box::new(term),
                    second: Box::new(result),
                    info: span_info,
                };
            }
            
            Ok(DesugarOutput {
                result,
                dependencies: deps,
            })
        }
        _ => {
            // TODO: Implement other special forms
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError(format!("Special form {} not yet implemented", form)),
                vec![],
                span_info
            ))
        }
    }
}

// Helper functions for converting types

fn desugar_parsed_name(name: AstParsedName) -> ParsedName {
    match name {
        AstParsedName::BN(bare) => ParsedName::BN(BareName(bare.0)),
        AstParsedName::QN(qual) => ParsedName::QN(QualifiedName {
            name: qual.name,
            module: desugar_module_name(qual.module),
        }),
        AstParsedName::DN(dyn_name) => ParsedName::DN(DynamicName {
            name: dyn_name.name,
            field: dyn_name.field,
        }),
    }
}

fn desugar_module_name(name: AstModuleName) -> ModuleName {
    ModuleName {
        name: name.name,
        namespace: name.namespace.map(|ns| NamespaceName(ns.0)),
    }
}

fn desugar_literal(lit: ParsedLiteral) -> Literal {
    match lit {
        ParsedLiteral::LString(s) => Literal::LString(s),
        ParsedLiteral::LInteger(i) => Literal::LInteger(i),
        ParsedLiteral::LDecimal(d) => Literal::LDecimal(Decimal {
            precision: d.precision,
            mantissa: d.mantissa,
        }),
        ParsedLiteral::LBool(b) => Literal::LBool(b),
        ParsedLiteral::LUnit => Literal::LUnit,
    }
}

fn desugar_span_info(info: ParsedSpanInfo) -> SpanInfo {
    SpanInfo {
        start: info.start,
        end: info.end,
    }
}

fn desugar_arg(arg: MArg<ParsedSpanInfo>) -> Arg<Type, SpanInfo> {
    Arg {
        name: arg.name,
        ty: arg.ty.map(desugar_type),
        info: desugar_span_info(arg.info),
    }
}

fn desugar_type(ty: ParsedType) -> Type {
    match ty {
        ParsedType::TyPrim(prim) => Type::TyPrim(desugar_prim_type(prim)),
        ParsedType::TyList(inner) => Type::TyList(Box::new(desugar_type(*inner))),
        ParsedType::TyPolyList => Type::TyPolyList,
        ParsedType::TyModRef(modules) => Type::TyModRef(
            modules.into_iter().map(desugar_module_name).collect()
        ),
        ParsedType::TyKeyset => Type::TyKeyset,
        ParsedType::TyObject(name) => Type::TyObject(desugar_typed_name(name)),
        ParsedType::TyTable(name) => Type::TyTable(desugar_typed_name(name)),
        ParsedType::TyPolyObject => Type::TyPolyObject,
        ParsedType::TyAny => Type::TyAny,
    }
}

fn desugar_prim_type(prim: AstPrimType) -> PrimType {
    match prim {
        AstPrimType::PrimInt => PrimType::PrimInt,
        AstPrimType::PrimDecimal => PrimType::PrimDecimal,
        AstPrimType::PrimBool => PrimType::PrimBool,
        AstPrimType::PrimString => PrimType::PrimString,
        AstPrimType::PrimTime => PrimType::PrimTime,
        AstPrimType::PrimUnit => PrimType::PrimUnit,
        AstPrimType::PrimGuard => PrimType::PrimGuard,
    }
}

fn desugar_typed_name(name: ParsedTyName) -> TypedName {
    match name {
        ParsedTyName::TBN(bare) => TypedName::TBN(BareName(bare.0)),
        ParsedTyName::TQN(qual) => TypedName::TQN(QualifiedName {
            name: qual.name,
            module: desugar_module_name(qual.module),
        }),
    }
}

fn desugar_governance(gov: AstGovernance) -> Governance {
    match gov {
        AstGovernance::KeyGov(key) => Governance::KeyGov(key),
        AstGovernance::CapGov(cap) => Governance::CapGov(cap),
    }
}

fn desugar_ext_decl(decl: AstExtDecl<ParsedSpanInfo>, _ctx: &mut DesugarContext) -> Result<DesugarOutput<ExtDecl<SpanInfo>>, PactError<SpanInfo>> {
    match decl {
        AstExtDecl::ExtImport(import) => {
            let module_name = desugar_module_name(import.module.clone());
            Ok(DesugarOutput::new(ExtDecl::ExtImport(Import {
                module: module_name.clone(),
                hash: import.hash,
                imports: import.imports,
                info: desugar_span_info(import.info),
            })).with_dependency(module_name))
        }
        AstExtDecl::ExtImplements { module, info } => {
            let module_name = desugar_module_name(module.clone());
            Ok(DesugarOutput::new(ExtDecl::ExtImplements {
                module: module_name.clone(),
                info: desugar_span_info(info),
            }).with_dependency(module_name))
        }
        AstExtDecl::ExtBless { hash, info } => {
            Ok(DesugarOutput::new(ExtDecl::ExtBless {
                hash,
                info: desugar_span_info(info),
            }))
        }
    }
}

fn desugar_pact_ann(ann: AstPactAnn<ParsedSpanInfo>) -> Result<PactAnn<SpanInfo>, PactError<SpanInfo>> {
    match ann {
        AstPactAnn::PactDoc { ty, doc } => Ok(PactAnn::PactDoc {
            ty: desugar_pact_doc_type(ty),
            doc,
        }),
        AstPactAnn::PactModel(props) => {
            let converted_props = props.into_iter()
                .map(desugar_property_expr)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(PactAnn::PactModel(converted_props))
        }
    }
}

fn desugar_pact_doc_type(ty: AstPactDocType) -> PactDocType {
    match ty {
        AstPactDocType::PactDocAnn => PactDocType::PactDocAnn,
        AstPactDocType::PactDocString => PactDocType::PactDocString,
    }
}

fn desugar_property_expr(expr: AstPropertyExpr<ParsedSpanInfo>) -> Result<PropertyExpr<SpanInfo>, PactError<SpanInfo>> {
    match expr {
        AstPropertyExpr::PropAtom(name, info) => Ok(PropertyExpr::PropAtom(
            desugar_parsed_name(name),
            desugar_span_info(info)
        )),
        AstPropertyExpr::PropConstant(lit, info) => Ok(PropertyExpr::PropConstant(
            desugar_literal(lit),
            desugar_span_info(info)
        )),
        AstPropertyExpr::PropKeyword(kw, info) => Ok(PropertyExpr::PropKeyword(
            desugar_prop_keyword(kw),
            desugar_span_info(info)
        )),
        AstPropertyExpr::PropDelim(delim, info) => Ok(PropertyExpr::PropDelim(
            desugar_prop_delim(delim),
            desugar_span_info(info)
        )),
        AstPropertyExpr::PropSequence(seq, info) => {
            let converted_seq = seq.into_iter()
                .map(desugar_property_expr)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(PropertyExpr::PropSequence(
                converted_seq,
                desugar_span_info(info)
            ))
        }
    }
}

fn desugar_prop_keyword(kw: AstPropKeyword) -> PropKeyword {
    match kw {
        AstPropKeyword::KwLet => PropKeyword::KwLet,
        AstPropKeyword::KwLambda => PropKeyword::KwLambda,
    }
}

fn desugar_prop_delim(delim: AstPropDelim) -> PropDelim {
    match delim {
        AstPropDelim::DelimLBrace => PropDelim::DelimLBrace,
        AstPropDelim::DelimRBrace => PropDelim::DelimRBrace,
        AstPropDelim::DelimLBracket => PropDelim::DelimLBracket,
        AstPropDelim::DelimRBracket => PropDelim::DelimRBracket,
        AstPropDelim::DelimComma => PropDelim::DelimComma,
        AstPropDelim::DelimColon => PropDelim::DelimColon,
    }
}

// Stub implementations for remaining desugar functions
// These would be implemented following the same pattern

fn desugar_defcap(
    defcap: ParsedDefCap<ParsedSpanInfo>,
    ctx: &mut DesugarContext,
) -> Result<DesugarOutput<DefCap<Name, Type, CoreBuiltin, SpanInfo>>, PactError<SpanInfo>> {
    // Save current scope
    let saved_scope_len = ctx.local_scope.len();
    
    // Add function arguments to scope for body desugaring
    for arg in &defcap.args {
        ctx.push_var(arg.name.clone());
    }

    // Desugar capability body - following Haskell's nelToSequence pattern
    let body = if defcap.body.len() == 1 {
        let body_result = desugar_expr(defcap.body.into_iter().next().unwrap(), ctx)?;
        body_result.result
    } else {
        // Multiple expressions: create right-associative sequence
        let mut body_exprs: Vec<_> = defcap.body.into_iter().collect();
        let last_expr = body_exprs.pop().unwrap();
        let mut result = desugar_expr(last_expr, ctx)?.result;
        
        // Build sequence right-to-left: expr1 -> (expr2 -> (expr3 -> body))
        for expr in body_exprs.into_iter().rev() {
            let expr_result = desugar_expr(expr, ctx)?;
            result = Term::Sequence {
                first: Box::new(expr_result.result),
                second: Box::new(result),
                info: desugar_span_info(defcap.info),
            };
        }
        result
    };

    // Restore scope
    ctx.local_scope.truncate(saved_scope_len);

    // Desugar capability metadata following Haskell's desugarDefMeta
    let meta = match defcap.meta {
        Some(parsed_meta) => {
            desugar_dcap_meta_with_validation(parsed_meta, &defcap.args, defcap.info)?
        }
        None => Some(DCapMeta::DefManaged(None)), // Unmanaged capabilities
    };

    let result = DefCap {
        name: desugar_arg(defcap.name),
        args: defcap.args.into_iter().map(desugar_arg).collect(),
        body,
        annotations: defcap.annotations.into_iter()
            .map(desugar_pact_ann)
            .collect::<Result<Vec<_>, _>>()?,
        meta,
        info: desugar_span_info(defcap.info),
    };

    ctx.track_capability();
    Ok(DesugarOutput::new(result))
}

/// Desugar capability metadata with validation following Haskell's desugarDefMeta
fn desugar_dcap_meta_with_validation(
    meta: pact_parser::ast::DCapMeta,
    args: &[pact_parser::ast::MArg<ParsedSpanInfo>],
    info: ParsedSpanInfo,
) -> Result<Option<DCapMeta>, PactError<SpanInfo>> {
    use pact_parser::ast::DCapMeta as ParsedDCapMeta;
    
    match meta {
        ParsedDCapMeta::DefEvent => {
            Ok(Some(DCapMeta::DefEvent))
        }
        ParsedDCapMeta::DefManaged(managed_spec) => {
            match managed_spec {
                Some((arg_name, manager_name)) => {
                    // Find the argument in the capability's argument list
                    let arg_index = args.iter().position(|arg| arg.name == arg_name);
                    
                    match arg_index {
                        Some(_index) => {
                            // Create managed metadata with argument index and manager name
                            Ok(Some(DCapMeta::DefManaged(Some((
                                arg_name,
                                desugar_parsed_name(manager_name)
                            )))))
                        }
                        None => {
                            // Invalid managed argument - argument not found in capability args
                            Err(PactError::PEExecutionError(
                                EvalError::InvalidArgument(format!("Invalid managed argument '{}': not found in capability arguments", arg_name)),
                                vec![],
                                desugar_span_info(info)
                            ))
                        }
                    }
                }
                None => {
                    // Auto-managed capability
                    Ok(Some(DCapMeta::DefManaged(None)))
                }
            }
        }
    }
}

fn desugar_defschema(
    _defschema: DefSchema<ParsedSpanInfo>,
    _ctx: &mut DesugarContext,
) -> Result<DesugarOutput<crate::term::DefSchema<Type, SpanInfo>>, PactError<SpanInfo>> {
    Err(PactError::PEExecutionError(
        EvalError::RuntimeError("defschema desugaring not yet implemented".to_string()),
        vec![],
        SpanInfo { start: 0, end: 0 }
    ))
}

fn desugar_deftable(
    deftable: DefTable<ParsedSpanInfo>,
    _ctx: &mut DesugarContext,
) -> Result<DesugarOutput<crate::term::DefTable<SpanInfo>>, PactError<SpanInfo>> {
    let result = crate::term::DefTable {
        name: deftable.name,
        schema: desugar_parsed_name(deftable.schema),
        doc: deftable.doc.map(|(s, t)| (s, desugar_pact_doc_type(t))),
        info: desugar_span_info(deftable.info),
    };
    Ok(DesugarOutput::new(result))
}

fn desugar_defpact(
    defpact: DefPact<ParsedSpanInfo>,
    ctx: &mut DesugarContext,
) -> Result<DesugarOutput<crate::term::DefPact<Name, Type, CoreBuiltin, SpanInfo>>, PactError<SpanInfo>> {
    // Following Haskell implementation exactly:
    // 1. Check for entity prohibition in steps
    // 2. Desugar each step (regular or with rollback)
    // 3. Validate last step has no rollback
    // 4. Create DefPact structure
    
    // Save current scope
    let saved_scope_len = ctx.local_scope.len();
    
    // Add function arguments to scope
    for arg in &defpact.args {
        ctx.push_var(arg.name.clone());
    }
    
    // Process steps
    let mut steps = Vec::new();
    let mut dependencies = std::collections::HashSet::new();
    
    for step in defpact.steps {
        let step_result = desugar_pact_step(step, ctx)?;
        dependencies.extend(step_result.dependencies);
        steps.push(step_result.result);
    }
    
    // Validate: In DefPacts, last step is not allowed to rollback
    if let Some(last_step) = steps.last() {
        if has_rollback(last_step) {
            let qualified_name = QualifiedName {
                name: defpact.name.name.clone(),
                module: ctx.current_module.clone().unwrap_or_else(|| ModuleName {
                    name: "unknown".into(),
                    namespace: None,
                }),
            };
            return Err(PactError::PEExecutionError(
                EvalError::RuntimeError(format!("Last step with rollback in defpact: {}", qualified_name.name)),
                vec![],
                SpanInfo { start: 0, end: 0 }
            ));
        }
    }
    
    // Restore scope
    ctx.local_scope.truncate(saved_scope_len);
    
    // Convert annotations
    let annotations = defpact.annotations.into_iter()
        .map(desugar_pact_ann)
        .collect::<Result<Vec<_>, _>>()?;
    
    let result = crate::term::DefPact {
        name: desugar_arg(defpact.name),
        args: defpact.args.into_iter().map(desugar_arg).collect(),
        steps,
        annotations,
        info: desugar_span_info(defpact.info),
    };
    
    Ok(DesugarOutput {
        result,
        dependencies,
    })
}

/// Desugar a pact step
fn desugar_pact_step(
    step: pact_parser::ast::PactStep<ParsedSpanInfo>,
    ctx: &mut DesugarContext,
) -> Result<DesugarOutput<PactStep<Name, Type, CoreBuiltin, SpanInfo>>, PactError<SpanInfo>> {
    use pact_parser::ast::PactStep as AstPactStep;
    
    match step {
        AstPactStep::Step { entity, expr, model: _ } => {
            // Check: Entities are not allowed in defpacts
            if entity.is_some() {
                return Err(PactError::PEExecutionError(
                    EvalError::RuntimeError("Entity not allowed in defpact".to_string()),
                    vec![],
                    SpanInfo { start: 0, end: 0 }
                ));
            }
            
            let expr_result = desugar_expr(expr, ctx)?;
            Ok(DesugarOutput {
                result: PactStep::Step {
                    entity: None,
                    expr: expr_result.result,
                },
                dependencies: expr_result.dependencies,
            })
        }
        AstPactStep::StepWithRollback { entity, expr, rollback, model: _ } => {
            // Check: Entities are not allowed in defpacts
            if entity.is_some() {
                return Err(PactError::PEExecutionError(
                    EvalError::RuntimeError("Entity not allowed in defpact".to_string()),
                    vec![],
                    SpanInfo { start: 0, end: 0 }
                ));
            }
            
            let expr_result = desugar_expr(expr, ctx)?;
            let rollback_result = desugar_expr(rollback, ctx)?;
            
            let mut deps = expr_result.dependencies;
            deps.extend(rollback_result.dependencies);
            
            Ok(DesugarOutput {
                result: PactStep::StepWithRollback {
                    entity: None,
                    expr: expr_result.result,
                    rollback: rollback_result.result,
                },
                dependencies: deps,
            })
        }
    }
}

/// Check if a step has rollback capability
fn has_rollback(step: &PactStep<Name, Type, CoreBuiltin, SpanInfo>) -> bool {
    matches!(step, PactStep::StepWithRollback { .. })
}

/// Helper function to desugar capability metadata
fn desugar_dcap_meta(meta: pact_parser::ast::DCapMeta) -> Result<DCapMeta, PactError<SpanInfo>> {
    use pact_parser::ast::DCapMeta as AstDCapMeta;
    
    match meta {
        AstDCapMeta::DefManaged(managed_info) => {
            match managed_info {
                Some((param_name, manager_name)) => {
                    Ok(DCapMeta::DefManaged(Some((param_name, desugar_parsed_name(manager_name)))))
                }
                None => Ok(DCapMeta::DefManaged(None)),
            }
        }
        AstDCapMeta::DefEvent => Ok(DCapMeta::DefEvent),
    }
}

fn desugar_list(
    elements: Vec<ParsedExpr<ParsedSpanInfo>>,
    info: ParsedSpanInfo,
    ctx: &mut DesugarContext,
) -> Result<DesugarOutput<CoreTerm>, PactError<SpanInfo>> {
    // Desugar all list elements
    let mut desugared_elements = Vec::new();
    let mut all_dependencies = std::collections::HashSet::new();
    
    for element in elements {
        let element_result = desugar_expr(element, ctx)?;
        all_dependencies.extend(element_result.dependencies);
        desugared_elements.push(element_result.result);
    }
    
    // Create list literal term
    let result = Term::ListLit {
        elements: desugared_elements,
        info: desugar_span_info(info),
    };
    
    Ok(DesugarOutput {
        result,
        dependencies: all_dependencies,
    })
}

fn desugar_object(
    _fields: Vec<(AstField, ParsedExpr<ParsedSpanInfo>)>,
    info: ParsedSpanInfo,
    _ctx: &mut DesugarContext,
) -> Result<DesugarOutput<CoreTerm>, PactError<SpanInfo>> {
    Err(PactError::PEExecutionError(
        EvalError::RuntimeError("object desugaring not yet implemented".to_string()),
        vec![],
        desugar_span_info(info)
    ))
}

fn desugar_let(
    _form: LetForm,
    bindings: Vec<Binder<ParsedSpanInfo>>,
    body: Vec<ParsedExpr<ParsedSpanInfo>>,
    info: ParsedSpanInfo,
    ctx: &mut DesugarContext,
) -> Result<DesugarOutput<CoreTerm>, PactError<SpanInfo>> {
    // Following Haskell implementation exactly:
    // 1. nelToSequence i <$> traverse desugarLispTerm expr
    // 2. foldrM (binderToLet i) expr' binders
    
    // Step 1: Convert multiple body expressions to Sequence (nelToSequence in Haskell)
    let body_result = if body.len() == 1 {
        desugar_expr(body.into_iter().next().unwrap(), ctx)?
    } else {
        // Multiple expressions - create right-associative sequence
        let mut expr_results = Vec::new();
        let mut all_deps = std::collections::HashSet::new();
        
        for expr in body {
            let result = desugar_expr(expr, ctx)?;
            all_deps.extend(result.dependencies);
            expr_results.push(result.result);
        }
        
        // Build right-associative sequence: foldr (\a b -> Sequence a b info) (last nel) (init nel)
        let mut result = expr_results.pop().unwrap();
        for term in expr_results.into_iter().rev() {
            result = Term::Sequence {
                first: Box::new(term),
                second: Box::new(result),
                info: desugar_span_info(info),
            };
        }
        
        DesugarOutput {
            result,
            dependencies: all_deps,
        }
    };
    
    // Step 2: Right fold over bindings to create nested Let terms
    // foldrM (binderToLet i) expr' binders
    // This creates: Let x1 e1 (Let x2 e2 (Let x3 e3 body))
    let mut result = body_result;
    
    // Process bindings in reverse order to get right-associative nesting
    for binding in bindings.into_iter().rev() {
        // Save current scope length
        let saved_scope_len = ctx.local_scope.len();
        
        // Desugar the binding expression first (before adding to scope)
        let expr_result = desugar_expr(binding.expr, ctx)?;
        
        // Add binding to local scope for body
        ctx.push_var(binding.arg.name.clone());
        
        // Create the Let term: binderToLet creates Let (Arg n mty i) expr' term i
        let let_term = Term::Let {
            arg: Arg {
                name: binding.arg.name.clone(),
                ty: binding.arg.ty.map(desugar_type),
                info: desugar_span_info(info),
            },
            expr: Box::new(expr_result.result),
            body: Box::new(result.result),
            info: desugar_span_info(info),
        };
        
        // Combine dependencies
        let mut deps = expr_result.dependencies;
        deps.extend(result.dependencies);
        
        result = DesugarOutput {
            result: let_term,
            dependencies: deps,
        };
        
        // Restore scope
        ctx.local_scope.truncate(saved_scope_len);
    }
    
    Ok(result)
}

fn desugar_lambda(
    _args: Vec<MArg<ParsedSpanInfo>>,
    _body: Vec<ParsedExpr<ParsedSpanInfo>>,
    info: ParsedSpanInfo,
    _ctx: &mut DesugarContext,
) -> Result<DesugarOutput<CoreTerm>, PactError<SpanInfo>> {
    Err(PactError::PEExecutionError(
        EvalError::RuntimeError("lambda desugaring not yet implemented".to_string()),
        vec![],
        desugar_span_info(info)
    ))
}

fn desugar_if(
    cond: ParsedExpr<ParsedSpanInfo>,
    then_expr: ParsedExpr<ParsedSpanInfo>,
    else_expr: Option<ParsedExpr<ParsedSpanInfo>>,
    info: ParsedSpanInfo,
    ctx: &mut DesugarContext,
) -> Result<DesugarOutput<CoreTerm>, PactError<SpanInfo>> {
    let cond_result = desugar_expr(cond, ctx)?;
    let then_result = desugar_expr(then_expr, ctx)?;
    let else_result = if let Some(else_expr) = else_expr {
        Some(desugar_expr(else_expr, ctx)?)
    } else {
        None
    };
    
    let mut deps = cond_result.dependencies;
    deps.extend(then_result.dependencies);
    if let Some(ref else_res) = else_result {
        deps.extend(else_res.dependencies.clone());
    }
    
    Ok(DesugarOutput {
        result: Term::BuiltinForm {
            form: BuiltinForm::CIf {
                cond: Box::new(cond_result.result),
                then_expr: Box::new(then_result.result),
                else_expr: else_result.map(|r| Box::new(r.result)),
            },
            info: desugar_span_info(info),
        },
        dependencies: deps,
    })
}

fn desugar_with_capability(
    cap: ParsedExpr<ParsedSpanInfo>,
    body: Vec<ParsedExpr<ParsedSpanInfo>>,
    info: ParsedSpanInfo,
    ctx: &mut DesugarContext,
) -> Result<DesugarOutput<CoreTerm>, PactError<SpanInfo>> {
    // Desugar the capability expression
    let cap_result = desugar_expr(cap, ctx)?;
    
    // Desugar body expressions
    let mut body_results = Vec::new();
    let mut all_dependencies = std::collections::HashSet::new();
    
    for expr in body {
        let expr_result = desugar_expr(expr, ctx)?;
        all_dependencies.extend(expr_result.dependencies);
        body_results.push(Box::new(expr_result.result));
    }
    
    // Combine with capability dependencies
    all_dependencies.extend(cap_result.dependencies);
    
    // Track that we're processing a special form
    ctx.track_special_form();
    
    // Create with-capability builtin form
    let result = Term::BuiltinForm {
        form: BuiltinForm::CWithCapability {
            cap: Box::new(cap_result.result),
            body: body_results,
        },
        info: desugar_span_info(info),
    };
    
    Ok(DesugarOutput {
        result,
        dependencies: all_dependencies,
    })
}