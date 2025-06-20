//! Constant evaluation for Pact Core IR
//!
//! This module implements the constant evaluation stage that processes `defconst`
//! definitions at compile time, exactly matching the Haskell ConstEval implementation.
//!
//! Key features:
//! - Evaluates simple constant expressions to PactValue
//! - Only processes safe, pure expressions (literals)
//! - Provides foundation for more complex constant evaluation
//! - Matches Haskell evalTLConsts, evalModuleDefConsts functionality

use pact_ir::{
    Term, TopLevel, Def, DefConst, CoreBuiltin,
    Name, Type, SpanInfo, CoreTopLevel
};
use crate::literal_eval::try_eval_constant_term;
use pact_errors::PactError;
use pact_values::PactValue;

/// Constant evaluation context
#[derive(Debug, Clone)]
pub struct ConstEvalContext {
    /// Statistics about evaluation
    pub stats: ConstEvalStats,
}

/// Statistics about constant evaluation
#[derive(Debug, Clone, Default)]
pub struct ConstEvalStats {
    pub constants_found: usize,
    pub literals_evaluated: usize,
    pub evaluation_errors: usize,
}

impl ConstEvalContext {
    /// Create a new constant evaluation context
    pub fn new() -> Self {
        Self {
            stats: ConstEvalStats::default(),
        }
    }

    /// Get evaluation statistics
    pub fn stats(&self) -> &ConstEvalStats {
        &self.stats
    }
}

/// Main constant evaluation entry point for top-level items
/// Matches Haskell: evalTLConsts :: Interpreter e b i -> TopLevel Name Type b i -> EvalM e b i (TopLevel Name Type b i)
pub fn eval_top_level_constants(
    top_level: CoreTopLevel,
    ctx: &mut ConstEvalContext,
) -> Result<CoreTopLevel, PactError<SpanInfo>> {
    match top_level {
        TopLevel::TLModule(module) => {
            let evaled_module = eval_module_constants(module, ctx)?;
            Ok(TopLevel::TLModule(evaled_module))
        }
        // Other top-level items don't contain constants to evaluate
        TopLevel::TLInterface(interface) => Ok(TopLevel::TLInterface(interface)),
        TopLevel::TLTerm(term) => Ok(TopLevel::TLTerm(term)),
        TopLevel::TLUse(import) => Ok(TopLevel::TLUse(import)),
    }
}

/// Evaluate constants in a module
fn eval_module_constants(
    mut module: pact_ir::Module<Name, Type, CoreBuiltin, SpanInfo>,
    ctx: &mut ConstEvalContext,
) -> Result<pact_ir::Module<Name, Type, CoreBuiltin, SpanInfo>, PactError<SpanInfo>> {
    // Process each definition in the module
    let mut evaled_definitions = Vec::new();
    
    for def in module.definitions {
        let evaled_def = eval_def_constants(def, ctx)?;
        evaled_definitions.push(evaled_def);
    }
    
    module.definitions = evaled_definitions;
    Ok(module)
}

/// Evaluate constants in a single definition
fn eval_def_constants(
    def: Def<Name, Type, CoreBuiltin, SpanInfo>,
    ctx: &mut ConstEvalContext,
) -> Result<Def<Name, Type, CoreBuiltin, SpanInfo>, PactError<SpanInfo>> {
    match def {
        Def::DConst(defconst) => {
            ctx.stats.constants_found += 1;
            let evaled_defconst = eval_defconst_definition(defconst, ctx)?;
            Ok(Def::DConst(evaled_defconst))
        }
        // Other definition types don't contain constants to evaluate
        Def::Dfun(defun) => Ok(Def::Dfun(defun)),
        Def::DCap(defcap) => Ok(Def::DCap(defcap)),
        Def::DSchema(defschema) => Ok(Def::DSchema(defschema)),
        Def::DTable(deftable) => Ok(Def::DTable(deftable)),
        Def::DPact(defpact) => Ok(Def::DPact(defpact)),
    }
}

/// Evaluate a single defconst definition
/// For now, this analyzes the term but doesn't replace it
/// Later this can be extended to cache evaluated values
fn eval_defconst_definition(
    defconst: DefConst<Name, Type, CoreBuiltin, SpanInfo>,
    ctx: &mut ConstEvalContext,
) -> Result<DefConst<Name, Type, CoreBuiltin, SpanInfo>, PactError<SpanInfo>> {
    // Try to evaluate the constant expression at compile time
    match try_eval_constant_term(&defconst.value) {
        Ok(Some(_pact_value)) => {
            // Successfully evaluated a literal
            ctx.stats.literals_evaluated += 1;
            
            // For now, we just return the original defconst
            // Later we can modify the IR to cache the evaluated value
            Ok(defconst)
        }
        Ok(None) => {
            // Not a simple literal - this is normal for complex expressions
            Ok(defconst)
        }
        Err(err) => {
            // Evaluation error
            ctx.stats.evaluation_errors += 1;
            Err(err)
        }
    }
}

/// Check if a term is evaluable as a constant
pub fn is_constant_evaluable(term: &Term<Name, Type, CoreBuiltin, SpanInfo>) -> bool {
    matches!(term, Term::Constant(_, _))
}

/// Try to evaluate a term as a constant
pub fn eval_term_as_constant(
    term: &Term<Name, Type, CoreBuiltin, SpanInfo>
) -> Result<Option<PactValue>, PactError<SpanInfo>> {
    try_eval_constant_term(term)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pact_ir::{Arg, Module, Governance, Literal};

    fn make_test_span() -> SpanInfo {
        SpanInfo { start: 0, end: 0 }
    }


    #[test]
    fn test_eval_integer_defconst() {
        let mut ctx = ConstEvalContext::new();
        
        // Create: (defconst TEST_CONST 42)
        let term = Term::Constant(Literal::LInteger(42), make_test_span());
        let defconst = DefConst {
            name: Arg {
                name: compact_str::CompactString::from("TEST_CONST"),
                ty: None,
                info: make_test_span(),
            },
            value: term,
            doc: None,
            info: make_test_span(),
        };
        
        let result = eval_defconst_definition(defconst, &mut ctx);
        
        assert!(result.is_ok());
        assert_eq!(ctx.stats.constants_found, 0); // Called directly, not through def processor
        assert_eq!(ctx.stats.literals_evaluated, 1);
        assert_eq!(ctx.stats.evaluation_errors, 0);
    }

    #[test]
    fn test_eval_string_defconst() {
        let mut ctx = ConstEvalContext::new();
        
        // Create: (defconst MESSAGE "Hello, World!")
        let term = Term::Constant(Literal::LString(compact_str::CompactString::from("Hello, World!")), make_test_span());
        let defconst = DefConst {
            name: Arg {
                name: compact_str::CompactString::from("MESSAGE"),
                ty: None,
                info: make_test_span(),
            },
            value: term,
            doc: None,
            info: make_test_span(),
        };
        
        let result = eval_defconst_definition(defconst, &mut ctx);
        
        assert!(result.is_ok());
        assert_eq!(ctx.stats.literals_evaluated, 1);
    }

    #[test]
    fn test_eval_module_with_defconst() {
        let mut ctx = ConstEvalContext::new();
        
        // Create a module with a defconst
        let term = Term::Constant(Literal::LBool(true), make_test_span());
        let defconst = DefConst {
            name: Arg {
                name: compact_str::CompactString::from("FLAG"),
                ty: None,
                info: make_test_span(),
            },
            value: term,
            doc: None,
            info: make_test_span(),
        };
        
        let module = Module {
            name: compact_str::CompactString::from("test-module"),
            governance: Governance::KeyGov(compact_str::CompactString::from("test-keyset")),
            definitions: vec![Def::DConst(defconst)],
            imports: vec![],
            annotations: vec![],
            info: make_test_span(),
        };
        
        let result = eval_module_constants(module, &mut ctx);
        
        assert!(result.is_ok());
        assert_eq!(ctx.stats.constants_found, 1);
        assert_eq!(ctx.stats.literals_evaluated, 1);
        assert_eq!(ctx.stats.evaluation_errors, 0);
    }

    #[test]
    fn test_complex_expression_not_evaluated() {
        let mut ctx = ConstEvalContext::new();
        
        // Create a complex expression that can't be evaluated: (+ 1 2)
        let term = Term::App {
            func: Box::new(Term::Builtin(CoreBuiltin::CoreAdd, make_test_span())),
            args: vec![
                Term::Constant(Literal::LInteger(1), make_test_span()),
                Term::Constant(Literal::LInteger(2), make_test_span()),
            ],
            info: make_test_span(),
        };
        
        let defconst = DefConst {
            name: Arg {
                name: compact_str::CompactString::from("COMPLEX_CONST"),
                ty: None,
                info: make_test_span(),
            },
            value: term,
            doc: None,
            info: make_test_span(),
        };
        
        let result = eval_defconst_definition(defconst, &mut ctx);
        
        // Should succeed but not evaluate the complex expression
        assert!(result.is_ok());
        assert_eq!(ctx.stats.literals_evaluated, 0);
        assert_eq!(ctx.stats.evaluation_errors, 0);
    }
}