//! AST to Core IR lowering
//!
//! This module provides functions to lower the parsed AST to Core IR terms.

use crate::{CoreTerm, Literal, Name, ParsedName as IrParsedName, BareName, QualifiedName, Arg, Type, BuiltinForm, Decimal, ModuleName};
use pact_syntax::parser::{ParsedTopLevel, ParsedExpr, ParsedName as AstParsedName, LetForm, Binder, MArg};
use pact_core::shared::SpanInfo;
use pact_core::errors::{PactError, EvalError};
use compact_str::CompactString;

/// Lower a top-level AST node to Core IR
pub fn lower_toplevel(toplevel: &ParsedTopLevel<SpanInfo>) -> Result<CoreTerm, PactError<SpanInfo>> {
    match toplevel {
        ParsedTopLevel::TLTerm(expr) => lower_expr(expr),
        ParsedTopLevel::TLModule(_module) => {
            // For now, return a placeholder
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError("Module lowering not yet implemented".to_string()),
                vec![],
                SpanInfo::empty()
            ))
        }
        ParsedTopLevel::TLInterface(_interface) => {
            // For now, return a placeholder
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError("Interface lowering not yet implemented".to_string()),
                vec![],
                SpanInfo::empty()
            ))
        }
        ParsedTopLevel::TLUse(_import) => {
            // For now, return a placeholder
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError("Import lowering not yet implemented".to_string()),
                vec![],
                SpanInfo::empty()
            ))
        }
    }
}

/// Lower an expression to Core IR
pub fn lower_expr(expr: &ParsedExpr<SpanInfo>) -> Result<CoreTerm, PactError<SpanInfo>> {
    match expr {
        ParsedExpr::Var(name, info) => lower_parsed_name(name, *info),
        ParsedExpr::Constant(lit, info) => {
            let core_lit = lower_literal(lit)?;
            Ok(CoreTerm::Constant(core_lit, *info))
        }
        ParsedExpr::List(exprs, info) => {
            if exprs.is_empty() {
                // Empty list literal
                Ok(CoreTerm::ListLit { 
                    elements: vec![], 
                    info: *info 
                })
            } else {
                // Try to parse as special form or function application
                lower_list_expr(exprs, *info)
            }
        }
        ParsedExpr::Object(_fields, info) => {
            // Objects need to be represented differently in Core IR
            // For now, we'll represent them as a special builtin call
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError("Object literal lowering not yet implemented".to_string()),
                vec![],
                *info
            ))
        }
        ParsedExpr::App { func, args, info } => {
            let core_func = Box::new(lower_expr(func)?);
            let mut core_args = Vec::new();
            for arg in args {
                core_args.push(lower_expr(arg)?);
            }
            Ok(CoreTerm::App {
                func: core_func,
                args: core_args,
                info: *info,
            })
        }
        ParsedExpr::Let { form, bindings, body, info } => {
            lower_let_expr(form, bindings, body, *info)
        }
        ParsedExpr::Lam { args, body, info } => {
            let core_args = lower_args(args)?;
            let core_body = lower_block(body)?;
            Ok(CoreTerm::Lam {
                args: core_args,
                body: Box::new(core_body),
                info: *info,
            })
        }
        ParsedExpr::If { cond, then_expr, else_expr, info } => {
            Ok(CoreTerm::BuiltinForm {
                form: BuiltinForm::CIf {
                    cond: Box::new(lower_expr(cond)?),
                    then_expr: Box::new(lower_expr(then_expr)?),
                    else_expr: match else_expr {
                        Some(e) => Some(Box::new(lower_expr(e)?)),
                        None => None,
                    },
                },
                info: *info,
            })
        }
        ParsedExpr::And { left, right, info } => {
            Ok(CoreTerm::BuiltinForm {
                form: BuiltinForm::CAnd(
                    Box::new(lower_expr(left)?),
                    Box::new(lower_expr(right)?),
                ),
                info: *info,
            })
        }
        ParsedExpr::Or { left, right, info } => {
            Ok(CoreTerm::BuiltinForm {
                form: BuiltinForm::COr(
                    Box::new(lower_expr(left)?),
                    Box::new(lower_expr(right)?),
                ),
                info: *info,
            })
        }
        ParsedExpr::Enforce { cond, msg, info } => {
            Ok(CoreTerm::BuiltinForm {
                form: BuiltinForm::CEnforce {
                    cond: Box::new(lower_expr(cond)?),
                    msg: Box::new(lower_expr(msg)?),
                },
                info: *info,
            })
        }
        ParsedExpr::EnforceOne { msg: _msg, conds, info } => {
            let mut conditions = Vec::new();
            for cond in conds {
                conditions.push(Box::new(lower_expr(cond)?));
            }
            Ok(CoreTerm::BuiltinForm {
                form: BuiltinForm::CEnforceOne { conditions },
                info: *info,
            })
        }
        ParsedExpr::WithCapability { cap, body, info } => {
            let mut body_terms = Vec::new();
            for expr in body {
                body_terms.push(Box::new(lower_expr(expr)?));
            }
            Ok(CoreTerm::BuiltinForm {
                form: BuiltinForm::CWithCapability {
                    cap: Box::new(lower_expr(cap)?),
                    body: body_terms,
                },
                info: *info,
            })
        }
        ParsedExpr::Try { expr, catch, info } => {
            Ok(CoreTerm::BuiltinForm {
                form: BuiltinForm::CTry {
                    expr: Box::new(lower_expr(expr)?),
                    handler: Box::new(lower_expr(catch)?),
                },
                info: *info,
            })
        }
        _ => {
            // For now, return an error for unimplemented cases
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError("Expression lowering not yet implemented".to_string()),
                vec![],
                SpanInfo::empty()
            ))
        }
    }
}

/// Lower a ParsedName to a CoreTerm variable
fn lower_parsed_name(name: &AstParsedName, info: SpanInfo) -> Result<CoreTerm, PactError<SpanInfo>> {
    match name {
        AstParsedName::BN(bare_name) => {
            let ir_name = IrParsedName::BN(BareName(CompactString::from(bare_name.0.as_str())));
            Ok(CoreTerm::Var(Name::Parsed(ir_name), info))
        }
        AstParsedName::QN(qualified_name) => {
            let ir_qn = QualifiedName {
                name: CompactString::from(qualified_name.name.as_str()),
                module: ModuleName {
                    name: CompactString::from(qualified_name.module.name.as_str()),
                    namespace: qualified_name.module.namespace.as_ref().map(|ns| {
                        crate::NamespaceName(CompactString::from(ns.0.as_str()))
                    }),
                },
            };
            let ir_name = IrParsedName::QN(ir_qn);
            Ok(CoreTerm::Var(Name::Parsed(ir_name), info))
        }
        AstParsedName::DN(_dynamic_name) => {
            // Dynamic names need special handling
            Err(PactError::PEExecutionError(
                EvalError::RuntimeError("Dynamic name lowering not yet implemented".to_string()),
                vec![],
                info
            ))
        }
    }
}

/// Lower a literal
fn lower_literal(lit: &pact_syntax::parser::Literal) -> Result<Literal, PactError<SpanInfo>> {
    use pact_syntax::parser::Literal as ParsedLiteral;
    match lit {
        ParsedLiteral::LString(s) => Ok(Literal::LString(CompactString::from(s.as_str()))),
        ParsedLiteral::LInteger(n) => Ok(Literal::LInteger(*n)),
        ParsedLiteral::LDecimal(d) => {
            // Convert from pact_syntax::Decimal to pact_ir::Decimal
            Ok(Literal::LDecimal(Decimal {
                precision: d.precision,
                mantissa: d.mantissa,
            }))
        }
        ParsedLiteral::LBool(b) => Ok(Literal::LBool(*b)),
        ParsedLiteral::LUnit => Ok(Literal::LUnit),
    }
}

/// Lower a list expression (could be special form or application)
fn lower_list_expr(exprs: &[ParsedExpr<SpanInfo>], info: SpanInfo) -> Result<CoreTerm, PactError<SpanInfo>> {
    if exprs.is_empty() {
        return Ok(CoreTerm::ListLit { elements: vec![], info });
    }
    
    // Check if first element is a special form
    if let ParsedExpr::Var(AstParsedName::BN(name), _) = &exprs[0] {
        match name.0.as_str() {
            "if" => return lower_if_from_list(exprs, info),
            "and" => return lower_and_from_list(exprs, info),
            "or" => return lower_or_from_list(exprs, info),
            "let" | "let*" => {
                // These should have been parsed as Let expressions
                return Err(PactError::PEExecutionError(
                    EvalError::RuntimeError("let forms should be parsed as Let expressions".to_string()),
                    vec![],
                    info
                ));
            }
            _ => {} // Fall through to application
        }
    }
    
    // Otherwise, it's a function application
    if exprs.len() == 1 {
        // Single element list - just return the element
        lower_expr(&exprs[0])
    } else {
        // Function application
        let func = Box::new(lower_expr(&exprs[0])?);
        let mut args = Vec::new();
        for arg in &exprs[1..] {
            args.push(lower_expr(arg)?);
        }
        Ok(CoreTerm::App { func, args, info })
    }
}

/// Lower if expression from list
fn lower_if_from_list(exprs: &[ParsedExpr<SpanInfo>], info: SpanInfo) -> Result<CoreTerm, PactError<SpanInfo>> {
    if exprs.len() < 3 || exprs.len() > 4 {
        return Err(PactError::PEExecutionError(
            EvalError::ArgumentCountMismatch { 
                function: "if".to_string(),
                expected: 3, 
                received: exprs.len() 
            },
            vec![],
            info
        ));
    }
    
    let cond = Box::new(lower_expr(&exprs[1])?);
    let then_expr = Box::new(lower_expr(&exprs[2])?);
    let else_expr = if exprs.len() == 4 {
        Some(Box::new(lower_expr(&exprs[3])?))
    } else {
        None
    };
    
    Ok(CoreTerm::BuiltinForm {
        form: BuiltinForm::CIf { cond, then_expr, else_expr },
        info,
    })
}

/// Lower and expression from list
fn lower_and_from_list(exprs: &[ParsedExpr<SpanInfo>], info: SpanInfo) -> Result<CoreTerm, PactError<SpanInfo>> {
    if exprs.len() != 3 {
        return Err(PactError::PEExecutionError(
            EvalError::ArgumentCountMismatch { 
                function: "and".to_string(),
                expected: 2, 
                received: exprs.len() - 1 
            },
            vec![],
            info
        ));
    }
    
    Ok(CoreTerm::BuiltinForm {
        form: BuiltinForm::CAnd(
            Box::new(lower_expr(&exprs[1])?),
            Box::new(lower_expr(&exprs[2])?),
        ),
        info,
    })
}

/// Lower or expression from list
fn lower_or_from_list(exprs: &[ParsedExpr<SpanInfo>], info: SpanInfo) -> Result<CoreTerm, PactError<SpanInfo>> {
    if exprs.len() != 3 {
        return Err(PactError::PEExecutionError(
            EvalError::ArgumentCountMismatch { 
                function: "or".to_string(),
                expected: 2, 
                received: exprs.len() - 1 
            },
            vec![],
            info
        ));
    }
    
    Ok(CoreTerm::BuiltinForm {
        form: BuiltinForm::COr(
            Box::new(lower_expr(&exprs[1])?),
            Box::new(lower_expr(&exprs[2])?),
        ),
        info,
    })
}

/// Lower let expression
/// 
/// Following Haskell implementation exactly:
/// 1. Convert multiple body expressions to Sequence
/// 2. foldrM (binderToLet i) expr' binders - right fold to create nested Let terms
/// 3. Each binding creates a binary Let term that scopes over remaining bindings and body
fn lower_let_expr(
    _form: &LetForm, 
    bindings: &[Binder<SpanInfo>], 
    body: &[ParsedExpr<SpanInfo>], 
    info: SpanInfo
) -> Result<CoreTerm, PactError<SpanInfo>> {
    // Step 1: Convert multiple body expressions to Sequence (nelToSequence in Haskell)
    let body_term = lower_block(body)?;
    
    // Step 2: Right fold over bindings to create nested Let terms
    // foldrM (binderToLet i) expr' binders
    // This creates: Let x1 e1 (Let x2 e2 (Let x3 e3 body))
    let mut result = body_term;
    
    // Process bindings in reverse order to get right-associative nesting
    for binding in bindings.iter().rev() {
        let arg = Arg {
            name: binding.arg.name.clone(),
            ty: binding.arg.ty.as_ref().map(|ty| lower_type(ty)).transpose()?,
            info,
        };
        
        // binderToLet: creates Let (Arg n mty i) expr' term i
        result = CoreTerm::Let {
            arg,
            expr: Box::new(lower_expr(&binding.expr)?),
            body: Box::new(result),
            info,
        };
    }
    
    Ok(result)
}

/// Lower a block of expressions to a single expression
fn lower_block(exprs: &[ParsedExpr<SpanInfo>]) -> Result<CoreTerm, PactError<SpanInfo>> {
    if exprs.is_empty() {
        return Err(PactError::PEExecutionError(
            EvalError::RuntimeError("Empty block not allowed".to_string()),
            vec![],
            SpanInfo::empty()
        ));
    }
    
    if exprs.len() == 1 {
        lower_expr(&exprs[0])
    } else {
        // Create sequence of expressions using Term::Sequence
        let mut iter = exprs.iter();
        let first = lower_expr(iter.next().unwrap())?;
        
        let mut result = first;
        for expr in iter {
            let info = get_expr_info(expr);
            result = CoreTerm::Sequence {
                first: Box::new(result),
                second: Box::new(lower_expr(expr)?),
                info,
            };
        }
        
        Ok(result)
    }
}

/// Lower arguments
fn lower_args(args: &[MArg<SpanInfo>]) -> Result<Vec<Arg<Type, SpanInfo>>, PactError<SpanInfo>> {
    let mut result = Vec::new();
    
    for arg in args {
        result.push(Arg {
            name: arg.name.clone(),
            ty: Some(match &arg.ty {
                Some(ty) => lower_type(ty)?,
                None => Type::TyAny,
            }),
            info: arg.info,
        });
    }
    
    Ok(result)
}

/// Lower a type
fn lower_type(ty: &pact_syntax::parser::Type) -> Result<Type, PactError<SpanInfo>> {
    use pact_syntax::parser::Type as ParsedType;
    use pact_syntax::parser::PrimType;
    use crate::PrimType as IrPrimType;
    
    match ty {
        ParsedType::TyPrim(prim) => {
            let ir_prim = match prim {
                PrimType::PrimInt => IrPrimType::PrimInt,
                PrimType::PrimDecimal => IrPrimType::PrimDecimal,
                PrimType::PrimBool => IrPrimType::PrimBool,
                PrimType::PrimString => IrPrimType::PrimString,
                PrimType::PrimTime => IrPrimType::PrimTime,
                PrimType::PrimUnit => IrPrimType::PrimUnit,
                PrimType::PrimGuard => IrPrimType::PrimGuard,
            };
            Ok(Type::TyPrim(ir_prim))
        }
        ParsedType::TyList(elem_ty) => {
            Ok(Type::TyList(Box::new(lower_type(elem_ty)?)))
        }
        ParsedType::TyAny => Ok(Type::TyAny),
        _ => {
            // For now, return TyAny for unimplemented types
            Ok(Type::TyAny)
        }
    }
}

/// Get source info from an expression
fn get_expr_info(expr: &ParsedExpr<SpanInfo>) -> SpanInfo {
    match expr {
        ParsedExpr::Var(_, info) => *info,
        ParsedExpr::Constant(_, info) => *info,
        ParsedExpr::List(_, info) => *info,
        ParsedExpr::Object(_, info) => *info,
        ParsedExpr::App { info, .. } => *info,
        ParsedExpr::Let { info, .. } => *info,
        ParsedExpr::Lam { info, .. } => *info,
        ParsedExpr::If { info, .. } => *info,
        ParsedExpr::And { info, .. } => *info,
        ParsedExpr::Or { info, .. } => *info,
        ParsedExpr::Enforce { info, .. } => *info,
        _ => SpanInfo::empty(),
    }
}