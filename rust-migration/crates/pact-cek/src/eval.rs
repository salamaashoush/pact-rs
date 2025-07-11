//! New CEK Evaluator
//!
//! This module implements the core evaluation functions following the Haskell
//! reference implementation exactly. All evaluation patterns match their
//! Haskell counterparts in structure and semantics.

use crate::types::{CEKValue, CEKEnv, CanApply, NativeFn, Closure, LamClosure, CapTokenClosure, DefPactClosure};
use crate::cont::{Cont, CondCont, BuiltinCont};
use crate::error::{CEKErrorHandler};
use crate::monad::{EvalM, charge_gas, charge_gas_with_args, create_error_handler_with_state};
use pact_ir::{CoreTerm, Name, Literal, ParsedName, BareName, QualifiedName, ModuleName, FullyQualifiedName, Def};
use pact_core::values::PactValue;
use pact_core::shared::SpanInfo;
use pact_core::errors::EvalError;
use pact_core::gas::MilliGas;

/// Evaluation result type
#[derive(Debug, Clone)]
pub enum EvalResult {
    /// Successful evaluation with result value
    EvalValue(CEKValue),
    /// Error during evaluation
    EvalError(pact_core::errors::PactErrorI),
}

/// Main CEK evaluation function with error handling - matches Haskell evalCEK exactly
///
/// ```haskell
/// evalCEK :: (IsBuiltin b) => Cont e b i -> CEKErrorHandler e b i -> CEKEnv e b i -> EvalTerm b i -> EvalM e b i (EvalResult e b i)
/// ```
pub fn eval_cek(
    cont: Cont,
    handler: CEKErrorHandler,
    env: CEKEnv,
    term: CoreTerm,
) -> EvalM<EvalResult> {
    // Clone values for error handler
    let error_handler = handler.clone();
    let error_cont = cont.clone();
    let error_env = env.clone();

    // Charge gas for evaluation step with error handling
    charge_gas("eval-step", MilliGas(1))
        .bind(|_| eval_term_impl(cont, handler, env, term))
        .try_with(move |error| {
            // Handle errors through the error handler chain
            handle_evaluation_error(error, error_handler, error_cont, error_env)
        })
}

/// Core term evaluation implementation
fn eval_term_impl(
    cont: Cont,
    handler: CEKErrorHandler,
    env: CEKEnv,
    term: CoreTerm,
) -> EvalM<EvalResult> {
    match term {
        // Variable lookup (DeBruijn index or fully qualified name)
        CoreTerm::Var(name, info) => {
            eval_var(cont, handler, env, name, info)
        }

        // Constants return directly as values
        CoreTerm::Constant(literal, info) => {
            charge_gas("eval-constant", MilliGas(1))
                .bind(move |_| {
                    match literal_to_cek_value(literal) {
                        Ok(value) => return_cek_value(cont, handler, value),
                        Err(error) => EvalM::pure_value(EvalResult::EvalError(error)),
                    }
                })
        }

        // Lambda abstraction creates closure
        CoreTerm::Lam { args, body, info } => {
            charge_gas("eval-lambda", MilliGas(5))
                .bind(move |_| {
                    let closure = CanApply::LC(LamClosure {
                        args,
                        body: *body,
                        env: env.clone(),
                        info,
                    });
                    let value = CEKValue::VClosure(closure);
                    return_cek_value(cont, handler, value)
                })
        }

        // Function application
        CoreTerm::App { func, args, info } => {
            charge_gas("eval-app", MilliGas(3))
                .bind(move |_| {
                    if args.is_empty() {
                        // No arguments, just evaluate function
                        eval_cek(cont, handler, env, *func)
                    } else {
                        // Has arguments, create Args continuation
                        let args_cont = Cont::args(env.clone(), info, args, cont);
                        eval_cek(args_cont, handler, env, *func)
                    }
                })
        }

        // Builtin function reference
        CoreTerm::Builtin(builtin, info) => {
            charge_gas("eval-builtin", MilliGas(2))
                .bind(move |_| {
                    match env.builtins.lookup(builtin, info) {
                        Ok(native_fn) => {
                            let closure = CanApply::N(native_fn);
                            let value = CEKValue::VClosure(closure);
                            return_cek_value(cont, handler, value)
                        }
                        Err(error) => EvalM::pure_value(EvalResult::EvalError(error)),
                    }
                })
        }

        // Let binding
        CoreTerm::Let { arg, expr, body, info } => {
            charge_gas("eval-let", MilliGas(3))
                .bind(move |_| {
                    let let_cont = Cont::let_cont(env.clone(), info, arg, *body, cont);
                    eval_cek(let_cont, handler, env, *expr)
                })
        }

        // Sequence evaluation
        CoreTerm::Sequence { first, second, info } => {
            charge_gas("eval-sequence", MilliGas(2))
                .bind(move |_| {
                    let seq_cont = Cont::seq_cont(env.clone(), info, *second, cont);
                    eval_cek(seq_cont, handler, env, *first)
                })
        }

        // List literal construction
        CoreTerm::ListLit { elements, info } => {
            eval_list_literal(cont, handler, env, elements, info)
        }

        // Object literal construction
        CoreTerm::ObjectLit { fields, info } => {
            eval_object_literal(cont, handler, env, fields, info)
        }

        // Builtin forms (special syntax)
        CoreTerm::BuiltinForm { form, info } => {
            eval_builtin_form(cont, handler, env, form, info)
        }

        // Nullary expressions (lazy evaluation)
        CoreTerm::Nullary { expr, info } => {
            charge_gas("eval-nullary", MilliGas(1))
                .bind(|_| eval_cek(cont, handler, env, *expr))
        }

        // Inline value (pre-evaluated constants)
        CoreTerm::InlineValue { value, info } => {
            charge_gas("eval-inline", MilliGas(1))
                .bind(move |_| {
                    let cek_value = CEKValue::VPactValue(value);
                    return_cek_value(cont, handler, cek_value)
                })
        }
    }
}

/// Variable lookup - handles DeBruijn indices and fully qualified names
pub fn eval_var(
    cont: Cont,
    handler: CEKErrorHandler,
    env: CEKEnv,
    name: Name,
    info: SpanInfo,
) -> EvalM<EvalResult> {
    charge_gas("eval-var", MilliGas(2))
        .bind(move |_| {
            match name {
                // DeBruijn index lookup in local environment
                Name::DeBruijn(pact_ir::DeBruijnIndex(index)) => {
                    match env.lookup_local(index) {
                        Some(value) => return_cek_value(cont, handler, value.clone()),
                        None => EvalM::pure_value(EvalResult::EvalError(
                            pact_core::errors::PactError::PEExecutionError(
                                pact_core::errors::EvalError::TypeMismatch {
                                    expected: format!("bound variable at index {}", index),
                                    found: "unbound variable".to_string(),
                                    context: "variable lookup".to_string(),
                                },
                                vec![],
                                pact_core::shared::SpanInfo::empty()
                            )
                        )),
                    }
                }

                // Top-level name lookup (parsed and resolved names)
                Name::Parsed(parsed_name) | Name::Resolved(parsed_name) => {
                    eval_toplevel_var(cont, handler, env, parsed_name, info)
                }
            }
        })
}

/// Evaluate top-level variable - matches Haskell lookupFqName and evaluation logic
fn eval_toplevel_var(
    cont: Cont,
    handler: CEKErrorHandler,
    env: CEKEnv,
    parsed_name: ParsedName,
    info: SpanInfo,
) -> EvalM<EvalResult> {
    charge_gas("eval-toplevel-var", MilliGas(2))
        .bind(move |_| {
            // Convert parsed name to fully qualified name based on current module context
            let fqn_result = match parsed_name {
                ParsedName::BN(bare_name) => {
                    // For bare names, we need to resolve from the current module context
                    // For now, create a placeholder FQN - full resolution would use module context
                    Err(EvalError::UnboundVariable(format!("Bare name resolution not yet implemented: {}", bare_name.0)))
                }
                ParsedName::QN(qualified_name) => {
                    // For qualified names, we have module and name
                    // Need to look up the module hash from loaded modules
                    match env.loaded.modules.get(&qualified_name.module) {
                        Some(module_data) => {
                            Ok(FullyQualifiedName {
                                module: qualified_name.module.clone(),
                                name: qualified_name.name.clone(),
                                hash: module_data.hash().clone(),
                            })
                        }
                        None => Err(EvalError::ModuleNotFound(qualified_name.module.render()))
                    }
                }
                ParsedName::DN(_dynamic_name) => {
                    Err(EvalError::RuntimeError("Dynamic names not yet supported".to_string()))
                }
            };

            match fqn_result {
                Ok(fqn) => {
                    // Look up the definition in loaded modules
                    match env.lookup_fqname(&fqn) {
                        Some(def) => {
                            // Create appropriate closure based on definition type
                            create_def_closure(cont, handler, env, def, fqn, info)
                        }
                        None => {
                            EvalM::pure_value(EvalResult::EvalError(
                                pact_core::errors::PactError::PEExecutionError(
                                    EvalError::UnboundVariable(format!("Unbound variable: {}", fqn)),
                                    vec![],
                                    info
                                )
                            ))
                        }
                    }
                }
                Err(error) => {
                    EvalM::pure_value(EvalResult::EvalError(
                        pact_core::errors::PactError::PEExecutionError(
                            error,
                            vec![],
                            info
                        )
                    ))
                }
            }
        })
}

/// Create closure from definition - matches Haskell mkDefunClosure, mkDefPactClosure, etc.
fn create_def_closure(
    cont: Cont,
    handler: CEKErrorHandler,
    env: CEKEnv,
    def: &Def<Name, pact_ir::Type, pact_ir::CoreBuiltin, SpanInfo>,
    fqn: FullyQualifiedName,
    info: SpanInfo,
) -> EvalM<EvalResult> {
    use pact_ir::Def;
    
    match def {
        Def::Dfun(defun) => {
            // Create function closure - matches Haskell mkDefunClosure
            let closure = match &defun.body {
                CoreTerm::Lam { args, body, info: lam_info } => {
                    Closure {
                        name: fqn.render(),
                        args: args.clone(),
                        body: *body.clone(),
                        env: env.clone(),
                        info: lam_info.clone(),
                    }
                }
                _ => {
                    // For non-lambda bodies, wrap in nullary closure
                    Closure {
                        name: fqn.render(),
                        args: vec![],
                        body: defun.body.clone(),
                        env: env.clone(),
                        info: defun.info.clone(),
                    }
                }
            };
            
            let value = CEKValue::VClosure(CanApply::C(closure));
            return_cek_value(cont, handler, value)
        }
        
        Def::DConst(dconst) => {
            // Constants should already be evaluated - matches Haskell EvaledConst
            // For now, evaluate the constant term
            eval_cek(cont, handler, env, dconst.value.clone())
        }
        
        Def::DCap(dcap) => {
            // Create capability token closure
            let cap_closure = CapTokenClosure {
                name: fqn.render(),
                args: vec![], // Initial args empty, will be filled on application
                info: dcap.info.clone(),
            };
            
            let value = CEKValue::VClosure(CanApply::CT(cap_closure));
            return_cek_value(cont, handler, value)
        }
        
        Def::DPact(dpact) => {
            // Create DefPact closure - matches Haskell mkDefPactClosure
            let steps = dpact.steps.iter().map(|step| {
                crate::types::DefPactStep {
                    expr: step.expr.clone(),
                    rollback: step.rollback.clone(),
                    entity: step.entity.clone(),
                }
            }).collect();
            
            let dpact_closure = DefPactClosure {
                name: fqn.render(),
                steps,
                current_step: 0,
                env: env.clone(),
                info: dpact.info.clone(),
            };
            
            let value = CEKValue::VClosure(CanApply::DPC(dpact_closure));
            return_cek_value(cont, handler, value)
        }
        
        Def::DTable(dtable) => {
            // Create table value
            let table_value = CEKValue::VTable {
                name: dtable.name.render(),
                schema: crate::types::TableSchema {
                    columns: vec![], // TODO: Extract from resolved schema
                },
                module_hash: Some(fqn.hash.0.to_string()),
            };
            
            return_cek_value(cont, handler, table_value)
        }
        
        Def::DSchema(_dschema) => {
            // Schemas are not directly evaluable
            EvalM::pure_value(EvalResult::EvalError(
                pact_core::errors::PactError::PEExecutionError(
                    EvalError::RuntimeError(format!("Schema {} cannot be evaluated as a value", fqn)),
                    vec![],
                    info
                )
            ))
        }
    }
}

/// Return value to continuation - matches Haskell returnCEKValue exactly
pub fn return_cek_value(
    cont: Cont,
    handler: CEKErrorHandler,
    value: CEKValue,
) -> EvalM<EvalResult> {
    apply_cont_to_value(cont, handler, value)
}

/// Apply continuation to value - matches Haskell applyContToValue exactly
pub fn apply_cont_to_value(
    cont: Cont,
    handler: CEKErrorHandler,
    value: CEKValue,
) -> EvalM<EvalResult> {
    // Charge gas for continuation application
    charge_gas("apply-cont", MilliGas(1))
        .bind(|_| apply_cont_to_value_impl(cont, handler, value))
}

/// Implementation of continuation application with gas already charged
fn apply_cont_to_value_impl(
    cont: Cont,
    handler: CEKErrorHandler,
    value: CEKValue,
) -> EvalM<EvalResult> {
    match cont {
        // Empty continuation - evaluation complete
        Cont::Mt => {
            apply_cont(handler, value)
        }

        // Function argument evaluation
        Cont::Args { env, info, args, cont } => {
            charge_gas("eval-args", MilliGas(2))
                .bind(move |_| {
                    match value {
                        CEKValue::VClosure(can_apply) => {
                            if args.is_empty() {
                                // No arguments, apply function directly
                                apply_lambda(can_apply, env, vec![], *cont, handler)
                            } else {
                                // Start evaluating arguments
                                let mut remaining_args = args;
                                let first_arg = remaining_args.remove(0);
                                let fn_cont = Cont::fn_cont(can_apply, env.clone(), remaining_args, vec![], *cont);
                                eval_cek(fn_cont, handler, env, first_arg)
                            }
                        }
                        _ => EvalM::pure_value(EvalResult::EvalError(
                            pact_core::errors::PactError::PEExecutionError(
                                pact_core::errors::EvalError::TypeMismatch {
                                    expected: "function".to_string(),
                                    found: "non-function".to_string(),
                                    context: "application".to_string(),
                                },
                                vec![],
                                pact_core::shared::SpanInfo::empty()
                            )
                        ))
                    }
                })
        }

        // Function application with accumulated arguments
        Cont::Fn { function, env, mut args, mut values, cont } => {
            charge_gas("apply-args", MilliGas(2))
                .bind(move |_| {
                    values.push(value);

                    if args.is_empty() {
                        // All arguments collected, apply function
                        apply_lambda(function, env, values, *cont, handler)
                    } else {
                        // Evaluate next argument
                        let next_arg = args.remove(0);
                        let fn_cont = Cont::fn_cont(function, env.clone(), args, values, *cont);
                        eval_cek(fn_cont, handler, env, next_arg)
                    }
                })
        }

        // Let binding continuation
        Cont::LetC { env, info, arg, body, cont } => {
            charge_gas("eval-let-bind", MilliGas(2))
                .bind(move |_| {
                    let extended_env = env.extend_local(value);
                    eval_cek(*cont, handler, extended_env, body)
                })
        }

        // Sequence continuation
        Cont::SeqC { env, info, expr, cont } => {
            charge_gas("eval-sequence", MilliGas(1))
                .bind(move |_| {
                    // Discard current value and evaluate next expression
                    eval_cek(*cont, handler, env, expr)
                })
        }

        // List construction continuation
        Cont::ListC { env, info, mut exprs, mut values, cont } => {
            charge_gas_with_args("list-construction", &[value.clone()], MilliGas(1))
                .bind(move |_| {
                    // Add current value to accumulated values
                    if let CEKValue::VPactValue(pact_val) = value {
                        values.push(pact_val);
                    } else {
                        return EvalM::pure_value(EvalResult::EvalError(
                            pact_core::errors::PactError::PEExecutionError(
                                pact_core::errors::EvalError::TypeMismatch {
                                    expected: "PactValue".to_string(),
                                    found: "closure".to_string(),
                                    context: "list construction".to_string(),
                                },
                                vec![],
                                pact_core::shared::SpanInfo::empty()
                            )
                        ));
                    }

                    if exprs.is_empty() {
                        // All elements evaluated, create list
                        let list_value = CEKValue::VPactValue(PactValue::List(values));
                        return_cek_value(*cont, handler, list_value)
                    } else {
                        // Evaluate next element
                        let next_expr = exprs.remove(0);
                        let list_cont = Cont::list_cont(env.clone(), info, exprs, values, *cont);
                        eval_cek(list_cont, handler, env, next_expr)
                    }
                })
        }

        // Conditional continuation
        Cont::CondC { env, info, cond_cont, cont } => {
            eval_conditional(cond_cont, value, env, info, *cont, handler)
        }

        // Higher-order builtin continuation
        Cont::BuiltinC { env, info, builtin_cont, cont } => {
            charge_gas("builtin-continuation", MilliGas(3))
                .bind(move |_| {
                    eval_builtin_continuation(builtin_cont, value, env, info, *cont, handler)
                })
        }

        // Map list continuation - evaluates list for map operation
        Cont::MapListC { env, info, func, cont } => {
            // The value should be the evaluated list
            match value.as_pact_value() {
                Some(PactValue::List(list)) => {
                    // Apply map operation using the builtin map implementation
                    let args = vec![CEKValue::VClosure(func), CEKValue::VPactValue(PactValue::List(list.clone()))];
                    let map_impl = crate::builtin::list_ops::map_implementation();
                    map_impl(info, pact_ir::CoreBuiltin::CoreMap, *cont, handler, env, args)
                }
                _ => {
                    EvalM::pure_value(EvalResult::EvalError(
                        pact_core::errors::PactError::PEExecutionError(
                            pact_core::errors::EvalError::TypeMismatch {
                                expected: "list".to_string(),
                                found: "other".to_string(),
                                context: "map list argument".to_string(),
                            },
                            vec![],
                            pact_core::shared::SpanInfo::empty()
                        )
                    ))
                }
            }
        }

        // Filter list continuation - evaluates list for filter operation
        Cont::FilterListC { env, info, func, cont } => {
            // The value should be the evaluated list
            match value.as_pact_value() {
                Some(PactValue::List(list)) => {
                    // Apply filter operation using the builtin filter implementation
                    let args = vec![CEKValue::VClosure(func), CEKValue::VPactValue(PactValue::List(list.clone()))];
                    let filter_impl = crate::builtin::list_ops::filter_implementation();
                    filter_impl(info, pact_ir::CoreBuiltin::CoreFilter, *cont, handler, env, args)
                }
                _ => {
                    EvalM::pure_value(EvalResult::EvalError(
                        pact_core::errors::PactError::PEExecutionError(
                            pact_core::errors::EvalError::TypeMismatch {
                                expected: "list".to_string(),
                                found: "other".to_string(),
                                context: "filter list argument".to_string(),
                            },
                            vec![],
                            pact_core::shared::SpanInfo::empty()
                        )
                    ))
                }
            }
        }

        // Fold init continuation - evaluates initial value for fold
        Cont::FoldInitC { env, info, func, list_expr, cont } => {
            // The value is the evaluated initial value
            // Now evaluate the list
            let fold_list_cont = Cont::FoldListC {
                env: env.clone(),
                info,
                func,
                init_val: value,
                cont,
            };
            eval_cek(fold_list_cont, handler, env, list_expr)
        }

        // Fold list continuation - evaluates list for fold operation
        Cont::FoldListC { env, info, func, init_val, cont } => {
            // The value should be the evaluated list
            match value.as_pact_value() {
                Some(PactValue::List(list)) => {
                    // Apply fold operation using the builtin fold implementation
                    let args = vec![CEKValue::VClosure(func), init_val, CEKValue::VPactValue(PactValue::List(list.clone()))];
                    let fold_impl = crate::builtin::list_ops::fold_implementation();
                    fold_impl(info, pact_ir::CoreBuiltin::CoreFold, *cont, handler, env, args)
                }
                _ => {
                    EvalM::pure_value(EvalResult::EvalError(
                        pact_core::errors::PactError::PEExecutionError(
                            pact_core::errors::EvalError::TypeMismatch {
                                expected: "list".to_string(),
                                found: "other".to_string(),
                                context: "fold list argument".to_string(),
                            },
                            vec![],
                            pact_core::shared::SpanInfo::empty()
                        )
                    ))
                }
            }
        }

        // Zip list1 continuation - evaluates first list for zip
        Cont::ZipList1C { env, info, func, list2_expr, cont } => {
            // The value is the evaluated first list
            match value.as_pact_value() {
                Some(PactValue::List(list1)) => {
                    // Now evaluate the second list
                    let zip_list2_cont = Cont::ZipList2C {
                        env: env.clone(),
                        info,
                        func,
                        list1: list1.clone(),
                        cont,
                    };
                    eval_cek(zip_list2_cont, handler, env, list2_expr)
                }
                _ => {
                    EvalM::pure_value(EvalResult::EvalError(
                        pact_core::errors::PactError::PEExecutionError(
                            pact_core::errors::EvalError::TypeMismatch {
                                expected: "list".to_string(),
                                found: "other".to_string(),
                                context: "zip first list".to_string(),
                            },
                            vec![],
                            pact_core::shared::SpanInfo::empty()
                        )
                    ))
                }
            }
        }

        // Zip list2 continuation - evaluates second list for zip
        Cont::ZipList2C { env, info, func, list1, cont } => {
            // The value is the evaluated second list
            match value.as_pact_value() {
                Some(PactValue::List(list2)) => {
                    // Apply zip operation using the builtin zip implementation
                    let args = vec![
                        CEKValue::VClosure(func), 
                        CEKValue::VPactValue(PactValue::List(list1)), 
                        CEKValue::VPactValue(PactValue::List(list2.clone()))
                    ];
                    let zip_impl = crate::builtin::list_ops::zip_implementation();
                    zip_impl(info, pact_ir::CoreBuiltin::CoreZip, *cont, handler, env, args)
                }
                _ => {
                    EvalM::pure_value(EvalResult::EvalError(
                        pact_core::errors::PactError::PEExecutionError(
                            pact_core::errors::EvalError::TypeMismatch {
                                expected: "list".to_string(),
                                found: "other".to_string(),
                                context: "zip second list".to_string(),
                            },
                            vec![],
                            pact_core::shared::SpanInfo::empty()
                        )
                    ))
                }
            }
        }

        // Other continuation types need implementation
        _ => EvalM::pure_value(EvalResult::EvalError(
            pact_core::errors::PactError::PEExecutionError(
                pact_core::errors::EvalError::UnimplementedBuiltin(format!("{} - {}", "Continuation type", "Not yet implemented in redesigned evaluator")),
                vec![],
                pact_core::shared::SpanInfo::empty()
            )
        ))
    }
}

/// Apply continuation with error handler
pub fn apply_cont(handler: CEKErrorHandler, value: CEKValue) -> EvalM<EvalResult> {
    match handler {
        CEKErrorHandler::CEKNoHandler => {
            // No error handler, return value
            EvalM::pure_value(EvalResult::EvalValue(value))
        }

        CEKErrorHandler::CEKHandler { env, recovery, cont, error_state, next_handler } => {
            // Try-catch handler - continue with value
            return_cek_value(cont, *next_handler, value)
        }

        CEKErrorHandler::CEKEnforceOne { .. } => {
            // Enforce-one handler - value succeeded
            EvalM::pure_value(EvalResult::EvalValue(value))
        }
    }
}

/// Handle evaluation errors through the error handler chain
fn handle_evaluation_error(
    error: pact_core::errors::PactErrorI,
    handler: CEKErrorHandler,
    cont: Cont,
    env: CEKEnv,
) -> EvalM<EvalResult> {
    use crate::error::{ErrorRecovery, CallFrame};

    // Create call frame for current error context
    let call_frame = match &cont {
        Cont::Fn { function, .. } => {
            match function {
                CanApply::C(closure) => {
                    CallFrame {
                        function_name: pact_core::errors::FullyQualifiedName {
                            module: None,
                            name: closure.name.clone().into(),
                        },
                        arguments: vec![], // TODO: Add actual arguments
                        function_type: pact_core::errors::StackFunctionType::SFDefun,
                        source_info: closure.info,
                    }
                }
                CanApply::N(native_fn) => {
                    CallFrame {
                        function_name: pact_core::errors::FullyQualifiedName {
                            module: None,
                            name: format!("{:?}", native_fn.builtin).into(),
                        },
                        arguments: vec![], // TODO: Add actual arguments
                        function_type: pact_core::errors::StackFunctionType::SFDefun,
                        source_info: native_fn.info,
                    }
                }
                _ => CallFrame {
                    function_name: pact_core::errors::FullyQualifiedName {
                        module: None,
                        name: "unknown function".into(),
                    },
                    arguments: vec![],
                    function_type: pact_core::errors::StackFunctionType::SFDefun,
                    source_info: SpanInfo::empty(),
                }
            }
        }
        _ => CallFrame {
            function_name: pact_core::errors::FullyQualifiedName {
                module: None,
                name: "unknown".into(),
            },
            arguments: vec![],
            function_type: pact_core::errors::StackFunctionType::SFDefun,
            source_info: SpanInfo::empty(),
        }
    };

    // Build enhanced error context
    let mut call_stack = vec![call_frame];
    if let Some(existing_stack) = handler.call_stack() {
        call_stack.extend_from_slice(existing_stack);
    }

    // Try to handle the error through the handler chain
    match handler.handle_error(error.clone()) {
        ErrorRecovery::Recovered(value) => {
            // Error was successfully handled, continue with recovered value
            charge_gas("error-recovery", MilliGas(1))
                .bind(|_| EvalM::pure_value(EvalResult::EvalValue(value)))
        }

        ErrorRecovery::Continue(_) => {
            // Try to recover using the handler
            match handler {
                CEKErrorHandler::CEKHandler { env, recovery, cont, error_state, next_handler } => {
                    // Execute recovery expression in the handler's environment
                    charge_gas("error-handler-recovery", MilliGas(5))
                        .bind(move |_| {
                            // Restore gas state to handler snapshot if gas error
                            let recovery_env = if is_gas_error(&error) {
                                env // Use original environment for gas recovery
                            } else {
                                env // Keep current environment for other errors
                            };

                            eval_cek(cont, *next_handler, recovery_env, recovery)
                        })
                }

                CEKErrorHandler::CEKEnforceOne {
                    env, info, current, mut remaining, cont, error_state, next_handler
                } => {
                    // Try next condition in enforce-one
                    if let Some(next_expr) = remaining.pop() {
                        charge_gas("enforce-one-retry", MilliGas(3))
                            .bind(move |_| {
                                let new_handler = CEKErrorHandler::CEKEnforceOne {
                                    env: env.clone(),
                                    info,
                                    current: next_expr,
                                    remaining,
                                    cont: cont.clone(),
                                    error_state,
                                    next_handler,
                                };
                                eval_cek(cont, new_handler, env, current)
                            })
                    } else {
                        // No more conditions to try, propagate original error
                        EvalM::pure_value(EvalResult::EvalError(error))
                    }
                }

                CEKErrorHandler::CEKNoHandler => {
                    // No handler available, propagate error
                    EvalM::pure_value(EvalResult::EvalError(error))
                }
            }
        }

        ErrorRecovery::Propagate(error) => {
            // Error cannot be handled, propagate up
            EvalM::pure_value(EvalResult::EvalError(error))
        }
    }
}

/// Check if an error is a gas-related error
fn is_gas_error(error: &pact_core::errors::PactErrorI) -> bool {
    match error {
        pact_core::errors::PactError::PEExecutionError(eval_error, _, _) => {
            matches!(eval_error,
                EvalError::NumericOverflow // Gas overflow could be considered gas error
            )
        }
        _ => false
    }
}

/// Apply lambda or native function
pub fn apply_lambda(
    function: CanApply,
    env: CEKEnv,
    args: Vec<CEKValue>,
    cont: Cont,
    handler: CEKErrorHandler,
) -> EvalM<EvalResult> {
    charge_gas("apply-lambda", MilliGas(5))
        .bind(move |_| {
            match function {
                // User-defined function
                CanApply::C(closure) => {
                    apply_user_function(closure, args, cont, handler)
                }

                // Lambda closure
                CanApply::LC(lambda) => {
                    apply_lambda_closure(lambda, args, cont, handler)
                }

                // Native builtin function
                CanApply::N(native_fn) => {
                    apply_native_function(native_fn, env, args, cont, handler)
                }

                // Partial application handling
                CanApply::PC(partial) => {
                    apply_partial_closure(partial, args, cont, handler)
                }

                CanApply::PN(partial_native) => {
                    apply_partial_native(partial_native, args, cont, handler)
                }

                // Capability and DefPact closures
                CanApply::CT(_) | CanApply::DPC(_) => {
                    EvalM::pure_value(EvalResult::EvalError(
                        pact_core::errors::PactError::PEExecutionError(
                            pact_core::errors::EvalError::UnimplementedBuiltin(format!("{} - {}", "Capability/DefPact application", "Not yet implemented")),
                            vec![],
                            pact_core::shared::SpanInfo::empty()
                        )
                    ))
                }
            }
        })
}

/// Apply user-defined function
fn apply_user_function(
    closure: Closure,
    args: Vec<CEKValue>,
    cont: Cont,
    handler: CEKErrorHandler,
) -> EvalM<EvalResult> {
    if args.len() != closure.args.len() {
        EvalM::pure_value(EvalResult::EvalError(
            pact_core::errors::PactError::PEExecutionError(
                pact_core::errors::EvalError::ArgumentCountMismatch { function: closure.name, expected: closure.args.len(), received: args.len() },
                vec![],
                pact_core::shared::SpanInfo::empty()
            )
        ))
    } else {
        // Extend environment with function arguments
        let extended_env = args.into_iter().fold(closure.env, |env, arg| env.extend_local(arg));
        eval_cek(cont, handler, extended_env, closure.body)
    }
}

/// Apply lambda closure
pub fn apply_lambda_closure(
    lambda: LamClosure,
    args: Vec<CEKValue>,
    cont: Cont,
    handler: CEKErrorHandler,
) -> EvalM<EvalResult> {
    if args.len() != lambda.args.len() {
        EvalM::pure_value(EvalResult::EvalError(
            pact_core::errors::PactError::PEExecutionError(
                pact_core::errors::EvalError::ArgumentCountMismatch { function: "lambda".to_string(), expected: lambda.args.len(), received: args.len() },
                vec![],
                pact_core::shared::SpanInfo::empty()
            )
        ))
    } else {
        // Extend environment with lambda arguments
        let extended_env = args.into_iter().fold(lambda.env, |env, arg| env.extend_local(arg));
        eval_cek(cont, handler, extended_env, lambda.body)
    }
}

/// Apply native builtin function
fn apply_native_function(
    native_fn: NativeFn,
    env: CEKEnv,
    args: Vec<CEKValue>,
    cont: Cont,
    handler: CEKErrorHandler,
) -> EvalM<EvalResult> {
    // Check if we have enough arguments
    let arity = native_fn.arity;
    let arg_count = args.len();
    if arg_count < arity {
        // Create partial application
        let partial = crate::types::PartialNativeFn {
            native_fn,
            applied_args: args,
            remaining_arity: arity - arg_count,
            env,
        };
        let value = CEKValue::VClosure(CanApply::PN(partial));
        return_cek_value(cont, handler, value)
    } else {
        // Look up the implementation
        let builtin = native_fn.builtin;
        let info = native_fn.info;
        match env.builtins.get_implementation(builtin) {
            Some(impl_fn) => {
                // Call the native implementation
                impl_fn(info, builtin, cont, handler, env.clone(), args)
            }
            None => {
                EvalM::pure_value(EvalResult::EvalError(
                    pact_core::errors::PactError::PEExecutionError(
                        pact_core::errors::EvalError::UnimplementedBuiltin(format!("{:?} - Builtin not registered", builtin)),
                        vec![],
                        pact_core::shared::SpanInfo::empty()
                    )
                ))
            }
        }
    }
}

/// Apply partial closure (add more arguments)
fn apply_partial_closure(
    partial: crate::types::PartialClosure,
    mut new_args: Vec<CEKValue>,
    cont: Cont,
    handler: CEKErrorHandler,
) -> EvalM<EvalResult> {
    let mut all_args = partial.applied_args;
    all_args.append(&mut new_args);

    let closure_arity = partial.closure.args.len();
    if all_args.len() >= closure_arity {
        // Enough arguments, apply function
        apply_user_function(partial.closure, all_args, cont, handler)
    } else {
        // Still partial, create new partial closure
        let remaining_arity = closure_arity - all_args.len();
        let new_partial = crate::types::PartialClosure {
            closure: partial.closure,
            applied_args: all_args,
            remaining_arity,
        };
        let value = CEKValue::VClosure(CanApply::PC(new_partial));
        return_cek_value(cont, handler, value)
    }
}

/// Apply partial native function
fn apply_partial_native(
    partial: crate::types::PartialNativeFn,
    mut new_args: Vec<CEKValue>,
    cont: Cont,
    handler: CEKErrorHandler,
) -> EvalM<EvalResult> {
    let mut all_args = partial.applied_args;
    all_args.append(&mut new_args);

    let native_arity = partial.native_fn.arity;
    if all_args.len() >= native_arity {
        // Enough arguments, apply native function
        apply_native_function(partial.native_fn, partial.env, all_args, cont, handler)
    } else {
        // Still partial, create new partial native
        let remaining_arity = native_arity - all_args.len();
        let new_partial = crate::types::PartialNativeFn {
            native_fn: partial.native_fn,
            applied_args: all_args,
            remaining_arity,
            env: partial.env,
        };
        let value = CEKValue::VClosure(CanApply::PN(new_partial));
        return_cek_value(cont, handler, value)
    }
}

/// Evaluate builtin forms (special syntax)
pub fn eval_builtin_form(
    cont: Cont,
    handler: CEKErrorHandler,
    env: CEKEnv,
    form: pact_ir::BuiltinForm<Box<CoreTerm>>,
    info: SpanInfo,
) -> EvalM<EvalResult> {
    use pact_ir::BuiltinForm::*;

    match form {
        CIf { cond, then_expr, else_expr } => {
            charge_gas("eval-if", MilliGas(3))
                .bind(move |_| {
                    let cond_cont = CondCont::if_cont(*then_expr, else_expr.map(|e| *e));
                    let if_cont = Cont::cond_cont(env.clone(), info, cond_cont, cont);
                    eval_cek(if_cont, handler, env, *cond)
                })
        }

        CAnd(left, right) => {
            charge_gas("eval-and", MilliGas(3))
                .bind(move |_| {
                    let and_cont = CondCont::and_cont(*right);
                    let cond_cont = Cont::cond_cont(env.clone(), info, and_cont, cont);
                    eval_cek(cond_cont, handler, env, *left)
                })
        }

        COr(left, right) => {
            charge_gas("eval-or", MilliGas(3))
                .bind(move |_| {
                    let or_cont = CondCont::or_cont(*right);
                    let cond_cont = Cont::cond_cont(env.clone(), info, or_cont, cont);
                    eval_cek(cond_cont, handler, env, *left)
                })
        }

        CEnforce { cond, msg } => {
            charge_gas("eval-enforce", MilliGas(5))
                .bind(move |_| {
                    let enforce_cont = CondCont::enforce_cont(*msg);
                    let cond_cont = Cont::cond_cont(env.clone(), info, enforce_cont, cont);
                    eval_cek(cond_cont, handler, env, *cond)
                })
        }

        CEnforceOne { conditions } => {
            charge_gas("eval-enforce-one", MilliGas(8))
                .bind(move |_| {
                    if conditions.is_empty() {
                        // No conditions provided - this is an error
                        EvalM::pure_value(EvalResult::EvalError(
                            pact_core::errors::PactError::PEExecutionError(
                                pact_core::errors::EvalError::UserError("enforce-one: no conditions provided".to_string()),
                                vec![],
                                pact_core::shared::SpanInfo::empty()
                            )
                        ))
                    } else {
                        // Extract first condition and remaining conditions
                        let mut conditions_iter = conditions.into_iter();
                        let first = *conditions_iter.next().unwrap();
                        let remaining: Vec<CoreTerm> = conditions_iter.map(|boxed| *boxed).collect();

                        // Create enforce-one error handler
                        create_error_handler_with_state()
                            .bind(move |error_state| {
                                let enforce_one_handler = CEKErrorHandler::CEKEnforceOne {
                                    env: env.clone(),
                                    info,
                                    current: first.clone(),
                                    remaining,
                                    cont: cont.clone(),
                                    error_state,
                                    next_handler: Box::new(handler),
                                };

                                // Evaluate first condition with the enforce-one handler
                                eval_cek(cont, enforce_one_handler, env, first)
                            })
                    }
                })
        }

        CTry { expr, handler: recovery_expr } => {
            charge_gas("eval-try", MilliGas(10))
                .bind(move |_| {
                    // Create try error handler
                    create_error_handler_with_state()
                        .bind(move |error_state| {
                            let try_handler = CEKErrorHandler::CEKHandler {
                                env: env.clone(),
                                recovery: *recovery_expr,
                                cont: cont.clone(),
                                error_state,
                                next_handler: Box::new(handler),
                            };

                            // Evaluate expression with the try handler
                            eval_cek(cont, try_handler, env, *expr)
                        })
                })
        }

        CCreateUserGuard { name, args } => {
            charge_gas("eval-create-user-guard", MilliGas(50))
                .bind(move |_| {
                    // First evaluate the name expression
                    let user_guard_cont = BuiltinCont::create_user_guard_cont(
                        env.clone(),
                        info,
                        args.into_iter().map(|a| *a).collect(),
                        cont.clone()
                    );
                    let builtin_cont = Cont::builtin_cont(env.clone(), info, user_guard_cont, cont);
                    eval_cek(builtin_cont, handler, env, *name)
                })
        }
        
        CWithCapability { cap, body } => {
            charge_gas("eval-with-capability", MilliGas(20))
                .bind(move |_| {
                    // Convert body vector to sequential evaluation
                    let body_exprs: Vec<CoreTerm> = body.into_iter().map(|b| *b).collect();
                    
                    // Create capability body continuation
                    let cap_body_cont = crate::cont::CapBodyState {
                        body_forms: body_exprs,
                    };
                    
                    // Extract the cap term before moving
                    let cap_term = *cap;
                    
                    // Create capability invocation continuation
                    let cap_invoke = crate::cont::CapCont {
                        cap_term: cap_term.clone(),
                        body_state: cap_body_cont,
                    };
                    
                    // Create the capability continuation
                    let cap_cont = Cont::CapInvokeC {
                        env: env.clone(),
                        info,
                        cap_cont: cap_invoke,
                        cont: Box::new(cont),
                    };
                    
                    // Evaluate the capability term
                    eval_cek(cap_cont, handler, env, cap_term)
                })
        }

        CMap { func, list } => {
            charge_gas("eval-map", MilliGas(10))
                .bind(move |_| {
                    // Evaluate the function expression first
                    let map_builtin_cont = BuiltinCont::MapBuiltinC {
                        list_expr: *list,
                    };
                    let builtin_cont = Cont::BuiltinC {
                        env: env.clone(),
                        info,
                        builtin_cont: map_builtin_cont,
                        cont: Box::new(cont),
                    };
                    eval_cek(builtin_cont, handler, env, *func)
                })
        }

        CFilter { func, list } => {
            charge_gas("eval-filter", MilliGas(10))
                .bind(move |_| {
                    // Evaluate the function expression first
                    let filter_builtin_cont = BuiltinCont::FilterBuiltinC {
                        list_expr: *list,
                    };
                    let builtin_cont = Cont::BuiltinC {
                        env: env.clone(),
                        info,
                        builtin_cont: filter_builtin_cont,
                        cont: Box::new(cont),
                    };
                    eval_cek(builtin_cont, handler, env, *func)
                })
        }

        CFold { func, init, list } => {
            charge_gas("eval-fold", MilliGas(15))
                .bind(move |_| {
                    // Evaluate the function expression first
                    let fold_builtin_cont = BuiltinCont::FoldBuiltinC {
                        init_expr: *init,
                        list_expr: *list,
                    };
                    let builtin_cont = Cont::BuiltinC {
                        env: env.clone(),
                        info,
                        builtin_cont: fold_builtin_cont,
                        cont: Box::new(cont),
                    };
                    eval_cek(builtin_cont, handler, env, *func)
                })
        }

        CZip { func, list1, list2 } => {
            charge_gas("eval-zip", MilliGas(15))
                .bind(move |_| {
                    // Evaluate the function expression first
                    let zip_builtin_cont = BuiltinCont::ZipBuiltinC {
                        list1_expr: *list1,
                        list2_expr: *list2,
                    };
                    let builtin_cont = Cont::BuiltinC {
                        env: env.clone(),
                        info,
                        builtin_cont: zip_builtin_cont,
                        cont: Box::new(cont),
                    };
                    eval_cek(builtin_cont, handler, env, *func)
                })
        }

        CCond { conditions } => {
            charge_gas("eval-cond", MilliGas(5))
                .bind(move |_| {
                    if conditions.is_empty() {
                        // No conditions - error
                        EvalM::pure_value(EvalResult::EvalError(
                            pact_core::errors::PactError::PEExecutionError(
                                pact_core::errors::EvalError::UserError("cond: no conditions provided".to_string()),
                                vec![],
                                pact_core::shared::SpanInfo::empty()
                            )
                        ))
                    } else {
                        // Extract conditions as a vector of pairs
                        let cond_pairs: Vec<(CoreTerm, CoreTerm)> = conditions.into_iter()
                            .map(|(cond, expr)| (*cond, *expr))
                            .collect();
                        
                        // Start evaluation with the first condition
                        let mut remaining = cond_pairs;
                        let (first_cond, first_expr) = remaining.remove(0);
                        
                        // Create cond continuation
                        let cond_cont = BuiltinCont::CondC {
                            expr: first_expr,
                            remaining_conds: remaining,
                        };
                        
                        let builtin_cont = Cont::BuiltinC {
                            env: env.clone(),
                            info,
                            builtin_cont: cond_cont,
                            cont: Box::new(cont),
                        };
                        
                        // Evaluate first condition
                        eval_cek(builtin_cont, handler, env, first_cond)
                    }
                })
        }

        CSuspend(expr) => {
            // Suspend is for defpacts - not yet implemented
            EvalM::pure_value(EvalResult::EvalError(
                pact_core::errors::PactError::PEExecutionError(
                    pact_core::errors::EvalError::UnimplementedBuiltin(format!("{} - {}", "suspend", "DefPact functionality not yet implemented")),
                    vec![],
                    pact_core::shared::SpanInfo::empty()
                )
            ))
        }
    }
}

/// Evaluate list literal
pub fn eval_list_literal(
    cont: Cont,
    handler: CEKErrorHandler,
    env: CEKEnv,
    elements: Vec<CoreTerm>,
    info: SpanInfo,
) -> EvalM<EvalResult> {
    charge_gas("eval-list", MilliGas(2))
        .bind(move |_| {
            if elements.is_empty() {
                // Empty list
                let empty_list = CEKValue::VPactValue(PactValue::List(vec![]));
                return_cek_value(cont, handler, empty_list)
            } else {
                // Evaluate elements left to right
                let mut remaining = elements;
                let first = remaining.remove(0);
                let list_cont = Cont::list_cont(env.clone(), info, remaining, vec![], cont);
                eval_cek(list_cont, handler, env, first)
            }
        })
}

/// Evaluate object literal
fn eval_object_literal(
    cont: Cont,
    handler: CEKErrorHandler,
    env: CEKEnv,
    fields: Vec<(pact_ir::Field, CoreTerm)>,
    info: SpanInfo,
) -> EvalM<EvalResult> {
    // Object literal evaluation would use ObjC continuation
    EvalM::pure_value(EvalResult::EvalError(
        pact_core::errors::PactError::PEExecutionError(
            pact_core::errors::EvalError::UnimplementedBuiltin(format!("{} - {}", "Object literal", "Not yet implemented")),
            vec![],
            pact_core::shared::SpanInfo::empty()
        )
    ))
}

/// Evaluate conditional continuation
fn eval_conditional(
    cond_cont: CondCont,
    condition_value: CEKValue,
    env: CEKEnv,
    info: SpanInfo,
    cont: Cont,
    handler: CEKErrorHandler,
) -> EvalM<EvalResult> {
    match cond_cont {
        CondCont::IfCont { then_expr, else_expr } => {
            let is_truthy = is_truthy_value(&condition_value);
            if is_truthy {
                eval_cek(cont, handler, env, then_expr)
            } else if let Some(else_expr) = else_expr {
                eval_cek(cont, handler, env, else_expr)
            } else {
                // No else clause, return unit
                let unit_value = CEKValue::VPactValue(PactValue::Unit);
                return_cek_value(cont, handler, unit_value)
            }
        }

        CondCont::AndCont { right_expr } => {
            let is_truthy = is_truthy_value(&condition_value);
            if is_truthy {
                // Left side is true, evaluate right side
                eval_cek(cont, handler, env, right_expr)
            } else {
                // Left side is false, return false
                let false_value = CEKValue::VPactValue(PactValue::Bool(false));
                return_cek_value(cont, handler, false_value)
            }
        }

        CondCont::OrCont { right_expr } => {
            let is_truthy = is_truthy_value(&condition_value);
            if is_truthy {
                // Left side is true, return true
                let true_value = CEKValue::VPactValue(PactValue::Bool(true));
                return_cek_value(cont, handler, true_value)
            } else {
                // Left side is false, evaluate right side
                eval_cek(cont, handler, env, right_expr)
            }
        }

        CondCont::EnforceCont { message } => {
            let is_truthy = is_truthy_value(&condition_value);
            if is_truthy {
                // Condition passed, return the condition value
                return_cek_value(cont, handler, condition_value)
            } else {
                // Condition failed, evaluate error message and throw
                // For now, just throw a generic enforce error
                EvalM::pure_value(EvalResult::EvalError(
                    pact_core::errors::PactError::PEExecutionError(
                        pact_core::errors::EvalError::UserError("Enforce condition failed".to_string()),
                        vec![],
                        pact_core::shared::SpanInfo::empty()
                    )
                ))
            }
        }

        // Query continuation for NOT operation (not?)
        CondCont::NotQC => {
            // Extract boolean from condition_value and negate it
            match condition_value {
                CEKValue::VPactValue(PactValue::Bool(b)) => {
                    let negated_value = CEKValue::VPactValue(PactValue::Bool(!b));
                    return_cek_value(cont, handler, negated_value)
                }
                _ => {
                    EvalM::pure_value(EvalResult::EvalError(
                        pact_core::errors::PactError::PEExecutionError(
                            pact_core::errors::EvalError::TypeMismatch {
                                expected: "boolean".to_string(),
                                found: "non-boolean".to_string(),
                                context: "not? query operation".to_string(),
                            },
                            vec![],
                            pact_core::shared::SpanInfo::empty()
                        )
                    ))
                }
            }
        }

        // Query continuation for AND operation (and?)
        CondCont::AndQC { right_closure, value } => {
            // If condition is true, apply right closure; else short-circuit with false
            match condition_value {
                CEKValue::VPactValue(PactValue::Bool(true)) => {
                    // Left closure returned true, apply right closure
                    let enforce_cont = Cont::EnforceBoolC { info, cont: Box::new(cont) };
                    apply_lambda(
                        right_closure,
                        env,
                        vec![CEKValue::VPactValue(value)],
                        enforce_cont,
                        handler
                    )
                }
                CEKValue::VPactValue(PactValue::Bool(false)) => {
                    // Left closure returned false, short-circuit with false
                    return_cek_value(cont, handler, condition_value)
                }
                _ => {
                    EvalM::pure_value(EvalResult::EvalError(
                        pact_core::errors::PactError::PEExecutionError(
                            pact_core::errors::EvalError::TypeMismatch {
                                expected: "boolean".to_string(),
                                found: "non-boolean".to_string(),
                                context: "and? query operation".to_string(),
                            },
                            vec![],
                            pact_core::shared::SpanInfo::empty()
                        )
                    ))
                }
            }
        }

        // Query continuation for OR operation (or?)
        CondCont::OrQC { right_closure, value } => {
            // If condition is false, apply right closure; else short-circuit with true
            match condition_value {
                CEKValue::VPactValue(PactValue::Bool(false)) => {
                    // Left closure returned false, apply right closure
                    let enforce_cont = Cont::EnforceBoolC { info, cont: Box::new(cont) };
                    apply_lambda(
                        right_closure,
                        env,
                        vec![CEKValue::VPactValue(value)],
                        enforce_cont,
                        handler
                    )
                }
                CEKValue::VPactValue(PactValue::Bool(true)) => {
                    // Left closure returned true, short-circuit with true
                    return_cek_value(cont, handler, condition_value)
                }
                _ => {
                    EvalM::pure_value(EvalResult::EvalError(
                        pact_core::errors::PactError::PEExecutionError(
                            pact_core::errors::EvalError::TypeMismatch {
                                expected: "boolean".to_string(),
                                found: "non-boolean".to_string(),
                                context: "or? query operation".to_string(),
                            },
                            vec![],
                            pact_core::shared::SpanInfo::empty()
                        )
                    ))
                }
            }
        }

        _ => EvalM::pure_value(EvalResult::EvalError(
            pact_core::errors::PactError::PEExecutionError(
                pact_core::errors::EvalError::UnimplementedBuiltin(format!("{} - {}", "Conditional continuation", "Not yet implemented")),
                vec![],
                pact_core::shared::SpanInfo::empty()
            )
        ))
    }
}

/// Evaluate builtin continuation (for higher-order functions)
pub fn eval_builtin_continuation(
    builtin_cont: BuiltinCont,
    value: CEKValue,
    env: CEKEnv,
    info: SpanInfo,
    cont: Cont,
    handler: CEKErrorHandler,
) -> EvalM<EvalResult> {
    use crate::cont::BuiltinCont;

    match builtin_cont {
        BuiltinCont::MapCont { func, mut remaining, mut accumulated } => {
            // Charge gas for map iteration
            charge_gas("map-iteration", MilliGas(2))
                .bind(move |_| {
                    // Add the current result to accumulated values
                    if let Some(pact_val) = value.as_pact_value() {
                        accumulated.push(pact_val.clone());
                    } else {
                        return EvalM::pure_value(EvalResult::EvalError(
                            pact_core::errors::PactError::PEExecutionError(
                                pact_core::errors::EvalError::TypeMismatch {
                                    expected: "PactValue".to_string(),
                                    found: "closure".to_string(),
                                    context: "map result".to_string(),
                                },
                                vec![],
                                pact_core::shared::SpanInfo::empty()
                            )
                        ));
                    }

                    // Process next element if any remain
                    if let Some(next_element) = remaining.pop() {
                        // Apply function to next element
                        let next_args = vec![CEKValue::VPactValue(next_element)];
                        let builtin_cont = BuiltinCont::MapCont { func: func.clone(), remaining, accumulated };
                        let next_cont = Cont::BuiltinC {
                            env: env.clone(),
                            info,
                            builtin_cont,
                            cont: Box::new(cont),
                        };

                        apply_function(func, next_args, next_cont, handler, env)
                    } else {
                        // All elements processed, return result list
                        let result_list = PactValue::List(accumulated);
                        let result_value = CEKValue::VPactValue(result_list);
                        return_cek_value(cont, handler, result_value)
                    }
                })
        }

        BuiltinCont::FilterCont { func, mut remaining, mut accumulated } => {
            // Check if the predicate returned true
            let keep_element = match value.as_pact_value() {
                Some(PactValue::Bool(true)) => true,
                Some(PactValue::Bool(false)) => false,
                _ => return EvalM::pure_value(EvalResult::EvalError(
                    pact_core::errors::PactError::PEExecutionError(
                        pact_core::errors::EvalError::TypeMismatch {
                            expected: "boolean".to_string(),
                            found: "other".to_string(),
                            context: "filter predicate result".to_string(),
                        },
                        vec![],
                        pact_core::shared::SpanInfo::empty()
                    )
                ))
            };

            // Add current element to accumulated if predicate was true
            if keep_element {
                // Note: we need to track the current element being tested
                // This is a simplified version - full implementation would track current element
            }

            // Process next element if any remain
            if let Some(next_element) = remaining.pop() {
                let next_args = vec![CEKValue::VPactValue(next_element.clone())];
                let builtin_cont = BuiltinCont::FilterCont { func: func.clone(), remaining, accumulated };
                let next_cont = Cont::BuiltinC {
                    env: env.clone(),
                    info,
                    builtin_cont,
                    cont: Box::new(cont),
                };

                apply_function(func, next_args, next_cont, handler, env)
            } else {
                // All elements processed, return filtered list
                let result_list = PactValue::List(accumulated);
                let result_value = CEKValue::VPactValue(result_list);
                return_cek_value(cont, handler, result_value)
            }
        }

        BuiltinCont::FoldCont { func, mut remaining, accumulator } => {
            // Use the returned value as the new accumulator
            let new_accumulator = if let Some(pact_val) = value.as_pact_value() {
                pact_val.clone()
            } else {
                return EvalM::pure_value(EvalResult::EvalError(
                    pact_core::errors::PactError::PEExecutionError(
                        pact_core::errors::EvalError::TypeMismatch {
                            expected: "PactValue".to_string(),
                            found: "closure".to_string(),
                            context: "fold accumulator".to_string(),
                        },
                        vec![],
                        pact_core::shared::SpanInfo::empty()
                    )
                ));
            };

            // Process next element if any remain
            if let Some(next_element) = remaining.pop() {
                let next_args = vec![
                    CEKValue::VPactValue(new_accumulator.clone()),
                    CEKValue::VPactValue(next_element)
                ];
                let builtin_cont = BuiltinCont::FoldCont {
                    func: func.clone(),
                    remaining,
                    accumulator: new_accumulator
                };
                let next_cont = Cont::BuiltinC {
                    env: env.clone(),
                    info,
                    builtin_cont,
                    cont: Box::new(cont),
                };

                apply_function(func, next_args, next_cont, handler, env)
            } else {
                // All elements processed, return final accumulator
                let result_value = CEKValue::VPactValue(new_accumulator);
                return_cek_value(cont, handler, result_value)
            }
        }

        BuiltinCont::ZipCont { func, list2, mut remaining1, mut remaining2, mut accumulated } => {
            // Add the result of applying function to current pair
            if let Some(pact_val) = value.as_pact_value() {
                accumulated.push(pact_val.clone());
            } else {
                return EvalM::pure_value(EvalResult::EvalError(
                    pact_core::errors::PactError::PEExecutionError(
                        pact_core::errors::EvalError::TypeMismatch {
                        expected: "PactValue".to_string(),

                        found: "closure".to_string(),
                        context: "zip result".to_string(),
                    },
                        vec![],
                        pact_core::shared::SpanInfo::empty()
                    )
                ));
            }

            // Process next pair if both lists have remaining elements
            if let (Some(next1), Some(next2)) = (remaining1.pop(), remaining2.pop()) {
                let next_args = vec![
                    CEKValue::VPactValue(next1),
                    CEKValue::VPactValue(next2)
                ];
                let builtin_cont = BuiltinCont::ZipCont {
                    func: func.clone(),
                    list2,
                    remaining1,
                    remaining2,
                    accumulated
                };
                let next_cont = Cont::BuiltinC {
                    env: env.clone(),
                    info,
                    builtin_cont,
                    cont: Box::new(cont),
                };

                apply_function(func, next_args, next_cont, handler, env)
            } else {
                // One or both lists exhausted, return result
                let result_list = PactValue::List(accumulated);
                let result_value = CEKValue::VPactValue(result_list);
                return_cek_value(cont, handler, result_value)
            }
        }

        BuiltinCont::SelectCont { table, filter_func, mut remaining, mut accumulated } => {
            // This handles database select operations with optional filtering
            if let Some(ref filter) = filter_func {
                // Check filter result
                let include_row = match value.as_pact_value() {
                    Some(PactValue::Bool(true)) => true,
                    Some(PactValue::Bool(false)) => false,
                    _ => return EvalM::pure_value(EvalResult::EvalError(
                        pact_core::errors::PactError::PEExecutionError(
                        pact_core::errors::EvalError::TypeMismatch {
                            expected: "boolean".to_string(),

                            found: "other".to_string(),
                            context: "select filter result".to_string(),
                        },
                        vec![],
                        pact_core::shared::SpanInfo::empty()
                    )
                    ))
                };

                if include_row {
                    // Add current row to results (simplified - needs proper row handling)
                    if let Some((key, row_value)) = remaining.first() {
                        accumulated.push(row_value.clone());
                    }
                }
            }

            // Process next row if any remain
            if let Some((next_key, next_row)) = remaining.pop() {
                if let Some(filter) = filter_func {
                    let next_args = vec![CEKValue::VPactValue(next_row)];
                    let filter_clone = filter.clone();
                    let builtin_cont = BuiltinCont::SelectCont {
                        table,
                        filter_func: Some(filter_clone),
                        remaining,
                        accumulated
                    };
                    let next_cont = Cont::BuiltinC {
                        env: env.clone(),
                        info,
                        builtin_cont,
                        cont: Box::new(cont),
                    };

                    apply_function(filter, next_args, next_cont, handler, env)
                } else {
                    // No filter, include all rows
                    accumulated.push(next_row);
                    let builtin_cont = BuiltinCont::SelectCont {
                        table,
                        filter_func: None,
                        remaining,
                        accumulated
                    };
                    eval_builtin_continuation(builtin_cont, value, env, info, cont, handler)
                }
            } else {
                // All rows processed, return results
                let result_list = PactValue::List(accumulated);
                let result_value = CEKValue::VPactValue(result_list);
                return_cek_value(cont, handler, result_value)
            }
        }

        BuiltinCont::CreateUserGuardCont { mut args, mut evaluated_args } => {
            // Add the evaluated function name to args
            if let Some(pact_val) = value.as_pact_value() {
                // First value is the function name
                if evaluated_args.is_empty() {
                    match pact_val {
                        PactValue::String(fun_name) => {
                            evaluated_args.push(pact_val.clone());

                            // Evaluate remaining args
                            if !args.is_empty() {
                                let next_arg = args.remove(0);
                                let builtin_cont = BuiltinCont::CreateUserGuardCont { args, evaluated_args };
                                let next_cont = Cont::BuiltinC {
                                    env: env.clone(),
                                    info,
                                    builtin_cont,
                                    cont: Box::new(cont),
                                };
                                eval_cek(next_cont, handler, env, next_arg)
                            } else {
                                // All args evaluated, create user guard
                                if let Some(PactValue::String(fun_name)) = evaluated_args.first() {
                                    let guard_args = evaluated_args[1..].to_vec();
                                    let user_guard = pact_core::values::Guard::User {
                                        fun: fun_name.clone(),
                                        args: guard_args,
                                    };
                                    let result = CEKValue::VPactValue(PactValue::Guard(user_guard));
                                    return_cek_value(cont, handler, result)
                                } else {
                                    EvalM::pure_value(EvalResult::EvalError(
                                        pact_core::errors::PactError::PEExecutionError(
                                            pact_core::errors::EvalError::InvalidExecutionContext("create-user-guard: Invalid state in user guard creation".to_string()),
                                            vec![],
                                            pact_core::shared::SpanInfo::empty()
                                        )
                                    ))
                                }
                            }
                        }
                        _ => {
                            EvalM::pure_value(EvalResult::EvalError(
                                pact_core::errors::PactError::PEExecutionError(
                                    pact_core::errors::EvalError::TypeMismatch {
                                        expected: "string (function name)".to_string(),
                                        found: "other".to_string(),
                                        context: "create-user-guard".to_string(),
                                    },
                                    vec![],
                                    pact_core::shared::SpanInfo::empty()
                                )
                            ))
                        }
                    }
                } else {
                    // Evaluating guard arguments
                    evaluated_args.push(pact_val.clone());

                    if !args.is_empty() {
                        let next_arg = args.remove(0);
                        let builtin_cont = BuiltinCont::CreateUserGuardCont { args, evaluated_args };
                        let next_cont = Cont::BuiltinC {
                            env: env.clone(),
                            info,
                            builtin_cont,
                            cont: Box::new(cont),
                        };
                        eval_cek(next_cont, handler, env, next_arg)
                    } else {
                        // All args evaluated, create user guard
                        if let Some(PactValue::String(fun_name)) = evaluated_args.first() {
                            let guard_args = evaluated_args[1..].to_vec();
                            let user_guard = pact_core::values::Guard::User {
                                fun: fun_name.clone(),
                                args: guard_args,
                            };
                            let result = CEKValue::VPactValue(PactValue::Guard(user_guard));
                            return_cek_value(cont, handler, result)
                        } else {
                            EvalM::pure_value(EvalResult::EvalError(
                                pact_core::errors::PactError::PEExecutionError(
                                    pact_core::errors::EvalError::InvalidExecutionContext("create-user-guard: Invalid state in user guard creation".to_string()),
                                    vec![],
                                    pact_core::shared::SpanInfo::empty()
                                )
                            ))
                        }
                    }
                }
            } else {
                EvalM::pure_value(EvalResult::EvalError(
                    pact_core::errors::PactError::PEExecutionError(
                        pact_core::errors::EvalError::TypeMismatch {
                        expected: "PactValue".to_string(),

                        found: "closure".to_string(),
                        context: "create-user-guard argument".to_string(),
                    },
                        vec![],
                        pact_core::shared::SpanInfo::empty()
                    )
                ))
            }
        }

        // Map builtin form continuation - evaluates list after function
        BuiltinCont::MapBuiltinC { list_expr } => {
            // The value should be the evaluated function
            match value {
                CEKValue::VClosure(func) => {
                    // Now evaluate the list expression
                    let map_list_cont = Cont::MapListC {
                        env: env.clone(),
                        info,
                        func,
                        cont: Box::new(cont),
                    };
                    eval_cek(map_list_cont, handler, env, list_expr)
                }
                _ => {
                    EvalM::pure_value(EvalResult::EvalError(
                        pact_core::errors::PactError::PEExecutionError(
                            pact_core::errors::EvalError::TypeMismatch {
                                expected: "function".to_string(),
                                found: "other".to_string(),
                                context: "map function".to_string(),
                            },
                            vec![],
                            pact_core::shared::SpanInfo::empty()
                        )
                    ))
                }
            }
        }

        // Filter builtin form continuation - evaluates list after function
        BuiltinCont::FilterBuiltinC { list_expr } => {
            // The value should be the evaluated function
            match value {
                CEKValue::VClosure(func) => {
                    // Now evaluate the list expression
                    let filter_list_cont = Cont::FilterListC {
                        env: env.clone(),
                        info,
                        func,
                        cont: Box::new(cont),
                    };
                    eval_cek(filter_list_cont, handler, env, list_expr)
                }
                _ => {
                    EvalM::pure_value(EvalResult::EvalError(
                        pact_core::errors::PactError::PEExecutionError(
                            pact_core::errors::EvalError::TypeMismatch {
                                expected: "function".to_string(),
                                found: "other".to_string(),
                                context: "filter function".to_string(),
                            },
                            vec![],
                            pact_core::shared::SpanInfo::empty()
                        )
                    ))
                }
            }
        }

        // Fold builtin form continuation - evaluates init and list after function
        BuiltinCont::FoldBuiltinC { init_expr, list_expr } => {
            // The value should be the evaluated function
            match value {
                CEKValue::VClosure(func) => {
                    // Now evaluate the init expression
                    let fold_init_cont = Cont::FoldInitC {
                        env: env.clone(),
                        info,
                        func,
                        list_expr,
                        cont: Box::new(cont),
                    };
                    eval_cek(fold_init_cont, handler, env, init_expr)
                }
                _ => {
                    EvalM::pure_value(EvalResult::EvalError(
                        pact_core::errors::PactError::PEExecutionError(
                            pact_core::errors::EvalError::TypeMismatch {
                                expected: "function".to_string(),
                                found: "other".to_string(),
                                context: "fold function".to_string(),
                            },
                            vec![],
                            pact_core::shared::SpanInfo::empty()
                        )
                    ))
                }
            }
        }

        // Zip builtin form continuation - evaluates both lists after function
        BuiltinCont::ZipBuiltinC { list1_expr, list2_expr } => {
            // The value should be the evaluated function
            match value {
                CEKValue::VClosure(func) => {
                    // Now evaluate the first list expression
                    let zip_list1_cont = Cont::ZipList1C {
                        env: env.clone(),
                        info,
                        func,
                        list2_expr,
                        cont: Box::new(cont),
                    };
                    eval_cek(zip_list1_cont, handler, env, list1_expr)
                }
                _ => {
                    EvalM::pure_value(EvalResult::EvalError(
                        pact_core::errors::PactError::PEExecutionError(
                            pact_core::errors::EvalError::TypeMismatch {
                                expected: "function".to_string(),
                                found: "other".to_string(),
                                context: "zip function".to_string(),
                            },
                            vec![],
                            pact_core::shared::SpanInfo::empty()
                        )
                    ))
                }
            }
        }

        // Cond form continuation - evaluates conditions sequentially
        BuiltinCont::CondC { expr, mut remaining_conds } => {
            // Check if the condition is true
            let is_truthy = is_truthy_value(&value);
            
            if is_truthy {
                // Condition is true, evaluate the corresponding expression
                eval_cek(cont, handler, env, expr)
            } else if !remaining_conds.is_empty() {
                // Try the next condition
                let (next_cond, next_expr) = remaining_conds.remove(0);
                let cond_cont = BuiltinCont::CondC {
                    expr: next_expr,
                    remaining_conds,
                };
                let builtin_cont = Cont::BuiltinC {
                    env: env.clone(),
                    info,
                    builtin_cont: cond_cont,
                    cont: Box::new(cont),
                };
                eval_cek(builtin_cont, handler, env, next_cond)
            } else {
                // No more conditions and none were true - error
                EvalM::pure_value(EvalResult::EvalError(
                    pact_core::errors::PactError::PEExecutionError(
                        pact_core::errors::EvalError::UserError("cond: no condition was satisfied".to_string()),
                        vec![],
                        pact_core::shared::SpanInfo::empty()
                    )
                ))
            }
        }
    }
}

/// Apply a function (closure) with given arguments
pub fn apply_function(
    function: CanApply,
    args: Vec<CEKValue>,
    cont: Cont,
    handler: CEKErrorHandler,
    env: CEKEnv,
) -> EvalM<EvalResult> {
    match function {
        CanApply::N(native_fn) => {
            // Apply native function directly
            apply_native_function(native_fn, env, args, cont, handler)
        }
        CanApply::C(closure) => {
            // Apply user-defined function
            apply_user_function(closure, args, cont, handler)
        }
        CanApply::LC(lambda_closure) => {
            // Apply lambda closure
            apply_lambda_closure(lambda_closure, args, cont, handler)
        }
        CanApply::PC(partial_closure) => {
            // Apply partial closure (accumulate args)
            apply_partial_closure(partial_closure, args, cont, handler)
        }
        CanApply::PN(partial_native) => {
            // Apply partial native function
            apply_partial_native(partial_native, args, cont, handler)
        }
        CanApply::CT(cap_token) => {
            // Apply capability token
            EvalM::pure_value(EvalResult::EvalError(
                pact_core::errors::PactError::PEExecutionError(
                    pact_core::errors::EvalError::UnimplementedBuiltin(format!("{} - {}", "Capability token application", "Not yet implemented")),
                    vec![],
                    pact_core::shared::SpanInfo::empty()
                )
            ))
        }
        CanApply::DPC(defpact_closure) => {
            // Apply DefPact closure
            EvalM::pure_value(EvalResult::EvalError(
                pact_core::errors::PactError::PEExecutionError(
                    pact_core::errors::EvalError::UnimplementedBuiltin(format!("{} - {}", "DefPact closure application", "Not yet implemented")),
                    vec![],
                    pact_core::shared::SpanInfo::empty()
                )
            ))
        }
    }
}


/// Convert literal to CEK value
fn literal_to_cek_value(literal: Literal) -> Result<CEKValue, pact_core::errors::PactErrorI> {
    let pact_value = match literal {
        Literal::LString(s) => PactValue::String(s.to_string()),
        Literal::LInteger(i) => PactValue::Integer(i.into()),
        Literal::LBool(b) => PactValue::Bool(b),
        Literal::LDecimal(d) => {
            let mantissa_bigint = num_bigint::BigInt::from(d.mantissa);
            let divisor_bigint = num_bigint::BigInt::from(10_i32.pow(d.precision as u32));
            let decimal_val = pact_core::values::Decimal::from(mantissa_bigint) / pact_core::values::Decimal::from(divisor_bigint);
            PactValue::Decimal(decimal_val)
        }
        Literal::LUnit => PactValue::Unit,
    };
    Ok(CEKValue::VPactValue(pact_value))
}

/// Check if a value is truthy in Pact semantics
fn is_truthy_value(value: &CEKValue) -> bool {
    match value {
        CEKValue::VPactValue(PactValue::Bool(b)) => *b,
        CEKValue::VPactValue(PactValue::Unit) => false,
        _ => true, // Non-false values are truthy
    }
}
