use super::*;
use pact_core::values::PactValue;
use num_traits::ToPrimitive;
use crate::builtin::{throw_argument_count_error, throw_type_mismatch_error};
use crate::monad::{charge_gas, charge_gas_with_args};
use crate::eval::return_cek_value;

/// Helper function to create a builtin error with current stack frame
fn builtin_error<T>(error: pact_core::errors::EvalError) -> Result<T, pact_core::errors::PactErrorI> {
    // In a real implementation, we would get the current stack from the monad state
    // For now, we'll use an empty vec but with a TODO to fix
    // TODO: Get actual stack frames from EvalM state
    Err(pact_core::errors::PactError::PEExecutionError(
        error,
        vec![], // TODO: Get from EvalM::get_stack()
        pact_core::shared::SpanInfo::empty() // TODO: Get from current context
    ))
}

/// Helper function to create a builtin error with specific info
fn builtin_error_with_info<T>(error: pact_core::errors::EvalError, info: pact_core::shared::SpanInfo) -> Result<T, pact_core::errors::PactErrorI> {
    // TODO: Get actual stack frames from EvalM state
    Err(pact_core::errors::PactError::PEExecutionError(
        error,
        vec![], // TODO: Get from EvalM::get_stack()
        info
    ))
}

/// Helper function to create an error result for EvalM
fn builtin_error_result(error: pact_core::errors::EvalError, info: pact_core::shared::SpanInfo) -> EvalM<EvalResult> {
    // TODO: Get actual stack frames from EvalM state
    EvalM::pure_value(EvalResult::EvalError(
        pact_core::errors::PactError::PEExecutionError(
            error,
            vec![], // TODO: Get from EvalM::get_stack()
            info
        )
    ))
}


pub fn register_list_builtins(builtin_env: &mut BuiltinEnv) -> Result<(), pact_core::errors::PactErrorI> {
    // List-specific operations
    builtin_env.register(CoreBuiltin::CoreMakeList, BuiltinSpec {
        name: "make-list",
        arity: 2,
        implementation: make_list_implementation(),
    });

    builtin_env.register(CoreBuiltin::CoreStrToList, BuiltinSpec {
        name: "str-to-list",
        arity: 1,
        implementation: str_to_list_implementation(),
    });

    builtin_env.register(CoreBuiltin::CoreEnumerate, BuiltinSpec {
        name: "enumerate",
        arity: 2,
        implementation: enumerate_implementation(),
    });

    builtin_env.register(CoreBuiltin::CoreEnumerateStepN, BuiltinSpec {
        name: "enumerate",
        arity: 3,  // Overloaded version with step
        implementation: enumerate_step_implementation(),
    });

    builtin_env.register(CoreBuiltin::CoreDistinct, BuiltinSpec {
        name: "distinct",
        arity: 1,
        implementation: distinct_implementation(),
    });

    // Higher-order functions with full CEK integration
    builtin_env.register(CoreBuiltin::CoreMap, BuiltinSpec {
        name: "map",
        arity: 2,
        implementation: map_implementation(),
    });

    builtin_env.register(CoreBuiltin::CoreFilter, BuiltinSpec {
        name: "filter",
        arity: 2,
        implementation: filter_implementation(),
    });

    builtin_env.register(CoreBuiltin::CoreFold, BuiltinSpec {
        name: "fold",
        arity: 3,
        implementation: fold_implementation(),
    });

    builtin_env.register(CoreBuiltin::CoreZip, BuiltinSpec {
        name: "zip",
        arity: 3,
        implementation: zip_implementation(),
    });

    builtin_env.register(CoreBuiltin::CoreSort, BuiltinSpec {
        name: "sort",
        arity: 1,
        implementation: sort_implementation(),
    });

    builtin_env.register(CoreBuiltin::CoreReverse, BuiltinSpec {
        name: "reverse",
        arity: 1,
        implementation: reverse_implementation(),
    });

    builtin_env.register(CoreBuiltin::CoreContains, BuiltinSpec {
        name: "contains",
        arity: 2,
        implementation: contains_implementation(),
    });

    builtin_env.register(CoreBuiltin::CoreWhere, BuiltinSpec {
        name: "where",
        arity: 3,
        implementation: where_implementation(),
    });

    Ok(())
}


/// Zip implementation - combines two lists with a function
pub fn zip_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, env, args| {
        charge_gas_with_args("zip", &args, MilliGas(10))
            .bind(move |_| {
                if args.len() != 3 {
                    throw_argument_count_error(info, "zip", 3, args.len())
                } else {
                    match (&args[0], &args[1], &args[2]) {
                        (CEKValue::VClosure(func), CEKValue::VPactValue(PactValue::List(list1)), 
                         CEKValue::VPactValue(PactValue::List(list2))) => {
                            if list1.is_empty() || list2.is_empty() {
                                // Empty list - return empty result
                                let empty_result = CEKValue::VPactValue(PactValue::List(vec![]));
                                return_cek_value(cont, handler, empty_result)
                            } else {
                                // Start zip operation with first elements
                                let mut remaining1 = list1.clone();
                                let mut remaining2 = list2.clone();
                                let elem1 = remaining1.remove(0);
                                let elem2 = remaining2.remove(0);
                                
                                // Create zip continuation
                                let zip_cont = crate::cont::BuiltinCont::ZipCont {
                                    func: func.clone(),
                                    list2: list2.clone(),
                                    remaining1,
                                    remaining2,
                                    accumulated: vec![],
                                };
                                
                                let builtin_cont = Cont::BuiltinC {
                                    env: env.clone(),
                                    info,
                                    builtin_cont: zip_cont,
                                    cont: Box::new(cont),
                                };
                                
                                // Apply function to first pair
                                let zip_args = vec![
                                    CEKValue::VPactValue(elem1),
                                    CEKValue::VPactValue(elem2),
                                ];
                                crate::eval::apply_function(func.clone(), zip_args, builtin_cont, handler, env)
                            }
                        }
                        _ => throw_type_mismatch_error(
                            info,
                            "function and two lists",
                            "other",
                            "zip"
                        )
                    }
                }
            })
    })
}

/// Map implementation with full CEK integration
pub fn map_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, env, args| {
        charge_gas_with_args("map", &args, MilliGas(10))
            .bind(move |_| {
                if args.len() != 2 {
                    throw_argument_count_error(info, "map", 2, args.len())
                } else {
                    match (&args[0], &args[1]) {
                        (CEKValue::VClosure(func), CEKValue::VPactValue(PactValue::List(list))) => {
                            if list.is_empty() {
                                // Empty list - return empty result
                                let empty_result = CEKValue::VPactValue(PactValue::List(vec![]));
                                return_cek_value(cont, handler, empty_result)
                            } else {
                                // Start map operation with first element
                                let mut remaining: Vec<PactValue> = list.clone();
                                let first_element = remaining.remove(0);

                                // Create map continuation
                                let map_cont = crate::cont::BuiltinCont::MapCont {
                                    func: func.clone(),
                                    remaining,
                                    accumulated: vec![],
                                };

                                let builtin_cont = Cont::BuiltinC {
                                    env: env.clone(),
                                    info,
                                    builtin_cont: map_cont,
                                    cont: Box::new(cont),
                                };

                                // Apply function to first element
                                let first_args = vec![CEKValue::VPactValue(first_element)];
                                crate::eval::apply_function(func.clone(), first_args, builtin_cont, handler, env)
                            }
                        }
                        _ => throw_type_mismatch_error(info, "function and list", "other", "map")
                    }
                }
            })
    })
}

/// Filter implementation with full CEK integration
pub fn filter_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, env, args| {
        charge_gas_with_args("filter", &args, MilliGas(10))
            .bind(move |_| {
                if args.len() != 2 {
                    throw_argument_count_error(info, "filter", 2, args.len())
                } else {
                    match (&args[0], &args[1]) {
                        (CEKValue::VClosure(func), CEKValue::VPactValue(PactValue::List(list))) => {
                            if list.is_empty() {
                                // Empty list - return empty result
                                let empty_result = CEKValue::VPactValue(PactValue::List(vec![]));
                                return_cek_value(cont, handler, empty_result)
                            } else {
                                // Start filter operation with first element
                                let mut remaining: Vec<PactValue> = list.clone();
                                let first_element = remaining.remove(0);

                                // Create filter continuation
                                let filter_cont = crate::cont::BuiltinCont::FilterCont {
                                    func: func.clone(),
                                    remaining,
                                    accumulated: vec![],
                                };

                                let builtin_cont = Cont::BuiltinC {
                                    env: env.clone(),
                                    info,
                                    builtin_cont: filter_cont,
                                    cont: Box::new(cont),
                                };

                                // Apply predicate to first element
                                let first_args = vec![CEKValue::VPactValue(first_element)];
                                crate::eval::apply_function(func.clone(), first_args, builtin_cont, handler, env)
                            }
                        }
                        _ => throw_type_mismatch_error(info, "function and list", "other", "filter")
                    }
                }
            })
    })
}

/// Fold implementation with full CEK integration
pub fn fold_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, env, args| {
        charge_gas_with_args("fold", &args, MilliGas(15))
            .bind(move |_| {
                if args.len() != 3 {
                    throw_argument_count_error(info, "fold", 3, args.len())
                } else {
                    match (&args[0], &args[1], &args[2]) {
                        (CEKValue::VClosure(func), initial_val, CEKValue::VPactValue(PactValue::List(list))) => {
                            if list.is_empty() {
                                // Empty list - return initial value
                                return_cek_value(cont, handler, initial_val.clone())
                            } else {
                                // Start fold operation with first element
                                let mut remaining: Vec<PactValue> = list.clone();
                                let first_element = remaining.remove(0);

                                // Create fold continuation
                                let fold_cont = crate::cont::BuiltinCont::FoldCont {
                                    func: func.clone(),
                                    remaining,
                                    accumulator: initial_val.as_pact_value().unwrap().clone(),
                                };

                                let builtin_cont = Cont::BuiltinC {
                                    env: env.clone(),
                                    info,
                                    builtin_cont: fold_cont,
                                    cont: Box::new(cont),
                                };

                                // Apply function to accumulator and first element
                                let fold_args = vec![initial_val.clone(), CEKValue::VPactValue(first_element)];
                                crate::eval::apply_function(func.clone(), fold_args, builtin_cont, handler, env)
                            }
                        }
                        _ => throw_type_mismatch_error(info, "function, initial value, and list", "other", "fold")
                    }
                }
            })
    })
}


/// Distinct implementation - removes duplicate elements from list
fn distinct_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("distinct", &args, MilliGas(10))
            .bind(move |_| {
                if args.len() != 1 {
                    throw_argument_count_error(info, "distinct", 1, args.len())
                } else {
                    match args[0].as_pact_value() {
                        Some(PactValue::List(list)) => {
                            let mut seen = std::collections::HashSet::new();
                            let mut result = Vec::new();
                            
                            for item in list {
                                // Use format for hashing since PactValue might not impl Hash
                                let key = format!("{:?}", item);
                                if seen.insert(key) {
                                    result.push(item.clone());
                                }
                            }
                            
                            let result = CEKValue::VPactValue(PactValue::List(result));
                            return_cek_value(cont, handler, result)
                        }
                        _ => throw_type_mismatch_error(info, "list", "other", "distinct")
                    }
                }
            })
    })
}


/// Make-list implementation - creates list of repeated elements
fn make_list_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("make-list", &args, MilliGas(5))
            .bind(move |_| {
                if args.len() != 2 {
                    throw_argument_count_error(info, "make-list", 2, args.len())
                } else {
                    match (args[0].as_pact_value(), args[1].as_pact_value()) {
                        (Some(PactValue::Integer(count)), Some(value)) => {
                            let n = count.to_i64().unwrap_or(0).max(0) as usize;
                            let list = vec![value.clone(); n];
                            let result = CEKValue::VPactValue(PactValue::List(list));
                            return_cek_value(cont, handler, result)
                        }
                        _ => throw_type_mismatch_error(info, "integer and value", "other", "make-list")
                    }
                }
            })
    })
}

/// Str-to-list implementation - converts string to list of single-char strings
fn str_to_list_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("str-to-list", &args, MilliGas(3))
            .bind(move |_| {
                if args.len() != 1 {
                    throw_argument_count_error(info, "str-to-list", 1, args.len())
                } else {
                    match args[0].as_pact_value() {
                        Some(PactValue::String(s)) => {
                            let char_list: Vec<PactValue> = s.chars()
                                .map(|c| PactValue::String(c.to_string()))
                                .collect();
                            let result = CEKValue::VPactValue(PactValue::List(char_list));
                            return_cek_value(cont, handler, result)
                        }
                        _ => throw_type_mismatch_error(info, "string", "other", "str-to-list")
                    }
                }
            })
    })
}

/// Enumerate implementation - creates list from start to end (inclusive)
fn enumerate_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("enumerate", &args, MilliGas(5))
            .bind(move |_| {
                if args.len() != 2 {
                    throw_argument_count_error(info, "enumerate", 2, args.len())
                } else {
                    match (args[0].as_pact_value(), args[1].as_pact_value()) {
                        (Some(PactValue::Integer(start)), Some(PactValue::Integer(end))) => {
                            let start_i64 = start.to_i64().unwrap_or(0);
                            let end_i64 = end.to_i64().unwrap_or(0);
                            
                            let range: Vec<PactValue> = if start_i64 <= end_i64 {
                                (start_i64..=end_i64)
                                    .map(|i| PactValue::Integer(i.into()))
                                    .collect()
                            } else {
                                (end_i64..=start_i64)
                                    .rev()
                                    .map(|i| PactValue::Integer(i.into()))
                                    .collect()
                            };
                            
                            let result = CEKValue::VPactValue(PactValue::List(range));
                            return_cek_value(cont, handler, result)
                        }
                        _ => throw_type_mismatch_error(info, "two integers", "other", "enumerate")
                    }
                }
            })
    })
}

/// Enumerate-step implementation - creates list from start to end with step
fn enumerate_step_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("enumerate", &args, MilliGas(5))
            .bind(move |_| {
                if args.len() != 3 {
                    throw_argument_count_error(info, "enumerate", 3, args.len())
                } else {
                    match (args[0].as_pact_value(), args[1].as_pact_value(), args[2].as_pact_value()) {
                        (Some(PactValue::Integer(start)), Some(PactValue::Integer(end)), Some(PactValue::Integer(step))) => {
                            let start_i64 = start.to_i64().unwrap_or(0);
                            let end_i64 = end.to_i64().unwrap_or(0);
                            let step_i64 = step.to_i64().unwrap_or(1);
                            
                            if step_i64 == 0 {
                                return EvalM::pure_value(EvalResult::EvalError(
                                    pact_core::errors::PactError::PEExecutionError(
                                        pact_core::errors::EvalError::InvalidArgument(format!("enumerate: step cannot be zero")),
                                        vec![],
                                        info.clone()
                                    )
                                ));
                            }
                            
                            let mut range = Vec::new();
                            
                            if step_i64 > 0 && start_i64 <= end_i64 {
                                let mut i = start_i64;
                                while i <= end_i64 {
                                    range.push(PactValue::Integer(i.into()));
                                    i += step_i64;
                                }
                            } else if step_i64 < 0 && start_i64 >= end_i64 {
                                let mut i = start_i64;
                                while i >= end_i64 {
                                    range.push(PactValue::Integer(i.into()));
                                    i += step_i64;
                                }
                            }
                            
                            let result = CEKValue::VPactValue(PactValue::List(range));
                            return_cek_value(cont, handler, result)
                        }
                        _ => throw_type_mismatch_error(info, "three integers", "other", "enumerate")
                    }
                }
            })
    })
}

/// Sort implementation - sorts a list
/// From Haskell:
/// ```haskell
/// rawSort info b _env = \case
///   [VList vli]
///     | V.null vli -> return (VList mempty)
///     | otherwise -> do
///     sizes <- traverse (sizeOf info SizeOfV0) vli
///     let maxSize = maximum sizes
///     chargeGasArgs info (GComparison (SortComparisons maxSize (V.length vli)))
///     vli' <- liftIO $ do
///       v' <- V.thaw vli
///       V.sortBy sortPv v'
///       V.freeze v'
///     return (VList vli')
/// ```
fn sort_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("sort", &args, MilliGas(10))
            .bind(move |_| {
                if args.len() != 1 {
                    throw_argument_count_error(info, "sort", 1, args.len())
                } else {
                    match args[0].as_pact_value() {
                        Some(PactValue::List(list)) => {
                            if list.is_empty() {
                                let result = CEKValue::VPactValue(PactValue::List(vec![]));
                                return_cek_value(cont, handler, result)
                            } else {
                                // Clone list for closure
                                let list_clone = list.clone();
                                // Charge gas based on list size
                                let list_len = list.len() as u64;
                                charge_gas("sort", MilliGas(list_len * list_len))
                                    .bind(move |_| {
                                        // Sort the list
                                        let mut sorted_list = list_clone;
                                        sorted_list.sort_by(|a, b| compare_pact_values(a, b));
                                        
                                        let result = CEKValue::VPactValue(PactValue::List(sorted_list));
                                        return_cek_value(cont, handler, result)
                                    })
                            }
                        }
                        _ => throw_type_mismatch_error(info, "list", "other", "sort")
                    }
                }
            })
    })
}

/// Reverse implementation - reverses a list or string
/// From Haskell:
/// ```haskell
/// rawReverse info b _env = \case
///   [VList li] -> do
///     chargeGasArgs info (GConcat (ListConcat (GasListLength (V.length li))))
///     return (VList (V.reverse li))
///   [VLiteral (LString t)] -> do
///     chargeGasArgs info (GConcat (TextConcat (GasTextLength (T.length t))))
///     return  (VLiteral (LString (T.reverse t)))
/// ```
fn reverse_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("reverse", &args, MilliGas(5))
            .bind(move |_| {
                if args.len() != 1 {
                    throw_argument_count_error(info, "reverse", 1, args.len())
                } else {
                    match args[0].as_pact_value() {
                        Some(PactValue::List(list)) => {
                            // Clone list for closure
                            let list_clone = list.clone();
                            // Charge gas based on list size
                            let list_len = list.len() as u64;
                            charge_gas("reverse", MilliGas(list_len))
                                .bind(move |_| {
                                    let mut reversed = list_clone;
                                    reversed.reverse();
                                    let result = CEKValue::VPactValue(PactValue::List(reversed));
                                    return_cek_value(cont, handler, result)
                                })
                        }
                        Some(PactValue::String(s)) => {
                            // Clone string for closure
                            let s_clone = s.clone();
                            // Charge gas based on string length
                            let str_len = s.len() as u64;
                            charge_gas("reverse", MilliGas(str_len))
                                .bind(move |_| {
                                    let reversed: String = s_clone.chars().rev().collect();
                                    let result = CEKValue::VPactValue(PactValue::String(reversed));
                                    return_cek_value(cont, handler, result)
                                })
                        }
                        _ => throw_type_mismatch_error(info, "list or string", "other", "reverse")
                    }
                }
            })
    })
}

/// Contains implementation - checks if element is in list, field in object, or substring in string
/// From Haskell:
/// ```haskell
/// rawContains info b _env = \case
///   [VString f, VObject o] -> do
///     chargeGasArgs info $ GSearch $ FieldSearch (M.size o)
///     return (VBool (M.member (Field f) o))
///   [VString needle, VString hay] -> do
///     chargeGasArgs info $ GSearch $ SubstringSearch needle hay
///     return (VBool (needle `T.isInfixOf` hay))
///   [VPactValue v, VList vli] -> do
///     let search True _ = pure True
///         search _ el = valEqGassed info v el
///     res <- foldlM search False vli
///     return (VBool res)
/// ```
fn contains_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("contains", &args, MilliGas(5))
            .bind(move |_| {
                if args.len() != 2 {
                    throw_argument_count_error(info, "contains", 2, args.len())
                } else {
                    match (args[0].as_pact_value(), args[1].as_pact_value()) {
                        // Field in object
                        (Some(PactValue::String(field)), Some(PactValue::Object(obj))) => {
                            let field_clone = field.clone();
                            let obj_clone = obj.clone();
                            // Charge gas based on object size
                            let obj_size = obj.to_hashmap().len() as u64;
                            charge_gas("contains", MilliGas(obj_size))
                                .bind(move |_| {
                                    let has_field = obj_clone.get(&field_clone).is_some();
                                    let result = CEKValue::VPactValue(PactValue::Bool(has_field));
                                    return_cek_value(cont, handler, result)
                                })
                        }
                        // Substring in string
                        (Some(PactValue::String(needle)), Some(PactValue::String(haystack))) => {
                            let needle_clone = needle.clone();
                            let haystack_clone = haystack.clone();
                            // Charge gas based on string lengths
                            let search_cost = (needle.len() * haystack.len()) as u64;
                            charge_gas("contains", MilliGas(search_cost / 10 + 1))
                                .bind(move |_| {
                                    let contains = haystack_clone.contains(&needle_clone);
                                    let result = CEKValue::VPactValue(PactValue::Bool(contains));
                                    return_cek_value(cont, handler, result)
                                })
                        }
                        // Element in list
                        (Some(value), Some(PactValue::List(list))) => {
                            let value_clone = value.clone();
                            let list_clone = list.clone();
                            // Charge gas based on list size
                            let list_len = list.len() as u64;
                            charge_gas("contains", MilliGas(list_len))
                                .bind(move |_| {
                                    let contains = list_clone.iter().any(|item| values_equal(item, &value_clone));
                                    let result = CEKValue::VPactValue(PactValue::Bool(contains));
                                    return_cek_value(cont, handler, result)
                                })
                        }
                        _ => throw_type_mismatch_error(info, "(string, object), (string, string), or (any, list)", "other", "contains")
                    }
                }
            })
    })
}

/// Helper function to compare PactValues for sorting
fn compare_pact_values(a: &PactValue, b: &PactValue) -> std::cmp::Ordering {
    use std::cmp::Ordering;
    
    match (a, b) {
        // Literals (string, int, decimal, bool) compare directly
        (PactValue::String(s1), PactValue::String(s2)) => s1.cmp(s2),
        (PactValue::Integer(i1), PactValue::Integer(i2)) => i1.cmp(i2),
        (PactValue::Decimal(d1), PactValue::Decimal(d2)) => {
            // Convert to f64 for comparison
            let f1 = d1.to_f64();
            let f2 = d2.to_f64();
            f1.partial_cmp(&f2).unwrap_or(Ordering::Equal)
        }
        (PactValue::Bool(b1), PactValue::Bool(b2)) => b1.cmp(b2),
        (PactValue::Time(t1), PactValue::Time(t2)) => t1.as_micros().cmp(&t2.as_micros()),
        
        // Mixed literal types follow Haskell ordering: 
        // Literals < Time, and within literals: bool < string < number
        (PactValue::Bool(_), PactValue::String(_)) => Ordering::Less,
        (PactValue::Bool(_), PactValue::Integer(_)) => Ordering::Less,
        (PactValue::Bool(_), PactValue::Decimal(_)) => Ordering::Less,
        (PactValue::String(_), PactValue::Bool(_)) => Ordering::Greater,
        (PactValue::String(_), PactValue::Integer(_)) => Ordering::Less,
        (PactValue::String(_), PactValue::Decimal(_)) => Ordering::Less,
        (PactValue::Integer(_), PactValue::Bool(_)) => Ordering::Greater,
        (PactValue::Integer(_), PactValue::String(_)) => Ordering::Greater,
        (PactValue::Decimal(_), PactValue::Bool(_)) => Ordering::Greater,
        (PactValue::Decimal(_), PactValue::String(_)) => Ordering::Greater,
        
        // Literals < Time
        (PactValue::Bool(_), PactValue::Time(_)) => Ordering::Less,
        (PactValue::String(_), PactValue::Time(_)) => Ordering::Less,
        (PactValue::Integer(_), PactValue::Time(_)) => Ordering::Less,
        (PactValue::Decimal(_), PactValue::Time(_)) => Ordering::Less,
        (PactValue::Time(_), PactValue::Bool(_)) => Ordering::Greater,
        (PactValue::Time(_), PactValue::String(_)) => Ordering::Greater,
        (PactValue::Time(_), PactValue::Integer(_)) => Ordering::Greater,
        (PactValue::Time(_), PactValue::Decimal(_)) => Ordering::Greater,
        
        // Other types are considered equal for sorting
        _ => Ordering::Equal,
    }
}

/// Helper function to check equality of PactValues
fn values_equal(a: &PactValue, b: &PactValue) -> bool {
    match (a, b) {
        (PactValue::String(s1), PactValue::String(s2)) => s1 == s2,
        (PactValue::Integer(i1), PactValue::Integer(i2)) => i1 == i2,
        (PactValue::Decimal(d1), PactValue::Decimal(d2)) => d1 == d2,
        (PactValue::Bool(b1), PactValue::Bool(b2)) => b1 == b2,
        (PactValue::Time(t1), PactValue::Time(t2)) => t1.as_micros() == t2.as_micros(),
        (PactValue::List(l1), PactValue::List(l2)) => {
            l1.len() == l2.len() && l1.iter().zip(l2.iter()).all(|(a, b)| values_equal(a, b))
        }
        (PactValue::Object(o1), PactValue::Object(o2)) => {
            let map1 = o1.to_hashmap();
            let map2 = o2.to_hashmap();
            map1.len() == map2.len() && map1.iter().all(|(k, v1)| {
                map2.get(k).map_or(false, |v2| values_equal(v1, v2))
            })
        }
        _ => false,
    }
}

/// Where implementation - applies predicate to field in object
/// From Haskell:
/// ```haskell
/// coreWhere info b cont handler _env = \case
///   [VString field, VClosure app, VObject o] -> do
///     chargeGasArgs info (GObjOp (ObjOpLookup field (M.size o)))
///     case M.lookup (Field field) o of
///       Just v -> do
///         let cont' = EnforceBoolC info cont
///         applyLam app [VPactValue v] cont' handler
///       Nothing ->
///         throwExecutionError info (ObjectIsMissingField (Field field) (ObjectData o))
/// ```
fn where_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, env, args| {
        if args.len() != 3 {
            throw_argument_count_error(info, "where", 3, args.len())
        } else {
            // Extract args before moving into closure
            let arg0 = args[0].clone();
            let arg1 = args[1].clone();
            let arg2 = args[2].clone();
            
            charge_gas_with_args("where", &args, MilliGas(5))
                .bind(move |_| {
                    match (arg0, arg1, arg2) {
                        (CEKValue::VPactValue(PactValue::String(field)), 
                         CEKValue::VClosure(predicate), 
                         CEKValue::VPactValue(PactValue::Object(obj))) => {
                            // Charge gas for object lookup
                            let obj_size = obj.len() as u64;
                            charge_gas("where-lookup", MilliGas(obj_size))
                                .bind(move |_| {
                                    match obj.get(&field) {
                                        Some(value) => {
                                            // Create continuation that enforces boolean result
                                            let enforce_bool_cont = Cont::EnforceBoolC {
                                                info,
                                                cont: Box::new(cont),
                                            };
                                            // Apply predicate to field value
                                            let field_args = vec![CEKValue::VPactValue(value.clone())];
                                            crate::eval::apply_function(predicate.clone(), field_args, enforce_bool_cont, handler, env)
                                        }
                                        None => {
                                            throw_execution_error(
                                                info, 
                                                EvalError::ObjectMissingField(field.clone())
                                            )
                                        }
                                    }
                                })
                        }
                        _ => throw_type_mismatch_error(
                            info, 
                            "field name, predicate function, and object", 
                            "other", 
                            "where"
                        )
                    }
                })
        }
    })
}
