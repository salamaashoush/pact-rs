//! Polymorphic builtin operations
//!
//! This module contains builtin operations that work with multiple types
//! (lists, strings, objects) following the Haskell implementation

use super::*;
use pact_values::PactValue;
use num_traits::ToPrimitive;

/// Helper function to create a builtin error with current stack frame
fn builtin_error<T>(error: pact_errors::EvalError) -> Result<T, pact_errors::PactErrorI> {
    // In a real implementation, we would get the current stack from the monad state
    // For now, we'll use an empty vec but with a TODO to fix
    // TODO: Get actual stack frames from EvalM state
    Err(pact_errors::PactError::PEExecutionError(
        error,
        vec![], // TODO: Get from EvalM::get_stack()
        pact_shared_types::SpanInfo::empty() // TODO: Get from current context
    ))
}

/// Helper function to create a builtin error with specific info
fn builtin_error_with_info<T>(error: pact_errors::EvalError, info: pact_shared_types::SpanInfo) -> Result<T, pact_errors::PactErrorI> {
    // TODO: Get actual stack frames from EvalM state
    Err(pact_errors::PactError::PEExecutionError(
        error,
        vec![], // TODO: Get from EvalM::get_stack()
        info
    ))
}

/// Helper function to create an error result for EvalM
fn builtin_error_result(error: pact_errors::EvalError, info: pact_shared_types::SpanInfo) -> EvalM<EvalResult> {
    // TODO: Get actual stack frames from EvalM state
    EvalM::pure_value(EvalResult::EvalError(
        pact_errors::PactError::PEExecutionError(
            error,
            vec![], // TODO: Get from EvalM::get_stack()
            info
        )
    ))
}


pub fn register_polymorphic_builtins(builtin_env: &mut BuiltinEnv) -> Result<(), pact_errors::PactErrorI> {
    // Length - works with lists, strings, and objects
    builtin_env.register(CoreBuiltin::CoreLength, BuiltinSpec {
        name: "length",
        arity: 1,
        implementation: length_implementation(),
    });

    // Take - works with lists, strings, and objects
    builtin_env.register(CoreBuiltin::CoreTake, BuiltinSpec {
        name: "take",
        arity: 2,
        implementation: take_implementation(),
    });

    // Drop - works with lists, strings, and objects
    builtin_env.register(CoreBuiltin::CoreDrop, BuiltinSpec {
        name: "drop",
        arity: 2,
        implementation: drop_implementation(),
    });

    // Reverse - works with lists and strings
    builtin_env.register(CoreBuiltin::CoreReverse, BuiltinSpec {
        name: "reverse",
        arity: 1,
        implementation: reverse_implementation(),
    });

    // Contains - works with lists, strings, and objects
    builtin_env.register(CoreBuiltin::CoreContains, BuiltinSpec {
        name: "contains",
        arity: 2,
        implementation: contains_implementation(),
    });

    // At/Access - works with lists, strings, and objects
    builtin_env.register(CoreBuiltin::CoreAt, BuiltinSpec {
        name: "at",
        arity: 2,
        implementation: at_implementation(),
    });

    // Sort - works with lists
    builtin_env.register(CoreBuiltin::CoreSort, BuiltinSpec {
        name: "sort",
        arity: 1,
        implementation: sort_implementation(),
    });

    // Sort with fields - works with list of objects
    builtin_env.register(CoreBuiltin::CoreSortObject, BuiltinSpec {
        name: "sort",
        arity: 2,  // Overloaded version with field selector
        implementation: sort_object_implementation(),
    });

    // Remove - works with lists and objects
    builtin_env.register(CoreBuiltin::CoreRemove, BuiltinSpec {
        name: "remove",
        arity: 2,
        implementation: remove_implementation(),
    });

    // Concat - concatenates list of strings into a single string (following Haskell)
    builtin_env.register(CoreBuiltin::CoreConcat, BuiltinSpec {
        name: "concat",
        arity: 1,
        implementation: concat_implementation(),
    });

    Ok(())
}

/// Length implementation - returns length of list, string, or object
fn length_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("length", &args, MilliGas(2))
            .bind(move |_| {
                if args.len() != 1 {
                    throw_argument_count_error(info, "length", 1, args.len())
                } else {
                    match args[0].as_pact_value() {
                        Some(PactValue::List(list)) => {
                            let length = list.len() as i64;
                            let result = CEKValue::VPactValue(PactValue::Integer(length.into()));
                            return_cek_value(cont, handler, result)
                        }
                        Some(PactValue::String(s)) => {
                            let length = s.chars().count() as i64;
                            let result = CEKValue::VPactValue(PactValue::Integer(length.into()));
                            return_cek_value(cont, handler, result)
                        }
                        Some(PactValue::Object(obj)) => {
                            let length = obj.len() as i64;
                            let result = CEKValue::VPactValue(PactValue::Integer(length.into()));
                            return_cek_value(cont, handler, result)
                        }
                        _ => throw_type_mismatch_error(info, "list, string, or object", "other", "length")
                    }
                }
            })
    })
}

/// Take implementation - polymorphic over lists, strings, and objects
fn take_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("take", &args, MilliGas(3))
            .bind(move |_| {
                if args.len() != 2 {
                    throw_argument_count_error(info, "take", 2, args.len())
                } else {
                    match (args[0].as_pact_value(), args[1].as_pact_value()) {
                        // Take from list
                        (Some(PactValue::Integer(n)), Some(PactValue::List(list))) => {
                            let n_i64 = n.to_i64().unwrap_or(0);
                            let result = if n_i64 >= 0 {
                                let n_usize = n_i64.min(list.len() as i64) as usize;
                                CEKValue::VPactValue(PactValue::List(list[..n_usize].to_vec()))
                            } else {
                                // Negative take drops from end
                                let n_usize = (-n_i64).min(list.len() as i64) as usize;
                                let drop_from = list.len().saturating_sub(n_usize);
                                CEKValue::VPactValue(PactValue::List(list[drop_from..].to_vec()))
                            };
                            return_cek_value(cont, handler, result)
                        }
                        // Take from string
                        (Some(PactValue::Integer(n)), Some(PactValue::String(s))) => {
                            let n_i64 = n.to_i64().unwrap_or(0);
                            let chars: Vec<char> = s.chars().collect();
                            let result = if n_i64 >= 0 {
                                let n_usize = n_i64.min(chars.len() as i64) as usize;
                                let taken: String = chars[..n_usize].iter().collect();
                                CEKValue::VPactValue(PactValue::String(taken))
                            } else {
                                // Negative take drops from end
                                let n_usize = (-n_i64).min(chars.len() as i64) as usize;
                                let drop_from = chars.len().saturating_sub(n_usize);
                                let taken: String = chars[drop_from..].iter().collect();
                                CEKValue::VPactValue(PactValue::String(taken))
                            };
                            return_cek_value(cont, handler, result)
                        }
                        // Take from object (list of keys)
                        (Some(PactValue::List(keys)), Some(PactValue::Object(obj))) => {
                            let mut result_obj = pact_values::Object::new();
                            for key in keys {
                                if let PactValue::String(k) = key {
                                    if let Some(v) = obj.get(k) {
                                        result_obj.insert(k.clone(), v.clone());
                                    }
                                }
                            }
                            let result = CEKValue::VPactValue(PactValue::Object(result_obj));
                            return_cek_value(cont, handler, result)
                        }
                        _ => throw_type_mismatch_error(info, "(integer, list/string) or (list-of-keys, object)", "other", "take")
                    }
                }
            })
    })
}

/// Drop implementation - polymorphic over lists, strings, and objects
fn drop_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("drop", &args, MilliGas(3))
            .bind(move |_| {
                if args.len() != 2 {
                    throw_argument_count_error(info, "drop", 2, args.len())
                } else {
                    match (args[0].as_pact_value(), args[1].as_pact_value()) {
                        // Drop from list
                        (Some(PactValue::Integer(n)), Some(PactValue::List(list))) => {
                            let n_i64 = n.to_i64().unwrap_or(0);
                            let result = if n_i64 >= 0 {
                                let n_usize = n_i64.min(list.len() as i64) as usize;
                                CEKValue::VPactValue(PactValue::List(list[n_usize..].to_vec()))
                            } else {
                                // Negative drop takes from start
                                let n_usize = (-n_i64).min(list.len() as i64) as usize;
                                let take_up_to = list.len().saturating_sub(n_usize);
                                CEKValue::VPactValue(PactValue::List(list[..take_up_to].to_vec()))
                            };
                            return_cek_value(cont, handler, result)
                        }
                        // Drop from string
                        (Some(PactValue::Integer(n)), Some(PactValue::String(s))) => {
                            let n_i64 = n.to_i64().unwrap_or(0);
                            let chars: Vec<char> = s.chars().collect();
                            let result = if n_i64 >= 0 {
                                let n_usize = n_i64.min(chars.len() as i64) as usize;
                                let dropped: String = chars[n_usize..].iter().collect();
                                CEKValue::VPactValue(PactValue::String(dropped))
                            } else {
                                // Negative drop takes from start
                                let n_usize = (-n_i64).min(chars.len() as i64) as usize;
                                let take_up_to = chars.len().saturating_sub(n_usize);
                                let dropped: String = chars[..take_up_to].iter().collect();
                                CEKValue::VPactValue(PactValue::String(dropped))
                            };
                            return_cek_value(cont, handler, result)
                        }
                        // Drop from object (list of keys to exclude)
                        (Some(PactValue::List(keys)), Some(PactValue::Object(obj))) => {
                            let mut result_obj = obj.clone();
                            for key in keys {
                                if let PactValue::String(k) = key {
                                    result_obj.remove(k);
                                }
                            }
                            let result = CEKValue::VPactValue(PactValue::Object(result_obj));
                            return_cek_value(cont, handler, result)
                        }
                        _ => throw_type_mismatch_error(info, "(integer, list/string) or (list-of-keys, object)", "other", "drop")
                    }
                }
            })
    })
}

/// Reverse implementation for lists and strings
fn reverse_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("reverse", &args, MilliGas(3))
            .bind(move |_| {
                if args.len() != 1 {
                    throw_argument_count_error(info, "reverse", 1, args.len())
                } else {
                    match args[0].as_pact_value() {
                        Some(PactValue::List(list)) => {
                            let mut reversed = list.clone();
                            reversed.reverse();
                            let result = CEKValue::VPactValue(PactValue::List(reversed));
                            return_cek_value(cont, handler, result)
                        }
                        Some(PactValue::String(s)) => {
                            let reversed: String = s.chars().rev().collect();
                            let result = CEKValue::VPactValue(PactValue::String(reversed));
                            return_cek_value(cont, handler, result)
                        }
                        _ => throw_type_mismatch_error(info, "list or string", "other", "reverse")
                    }
                }
            })
    })
}

/// Contains implementation - checks if element is in list/string/object
fn contains_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("contains", &args, MilliGas(5))
            .bind(move |_| {
                if args.len() != 2 {
                    throw_argument_count_error(info, "contains", 2, args.len())
                } else {
                    match (args[0].as_pact_value(), args[1].as_pact_value()) {
                        // Check if string field is in object
                        (Some(PactValue::String(key)), Some(PactValue::Object(obj))) => {
                            let contains = obj.contains_field(key);
                            let result = CEKValue::VPactValue(PactValue::Bool(contains));
                            return_cek_value(cont, handler, result)
                        }
                        // Check if substring is in string
                        (Some(PactValue::String(needle)), Some(PactValue::String(haystack))) => {
                            let contains = haystack.contains(needle.as_str());
                            let result = CEKValue::VPactValue(PactValue::Bool(contains));
                            return_cek_value(cont, handler, result)
                        }
                        // Check if element is in list
                        (Some(needle), Some(PactValue::List(haystack))) => {
                            let contains = haystack.contains(needle);
                            let result = CEKValue::VPactValue(PactValue::Bool(contains));
                            return_cek_value(cont, handler, result)
                        }
                        _ => throw_type_mismatch_error(info, "(key, object), (substring, string), or (element, list)", "other", "contains")
                    }
                }
            })
    })
}

/// At/Access implementation - gets element at index from list/object/string
fn at_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("at", &args, MilliGas(3))
            .bind(move |_| {
                if args.len() != 2 {
                    throw_argument_count_error(info, "at", 2, args.len())
                } else {
                    match (args[0].as_pact_value(), args[1].as_pact_value()) {
                        // Access list by index
                        (Some(PactValue::Integer(idx)), Some(PactValue::List(list))) => {
                            let index = idx.to_i64().unwrap_or(0) as usize;
                            match list.get(index) {
                                Some(value) => {
                                    let result = CEKValue::VPactValue(value.clone());
                                    return_cek_value(cont, handler, result)
                                }
                                None => EvalM::throw_error(
                                    pact_errors::PactError::PEExecutionError(
                                        pact_errors::EvalError::InvalidIndex(index.to_string()),
                                        vec![],
                                        info.clone()
                                    )
                                )
                            }
                        }
                        // Access object by key
                        (Some(PactValue::String(key)), Some(PactValue::Object(obj))) => {
                            match obj.get(key) {
                                Some(value) => {
                                    let result = CEKValue::VPactValue(value.clone());
                                    return_cek_value(cont, handler, result)
                                }
                                None => EvalM::throw_error(
                                    pact_errors::PactError::PEExecutionError(
                                        pact_errors::EvalError::ObjectMissingField(key.clone()),
                                        vec![],
                                        info.clone()
                                    )
                                )
                            }
                        }
                        _ => throw_type_mismatch_error(info, "(index, list) or (key, object)", "other", "at")
                    }
                }
            })
    })
}

/// Sort implementation - sorts list of comparable values
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
                                let mut sorted = list.clone();
                                // Sort using PactValue's Ord implementation
                                sorted.sort_by(|a, b| {
                                    // Custom comparison for PactValue following Haskell ordering
                                    match (a, b) {
                                        (PactValue::Integer(a), PactValue::Integer(b)) => a.cmp(b),
                                        (PactValue::Decimal(a), PactValue::Decimal(b)) => a.cmp(b),
                                        (PactValue::String(a), PactValue::String(b)) => a.cmp(b),
                                        (PactValue::Time(a), PactValue::Time(b)) => a.cmp(b),
                                        (PactValue::Bool(a), PactValue::Bool(b)) => a.cmp(b),
                                        // Literals come before times
                                        (PactValue::Integer(_), PactValue::Time(_)) => std::cmp::Ordering::Less,
                                        (PactValue::Time(_), PactValue::Integer(_)) => std::cmp::Ordering::Greater,
                                        (PactValue::Decimal(_), PactValue::Time(_)) => std::cmp::Ordering::Less,
                                        (PactValue::Time(_), PactValue::Decimal(_)) => std::cmp::Ordering::Greater,
                                        (PactValue::String(_), PactValue::Time(_)) => std::cmp::Ordering::Less,
                                        (PactValue::Time(_), PactValue::String(_)) => std::cmp::Ordering::Greater,
                                        (PactValue::Bool(_), PactValue::Time(_)) => std::cmp::Ordering::Less,
                                        (PactValue::Time(_), PactValue::Bool(_)) => std::cmp::Ordering::Greater,
                                        // For other combinations, use type-based ordering
                                        _ => std::cmp::Ordering::Equal
                                    }
                                });
                                let result = CEKValue::VPactValue(PactValue::List(sorted));
                                return_cek_value(cont, handler, result)
                            }
                        }
                        _ => throw_type_mismatch_error(info, "list", "other", "sort")
                    }
                }
            })
    })
}

/// Sort-object implementation - sorts list of objects by fields
fn sort_object_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("sort", &args, MilliGas(15))
            .bind(move |_| {
                if args.len() != 2 {
                    throw_argument_count_error(info, "sort", 2, args.len())
                } else {
                    match (args[0].as_pact_value(), args[1].as_pact_value()) {
                        (Some(PactValue::List(fields)), Some(PactValue::List(objects))) => {
                            if fields.is_empty() || objects.is_empty() {
                                let result = CEKValue::VPactValue(PactValue::List(objects.clone()));
                                return_cek_value(cont, handler, result)
                            } else {
                                // Extract field names
                                let mut field_names = Vec::new();
                                for field in fields {
                                    match field {
                                        PactValue::String(s) => field_names.push(s.clone()),
                                        _ => {
                                            return throw_type_mismatch_error(info, "list of field names", "non-string field", "sort");
                                        }
                                    }
                                }

                                // Verify all elements are objects
                                let mut object_list = Vec::new();
                                for item in objects {
                                    match item {
                                        PactValue::Object(obj) => object_list.push(obj.clone()),
                                        _ => {
                                            return throw_type_mismatch_error(info, "list of objects", "non-object in list", "sort");
                                        }
                                    }
                                }
                                
                                // Sort by fields in order
                                let mut indexed: Vec<(usize, pact_values::Object)> = 
                                    object_list.into_iter().enumerate().collect();
                                
                                indexed.sort_by(|(_, a), (_, b)| {
                                    for field in &field_names {
                                        let a_val = a.get(field);
                                        let b_val = b.get(field);
                                        let ordering = match (a_val, b_val) {
                                            (Some(a), Some(b)) => match (a, b) {
                                                (PactValue::Integer(ai), PactValue::Integer(bi)) => ai.cmp(bi),
                                                (PactValue::Decimal(ad), PactValue::Decimal(bd)) => ad.cmp(bd),
                                                (PactValue::String(as_), PactValue::String(bs)) => as_.cmp(bs),
                                                (PactValue::Time(at), PactValue::Time(bt)) => at.cmp(bt),
                                                (PactValue::Bool(ab), PactValue::Bool(bb)) => ab.cmp(bb),
                                                _ => std::cmp::Ordering::Equal
                                            },
                                            (Some(_), None) => std::cmp::Ordering::Less,
                                            (None, Some(_)) => std::cmp::Ordering::Greater,
                                            (None, None) => std::cmp::Ordering::Equal
                                        };
                                        match ordering {
                                            std::cmp::Ordering::Equal => continue,
                                            other => return other,
                                        }
                                    }
                                    std::cmp::Ordering::Equal
                                });
                                
                                let sorted: Vec<PactValue> = indexed.into_iter()
                                    .map(|(_, obj)| PactValue::Object(obj))
                                    .collect();
                                
                                let result = CEKValue::VPactValue(PactValue::List(sorted));
                                return_cek_value(cont, handler, result)
                            }
                        }
                        _ => throw_type_mismatch_error(info, "list of fields and list of objects", "other", "sort")
                    }
                }
            })
    })
}

/// Remove implementation - removes field from object
fn remove_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("remove", &args, MilliGas(5))
            .bind(move |_| {
                if args.len() != 2 {
                    throw_argument_count_error(info, "remove", 2, args.len())
                } else {
                    match (args[0].as_pact_value(), args[1].as_pact_value()) {
                        (Some(PactValue::String(key)), Some(PactValue::Object(obj))) => {
                            let mut result_obj = obj.clone();
                            result_obj.remove(key);
                            let result = CEKValue::VPactValue(PactValue::Object(result_obj));
                            return_cek_value(cont, handler, result)
                        }
                        _ => throw_type_mismatch_error(info, "key and object", "other", "remove")
                    }
                }
            })
    })
}

/// Concat implementation - concatenates list of strings into a single string
/// Following Haskell implementation exactly
fn concat_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("concat", &args, MilliGas(5))
            .bind(move |_| {
                if args.len() != 1 {
                    throw_argument_count_error(info, "concat", 1, args.len())
                } else {
                    match args[0].as_pact_value() {
                        Some(PactValue::List(list)) => {
                            if list.is_empty() {
                                // Empty list returns empty string (following Haskell)
                                let result = CEKValue::VPactValue(PactValue::String(String::new()));
                                return_cek_value(cont, handler, result)
                            } else {
                                // All elements must be strings
                                let mut strings = Vec::new();
                                for item in list {
                                    match item {
                                        PactValue::String(s) => strings.push(s.clone()),
                                        _ => {
                                            return throw_type_mismatch_error(info, "list of strings", "non-string element", "concat");
                                        }
                                    }
                                }
                                
                                let result = CEKValue::VPactValue(PactValue::String(strings.join("")));
                                return_cek_value(cont, handler, result)
                            }
                        }
                        _ => throw_type_mismatch_error(info, "list", "other", "concat")
                    }
                }
            })
    })
}