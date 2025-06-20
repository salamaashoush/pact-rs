use super::*;
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


pub fn register_string_builtins(builtin_env: &mut BuiltinEnv) -> Result<(), pact_errors::PactErrorI> {
    // Format - string interpolation
    builtin_env.register(CoreBuiltin::CoreFormat, BuiltinSpec {
        name: "format",
        arity: 2,
        implementation: format_implementation(),
    });

    // Show - convert any value to string representation
    builtin_env.register(CoreBuiltin::CoreShow, BuiltinSpec {
        name: "show",
        arity: 1,
        implementation: show_implementation(),
    });

    // String to integer conversion
    builtin_env.register(CoreBuiltin::CoreStrToInt, BuiltinSpec {
        name: "str-to-int",
        arity: 1,
        implementation: str_to_int_implementation(),
    });

    // String to integer with base
    builtin_env.register(CoreBuiltin::CoreStrToIntBase, BuiltinSpec {
        name: "str-to-int",
        arity: 2,
        implementation: str_to_int_base_implementation(),
    });

    // Integer to string conversion
    builtin_env.register(CoreBuiltin::CoreIntToStr, BuiltinSpec {
        name: "int-to-str",
        arity: 2,
        implementation: int_to_str_implementation(),
    });

    // String to list of characters
    builtin_env.register(CoreBuiltin::CoreStrToList, BuiltinSpec {
        name: "str-to-list",
        arity: 1,
        implementation: str_to_list_implementation(),
    });

    Ok(())
}

/// Format implementation - string interpolation
fn format_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("format", &args, MilliGas(5))
            .bind(move |_| {
                if args.len() != 2 {
                    return args_error(info, "format", &args);
                }
                
                match (args[0].as_pact_value(), args[1].as_pact_value()) {
                    (Some(PactValue::String(template)), Some(PactValue::List(values))) => {
                        // Simple {} replacement implementation
                        let mut result = template.clone();
                        for value in values {
                            if let Some(pos) = result.find("{}") {
                                let value_str = format_pact_value(value);
                                result.replace_range(pos..pos+2, &value_str);
                            }
                        }
                        let result = CEKValue::VPactValue(PactValue::String(result));
                        return_cek_value(cont, handler, result)
                    }
                    _ => {
                        args_error(info, "format", &args)
                    }
                }
            })
    })
}

/// Show implementation - convert any value to string
fn show_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("show", &args, MilliGas(2))
            .bind(move |_| {
                if args.len() != 1 {
                    return args_error(info, "show", &args);
                }
                
                match &args[0] {
                    CEKValue::VPactValue(pv) => {
                        let str_repr = format_pact_value(pv);
                        let result = CEKValue::VPactValue(PactValue::String(str_repr));
                        return_cek_value(cont, handler, result)
                    }
                    CEKValue::VClosure(_) => {
                        let result = CEKValue::VPactValue(PactValue::String("<closure>".to_string()));
                        return_cek_value(cont, handler, result)
                    }
                    CEKValue::VTable { .. } => {
                        let result = CEKValue::VPactValue(PactValue::String("<table>".to_string()));
                        return_cek_value(cont, handler, result)
                    }
                }
            })
    })
}

/// String to integer conversion
fn str_to_int_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("str-to-int", &args, MilliGas(2))
            .bind(move |_| {
                if args.len() != 1 {
                    return args_error(info, "str-to-int", &args);
                }
                
                match args[0].as_pact_value() {
                    Some(PactValue::String(s)) => {
                        match s.parse::<i64>() {
                            Ok(n) => {
                                let result = CEKValue::VPactValue(PactValue::Integer(n.into()));
                                return_cek_value(cont, handler, result)
                            }
                            Err(_) => {
                                throw_execution_error(info, EvalError::InvalidArgument(format!("Invalid integer string: {}", s)))
                            }
                        }
                    }
                    _ => {
                        args_error(info, "str-to-int", &args)
                    }
                }
            })
    })
}

/// String to integer with base conversion
fn str_to_int_base_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("str-to-int", &args, MilliGas(3))
            .bind(move |_| {
                if args.len() != 2 {
                    return args_error(info, "str-to-int", &args);
                }
                
                match (args[0].as_pact_value(), args[1].as_pact_value()) {
                    (Some(PactValue::Integer(base)), Some(PactValue::String(s))) => {
                        if let Some(base_u32) = base.to_u32() {
                            if base_u32 < 2 || base_u32 > 36 {
                                throw_execution_error(info, EvalError::InvalidArgument(format!("Base must be between 2 and 36, got {}", base_u32)))
                            } else {
                                match i64::from_str_radix(s, base_u32) {
                                    Ok(n) => {
                                        let result = CEKValue::VPactValue(PactValue::Integer(n.into()));
                                        return_cek_value(cont, handler, result)
                                    }
                                    Err(_) => {
                                        throw_execution_error(info, EvalError::InvalidArgument(format!("Invalid base {} integer string: {}", base_u32, s)))
                                    }
                                }
                            }
                        } else {
                            throw_execution_error(info, EvalError::InvalidArgument("Base too large".to_string()))
                        }
                    }
                    _ => {
                        args_error(info, "str-to-int", &args)
                    }
                }
            })
    })
}

/// Integer to string conversion with base
fn int_to_str_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("int-to-str", &args, MilliGas(2))
            .bind(move |_| {
                if args.len() != 2 {
                    return args_error(info, "int-to-str", &args);
                }
                
                match (args[0].as_pact_value(), args[1].as_pact_value()) {
                    (Some(PactValue::Integer(base)), Some(PactValue::Integer(n))) => {
                        if let Some(base_u32) = base.to_u32() {
                            if base_u32 < 2 || base_u32 > 36 {
                                throw_execution_error(info, EvalError::InvalidArgument(format!("Base must be between 2 and 36, got {}", base_u32)))
                            } else if let Some(n_i64) = n.to_i64() {
                                let result_str = if base_u32 == 10 {
                                    n_i64.to_string()
                                } else {
                                    // Use radix formatting
                                    format_radix(n_i64, base_u32)
                                };
                                let result = CEKValue::VPactValue(PactValue::String(result_str));
                                return_cek_value(cont, handler, result)
                            } else {
                                throw_execution_error(info, EvalError::InvalidArgument("Integer too large".to_string()))
                            }
                        } else {
                            throw_execution_error(info, EvalError::InvalidArgument("Base too large".to_string()))
                        }
                    }
                    _ => {
                        args_error(info, "int-to-str", &args)
                    }
                }
            })
    })
}

/// String to list of single-character strings
fn str_to_list_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("str-to-list", &args, MilliGas(3))
            .bind(move |_| {
                if args.len() != 1 {
                    return args_error(info, "str-to-list", &args);
                }
                
                match args[0].as_pact_value() {
                    Some(PactValue::String(s)) => {
                        let char_list: Vec<PactValue> = s.chars()
                            .map(|c| PactValue::String(c.to_string()))
                            .collect();
                        let result = CEKValue::VPactValue(PactValue::List(char_list));
                        return_cek_value(cont, handler, result)
                    }
                    _ => {
                        args_error(info, "str-to-list", &args)
                    }
                }
            })
    })
}

// Helper function to format PactValue as string
fn format_pact_value(value: &PactValue) -> String {
    match value {
        PactValue::String(s) => s.clone(),
        PactValue::Integer(i) => i.to_string(),
        PactValue::Decimal(d) => d.to_string(),
        PactValue::Bool(b) => b.to_string(),
        PactValue::Time(t) => t.to_string(),
        PactValue::List(l) => {
            let items: Vec<String> = l.iter().map(format_pact_value).collect();
            format!("[{}]", items.join(", "))
        }
        PactValue::Object(o) => {
            let items: Vec<String> = o.entries()
                .map(|(k, v)| format!("{}: {}", k, format_pact_value(v)))
                .collect();
            format!("{{{}}}", items.join(", "))
        }
        _ => "<unknown>".to_string(),
    }
}

// Helper function to format integer in given radix
fn format_radix(n: i64, radix: u32) -> String {
    if n == 0 {
        return "0".to_string();
    }
    
    let mut result = String::new();
    let mut num = n.abs() as u64;
    let digits = "0123456789abcdefghijklmnopqrstuvwxyz";
    
    while num > 0 {
        let digit = (num % radix as u64) as usize;
        result.push(digits.chars().nth(digit).unwrap());
        num /= radix as u64;
    }
    
    if n < 0 {
        result.push('-');
    }
    
    result.chars().rev().collect()
}
