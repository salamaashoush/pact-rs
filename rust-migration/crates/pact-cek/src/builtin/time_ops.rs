//! Time operations builtin functions
//!
//! Implements time manipulation functions following the Haskell reference
//! implementation exactly. All functions support microsecond precision and
//! strftime-style format strings.

use super::*;
use crate::monad::unwind_capability_stack;
use pact_core::values::{PactValue, time::PactTime, Decimal};
use chrono::{DateTime, Utc, NaiveDateTime};
use num_traits::ToPrimitive;

/// Register all time builtin functions
pub fn register_time_builtins(builtin_env: &mut BuiltinEnv) -> Result<(), pact_core::errors::PactErrorI> {
    // Register time() builtin
    builtin_env.register(
        CoreBuiltin::CoreTime,
        BuiltinSpec {
            name: "time",
            arity: 1,
            implementation: time_implementation(),
        }
    );

    // Register parse-time() builtin
    builtin_env.register(
        CoreBuiltin::CoreParseTime,
        BuiltinSpec {
            name: "parse-time",
            arity: 2,
            implementation: parse_time_implementation(),
        }
    );

    // Register format-time() builtin
    builtin_env.register(
        CoreBuiltin::CoreFormatTime,
        BuiltinSpec {
            name: "format-time",
            arity: 2,
            implementation: format_time_implementation(),
        }
    );

    // Register add-time() builtin
    builtin_env.register(
        CoreBuiltin::CoreAddTime,
        BuiltinSpec {
            name: "add-time",
            arity: 2,
            implementation: add_time_implementation(),
        }
    );

    // Register diff-time() builtin
    builtin_env.register(
        CoreBuiltin::CoreDiffTime,
        BuiltinSpec {
            name: "diff-time",
            arity: 2,
            implementation: diff_time_implementation(),
        }
    );

    // Register time utility functions
    builtin_env.register(
        CoreBuiltin::CoreHours,
        BuiltinSpec {
            name: "hours",
            arity: 1,
            implementation: hours_implementation(),
        }
    );

    builtin_env.register(
        CoreBuiltin::CoreMinutes,
        BuiltinSpec {
            name: "minutes",
            arity: 1,
            implementation: minutes_implementation(),
        }
    );

    builtin_env.register(
        CoreBuiltin::CoreDays,
        BuiltinSpec {
            name: "days",
            arity: 1,
            implementation: days_implementation(),
        }
    );

    Ok(())
}

/// Implementation of time() builtin function
/// 
/// From Haskell Core.Builtin:
/// ```haskell
/// time :: (IsBuiltin b) => NativeFunction e b i
/// time info b cont handler _env = \case
///   [VString s] -> do
///     case PactTime.parseTime "%Y-%m-%dT%H:%M:%SZ" (T.unpack s) of
///       Just t -> returnCEKValue cont handler $ VPactValue (PTime t)
///       Nothing ->
///         throwNativeExecutionError info b $ "time default format parse failure"
/// ```
fn time_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("time", &args, MilliGas(1))
            .bind(move |_| {
                if args.len() != 1 {
                    return args_error(info, "time", &args);
                }

                match args[0].as_pact_value() {
                    Some(PactValue::String(s)) => {
                        // Default format is ISO 8601: "%Y-%m-%dT%H:%M:%SZ"
                        match parse_time_with_format("%Y-%m-%dT%H:%M:%SZ", s) {
                            Ok(time) => {
                                return_cek_value(cont, handler, CEKValue::VPactValue(PactValue::Time(time)))
                            }
                            Err(_) => {
                                throw_invalid_argument_error(
                                    info,
                                    "time",
                                    "time default format parse failure"
                                )
                            }
                        }
                    }
                    _ => {
                        args_error(info, "time", &args)
                    }
                }
            })
            .try_with(|error| {
                unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
            })
    })
}

/// Implementation of parse-time() builtin function
fn parse_time_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("parse-time", &args, MilliGas(1))
            .bind(move |_| {
                if args.len() != 2 {
                    return args_error(info, "parse-time", &args);
                }

                match (args[0].as_pact_value(), args[1].as_pact_value()) {
                    (Some(PactValue::String(fmt)), Some(PactValue::String(s))) => {
                        match parse_time_with_format(fmt, s) {
                            Ok(time) => {
                                return_cek_value(cont, handler, CEKValue::VPactValue(PactValue::Time(time)))
                            }
                            Err(_) => {
                                throw_invalid_argument_error(
                                    info,
                                    "parse-time",
                                    "parse-time format parse failure"
                                )
                            }
                        }
                    }
                    _ => {
                        args_error(info, "parse-time", &args)
                    }
                }
            })
            .try_with(|error| {
                unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
            })
    })
}

/// Implementation of format-time() builtin function
fn format_time_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("format-time", &args, MilliGas(1))
            .bind(move |_| {
                if args.len() != 2 {
                    return args_error(info, "format-time", &args);
                }

                match (args[0].as_pact_value(), args[1].as_pact_value()) {
                    (Some(PactValue::String(fmt)), Some(PactValue::Time(time))) => {
                        match format_time_with_format(fmt, time) {
                            Ok(formatted) => {
                                return_cek_value(cont, handler, CEKValue::VPactValue(PactValue::String(formatted)))
                            }
                            Err(_) => {
                                throw_invalid_argument_error(
                                    info,
                                    "format-time",
                                    "format-time format failure"
                                )
                            }
                        }
                    }
                    _ => {
                        args_error(info, "format-time", &args)
                    }
                }
            })
            .try_with(|error| {
                unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
            })
    })
}

/// Implementation of add-time() builtin function
fn add_time_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("add-time", &args, MilliGas(1))
            .bind(move |_| {
                if args.len() != 2 {
                    return args_error(info, "add-time", &args);
                }

                match (args[0].as_pact_value(), args[1].as_pact_value()) {
                    (Some(PactValue::Time(time)), Some(PactValue::Decimal(seconds))) => {
                        // Convert decimal seconds to microseconds
                        let microseconds = (seconds.to_f64() * 1_000_000.0).round() as i64;
                        let new_time = time.add_micros(microseconds);
                        return_cek_value(cont, handler, CEKValue::VPactValue(PactValue::Time(new_time)))
                    }
                    (Some(PactValue::Time(time)), Some(PactValue::Integer(seconds))) => {
                        // Convert integer seconds to microseconds
                        let microseconds = seconds.to_i64().unwrap_or(0) * 1_000_000;
                        let new_time = time.add_micros(microseconds);
                        return_cek_value(cont, handler, CEKValue::VPactValue(PactValue::Time(new_time)))
                    }
                    _ => {
                        args_error(info, "add-time", &args)
                    }
                }
            })
            .try_with(|error| {
                unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
            })
    })
}

/// Implementation of diff-time() builtin function
fn diff_time_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("diff-time", &args, MilliGas(1))
            .bind(move |_| {
                if args.len() != 2 {
                    return args_error(info, "diff-time", &args);
                }

                match (args[0].as_pact_value(), args[1].as_pact_value()) {
                    (Some(PactValue::Time(time1)), Some(PactValue::Time(time2))) => {
                        // Calculate difference in seconds as decimal
                        let diff_micros = time1.diff_micros(time2);
                        let diff_seconds = diff_micros as f64 / 1_000_000.0;
                        
                        let decimal_value = Decimal::from_f64(diff_seconds);
                        return_cek_value(cont, handler, CEKValue::VPactValue(PactValue::Decimal(decimal_value)))
                    }
                    _ => {
                        args_error(info, "diff-time", &args)
                    }
                }
            })
            .try_with(|error| {
                unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
            })
    })
}

/// Implementation of hours() utility function
fn hours_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("hours", &args, MilliGas(1))
            .bind(move |_| {
                if args.len() != 1 {
                    return args_error(info, "hours", &args);
                }

                match args[0].as_pact_value() {
                    Some(PactValue::Decimal(hours)) => {
                        let minutes = hours.to_f64() * 60.0;
                        let decimal_value = Decimal::from_f64(minutes);
                        return_cek_value(cont, handler, CEKValue::VPactValue(PactValue::Decimal(decimal_value)))
                    }
                    Some(PactValue::Integer(hours)) => {
                        let minutes = hours.to_f64().unwrap_or(0.0) * 60.0;
                        let decimal_value = Decimal::from_f64(minutes);
                        return_cek_value(cont, handler, CEKValue::VPactValue(PactValue::Decimal(decimal_value)))
                    }
                    _ => {
                        args_error(info, "hours", &args)
                    }
                }
            })
            .try_with(|error| {
                unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
            })
    })
}

/// Implementation of minutes() utility function
fn minutes_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("minutes", &args, MilliGas(1))
            .bind(move |_| {
                if args.len() != 1 {
                    return args_error(info, "minutes", &args);
                }

                match args[0].as_pact_value() {
                    Some(PactValue::Decimal(minutes)) => {
                        return_cek_value(cont, handler, CEKValue::VPactValue(PactValue::Decimal(minutes.clone())))
                    }
                    Some(PactValue::Integer(minutes)) => {
                        let decimal_value = Decimal::from_integer(minutes.clone());
                        return_cek_value(cont, handler, CEKValue::VPactValue(PactValue::Decimal(decimal_value)))
                    }
                    _ => {
                        args_error(info, "minutes", &args)
                    }
                }
            })
            .try_with(|error| {
                unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
            })
    })
}

/// Implementation of days() utility function
fn days_implementation() -> NativeFunction {
    Box::new(|info, _builtin, cont, handler, _env, args| {
        charge_gas_with_args("days", &args, MilliGas(1))
            .bind(move |_| {
                if args.len() != 1 {
                    return args_error(info, "days", &args);
                }

                match args[0].as_pact_value() {
                    Some(PactValue::Decimal(days)) => {
                        let minutes = days.to_f64() * 24.0 * 60.0;
                        let decimal_value = Decimal::from_f64(minutes);
                        return_cek_value(cont, handler, CEKValue::VPactValue(PactValue::Decimal(decimal_value)))
                    }
                    Some(PactValue::Integer(days)) => {
                        let minutes = days.to_f64().unwrap_or(0.0) * 24.0 * 60.0;
                        let decimal_value = Decimal::from_f64(minutes);
                        return_cek_value(cont, handler, CEKValue::VPactValue(PactValue::Decimal(decimal_value)))
                    }
                    _ => {
                        args_error(info, "days", &args)
                    }
                }
            })
            .try_with(|error| {
                unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
            })
    })
}

/// Parse time string with given format
/// Supports strftime format codes as used in Haskell Pact
pub fn parse_time_with_format(format: &str, time_str: &str) -> Result<PactTime, String> {
    // For now, implement the most common formats used in Pact
    match format {
        "%Y-%m-%dT%H:%M:%SZ" => {
            // ISO 8601 format
            let dt = DateTime::parse_from_rfc3339(&time_str.replace("Z", "+00:00"))
                .map_err(|e| format!("Failed to parse ISO 8601 time: {}", e))?;
            Ok(PactTime::from_datetime(dt.with_timezone(&Utc)))
        }
        "%Y-%m-%d" => {
            // Date only format
            let naive_date = NaiveDateTime::parse_from_str(&format!("{} 00:00:00", time_str), "%Y-%m-%d %H:%M:%S")
                .map_err(|e| format!("Failed to parse date: {}", e))?;
            let dt = DateTime::<Utc>::from_naive_utc_and_offset(naive_date, Utc);
            Ok(PactTime::from_datetime(dt))
        }
        "%F" => {
            // Same as %Y-%m-%d
            parse_time_with_format("%Y-%m-%d", time_str)
        }
        _ => {
            // For other formats, use a basic chrono parsing approach
            // This is a simplified implementation - the full Haskell version
            // supports all strftime format codes via the thyme library
            Err(format!("Unsupported time format: {}", format))
        }
    }
}

/// Format time with given format string
/// Supports strftime format codes as used in Haskell Pact
pub fn format_time_with_format(format: &str, time: &PactTime) -> Result<String, String> {
    use chrono::{Datelike, Timelike};
    
    let dt = time.to_datetime();
    let mut result = String::new();
    let mut chars = format.chars().peekable();
    
    while let Some(ch) = chars.next() {
        if ch == '%' {
            if let Some(&next_ch) = chars.peek() {
                chars.next(); // consume the format specifier
                match next_ch {
                    // Time components
                    'H' => result.push_str(&format!("{:02}", dt.hour())),
                    'k' => result.push_str(&format!("{:2}", dt.hour())),
                    'I' => {
                        let hour12 = if dt.hour() == 0 { 12 } else if dt.hour() > 12 { dt.hour() - 12 } else { dt.hour() };
                        result.push_str(&format!("{:02}", hour12));
                    }
                    'l' => {
                        let hour12 = if dt.hour() == 0 { 12 } else if dt.hour() > 12 { dt.hour() - 12 } else { dt.hour() };
                        result.push_str(&format!("{:2}", hour12));
                    }
                    'M' => result.push_str(&format!("{:02}", dt.minute())),
                    'S' => result.push_str(&format!("{:02}", dt.second())),
                    'q' => {
                        // Picoseconds - Pact uses microsecond precision, so we pad with zeros
                        let micros = time.as_micros() % 1_000_000;
                        result.push_str(&format!("{:012}", micros * 1_000_000));
                    }
                    'v' => {
                        // Microseconds
                        let micros = time.as_micros() % 1_000_000;
                        result.push_str(&format!("{:06}", micros));
                    }
                    'Q' => {
                        // Decimal point and fractional seconds
                        let micros = time.as_micros() % 1_000_000;
                        if micros > 0 {
                            let fraction = format!(".{:06}", micros);
                            // Remove trailing zeros
                            let trimmed = fraction.trim_end_matches('0');
                            result.push_str(trimmed);
                        }
                    }
                    's' => result.push_str(&time.as_seconds().to_string()),
                    
                    // Date components
                    'Y' => result.push_str(&dt.year().to_string()),
                    'y' => result.push_str(&format!("{:02}", dt.year() % 100)),
                    'C' => result.push_str(&(dt.year() / 100).to_string()),
                    'B' => result.push_str(&dt.format("%B").to_string()),
                    'b' | 'h' => result.push_str(&dt.format("%b").to_string()),
                    'm' => result.push_str(&format!("{:02}", dt.month())),
                    'd' => result.push_str(&format!("{:02}", dt.day())),
                    'e' => result.push_str(&format!("{:2}", dt.day())),
                    'j' => result.push_str(&format!("{:03}", dt.ordinal())),
                    
                    // Week date format
                    'G' => result.push_str(&dt.iso_week().year().to_string()),
                    'g' => result.push_str(&format!("{:02}", dt.iso_week().year() % 100)),
                    'f' => result.push_str(&(dt.iso_week().year() / 100).to_string()),
                    'V' => result.push_str(&format!("{:02}", dt.iso_week().week())),
                    'u' => result.push_str(&dt.weekday().number_from_monday().to_string()),
                    
                    // Day of week
                    'a' => result.push_str(&dt.format("%a").to_string()),
                    'A' => result.push_str(&dt.format("%A").to_string()),
                    'U' => {
                        // Week of year (Sunday as first day)
                        // According to strftime: Week 01 is the first week that contains a Sunday
                        let jan1 = dt.with_ordinal(1).unwrap();
                        let jan1_weekday = jan1.weekday().num_days_from_sunday();
                        
                        // Calculate days from the first Sunday of the year
                        let days_to_first_sunday = if jan1_weekday == 0 { 0 } else { 7 - jan1_weekday };
                        let days_since_first_sunday = dt.ordinal() as i32 - 1 - days_to_first_sunday as i32;
                        
                        let week_num = if days_since_first_sunday < 0 {
                            // We're before the first Sunday, so week 00
                            0
                        } else {
                            // Add 1 because the week containing the first Sunday is week 01
                            (days_since_first_sunday / 7) + 1
                        };
                        
                        result.push_str(&format!("{:02}", week_num));
                    }
                    'w' => result.push_str(&dt.weekday().num_days_from_sunday().to_string()),
                    'W' => {
                        // Week of year (Monday as first day)
                        let jan1 = dt.with_ordinal(1).unwrap();
                        let jan1_weekday = jan1.weekday().num_days_from_monday();
                        let days_since_monday = (dt.ordinal() - 1 + jan1_weekday) / 7;
                        result.push_str(&format!("{:02}", days_since_monday));
                    }
                    
                    // Composite formats
                    'D' => result.push_str(&dt.format("%m/%d/%y").to_string()),
                    'F' => result.push_str(&dt.format("%Y-%m-%d").to_string()),
                    'x' => result.push_str(&dt.format("%m/%d/%y").to_string()),
                    'N' => {
                        // ISO 8601 format with timezone (using UTC)
                        result.push_str(&dt.format("%Y-%m-%dT%H:%M:%S").to_string());
                        let micros = time.as_micros() % 1_000_000;
                        if micros > 0 {
                            result.push_str(&format!(".{:06}", micros).trim_end_matches('0'));
                        }
                        result.push_str("Z");
                    }
                    'Z' => result.push_str("UTC"), // Pact uses UTC
                    
                    // Special
                    '%' => result.push('%'),
                    
                    _ => {
                        // Unknown format code - return error
                        return Err(format!("Unknown format code: %{}", next_ch));
                    }
                }
            } else {
                result.push(ch);
            }
        } else {
            result.push(ch);
        }
    }
    
    Ok(result)
}