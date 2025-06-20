use super::*;
use crate::monad::unwind_capability_stack;
use num_bigint::BigInt;
use num_traits::{One, Signed, ToPrimitive, Zero};
use pact_values::{Decimal, PactValue};

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


pub fn register_arithmetic_builtins(builtin_env: &mut BuiltinEnv) -> Result<(), pact_errors::PactErrorI> {
  // Basic arithmetic operations
  builtin_env.register(
    CoreBuiltin::CoreAdd,
    BuiltinSpec {
      name: "+",
      arity: 2,
      implementation: add_implementation(),
    },
  );

  builtin_env.register(
    CoreBuiltin::CoreSub,
    BuiltinSpec {
      name: "-",
      arity: 2,
      implementation: sub_implementation(),
    },
  );

  builtin_env.register(
    CoreBuiltin::CoreMultiply,
    BuiltinSpec {
      name: "*",
      arity: 2,
      implementation: mul_implementation(),
    },
  );

  builtin_env.register(
    CoreBuiltin::CoreDivide,
    BuiltinSpec {
      name: "/",
      arity: 2,
      implementation: div_implementation(),
    },
  );

  builtin_env.register(
    CoreBuiltin::CoreNegate,
    BuiltinSpec {
      name: "negate",
      arity: 1,
      implementation: negate_implementation(),
    },
  );

  builtin_env.register(
    CoreBuiltin::CoreAbs,
    BuiltinSpec {
      name: "abs",
      arity: 1,
      implementation: abs_implementation(),
    },
  );

  builtin_env.register(
    CoreBuiltin::CorePow,
    BuiltinSpec {
      name: "^",
      arity: 2,
      implementation: pow_implementation(),
    },
  );

  builtin_env.register(
    CoreBuiltin::CoreMod,
    BuiltinSpec {
      name: "mod",
      arity: 2,
      implementation: mod_implementation(),
    },
  );

  // Rounding operations
  builtin_env.register(
    CoreBuiltin::CoreRound,
    BuiltinSpec {
      name: "round",
      arity: 1,
      implementation: round_implementation(),
    },
  );

  builtin_env.register(
    CoreBuiltin::CoreCeiling,
    BuiltinSpec {
      name: "ceiling",
      arity: 1,
      implementation: ceiling_implementation(),
    },
  );

  builtin_env.register(
    CoreBuiltin::CoreFloor,
    BuiltinSpec {
      name: "floor",
      arity: 1,
      implementation: floor_implementation(),
    },
  );

  // Transcendental functions
  builtin_env.register(
    CoreBuiltin::CoreExp,
    BuiltinSpec {
      name: "exp",
      arity: 1,
      implementation: exp_implementation(),
    },
  );

  builtin_env.register(
    CoreBuiltin::CoreLn,
    BuiltinSpec {
      name: "ln",
      arity: 1,
      implementation: ln_implementation(),
    },
  );

  builtin_env.register(
    CoreBuiltin::CoreSqrt,
    BuiltinSpec {
      name: "sqrt",
      arity: 1,
      implementation: sqrt_implementation(),
    },
  );

  builtin_env.register(
    CoreBuiltin::CoreLogBase,
    BuiltinSpec {
      name: "log",
      arity: 2,
      implementation: log_base_implementation(),
    },
  );

  // Bitwise operations (integer only)
  builtin_env.register(
    CoreBuiltin::CoreBitwiseAnd,
    BuiltinSpec {
      name: "&",
      arity: 2,
      implementation: bitwise_and_implementation(),
    },
  );

  builtin_env.register(
    CoreBuiltin::CoreBitwiseOr,
    BuiltinSpec {
      name: "|",
      arity: 2,
      implementation: bitwise_or_implementation(),
    },
  );

  builtin_env.register(
    CoreBuiltin::CoreBitwiseXor,
    BuiltinSpec {
      name: "xor",
      arity: 2,
      implementation: bitwise_xor_implementation(),
    },
  );

  builtin_env.register(
    CoreBuiltin::CoreBitwiseFlip,
    BuiltinSpec {
      name: "~",
      arity: 1,
      implementation: bitwise_flip_implementation(),
    },
  );

  builtin_env.register(
    CoreBuiltin::CoreBitShift,
    BuiltinSpec {
      name: "shift",
      arity: 2,
      implementation: bit_shift_implementation(),
    },
  );

  Ok(())
}

/// Addition implementation with full CEK integration
fn add_implementation() -> NativeFunction {
  Box::new(|info, _builtin, cont, handler, _env, args| {
    charge_gas_with_args("+", &args, MilliGas(2))
      .bind(move |_| {
        // Check argument count and types - matches Haskell argsError pattern
        if args.len() != 2 {
          return args_error(info, "+", &args);
        }
        
        match (args[0].as_pact_value(), args[1].as_pact_value()) {
          (Some(PactValue::Integer(a)), Some(PactValue::Integer(b))) => {
            let result = CEKValue::VPactValue(PactValue::Integer(a + b));
            return_cek_value(cont, handler, result)
          }
          (Some(PactValue::Decimal(a)), Some(PactValue::Decimal(b))) => {
            let result = CEKValue::VPactValue(PactValue::Decimal(a + b));
            return_cek_value(cont, handler, result)
          }
          (Some(PactValue::Integer(a)), Some(PactValue::Decimal(b))) => {
            let a_decimal = pact_values::Decimal::from(a.clone());
            let result = CEKValue::VPactValue(PactValue::Decimal(a_decimal + b.clone()));
            return_cek_value(cont, handler, result)
          }
          (Some(PactValue::Decimal(a)), Some(PactValue::Integer(b))) => {
            let b_decimal = pact_values::Decimal::from(b.clone());
            let result = CEKValue::VPactValue(PactValue::Decimal(a + &b_decimal));
            return_cek_value(cont, handler, result)
          }
          _ => {
            // Use argsError for type mismatches - matches Haskell pattern exactly
            args_error(info, "+", &args)
          }
        }
      })
      .try_with(|error| {
        unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
      })
  })
}

/// Subtraction implementation with full CEK integration
fn sub_implementation() -> NativeFunction {
  Box::new(|info, _builtin, cont, handler, _env, args| {
    charge_gas_with_args("-", &args, MilliGas(2))
      .bind(move |_| {
        if args.len() != 2 {
          return args_error(info, "-", &args);
        }
        
        match (args[0].as_pact_value(), args[1].as_pact_value()) {
            (Some(PactValue::Integer(a)), Some(PactValue::Integer(b))) => {
              let result = CEKValue::VPactValue(PactValue::Integer(a - b));
              return_cek_value(cont, handler, result)
            }
            (Some(PactValue::Decimal(a)), Some(PactValue::Decimal(b))) => {
              let result = CEKValue::VPactValue(PactValue::Decimal(a - b));
              return_cek_value(cont, handler, result)
            }
            (Some(PactValue::Integer(a)), Some(PactValue::Decimal(b))) => {
              let a_decimal = pact_values::Decimal::from(a.clone());
              let result = CEKValue::VPactValue(PactValue::Decimal(a_decimal - b.clone()));
              return_cek_value(cont, handler, result)
            }
            (Some(PactValue::Decimal(a)), Some(PactValue::Integer(b))) => {
              let b_decimal = pact_values::Decimal::from(b.clone());
              let result = CEKValue::VPactValue(PactValue::Decimal(a - &b_decimal));
              return_cek_value(cont, handler, result)
            }
            _ => {
              // Use argsError for type mismatches - matches Haskell pattern exactly
              args_error(info, "-", &args)
            }
          }
      })
      .try_with(|error| {
        unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
      })
  })
}

/// Multiplication implementation with full CEK integration
fn mul_implementation() -> NativeFunction {
  Box::new(|info, _builtin, cont, handler, _env, args| {
    charge_gas_with_args("*", &args, MilliGas(3))
      .bind(move |_| {
        if args.len() != 2 {
          return args_error(info, "*", &args);
        }
        
        match (args[0].as_pact_value(), args[1].as_pact_value()) {
            (Some(PactValue::Integer(a)), Some(PactValue::Integer(b))) => {
              let result = CEKValue::VPactValue(PactValue::Integer(a * b));
              return_cek_value(cont, handler, result)
            }
            (Some(PactValue::Decimal(a)), Some(PactValue::Decimal(b))) => {
              let result = CEKValue::VPactValue(PactValue::Decimal(a * b));
              return_cek_value(cont, handler, result)
            }
            (Some(PactValue::Integer(a)), Some(PactValue::Decimal(b))) => {
              let a_decimal = pact_values::Decimal::from(a.clone());
              let result = CEKValue::VPactValue(PactValue::Decimal(a_decimal * b.clone()));
              return_cek_value(cont, handler, result)
            }
            (Some(PactValue::Decimal(a)), Some(PactValue::Integer(b))) => {
              let b_decimal = pact_values::Decimal::from(b.clone());
              let result = CEKValue::VPactValue(PactValue::Decimal(a * &b_decimal));
              return_cek_value(cont, handler, result)
            }
            _ => {
              // Use argsError for type mismatches - matches Haskell pattern exactly
              args_error(info, "*", &args)
            }
          }
      })
      .try_with(|error| {
        unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
      })
  })
}

/// Division implementation with full CEK integration and error handling
fn div_implementation() -> NativeFunction {
  Box::new(|info, _builtin, cont, handler, _env, args| {
    charge_gas_with_args("/", &args, MilliGas(4))
      .bind(move |_| {
        if args.len() != 2 {
          return args_error(info, "/", &args);
        }
        
        match (args[0].as_pact_value(), args[1].as_pact_value()) {
            (Some(PactValue::Integer(a)), Some(PactValue::Integer(b))) => {
              if *b == num_bigint::BigInt::from(0) {
                throw_execution_error(info, EvalError::ArithmeticException("div by zero".into()))
              } else {
                let result = CEKValue::VPactValue(PactValue::Integer(a / b));
                return_cek_value(cont, handler, result)
              }
            }
            (Some(PactValue::Decimal(a)), Some(PactValue::Decimal(b))) => {
              if *b == pact_values::Decimal::from(0) {
                throw_execution_error(info, EvalError::ArithmeticException("div by zero, decimal".into()))
              } else {
                let result = CEKValue::VPactValue(PactValue::Decimal(a / b));
                return_cek_value(cont, handler, result)
              }
            }
            (Some(PactValue::Integer(a)), Some(PactValue::Decimal(b))) => {
              if *b == pact_values::Decimal::from(0) {
                throw_execution_error(info, EvalError::ArithmeticException("div by zero, decimal".into()))
              } else {
                let a_decimal = pact_values::Decimal::from(a.clone());
                let result = CEKValue::VPactValue(PactValue::Decimal(a_decimal / b.clone()));
                return_cek_value(cont, handler, result)
              }
            }
            (Some(PactValue::Decimal(a)), Some(PactValue::Integer(b))) => {
              if *b == num_bigint::BigInt::from(0) {
                throw_execution_error(info, EvalError::ArithmeticException("div by zero, decimal".into()))
              } else {
                let b_decimal = pact_values::Decimal::from(b.clone());
                let result = CEKValue::VPactValue(PactValue::Decimal(a / &b_decimal));
                return_cek_value(cont, handler, result)
              }
            }
            _ => {
              // Use argsError for type mismatches - matches Haskell pattern exactly
              args_error(info, "/", &args)
            }
          }
      })
      .try_with(|error| {
        unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
      })
  })
}

/// Negation implementation - unary minus
fn negate_implementation() -> NativeFunction {
  Box::new(|info, _builtin, cont, handler, _env, args| {
    charge_gas_with_args("negate", &args, MilliGas(1))
      .bind(move |_| {
        if args.len() != 1 {
          return args_error(info, "negate", &args);
        }
        
        match args[0].as_pact_value() {
            Some(PactValue::Integer(a)) => {
              let result = CEKValue::VPactValue(PactValue::Integer(-a));
              return_cek_value(cont, handler, result)
            }
            Some(PactValue::Decimal(a)) => {
              let result = CEKValue::VPactValue(PactValue::Decimal(-a));
              return_cek_value(cont, handler, result)
            }
            _ => {
              // Use argsError for type mismatches - matches Haskell pattern exactly
              args_error(info, "negate", &args)
            }
          }
      })
      .try_with(|error| {
        unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
      })
  })
}

/// Absolute value implementation
fn abs_implementation() -> NativeFunction {
  Box::new(|info, _builtin, cont, handler, _env, args| {
    charge_gas_with_args("abs", &args, MilliGas(1))
      .bind(move |_| {
        if args.len() != 1 {
          return args_error(info, "abs", &args);
        }
        
        match args[0].as_pact_value() {
            Some(PactValue::Integer(a)) => {
              let result = CEKValue::VPactValue(PactValue::Integer(a.abs()));
              return_cek_value(cont, handler, result)
            }
            Some(PactValue::Decimal(a)) => {
              let result = CEKValue::VPactValue(PactValue::Decimal(a.abs()));
              return_cek_value(cont, handler, result)
            }
            _ => {
              // Use argsError for type mismatches - matches Haskell pattern exactly
              args_error(info, "abs", &args)
            }
          }
      })
      .try_with(|error| {
        unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
      })
  })
}

/// Power implementation with proper error handling
fn pow_implementation() -> NativeFunction {
  Box::new(|info, _builtin, cont, handler, _env, args| {
    charge_gas_with_args("^", &args, MilliGas(10))
      .bind(move |_| {
        if args.len() != 2 {
          return args_error(info, "^", &args);
        }
        
        match (args[0].as_pact_value(), args[1].as_pact_value()) {
            (Some(PactValue::Integer(base)), Some(PactValue::Integer(exp))) => {
              if exp < &BigInt::zero() {
                throw_execution_error(info, EvalError::InvalidArgument("negative exponent in integer power".to_string()))
              } else if let Some(exp_u32) = exp.to_u32() {
                let result = base.pow(exp_u32);
                let result = CEKValue::VPactValue(PactValue::Integer(result));
                return_cek_value(cont, handler, result)
              } else {
                throw_execution_error(info, EvalError::InvalidArgument("exponent too large".to_string()))
              }
            }
            (Some(PactValue::Decimal(base)), Some(PactValue::Decimal(exp))) => {
              if base.is_zero() && exp < &Decimal::zero() {
                throw_execution_error(info, EvalError::InvalidArgument("zero to a negative power is undefined".to_string()))
              } else {
                match decimal_pow(&base, &exp) {
                  Ok(result) => {
                    let result = CEKValue::VPactValue(PactValue::Decimal(result));
                    return_cek_value(cont, handler, result)
                  }
                  Err(msg) => throw_execution_error(info, EvalError::InvalidArgument(msg))
                }
              }
            }
            (Some(PactValue::Integer(base)), Some(PactValue::Decimal(exp))) => {
              let base_decimal = Decimal::from(base.clone());
              if base_decimal.is_zero() && exp < &Decimal::zero() {
                throw_execution_error(info, EvalError::InvalidArgument("zero to a negative power is undefined".to_string()))
              } else {
                match decimal_pow(&base_decimal, &exp) {
                  Ok(result) => {
                    let result = CEKValue::VPactValue(PactValue::Decimal(result));
                    return_cek_value(cont, handler, result)
                  }
                  Err(msg) => throw_execution_error(info, EvalError::InvalidArgument(msg))
                }
              }
            }
            (Some(PactValue::Decimal(base)), Some(PactValue::Integer(exp))) => {
              let exp_decimal = Decimal::from(exp.clone());
              if base.is_zero() && exp < &BigInt::zero() {
                throw_execution_error(info, EvalError::InvalidArgument("zero to a negative power is undefined".to_string()))
              } else {
                match decimal_pow(&base, &exp_decimal) {
                  Ok(result) => {
                    let result = CEKValue::VPactValue(PactValue::Decimal(result));
                    return_cek_value(cont, handler, result)
                  }
                  Err(msg) => throw_execution_error(info, EvalError::InvalidArgument(msg))
                }
              }
            }
            _ => {
              // Use argsError for type mismatches - matches Haskell pattern exactly
              args_error(info, "^", &args)
            }
          }
      })
      .try_with(|error| {
        unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
      })
  })
}

/// Modulo implementation (integers only)
fn mod_implementation() -> NativeFunction {
  Box::new(|info, _builtin, cont, handler, _env, args| {
    charge_gas_with_args("mod", &args, MilliGas(3))
      .bind(move |_| {
        if args.len() != 2 {
          return args_error(info, "mod", &args);
        }
        
        match (args[0].as_pact_value(), args[1].as_pact_value()) {
            (Some(PactValue::Integer(a)), Some(PactValue::Integer(b))) => {
              if b.is_zero() {
                throw_execution_error(info, EvalError::DivisionByZero)
              } else {
                let result = CEKValue::VPactValue(PactValue::Integer(a % b));
                return_cek_value(cont, handler, result)
              }
            }
            _ => {
              // Use argsError for type mismatches - matches Haskell pattern exactly
              args_error(info, "mod", &args)
            }
          }
      })
      .try_with(|error| {
        unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
      })
  })
}

/// Round implementation - rounds to nearest integer
fn round_implementation() -> NativeFunction {
  Box::new(|info, _builtin, cont, handler, _env, args| {
    charge_gas_with_args("round", &args, MilliGas(2))
      .bind(move |_| {
        if args.len() != 1 {
          return args_error(info, "round", &args);
        }
        
        match args[0].as_pact_value() {
            Some(PactValue::Decimal(d)) => {
              let result = CEKValue::VPactValue(PactValue::Integer(d.round_to_integer()));
              return_cek_value(cont, handler, result)
            }
            Some(PactValue::Integer(i)) => {
              let result = CEKValue::VPactValue(PactValue::Integer(i.clone()));
              return_cek_value(cont, handler, result)
            }
            _ => {
              // Use argsError for type mismatches - matches Haskell pattern exactly
              args_error(info, "round", &args)
            }
          }
      })
      .try_with(|error| {
        unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
      })
  })
}

/// Ceiling implementation - rounds up to nearest integer
fn ceiling_implementation() -> NativeFunction {
  Box::new(|info, _builtin, cont, handler, _env, args| {
    charge_gas_with_args("ceiling", &args, MilliGas(2))
      .bind(move |_| {
        if args.len() != 1 {
          return args_error(info, "ceiling", &args);
        }
        
        match args[0].as_pact_value() {
            Some(PactValue::Decimal(d)) => {
              let result = CEKValue::VPactValue(PactValue::Integer(d.ceil_to_integer()));
              return_cek_value(cont, handler, result)
            }
            Some(PactValue::Integer(i)) => {
              let result = CEKValue::VPactValue(PactValue::Integer(i.clone()));
              return_cek_value(cont, handler, result)
            }
            _ => {
              // Use argsError for type mismatches - matches Haskell pattern exactly
              args_error(info, "ceiling", &args)
            }
          }
      })
      .try_with(|error| {
        unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
      })
  })
}

/// Floor implementation - rounds down to nearest integer
fn floor_implementation() -> NativeFunction {
  Box::new(|info, _builtin, cont, handler, _env, args| {
    charge_gas_with_args("floor", &args, MilliGas(2))
      .bind(move |_| {
        if args.len() != 1 {
          return args_error(info, "floor", &args);
        }
        
        match args[0].as_pact_value() {
            Some(PactValue::Decimal(d)) => {
              let result = CEKValue::VPactValue(PactValue::Integer(d.floor_to_integer()));
              return_cek_value(cont, handler, result)
            }
            Some(PactValue::Integer(i)) => {
              let result = CEKValue::VPactValue(PactValue::Integer(i.clone()));
              return_cek_value(cont, handler, result)
            }
            _ => {
              // Use argsError for type mismatches - matches Haskell pattern exactly
              args_error(info, "floor", &args)
            }
          }
      })
      .try_with(|error| {
        unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
      })
  })
}

/// Exponential function (e^x)
fn exp_implementation() -> NativeFunction {
  Box::new(|info, _builtin, cont, handler, _env, args| {
    charge_gas_with_args("exp", &args, MilliGas(20))
      .bind(move |_| {
        if args.len() != 1 {
          return args_error(info, "exp", &args);
        }
        
        match args[0].as_pact_value() {
            Some(PactValue::Decimal(d)) => match decimal_exp(&d) {
              Ok(result) => {
                let result = CEKValue::VPactValue(PactValue::Decimal(result));
                return_cek_value(cont, handler, result)
              }
              Err(msg) => throw_execution_error(info, EvalError::InvalidArgument(msg))
            },
            Some(PactValue::Integer(i)) => {
              let d = Decimal::from(i.clone());
              match decimal_exp(&d) {
                Ok(result) => {
                  let result = CEKValue::VPactValue(PactValue::Decimal(result));
                  return_cek_value(cont, handler, result)
                }
                Err(msg) => throw_execution_error(info, EvalError::InvalidArgument(msg))
              }
            }
            _ => {
              // Use argsError for type mismatches - matches Haskell pattern exactly
              args_error(info, "exp", &args)
            }
          }
      })
      .try_with(|error| {
        unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
      })
  })
}

/// Natural logarithm implementation
fn ln_implementation() -> NativeFunction {
  Box::new(|info, _builtin, cont, handler, _env, args| {
    charge_gas_with_args("ln", &args, MilliGas(20))
      .bind(move |_| {
        if args.len() != 1 {
          return args_error(info, "ln", &args);
        }
        
        match args[0].as_pact_value() {
            Some(PactValue::Decimal(d)) => {
              if d <= &Decimal::zero() {
                throw_execution_error(info, EvalError::InvalidArgument("Natural logarithm must be greater than 0".to_string()))
              } else {
                match decimal_ln(&d) {
                  Ok(result) => {
                    let result = CEKValue::VPactValue(PactValue::Decimal(result));
                    return_cek_value(cont, handler, result)
                  }
                  Err(msg) => throw_execution_error(info, EvalError::InvalidArgument(msg))
                }
              }
            }
            Some(PactValue::Integer(i)) => {
              if i <= &BigInt::zero() {
                throw_execution_error(info, EvalError::InvalidArgument("Natural logarithm must be greater than 0".to_string()))
              } else {
                let d = Decimal::from(i.clone());
                match decimal_ln(&d) {
                  Ok(result) => {
                    let result = CEKValue::VPactValue(PactValue::Decimal(result));
                    return_cek_value(cont, handler, result)
                  }
                  Err(msg) => throw_execution_error(info, EvalError::InvalidArgument(msg))
                }
              }
            }
            _ => {
              // Use argsError for type mismatches - matches Haskell pattern exactly
              args_error(info, "ln", &args)
            }
          }
      })
      .try_with(|error| {
        unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
      })
  })
}

/// Square root implementation
fn sqrt_implementation() -> NativeFunction {
  Box::new(|info, _builtin, cont, handler, _env, args| {
    charge_gas_with_args("sqrt", &args, MilliGas(15))
      .bind(move |_| {
        if args.len() != 1 {
          return args_error(info, "sqrt", &args);
        }
        
        match args[0].as_pact_value() {
            Some(PactValue::Decimal(d)) => {
              if d < &Decimal::zero() {
                throw_execution_error(info, EvalError::InvalidArgument("Square root must be non-negative".to_string()))
              } else {
                match decimal_sqrt(&d) {
                  Ok(result) => {
                    let result = CEKValue::VPactValue(PactValue::Decimal(result));
                    return_cek_value(cont, handler, result)
                  }
                  Err(msg) => throw_execution_error(info, EvalError::InvalidArgument(msg))
                }
              }
            }
            Some(PactValue::Integer(i)) => {
              if i < &BigInt::zero() {
                throw_execution_error(info, EvalError::InvalidArgument("Square root must be non-negative".to_string()))
              } else {
                let d = Decimal::from(i.clone());
                match decimal_sqrt(&d) {
                  Ok(result) => {
                    let result = CEKValue::VPactValue(PactValue::Decimal(result));
                    return_cek_value(cont, handler, result)
                  }
                  Err(msg) => throw_execution_error(info, EvalError::InvalidArgument(msg))
                }
              }
            }
            _ => {
              // Use argsError for type mismatches - matches Haskell pattern exactly
              args_error(info, "sqrt", &args)
            }
          }
      })
      .try_with(|error| {
        unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
      })
  })
}

/// Logarithm with base implementation
fn log_base_implementation() -> NativeFunction {
  Box::new(|info, _builtin, cont, handler, _env, args| {
    charge_gas_with_args("log", &args, MilliGas(25))
      .bind(move |_| {
        if args.len() != 2 {
          return args_error(info, "log", &args);
        }
        
        match (args[0].as_pact_value(), args[1].as_pact_value()) {
            (Some(base_val), Some(arg_val)) => {
              let base_decimal = to_decimal(base_val);
              let arg_decimal = to_decimal(arg_val);

              match (base_decimal, arg_decimal) {
                (Some(base), Some(arg)) => {
                  if base <= Decimal::zero() {
                    throw_execution_error(info, EvalError::InvalidArgument("Negative log base".to_string()))
                  } else if base == Decimal::one() {
                    throw_execution_error(info, EvalError::InvalidArgument("Base 1 not supported".to_string()))
                  } else if arg <= Decimal::zero() {
                    throw_execution_error(info, EvalError::InvalidArgument("Non-positive log argument".to_string()))
                  } else {
                    match decimal_log_base(&base, &arg) {
                      Ok(result) => {
                        let result = CEKValue::VPactValue(PactValue::Decimal(result));
                        return_cek_value(cont, handler, result)
                      }
                      Err(msg) => throw_execution_error(info, EvalError::InvalidArgument(msg))
                    }
                  }
                }
                _ => {
                  // Use argsError for type mismatches - matches Haskell pattern exactly
                  args_error(info, "log", &args)
                }
              }
            }
            _ => {
              // Use argsError for type mismatches - matches Haskell pattern exactly
              args_error(info, "log", &args)
            }
          }
      })
      .try_with(|error| {
        unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
      })
  })
}

// Bitwise operations - integers only

/// Bitwise AND implementation
fn bitwise_and_implementation() -> NativeFunction {
  Box::new(|info, _builtin, cont, handler, _env, args| {
    charge_gas_with_args("&", &args, MilliGas(2))
      .bind(move |_| {
        if args.len() != 2 {
          return args_error(info, "&", &args);
        }
        
        match (args[0].as_pact_value(), args[1].as_pact_value()) {
            (Some(PactValue::Integer(a)), Some(PactValue::Integer(b))) => {
              let result = CEKValue::VPactValue(PactValue::Integer(a & b));
              return_cek_value(cont, handler, result)
            }
            _ => {
              // Use argsError for type mismatches - matches Haskell pattern exactly
              args_error(info, "&", &args)
            }
          }
      })
      .try_with(|error| {
        unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
      })
  })
}

/// Bitwise OR implementation
fn bitwise_or_implementation() -> NativeFunction {
  Box::new(|info, _builtin, cont, handler, _env, args| {
    charge_gas_with_args("|", &args, MilliGas(2))
      .bind(move |_| {
        if args.len() != 2 {
          return args_error(info, "|", &args);
        }
        
        match (args[0].as_pact_value(), args[1].as_pact_value()) {
            (Some(PactValue::Integer(a)), Some(PactValue::Integer(b))) => {
              let result = CEKValue::VPactValue(PactValue::Integer(a | b));
              return_cek_value(cont, handler, result)
            }
            _ => {
              // Use argsError for type mismatches - matches Haskell pattern exactly
              args_error(info, "|", &args)
            }
          }
      })
      .try_with(|error| {
        unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
      })
  })
}

/// Bitwise XOR implementation
fn bitwise_xor_implementation() -> NativeFunction {
  Box::new(|info, _builtin, cont, handler, _env, args| {
    charge_gas_with_args("xor", &args, MilliGas(2))
      .bind(move |_| {
        if args.len() != 2 {
          return args_error(info, "xor", &args);
        }
        
        match (args[0].as_pact_value(), args[1].as_pact_value()) {
            (Some(PactValue::Integer(a)), Some(PactValue::Integer(b))) => {
              let result = CEKValue::VPactValue(PactValue::Integer(a ^ b));
              return_cek_value(cont, handler, result)
            }
            _ => {
              // Use argsError for type mismatches - matches Haskell pattern exactly
              args_error(info, "xor", &args)
            }
          }
      })
      .try_with(|error| {
        unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
      })
  })
}

/// Bitwise complement (flip) implementation
fn bitwise_flip_implementation() -> NativeFunction {
  Box::new(|info, _builtin, cont, handler, _env, args| {
    charge_gas_with_args("~", &args, MilliGas(1))
      .bind(move |_| {
        if args.len() != 1 {
          return args_error(info, "~", &args);
        }
        
        match args[0].as_pact_value() {
            Some(PactValue::Integer(a)) => {
              let result = CEKValue::VPactValue(PactValue::Integer(!a));
              return_cek_value(cont, handler, result)
            }
            _ => {
              // Use argsError for type mismatches - matches Haskell pattern exactly
              args_error(info, "~", &args)
            }
          }
      })
      .try_with(|error| {
        unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
      })
  })
}

/// Bit shift implementation
fn bit_shift_implementation() -> NativeFunction {
  Box::new(|info, _builtin, cont, handler, _env, args| {
    charge_gas_with_args("shift", &args, MilliGas(3))
      .bind(move |_| {
        if args.len() != 2 {
          return args_error(info, "shift", &args);
        }
        
        match (args[0].as_pact_value(), args[1].as_pact_value()) {
            (Some(PactValue::Integer(value)), Some(PactValue::Integer(shift_amount))) => {
              if let Some(shift_usize) = shift_amount.to_usize() {
                let result = if shift_amount >= &BigInt::zero() {
                  // Left shift
                  value << shift_usize
                } else {
                  // Right shift
                  value >> shift_usize
                };
                let result = CEKValue::VPactValue(PactValue::Integer(result));
                return_cek_value(cont, handler, result)
              } else {
                throw_execution_error(info, EvalError::InvalidArgument("shift amount too large".to_string()))
              }
            }
            _ => {
              // Use argsError for type mismatches - matches Haskell pattern exactly
              args_error(info, "shift", &args)
            }
          }
      })
      .try_with(|error| {
        unwind_capability_stack(&error).bind(|_| EvalM::pure_value(EvalResult::EvalError(error)))
      })
  })
}

// Helper functions for transcendental operations

fn to_decimal(value: &PactValue) -> Option<Decimal> {
  match value {
    PactValue::Integer(i) => Some(Decimal::from(i.clone())),
    PactValue::Decimal(d) => Some(d.clone()),
    _ => None,
  }
}

fn decimal_pow(base: &Decimal, exp: &Decimal) -> Result<Decimal, String> {
  let base_f64 = base.to_f64();
  let exp_f64 = exp.to_f64();

  if base_f64.is_nan() || base_f64.is_infinite() {
    return Err("base too large for f64".to_string());
  }
  if exp_f64.is_nan() || exp_f64.is_infinite() {
    return Err("exponent too large for f64".to_string());
  }

  let result_f64 = base_f64.powf(exp_f64);

  if result_f64.is_nan() || result_f64.is_infinite() {
    Err("Floating operation resulted in Infinity or NaN".to_string())
  } else {
    Ok(Decimal::from_f64(result_f64))
  }
}

fn decimal_exp(x: &Decimal) -> Result<Decimal, String> {
  let x_f64 = x.to_f64();

  if x_f64.is_nan() || x_f64.is_infinite() {
    return Err("argument too large for f64".to_string());
  }

  let result_f64 = x_f64.exp();

  if result_f64.is_nan() || result_f64.is_infinite() {
    Err("Floating operation resulted in Infinity or NaN".to_string())
  } else {
    Ok(Decimal::from_f64(result_f64))
  }
}

fn decimal_ln(x: &Decimal) -> Result<Decimal, String> {
  let x_f64 = x.to_f64();

  if x_f64.is_nan() || x_f64.is_infinite() {
    return Err("argument too large for f64".to_string());
  }

  let result_f64 = x_f64.ln();

  if result_f64.is_nan() || result_f64.is_infinite() {
    Err("Floating operation resulted in Infinity or NaN".to_string())
  } else {
    Ok(Decimal::from_f64(result_f64))
  }
}

fn decimal_sqrt(x: &Decimal) -> Result<Decimal, String> {
  let x_f64 = x.to_f64();

  if x_f64.is_nan() || x_f64.is_infinite() {
    return Err("argument too large for f64".to_string());
  }

  let result_f64 = x_f64.sqrt();

  if result_f64.is_nan() || result_f64.is_infinite() {
    Err("Floating operation resulted in Infinity or NaN".to_string())
  } else {
    Ok(Decimal::from_f64(result_f64))
  }
}

fn decimal_log_base(base: &Decimal, arg: &Decimal) -> Result<Decimal, String> {
  let base_f64 = base.to_f64();
  let arg_f64 = arg.to_f64();

  if base_f64.is_nan() || base_f64.is_infinite() {
    return Err("base too large for f64".to_string());
  }
  if arg_f64.is_nan() || arg_f64.is_infinite() {
    return Err("argument too large for f64".to_string());
  }

  let result_f64 = arg_f64.log(base_f64);

  if result_f64.is_nan() || result_f64.is_infinite() {
    Err("Floating operation resulted in Infinity or NaN".to_string())
  } else {
    Ok(Decimal::from_f64(result_f64))
  }
}
