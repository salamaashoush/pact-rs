//! Literal evaluation for constant expressions
//!
//! This module handles evaluation of simple literal expressions that can be
//! safely computed at compile time without side effects.

use pact_ir::{Term, Literal, CoreBuiltin, Name, Type, SpanInfo};
use pact_core::values::{PactValue, Decimal};
use pact_core::errors::PactError;
use num_bigint::BigInt;
use num_rational::BigRational;

/// Evaluate a literal term to a PactValue
pub fn eval_literal(literal: &Literal) -> Result<PactValue, PactError<SpanInfo>> {
    match literal {
        Literal::LInteger(i) => Ok(PactValue::Integer(BigInt::from(*i))),
        
        Literal::LString(s) => Ok(PactValue::String(s.to_string())),
        
        Literal::LBool(b) => Ok(PactValue::Bool(*b)),
        
        Literal::LDecimal(d) => {
            // Convert decimal to BigRational
            // For now, use a simple conversion - this should be improved
            let rational = BigRational::new(BigInt::from(d.precision as i64), BigInt::from(1));
            Ok(PactValue::Decimal(Decimal::from_rational(rational)))
        }
        
        Literal::LUnit => Ok(PactValue::Unit),
    }
}

/// Check if a term is a simple literal that can be evaluated
pub fn is_evaluable_literal(term: &Term<Name, Type, CoreBuiltin, SpanInfo>) -> bool {
    matches!(term, Term::Constant(_, _))
}

/// Evaluate a simple constant term if possible
pub fn try_eval_constant_term(
    term: &Term<Name, Type, CoreBuiltin, SpanInfo>
) -> Result<Option<PactValue>, PactError<SpanInfo>> {
    match term {
        Term::Constant(literal, _) => {
            let pact_value = eval_literal(literal)?;
            Ok(Some(pact_value))
        }
        
        // For now, we only evaluate simple literals
        // Later we can add arithmetic operations, list literals, etc.
        _ => Ok(None),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval_integer_literal() {
        let literal = Literal::LInteger(42);
        let result = eval_literal(&literal).unwrap();
        
        match result {
            PactValue::Integer(i) => assert_eq!(i, BigInt::from(42)),
            _ => panic!("Expected integer value"),
        }
    }

    #[test]
    fn test_eval_string_literal() {
        let literal = Literal::LString(compact_str::CompactString::from("hello"));
        let result = eval_literal(&literal).unwrap();
        
        match result {
            PactValue::String(s) => assert_eq!(s, "hello"),
            _ => panic!("Expected string value"),
        }
    }

    #[test]
    fn test_eval_bool_literal() {
        let literal = Literal::LBool(true);
        let result = eval_literal(&literal).unwrap();
        
        match result {
            PactValue::Bool(b) => assert_eq!(b, true),
            _ => panic!("Expected bool value"),
        }
    }

    #[test]
    fn test_eval_unit_literal() {
        let literal = Literal::LUnit;
        let result = eval_literal(&literal).unwrap();
        
        match result {
            PactValue::Unit => {},
            _ => panic!("Expected unit value"),
        }
    }

    #[test]
    fn test_eval_decimal_literal() {
        let literal = Literal::LDecimal(pact_ir::Decimal { precision: 2, mantissa: 314159 });
        let result = eval_literal(&literal).unwrap();
        
        match result {
            PactValue::Decimal(_) => {},
            _ => panic!("Expected decimal value"),
        }
    }
}