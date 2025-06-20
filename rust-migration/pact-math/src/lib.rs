//! Mathematical operations for Pact
//!
//! This module provides arbitrary precision arithmetic operations
//! for Pact using pure Rust implementations.

use bigdecimal::{BigDecimal, FromPrimitive, One, Signed, ToPrimitive, Zero};
use num_bigint::{BigInt, Sign};
use num_rational::BigRational;
use num_traits::pow::Pow;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::str::FromStr;

/// Error type for mathematical operations
#[derive(Debug, Clone, thiserror::Error, PartialEq)]
pub enum MathError {
    #[error("Division by zero")]
    DivisionByZero,

    #[error("Invalid number format: {0}")]
    InvalidFormat(String),

    #[error("Overflow in operation: {0}")]
    Overflow(String),

    #[error("Invalid operation: {0}")]
    InvalidOperation(String),

    #[error("Precision loss: {0}")]
    PrecisionLoss(String),
}

/// Result type for math operations
pub type MathResult<T> = Result<T, MathError>;

/// Pact decimal type using BigDecimal for arbitrary precision
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PactDecimal(BigDecimal);

impl PactDecimal {
    /// Create a new decimal from a string
    pub fn from_str(s: &str) -> MathResult<Self> {
        BigDecimal::from_str(s)
            .map(PactDecimal)
            .map_err(|_| MathError::InvalidFormat(s.to_string()))
    }

    /// Create from an integer
    pub fn from_int(n: i64) -> Self {
        PactDecimal(BigDecimal::from(n))
    }

    /// Create from a float (with potential precision loss)
    pub fn from_f64(f: f64) -> MathResult<Self> {
        if f.is_finite() {
            Ok(PactDecimal(BigDecimal::from_f64(f).unwrap()))
        } else {
            Err(MathError::InvalidFormat("Non-finite float".to_string()))
        }
    }

    /// Get the underlying BigDecimal
    pub fn as_bigdecimal(&self) -> &BigDecimal {
        &self.0
    }

    /// Convert to string representation
    pub fn to_string(&self) -> String {
        self.0.to_string()
    }

    /// Addition
    pub fn add(&self, other: &PactDecimal) -> PactDecimal {
        PactDecimal(&self.0 + &other.0)
    }

    /// Subtraction
    pub fn sub(&self, other: &PactDecimal) -> PactDecimal {
        PactDecimal(&self.0 - &other.0)
    }

    /// Multiplication
    pub fn mul(&self, other: &PactDecimal) -> PactDecimal {
        PactDecimal(&self.0 * &other.0)
    }

    /// Division
    pub fn div(&self, other: &PactDecimal) -> MathResult<PactDecimal> {
        if other.0.is_zero() {
            Err(MathError::DivisionByZero)
        } else {
            Ok(PactDecimal(&self.0 / &other.0))
        }
    }

    /// Modulo operation
    pub fn modulo(&self, other: &PactDecimal) -> MathResult<PactDecimal> {
        if other.0.is_zero() {
            Err(MathError::DivisionByZero)
        } else {
            Ok(PactDecimal(&self.0 % &other.0))
        }
    }

    /// Power operation
    pub fn pow(&self, exp: i32) -> MathResult<PactDecimal> {
        if exp < 0 && self.0.is_zero() {
            Err(MathError::DivisionByZero)
        } else {
            // BigDecimal doesn't have built-in pow, so we implement it
            let result = if exp >= 0 {
                let mut result = BigDecimal::one();
                for _ in 0..exp {
                    result = result * &self.0;
                }
                result
            } else {
                let mut result = BigDecimal::one();
                for _ in 0..(-exp) {
                    result = result / &self.0;
                }
                result
            };
            Ok(PactDecimal(result))
        }
    }

    /// Absolute value
    pub fn abs(&self) -> PactDecimal {
        PactDecimal(self.0.abs())
    }

    /// Negate
    pub fn negate(&self) -> PactDecimal {
        PactDecimal(-&self.0)
    }

    /// Floor operation
    pub fn floor(&self) -> PactDecimal {
        PactDecimal(self.0.with_scale(0))
    }

    /// Ceiling operation
    pub fn ceil(&self) -> PactDecimal {
        let floored = self.0.with_scale(0);
        if self.0 > floored && self.0.is_positive() {
            PactDecimal(floored + BigDecimal::one())
        } else if self.0 < floored && self.0.is_negative() {
            PactDecimal(floored)
        } else {
            PactDecimal(floored)
        }
    }

    /// Round to nearest integer
    pub fn round(&self) -> PactDecimal {
        PactDecimal(self.0.round(0))
    }

    /// Check if zero
    pub fn is_zero(&self) -> bool {
        self.0.is_zero()
    }

    /// Check if positive
    pub fn is_positive(&self) -> bool {
        self.0.is_positive()
    }

    /// Check if negative
    pub fn is_negative(&self) -> bool {
        self.0.is_negative()
    }

    /// Square root (approximation)
    pub fn sqrt(&self) -> MathResult<PactDecimal> {
        if self.0.is_negative() {
            Err(MathError::InvalidOperation(
                "Square root of negative number".to_string(),
            ))
        } else {
            // Convert to f64, compute sqrt, convert back
            // This loses precision but is acceptable for most use cases
            match self.0.to_f64() {
                Some(f) => PactDecimal::from_f64(f.sqrt()),
                None => Err(MathError::Overflow("Number too large for sqrt".to_string())),
            }
        }
    }

    /// Natural logarithm (approximation)
    pub fn ln(&self) -> MathResult<PactDecimal> {
        if self.0.is_positive() {
            match self.0.to_f64() {
                Some(f) => PactDecimal::from_f64(f.ln()),
                None => Err(MathError::Overflow("Number too large for ln".to_string())),
            }
        } else {
            Err(MathError::InvalidOperation(
                "Logarithm of non-positive number".to_string(),
            ))
        }
    }

    /// Exponential function (approximation)
    pub fn exp(&self) -> MathResult<PactDecimal> {
        match self.0.to_f64() {
            Some(f) => {
                let result = f.exp();
                if result.is_finite() {
                    PactDecimal::from_f64(result)
                } else {
                    Err(MathError::Overflow("Exponential overflow".to_string()))
                }
            }
            None => Err(MathError::Overflow("Number too large for exp".to_string())),
        }
    }
}

impl fmt::Display for PactDecimal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for PactDecimal {
    type Err = MathError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        PactDecimal::from_str(s)
    }
}

/// Pact integer type using BigInt for arbitrary precision
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PactInteger(BigInt);

impl PactInteger {
    /// Create from string
    pub fn from_str(s: &str) -> MathResult<Self> {
        BigInt::from_str(s)
            .map(PactInteger)
            .map_err(|_| MathError::InvalidFormat(s.to_string()))
    }

    /// Create from i64
    pub fn from_i64(n: i64) -> Self {
        PactInteger(BigInt::from(n))
    }

    /// Convert to i64 if possible
    pub fn to_i64(&self) -> Option<i64> {
        self.0.to_i64()
    }

    /// Get the underlying BigInt
    pub fn as_bigint(&self) -> &BigInt {
        &self.0
    }

    /// Addition
    pub fn add(&self, other: &PactInteger) -> PactInteger {
        PactInteger(&self.0 + &other.0)
    }

    /// Subtraction
    pub fn sub(&self, other: &PactInteger) -> PactInteger {
        PactInteger(&self.0 - &other.0)
    }

    /// Multiplication
    pub fn mul(&self, other: &PactInteger) -> PactInteger {
        PactInteger(&self.0 * &other.0)
    }

    /// Division (truncating)
    pub fn div(&self, other: &PactInteger) -> MathResult<PactInteger> {
        if other.0.is_zero() {
            Err(MathError::DivisionByZero)
        } else {
            Ok(PactInteger(&self.0 / &other.0))
        }
    }

    /// Modulo
    pub fn modulo(&self, other: &PactInteger) -> MathResult<PactInteger> {
        if other.0.is_zero() {
            Err(MathError::DivisionByZero)
        } else {
            Ok(PactInteger(&self.0 % &other.0))
        }
    }

    /// Power
    pub fn pow(&self, exp: u32) -> PactInteger {
        PactInteger(self.0.clone().pow(exp))
    }

    /// Absolute value
    pub fn abs(&self) -> PactInteger {
        PactInteger(self.0.abs())
    }

    /// Negate
    pub fn negate(&self) -> PactInteger {
        PactInteger(-&self.0)
    }

    /// Check if zero
    pub fn is_zero(&self) -> bool {
        self.0.is_zero()
    }

    /// Check if positive
    pub fn is_positive(&self) -> bool {
        self.0.sign() == Sign::Plus && !self.0.is_zero()
    }

    /// Check if negative
    pub fn is_negative(&self) -> bool {
        self.0.sign() == Sign::Minus
    }

    /// Convert to PactDecimal
    pub fn to_decimal(&self) -> PactDecimal {
        PactDecimal(BigDecimal::from(self.0.clone()))
    }
}

impl fmt::Display for PactInteger {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for PactInteger {
    type Err = MathError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        PactInteger::from_str(s)
    }
}

/// Pact rational type for exact fractions
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PactRational(BigRational);

impl PactRational {
    /// Create from numerator and denominator
    pub fn new(num: PactInteger, den: PactInteger) -> MathResult<Self> {
        if den.is_zero() {
            Err(MathError::DivisionByZero)
        } else {
            Ok(PactRational(BigRational::new(num.0, den.0)))
        }
    }

    /// Create from integer
    pub fn from_integer(n: PactInteger) -> Self {
        PactRational(BigRational::from_integer(n.0))
    }

    /// Convert to decimal (may lose precision)
    pub fn to_decimal(&self) -> PactDecimal {
        let num = self.0.numer();
        let den = self.0.denom();
        let num_dec = BigDecimal::from(num.clone());
        let den_dec = BigDecimal::from(den.clone());
        PactDecimal(num_dec / den_dec)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decimal_arithmetic() {
        let a = PactDecimal::from_str("123.456").unwrap();
        let b = PactDecimal::from_str("78.9").unwrap();

        let sum = a.add(&b);
        assert_eq!(sum.to_string(), "202.356");

        let diff = a.sub(&b);
        assert_eq!(diff.to_string(), "44.556");

        let prod = a.mul(&b);
        assert_eq!(prod.to_string(), "9740.6784");

        let quot = a.div(&b).unwrap();
        // Note: actual precision may vary
        assert!(quot.to_string().starts_with("1.56"));
    }

    #[test]
    fn test_integer_arithmetic() {
        let a = PactInteger::from_str("123456789012345678901234567890").unwrap();
        let b = PactInteger::from_i64(12345);

        let sum = a.add(&b);
        let diff = a.sub(&b);
        let prod = a.mul(&b);
        let quot = a.div(&b).unwrap();

        // Verify operations complete without panic
        assert!(!sum.is_zero());
        assert!(!diff.is_zero());
        assert!(!prod.is_zero());
        assert!(!quot.is_zero());
    }

    #[test]
    fn test_division_by_zero() {
        let a = PactDecimal::from_int(10);
        let zero = PactDecimal::from_int(0);

        assert!(matches!(a.div(&zero), Err(MathError::DivisionByZero)));
    }

    #[test]
    fn test_transcendental_functions() {
        let x = PactDecimal::from_str("2.0").unwrap();

        let sqrt_x = x.sqrt().unwrap();
        assert!(sqrt_x.to_string().starts_with("1.41"));

        let ln_x = x.ln().unwrap();
        assert!(ln_x.to_string().starts_with("0.69"));

        let exp_x = x.exp().unwrap();
        assert!(exp_x.to_string().starts_with("7.38"));
    }
}
