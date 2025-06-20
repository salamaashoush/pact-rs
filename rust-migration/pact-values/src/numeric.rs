//! Numeric types and operations for Pact
//!
//! This module provides numeric types and operations matching Haskell's implementation.

use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::{Zero, One, Signed, ToPrimitive, FromPrimitive};
use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::{Add, Sub, Mul, Div, Neg};
use std::cmp::Ordering;
use std::str::FromStr;

/// Integer type (arbitrary precision)
pub type Integer = BigInt;

/// Decimal type (arbitrary precision rational)
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Decimal(BigRational);

impl Decimal {
    /// Create a new decimal from a rational
    pub fn new(num: BigInt, den: BigInt) -> Self {
        Decimal(BigRational::new(num, den))
    }
    
    /// Create from an integer
    pub fn from_integer(n: Integer) -> Self {
        Decimal(BigRational::from_integer(n))
    }
    
    /// Create from a rational
    pub fn from_rational(r: BigRational) -> Self {
        Decimal(r)
    }
    
    /// Create from f64
    pub fn from_f64(f: f64) -> Self {
        // Convert f64 to rational representation
        // This is a simplified implementation - in production we'd want
        // to handle special cases more carefully
        if let Some(r) = BigRational::from_f64(f) {
            Decimal(r)
        } else {
            Decimal(BigRational::zero())
        }
    }
    
    /// Convert to f64
    pub fn to_f64(&self) -> f64 {
        self.0.to_f64().unwrap_or(0.0)
    }
    
    /// Get the absolute value
    pub fn abs(&self) -> Self {
        Decimal(self.0.abs())
    }
    
    /// Power function
    pub fn powf(&self, exp: f64) -> Self {
        let base = self.to_f64();
        Decimal::from_f64(base.powf(exp))
    }
    
    /// Square root
    pub fn sqrt(&self) -> Self {
        let val = self.to_f64();
        Decimal::from_f64(val.sqrt())
    }
    
    /// Check if zero
    pub fn is_zero(&self) -> bool {
        self.0.is_zero()
    }
    
    /// Round to nearest integer
    pub fn round_to_integer(&self) -> Integer {
        self.0.round().to_integer()
    }
    
    /// Ceiling to integer
    pub fn ceil_to_integer(&self) -> Integer {
        self.0.ceil().to_integer()
    }
    
    /// Floor to integer
    pub fn floor_to_integer(&self) -> Integer {
        self.0.floor().to_integer()
    }
    
    /// Round to specified precision
    pub fn round_to_precision(&self, precision: u32) -> Self {
        // Scale up, round, then scale down
        let scale = BigInt::from(10).pow(precision);
        let scaled = &self.0 * &BigRational::from_integer(scale.clone());
        let rounded = scaled.round();
        Decimal(rounded / BigRational::from_integer(scale))
    }
    
    /// Ceiling to specified precision
    pub fn ceil_to_precision(&self, precision: u32) -> Self {
        let scale = BigInt::from(10).pow(precision);
        let scaled = &self.0 * &BigRational::from_integer(scale.clone());
        let ceiled = scaled.ceil();
        Decimal(ceiled / BigRational::from_integer(scale))
    }
    
    /// Floor to specified precision
    pub fn floor_to_precision(&self, precision: u32) -> Self {
        let scale = BigInt::from(10).pow(precision);
        let scaled = &self.0 * &BigRational::from_integer(scale.clone());
        let floored = scaled.floor();
        Decimal(floored / BigRational::from_integer(scale))
    }
}

// Implement Display for Decimal
impl fmt::Display for Decimal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Format as decimal string
        write!(f, "{}", self.to_f64())
    }
}

// Implement From<Integer> for Decimal
impl From<Integer> for Decimal {
    fn from(n: Integer) -> Self {
        Decimal::from_integer(n)
    }
}

// Implement From<i32> for Decimal
impl From<i32> for Decimal {
    fn from(n: i32) -> Self {
        Decimal::from_integer(BigInt::from(n))
    }
}

// Implement FromStr for Decimal
impl FromStr for Decimal {
    type Err = String;
    
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Try to parse as f64 first
        if let Ok(f) = s.parse::<f64>() {
            Ok(Decimal::from_f64(f))
        } else {
            // Try to parse as rational (e.g., "1/3")
            if let Ok(r) = BigRational::from_str(s) {
                Ok(Decimal(r))
            } else {
                Err(format!("Invalid decimal: {}", s))
            }
        }
    }
}

// Arithmetic operations for Decimal

impl Add for Decimal {
    type Output = Self;
    
    fn add(self, other: Self) -> Self {
        Decimal(self.0 + other.0)
    }
}

impl Add for &Decimal {
    type Output = Decimal;
    
    fn add(self, other: Self) -> Decimal {
        Decimal(&self.0 + &other.0)
    }
}

impl Sub for Decimal {
    type Output = Self;
    
    fn sub(self, other: Self) -> Self {
        Decimal(self.0 - other.0)
    }
}

impl Sub for &Decimal {
    type Output = Decimal;
    
    fn sub(self, other: Self) -> Decimal {
        Decimal(&self.0 - &other.0)
    }
}

impl Mul for Decimal {
    type Output = Self;
    
    fn mul(self, other: Self) -> Self {
        Decimal(self.0 * other.0)
    }
}

impl Mul for &Decimal {
    type Output = Decimal;
    
    fn mul(self, other: Self) -> Decimal {
        Decimal(&self.0 * &other.0)
    }
}

impl Div for Decimal {
    type Output = Self;
    
    fn div(self, other: Self) -> Self {
        Decimal(self.0 / other.0)
    }
}

impl Div for &Decimal {
    type Output = Decimal;
    
    fn div(self, other: Self) -> Decimal {
        Decimal(&self.0 / &other.0)
    }
}

impl Neg for Decimal {
    type Output = Self;
    
    fn neg(self) -> Self {
        Decimal(-self.0)
    }
}

impl Neg for &Decimal {
    type Output = Decimal;
    
    fn neg(self) -> Decimal {
        Decimal(-&self.0)
    }
}

// Comparison traits
impl Zero for Decimal {
    fn zero() -> Self {
        Decimal(BigRational::zero())
    }
    
    fn is_zero(&self) -> bool {
        self.0.is_zero()
    }
}

impl One for Decimal {
    fn one() -> Self {
        Decimal(BigRational::one())
    }
}

impl PartialEq<Integer> for Decimal {
    fn eq(&self, other: &Integer) -> bool {
        self == &Decimal::from_integer(other.clone())
    }
}

impl PartialOrd<Integer> for Decimal {
    fn partial_cmp(&self, other: &Integer) -> Option<Ordering> {
        self.partial_cmp(&Decimal::from_integer(other.clone()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_decimal_operations() {
        let a = Decimal::from(10);
        let b = Decimal::from(3);
        
        let sum = &a + &b;
        assert_eq!(sum, Decimal::from(13));
        
        let diff = &a - &b;
        assert_eq!(diff, Decimal::from(7));
        
        let prod = &a * &b;
        assert_eq!(prod, Decimal::from(30));
    }
    
    #[test]
    fn test_decimal_rounding() {
        let d = Decimal::from_f64(2.555);
        
        assert_eq!(d.round_to_precision(2).to_f64(), 2.56);
        assert_eq!(d.floor_to_precision(2).to_f64(), 2.55);
        assert_eq!(d.ceil_to_precision(2).to_f64(), 2.56);
    }
}