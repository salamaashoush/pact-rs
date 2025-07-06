//! Pairing operations for ZK cryptography - matches Haskell Pact implementation
//!
//! This module implements BN254 curve operations and pairing functions
//! exactly as implemented in Haskell Pact for ZK applications.

use crate::{CryptoResult, ExtendedCryptoError};
use pact_core::shared::CryptoError as SharedCryptoError;
use ark_bn254::{Bn254, Fr, Fq, G1Affine, G1Projective, G2Affine, G2Projective};
use ark_ec::{AffineRepr, CurveGroup, pairing::Pairing, Group};
use ark_ff::{Field, PrimeField, UniformRand, BigInteger};
use num_bigint::BigInt;
use num_traits::{Zero, One};

/// BN254 scalar field element wrapper (Fr - used for scalars)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldElement {
    pub inner: Fr,
}

/// BN254 base field element wrapper (Fq - used for coordinates)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BaseFieldElement {
    pub inner: Fq,
}

impl FieldElement {
    /// Create from BigInt (matches Haskell Integer conversion)
    pub fn from_bigint(value: &BigInt) -> CryptoResult<Self> {
        // Convert BigInt to bytes and then to field element
        let bytes = value.to_bytes_be().1;
        let fr = Fr::from_be_bytes_mod_order(&bytes);
        Ok(FieldElement { inner: fr })
    }
    
    /// Convert to BigInt
    pub fn to_bigint(&self) -> BigInt {
        let bytes = self.inner.into_bigint().to_bytes_be();
        BigInt::from_bytes_be(num_bigint::Sign::Plus, &bytes)
    }
    
    /// Create from u64
    pub fn from_u64(value: u64) -> Self {
        FieldElement { inner: Fr::from(value) }
    }
    
    /// Zero element
    pub fn zero() -> Self {
        FieldElement { inner: Fr::zero() }
    }
    
    /// One element
    pub fn one() -> Self {
        FieldElement { inner: Fr::one() }
    }
    
    /// Random element
    pub fn random() -> Self {
        use rand::rngs::OsRng;
        let mut rng = OsRng;
        FieldElement { inner: Fr::rand(&mut rng) }
    }
    
    /// Field operations
    pub fn add(&self, other: &Self) -> Self {
        FieldElement { inner: self.inner + other.inner }
    }
    
    pub fn sub(&self, other: &Self) -> Self {
        FieldElement { inner: self.inner - other.inner }
    }
    
    pub fn mul(&self, other: &Self) -> Self {
        FieldElement { inner: self.inner * other.inner }
    }
    
    pub fn div(&self, other: &Self) -> CryptoResult<Self> {
        if other.inner.is_zero() {
            return Err(ExtendedCryptoError::InvalidFieldElement {
                message: "Division by zero".to_string(),
            }.into());
        }
        let inv = other.inner.inverse().ok_or_else(|| -> SharedCryptoError {
            ExtendedCryptoError::InvalidFieldElement {
                message: "Cannot compute inverse".to_string(),
            }.into()
        })?;
        Ok(FieldElement { inner: self.inner * inv })
    }
    
    pub fn pow(&self, exp: u64) -> Self {
        FieldElement { inner: self.inner.pow(&[exp]) }
    }
    
    pub fn inverse(&self) -> CryptoResult<Self> {
        let inv = self.inner.inverse().ok_or_else(|| -> SharedCryptoError {
            ExtendedCryptoError::InvalidFieldElement {
                message: "Cannot compute inverse of zero".to_string(),
            }.into()
        })?;
        Ok(FieldElement { inner: inv })
    }
}

impl BaseFieldElement {
    /// Create from BigInt
    pub fn from_bigint(value: &BigInt) -> CryptoResult<Self> {
        let bytes = value.to_bytes_be().1;
        let fq = Fq::from_be_bytes_mod_order(&bytes);
        Ok(BaseFieldElement { inner: fq })
    }
    
    /// Convert to BigInt
    pub fn to_bigint(&self) -> BigInt {
        let bytes = self.inner.into_bigint().to_bytes_be();
        BigInt::from_bytes_be(num_bigint::Sign::Plus, &bytes)
    }
}

/// G1 point wrapper (matches Haskell Point1)
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct G1Point {
    pub inner: G1Affine,
}

impl G1Point {
    /// Create from coordinates - matches `point1 :: Integer -> Integer -> Point1`
    pub fn from_coords(x: &BigInt, y: &BigInt) -> CryptoResult<Self> {
        let x_fe = BaseFieldElement::from_bigint(x)?;
        let y_fe = BaseFieldElement::from_bigint(y)?;
        
        let point = G1Affine::new_unchecked(x_fe.inner, y_fe.inner);
        
        if !point.is_on_curve() {
            return Err(ExtendedCryptoError::InvalidCurvePoint {
                message: "Point is not on G1 curve".to_string(),
            }.into());
        }
        
        Ok(G1Point { inner: point })
    }
    
    /// Get x coordinate
    pub fn x(&self) -> BaseFieldElement {
        BaseFieldElement { inner: self.inner.x }
    }
    
    /// Get y coordinate
    pub fn y(&self) -> BaseFieldElement {
        BaseFieldElement { inner: self.inner.y }
    }
    
    /// Generator point
    pub fn generator() -> Self {
        G1Point { inner: G1Affine::generator() }
    }
    
    /// Identity/zero point
    pub fn identity() -> Self {
        G1Point { inner: G1Affine::identity() }
    }
    
    /// Point addition - matches `add1 :: Point1 -> Point1 -> Point1`
    pub fn add(&self, other: &Self) -> Self {
        let proj1: G1Projective = self.inner.into();
        let proj2: G1Projective = other.inner.into();
        let result = proj1 + proj2;
        G1Point { inner: result.into_affine() }
    }
    
    /// Point doubling
    pub fn double(&self) -> Self {
        let proj: G1Projective = self.inner.into();
        let result = proj.double();
        G1Point { inner: result.into_affine() }
    }
    
    /// Scalar multiplication - matches `mul1 :: Integer -> Point1 -> Point1`
    pub fn scalar_mul(&self, scalar: &BigInt) -> CryptoResult<Self> {
        let scalar_fe = FieldElement::from_bigint(scalar)?;
        let proj: G1Projective = self.inner.into();
        let result = proj * scalar_fe.inner;
        Ok(G1Point { inner: result.into_affine() })
    }
    
    /// Check if point is on curve
    pub fn is_on_curve(&self) -> bool {
        self.inner.is_on_curve()
    }
    
    /// Check if point is identity
    pub fn is_identity(&self) -> bool {
        self.inner.is_zero()
    }
}

/// G2 point wrapper (matches Haskell Point2)
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct G2Point {
    pub inner: G2Affine,
}

impl G2Point {
    /// Create from coordinates - matches `point2 :: (Integer, Integer) -> (Integer, Integer) -> Point2`
    pub fn from_coords(x0: &BigInt, x1: &BigInt, y0: &BigInt, y1: &BigInt) -> CryptoResult<Self> {
        use ark_bn254::{Fq2, Fq};
        
        // Convert BigInt to base field elements (Fq, not Fr)
        let x0_bytes = x0.to_bytes_be().1;
        let x1_bytes = x1.to_bytes_be().1;
        let y0_bytes = y0.to_bytes_be().1;
        let y1_bytes = y1.to_bytes_be().1;
        
        let x0_fq = Fq::from_be_bytes_mod_order(&x0_bytes);
        let x1_fq = Fq::from_be_bytes_mod_order(&x1_bytes);
        let y0_fq = Fq::from_be_bytes_mod_order(&y0_bytes);
        let y1_fq = Fq::from_be_bytes_mod_order(&y1_bytes);
        
        // G2 coordinates are Fq2 elements (complex field extension over Fq)
        let x = Fq2::new(x0_fq, x1_fq);
        let y = Fq2::new(y0_fq, y1_fq);
        
        let point = G2Affine::new_unchecked(x, y);
        
        if !point.is_on_curve() {
            return Err(ExtendedCryptoError::InvalidCurvePoint {
                message: "Point is not on G2 curve".to_string(),
            }.into());
        }
        
        Ok(G2Point { inner: point })
    }
    
    /// Generator point
    pub fn generator() -> Self {
        G2Point { inner: G2Affine::generator() }
    }
    
    /// Identity/zero point
    pub fn identity() -> Self {
        G2Point { inner: G2Affine::identity() }
    }
    
    /// Point addition - matches `add2 :: Point2 -> Point2 -> Point2`
    pub fn add(&self, other: &Self) -> Self {
        let proj1: G2Projective = self.inner.into();
        let proj2: G2Projective = other.inner.into();
        let result = proj1 + proj2;
        G2Point { inner: result.into_affine() }
    }
    
    /// Point doubling
    pub fn double(&self) -> Self {
        let proj: G2Projective = self.inner.into();
        let result = proj.double();
        G2Point { inner: result.into_affine() }
    }
    
    /// Scalar multiplication - matches `mul2 :: Integer -> Point2 -> Point2`
    pub fn scalar_mul(&self, scalar: &BigInt) -> CryptoResult<Self> {
        let scalar_fe = FieldElement::from_bigint(scalar)?;
        let proj: G2Projective = self.inner.into();
        let result = proj * scalar_fe.inner;
        Ok(G2Point { inner: result.into_affine() })
    }
    
    /// Check if point is on curve
    pub fn is_on_curve(&self) -> bool {
        self.inner.is_on_curve()
    }
    
    /// Check if point is identity
    pub fn is_identity(&self) -> bool {
        self.inner.is_zero()
    }
}

/// Pairing result wrapper (GT element)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PairingResult {
    pub inner: ark_bn254::Fq12,
}

impl PairingResult {
    /// Identity element
    pub fn identity() -> Self {
        PairingResult { inner: ark_bn254::Fq12::one() }
    }
    
    /// Multiply pairing results
    pub fn mul(&self, other: &Self) -> Self {
        PairingResult { inner: self.inner * other.inner }
    }
    
    /// Check if result is identity (used for pairing checks)
    pub fn is_identity(&self) -> bool {
        self.inner.is_one()
    }
}

/// Pairing operation - matches `pairing :: Point1 -> Point2 -> Integer`
pub fn pairing(p1: &G1Point, p2: &G2Point) -> CryptoResult<PairingResult> {
    let result = Bn254::pairing(p1.inner, p2.inner);
    Ok(PairingResult { inner: result.0 })
}

/// Batch pairing - matches `pairingBatch :: [(Point1, Point2)] -> Integer`
pub fn pairing_batch(pairs: &[(G1Point, G2Point)]) -> CryptoResult<PairingResult> {
    if pairs.is_empty() {
        return Ok(PairingResult::identity());
    }
    
    let g1_points: Vec<G1Affine> = pairs.iter().map(|(p1, _)| p1.inner).collect();
    let g2_points: Vec<G2Affine> = pairs.iter().map(|(_, p2)| p2.inner).collect();
    
    let result = Bn254::multi_pairing(g1_points, g2_points);
    Ok(PairingResult { inner: result.0 })
}

/// Check pairing equation: e(p1, p2) = e(q1, q2)
/// Matches `pairingCheck :: Point1 -> Point2 -> Point1 -> Point2 -> Bool`
pub fn pairing_check(p1: &G1Point, p2: &G2Point, q1: &G1Point, q2: &G2Point) -> CryptoResult<bool> {
    let pairs = vec![
        (*p1, *p2),
        (*q1, G2Point { inner: -q2.inner }), // Negate q2 for pairing check
    ];
    
    let result = pairing_batch(&pairs)?;
    Ok(result.is_identity())
}

/// Validate G1 point from integers - matches validation in Haskell
pub fn validate_g1_point(x: &BigInt, y: &BigInt) -> CryptoResult<()> {
    G1Point::from_coords(x, y)?;
    Ok(())
}

/// Validate G2 point from integers - matches validation in Haskell
pub fn validate_g2_point(x0: &BigInt, x1: &BigInt, y0: &BigInt, y1: &BigInt) -> CryptoResult<()> {
    G2Point::from_coords(x0, x1, y0, y1)?;
    Ok(())
}

/// BN254 curve order
pub fn curve_order() -> BigInt {
    // BN254 curve order: 21888242871839275222246405745257275088548364400416034343698204186575808495617
    "21888242871839275222246405745257275088548364400416034343698204186575808495617".parse().unwrap()
}

/// BN254 field modulus
pub fn field_modulus() -> BigInt {
    // BN254 base field modulus: 21888242871839275222246405745257275088696311157297823662689037894645226208583
    "21888242871839275222246405745257275088696311157297823662689037894645226208583".parse().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_field_element_operations() {
        let a = FieldElement::from_u64(5);
        let b = FieldElement::from_u64(3);
        
        // Addition
        let sum = a.add(&b);
        assert_eq!(sum.to_bigint(), BigInt::from(8));
        
        // Multiplication
        let product = a.mul(&b);
        assert_eq!(product.to_bigint(), BigInt::from(15));
        
        // Division
        let quotient = a.div(&b).unwrap();
        let reconstructed = quotient.mul(&b);
        assert_eq!(reconstructed.to_bigint(), a.to_bigint());
    }
    
    #[test]
    fn test_g1_point_operations() {
        let g = G1Point::generator();
        let double_g = g.double();
        let g_plus_g = g.add(&g);
        
        // Doubling should equal addition with self
        assert_eq!(double_g, g_plus_g);
        
        // Scalar multiplication by 2
        let two_g = g.scalar_mul(&BigInt::from(2)).unwrap();
        assert_eq!(two_g, double_g);
        
        // Identity operations
        let identity = G1Point::identity();
        let g_plus_identity = g.add(&identity);
        assert_eq!(g_plus_identity, g);
    }
    
    #[test]
    fn test_g2_point_operations() {
        let g = G2Point::generator();
        let double_g = g.double();
        let g_plus_g = g.add(&g);
        
        // Doubling should equal addition with self
        assert_eq!(double_g, g_plus_g);
        
        // Scalar multiplication by 2
        let two_g = g.scalar_mul(&BigInt::from(2)).unwrap();
        assert_eq!(two_g, double_g);
        
        // Identity operations
        let identity = G2Point::identity();
        let g_plus_identity = g.add(&identity);
        assert_eq!(g_plus_identity, g);
    }
    
    #[test]
    fn test_pairing_bilinearity() {
        let g1 = G1Point::generator();
        let g2 = G2Point::generator();
        
        let a = BigInt::from(5);
        let b = BigInt::from(7);
        
        // Scalar multiplication on G1
        let ag1 = g1.scalar_mul(&a).unwrap();
        let bg2 = g2.scalar_mul(&b).unwrap();
        
        // e(a*G1, b*G2) should equal e(G1, ab*G2)
        let pairing1 = pairing(&ag1, &bg2).unwrap();
        
        let ab = &a * &b;
        let abg2 = g2.scalar_mul(&ab).unwrap();
        let pairing2 = pairing(&g1, &abg2).unwrap();
        
        assert_eq!(pairing1, pairing2);
    }
    
    #[test]
    fn test_pairing_check() {
        let g1 = G1Point::generator();
        let g2 = G2Point::generator();
        
        // e(G1, G2) = e(G1, G2) should be true
        assert!(pairing_check(&g1, &g2, &g1, &g2).unwrap());
        
        // e(2*G1, G2) = e(G1, 2*G2) should be true
        let two_g1 = g1.scalar_mul(&BigInt::from(2)).unwrap();
        let two_g2 = g2.scalar_mul(&BigInt::from(2)).unwrap();
        assert!(pairing_check(&two_g1, &g2, &g1, &two_g2).unwrap());
    }
    
    #[test]
    fn test_batch_pairing() {
        let g1 = G1Point::generator();
        let g2 = G2Point::generator();
        
        // Single pairing
        let single = pairing(&g1, &g2).unwrap();
        
        // Batch pairing with one pair
        let batch = pairing_batch(&[(g1, g2)]).unwrap();
        
        assert_eq!(single, batch);
        
        // Empty batch should return identity
        let empty = pairing_batch(&[]).unwrap();
        assert!(empty.is_identity());
    }
    
    #[test]
    fn test_point_validation() {
        // Generator points should be valid
        let g1 = G1Point::generator();
        let g2 = G2Point::generator();
        
        assert!(g1.is_on_curve());
        assert!(g2.is_on_curve());
        
        // Identity points should be valid
        let id1 = G1Point::identity();
        let id2 = G2Point::identity();
        
        assert!(id1.is_on_curve());
        assert!(id2.is_on_curve());
        assert!(id1.is_identity());
        assert!(id2.is_identity());
    }
    
    #[test]
    fn test_bigint_conversion() {
        let value = BigInt::from(12345);
        let fe = FieldElement::from_bigint(&value).unwrap();
        let converted_back = fe.to_bigint();
        
        assert_eq!(value, converted_back);
    }
    
    #[test]
    fn test_curve_parameters() {
        let order = curve_order();
        let modulus = field_modulus();
        
        // Both should be large primes
        assert!(order > BigInt::zero());
        assert!(modulus > BigInt::zero());
        
        // Order should be smaller than modulus for BN254
        assert!(order < modulus);
    }
}