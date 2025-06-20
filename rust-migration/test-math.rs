#!/usr/bin/env rust-script
use bigdecimal::BigDecimal;
use std::str::FromStr;

fn main() {
    let a = BigDecimal::from_str("123.456").unwrap();
    let b = BigDecimal::from_str("78.9").unwrap();
    let prod = &a * &b;
    println!("Product: {}", prod);
}
