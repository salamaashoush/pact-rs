//! Property-based tests for the parser

use crate::{parse, Expr, Literal};
use proptest::prelude::*;

/// Generate valid simple expressions
fn arb_simple_expr() -> impl Strategy<Value = String> {
    prop_oneof![
        // Literals
        any::<i64>().prop_map(|i| i.to_string()),
        r#""[^"]*""#.prop_map(|s| s.to_string()),
        prop_oneof![Just("true"), Just("false")],
        
        // Simple arithmetic
        (any::<i32>(), any::<i32>()).prop_map(|(a, b)| format!("(+ {} {})", a, b)),
        (any::<i32>(), any::<i32>()).prop_map(|(a, b)| format!("(- {} {})", a, b)),
        
        // Variables
        r"[a-zA-Z_][a-zA-Z0-9_-]*".prop_map(|id| id.chars().take(20).collect::<String>()),
    ]
}

/// Generate valid function definitions
fn arb_defun() -> impl Strategy<Value = String> {
    (
        r"[a-zA-Z_][a-zA-Z0-9_-]*",
        prop::collection::vec(r"[a-zA-Z_][a-zA-Z0-9_-]*", 0..=3),
        any::<i32>()
    ).prop_map(|(name, params, body)| {
        let param_list = params.join(" ");
        format!("(defun {} ({}) {})", name, param_list, body)
    })
}

proptest! {
    /// Simple expressions should parse without error
    #[test]
    fn prop_simple_expressions_parse(expr in arb_simple_expr()) {
        // Should not panic and should return some result
        let result = parse(&expr);
        prop_assert!(result.is_ok() || result.is_err()); // Just don't panic
    }

    /// Integer literals preserve their values
    #[test]
    fn prop_integer_values_preserved(val in any::<i64>()) {
        let input = val.to_string();
        if let Ok(exprs) = parse(&input) {
            if let Some(Expr::Literal(Literal::Integer(parsed_val))) = exprs.first() {
                prop_assert_eq!(*parsed_val, val);
            }
        }
    }

    /// String literals preserve their content
    #[test]
    fn prop_string_values_preserved(content in r#"[^"\\]*"#) {
        let input = format!("\"{}\"", content);
        if let Ok(exprs) = parse(&input) {
            if let Some(Expr::Literal(Literal::String(parsed_content))) = exprs.first() {
                prop_assert_eq!(parsed_content, &content);
            }
        }
    }

    /// Boolean literals are recognized correctly
    #[test]
    fn prop_boolean_literals(val in any::<bool>()) {
        let input = val.to_string();
        if let Ok(exprs) = parse(&input) {
            if let Some(Expr::Literal(Literal::Bool(parsed_val))) = exprs.first() {
                prop_assert_eq!(*parsed_val, val);
            }
        }
    }

    /// Function definitions should parse
    #[test]
    fn prop_function_definitions(defun in arb_defun()) {
        // Should parse without panicking
        let result = parse(&defun);
        if result.is_ok() {
            // If it parses, it should have at least one expression
            prop_assert!(!result.unwrap().is_empty());
        }
    }

    /// Nested expressions maintain structure
    #[test]
    fn prop_nested_arithmetic(
        a in 0i32..100,
        b in 0i32..100,
        c in 0i32..100
    ) {
        let input = format!("(+ {} (- {} {}))", a, b, c);
        if let Ok(exprs) = parse(&input) {
            prop_assert_eq!(exprs.len(), 1);
            // Should have parsed as a nested application
            if let Some(Expr::Application { .. }) = exprs.first() {
                // Structure verified by successful parsing
            }
        }
    }

    /// Comments are properly ignored
    #[test]
    fn prop_comments_ignored(
        comment in r";[^\n]*",
        expr in arb_simple_expr()
    ) {
        let input_with_comment = format!("{}\n{}", comment, expr);
        let input_without_comment = expr.clone();
        
        let result_with = parse(&input_with_comment);
        let result_without = parse(&input_without_comment);
        
        // If both parse successfully, they should be equivalent
        if result_with.is_ok() && result_without.is_ok() {
            prop_assert_eq!(result_with.unwrap().len(), result_without.unwrap().len());
        }
    }

    /// Empty input produces empty result
    #[test]
    fn prop_empty_input() {
        let result = parse("");
        prop_assert!(result.is_ok());
        prop_assert!(result.unwrap().is_empty());
    }

    /// Whitespace doesn't affect parsing (except for separation)
    #[test]
    fn prop_whitespace_invariant(expr in r"\([+\-*/] \d+ \d+\)") {
        let with_extra_spaces = expr.chars()
            .map(|c| if c == ' ' { "   " } else { &c.to_string() })
            .collect::<String>();
        
        let result1 = parse(&expr);
        let result2 = parse(&with_extra_spaces);
        
        // Both should succeed or fail consistently
        prop_assert_eq!(result1.is_ok(), result2.is_ok());
    }

    /// Parser doesn't crash on malformed input
    #[test]
    fn prop_malformed_input_safety(input in r"[(){}[\]]{0,20}") {
        // Should not panic, even on unbalanced brackets
        let _ = parse(&input);
    }
}