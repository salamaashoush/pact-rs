//! Property-based tests for the lexer

use crate::{lex, Token};
use proptest::prelude::*;

/// Generate valid Pact identifiers
fn arb_identifier() -> impl Strategy<Value = String> {
    r"[a-zA-Z_][a-zA-Z0-9_\-]*".prop_map(|s| s.chars().take(50).collect())
}

/// Generate valid integer literals
fn arb_integer() -> impl Strategy<Value = String> {
    any::<i64>().prop_map(|i| i.to_string())
}

/// Generate valid decimal literals
fn arb_decimal() -> impl Strategy<Value = String> {
    (any::<i32>(), 1u32..=10)
        .prop_map(|(int_part, scale)| format!("{}.{}", int_part, "0".repeat(scale as usize)))
}

/// Generate valid string literals (without quotes)
fn arb_string_content() -> impl Strategy<Value = String> {
    r#"[^"\\]*"#.prop_map(|s| s.chars().take(100).collect())
}

proptest! {
    /// Identifiers should lex correctly
    #[test]
    fn prop_identifier_lexing(id in arb_identifier()) {
        let tokens = lex(&id).unwrap();
        assert_eq!(tokens.len(), 1); // just identifier (no EOF in current impl)
        match &tokens[0].0 {
            Token::Ident(name) => assert_eq!(name, &id),
            _ => panic!("Expected identifier token"),
        }
    }

    /// Numbers should lex correctly and maintain value
    #[test]
    fn prop_number_lexing(num_str in arb_integer()) {
        let tokens = lex(&num_str).unwrap();
        assert_eq!(tokens.len(), 1); // just number
        match &tokens[0].0 {
            Token::Number(actual) => assert_eq!(actual, &num_str),
            _ => panic!("Expected number token"),
        }
    }

    /// Decimals should lex correctly
    #[test]
    fn prop_decimal_lexing(dec_str in arb_decimal()) {
        let tokens = lex(&dec_str).unwrap();
        assert_eq!(tokens.len(), 1); // just decimal
        match &tokens[0].0 {
            Token::Number(actual) => assert_eq!(actual, &dec_str),
            _ => panic!("Expected number token"),
        }
    }

    /// String literals should preserve content
    #[test]
    fn prop_string_lexing(content in arb_string_content()) {
        let input = format!("\"{}\"", content);
        let tokens = lex(&input).unwrap();
        assert_eq!(tokens.len(), 1); // just string
        match &tokens[0].0 {
            Token::String(actual) => assert_eq!(actual, &content),
            _ => panic!("Expected string token"),
        }
    }

    /// Roundtrip: lex then format should preserve structure
    #[test]
    fn prop_simple_expression_roundtrip(
        op in r"\+|\-|\*|/",
        a in 0i64..1000,
        b in 0i64..1000
    ) {
        let input = format!("({} {} {})", op, a, b);
        let tokens = lex(&input).unwrap();

        // Should have: (, op, num, num, )
        assert_eq!(tokens.len(), 5);
        assert!(matches!(tokens[0].0, Token::OpenParens));
        assert!(matches!(tokens[4].0, Token::CloseParens));

        // Values should be preserved
        if let Token::Number(val_a) = &tokens[2].0 {
            assert_eq!(val_a, &a.to_string());
        }
        if let Token::Number(val_b) = &tokens[3].0 {
            assert_eq!(val_b, &b.to_string());
        }
    }

    /// Error recovery: malformed input should not panic
    #[test]
    fn prop_error_recovery(input in r"[^\x00-\x1F]{0,100}") {
        // Should not panic on any reasonable input
        let _ = lex(&input);
    }

    /// Keywords are recognized consistently
    #[test]
    fn prop_keyword_recognition(
        keyword in prop_oneof![
            Just("defun"),
            Just("module"),
            Just("let"),
            Just("lambda")
        ]
    ) {
        let tokens = lex(keyword).unwrap();
        assert_eq!(tokens.len(), 1); // just keyword

        // Should be recognized as keyword, not identifier
        match &tokens[0].0 {
            Token::Ident(_) => panic!("Keyword {} lexed as identifier", keyword),
            _ => {} // Good - recognized as specific keyword
        }
    }

    /// Advanced identifiers with special characters
    #[test]
    fn prop_advanced_identifiers(base in "[a-zA-Z][a-zA-Z0-9]*") {
        // Test identifiers with allowed special chars
        let special_chars = vec!['%', '#', '+', '-', '_', '&', '$', '@', '<', '>', '=', '^', '?', '*', '!', '|', '/', '~'];

        for &ch in &special_chars {
            let id = format!("{}{}", base, ch);
            let tokens = lex(&id).unwrap();

            if tokens.len() == 1 {
                match &tokens[0].0 {
                    Token::Ident(name) => assert_eq!(name, &id),
                    _ => panic!("Expected identifier token for {}", id),
                }
            }
        }
    }

    /// Single tick identifiers
    #[test]
    fn prop_single_tick_identifiers(base in "[a-zA-Z][a-zA-Z0-9\\-_]*") {
        let input = format!("'{}", base);
        let tokens = lex(&input).unwrap();
        assert_eq!(tokens.len(), 1);

        match &tokens[0].0 {
            Token::SingleTick(name) => assert_eq!(name, &input),
            _ => panic!("Expected SingleTick token"),
        }
    }
}
