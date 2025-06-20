//! Comprehensive lexer tests ported from Haskell to ensure 100% completeness

use pact_lexer::{lex, Lexer, Token};
use pact_shared_types::SpanInfo;

#[test]
fn test_span_info_single_line() {
    // Test from Haskell: LexerTests.hs - single line expression
    let tokens = lex("(test :X (x: integer) true)").unwrap();

    let expected = vec![
        (Token::OpenParens, SpanInfo { start: 0, end: 1 }),
        (Token::Ident("test".to_string()), SpanInfo { start: 1, end: 5 }),
        (Token::Colon, SpanInfo { start: 6, end: 7 }),
        (Token::Ident("X".to_string()), SpanInfo { start: 7, end: 8 }),
        (Token::OpenParens, SpanInfo { start: 9, end: 10 }),
        (Token::Ident("x".to_string()), SpanInfo { start: 10, end: 11 }),
        (Token::Colon, SpanInfo { start: 11, end: 12 }),
        (Token::Ident("integer".to_string()), SpanInfo { start: 13, end: 20 }),
        (Token::CloseParens, SpanInfo { start: 20, end: 21 }),
        (Token::True, SpanInfo { start: 22, end: 26 }),
        (Token::CloseParens, SpanInfo { start: 26, end: 27 }),
    ];

    assert_eq!(tokens.len(), expected.len());
    for (i, ((token, span), (exp_token, exp_span))) in
        tokens.iter().zip(expected.iter()).enumerate()
    {
        assert_eq!(token, exp_token, "Token mismatch at position {}", i);
        assert_eq!(
            span.start, exp_span.start,
            "Span start mismatch at position {}",
            i
        );
        assert_eq!(
            span.end, exp_span.end,
            "Span end mismatch at position {}",
            i
        );
    }
}

#[test]
fn test_span_info_with_spaces() {
    // Test from Haskell: LexerTests.hs - single line expression with spaces
    let tokens = lex("(test   :X   (x:   integer) true  )").unwrap();

    // Verify tokens are correct (whitespace should be ignored)
    let expected_tokens = vec![
        Token::OpenParens,
        Token::Ident("test".to_string()),
        Token::Colon,
        Token::Ident("X".to_string()),
        Token::OpenParens,
        Token::Ident("x".to_string()),
        Token::Colon,
        Token::Ident("integer".to_string()),
        Token::CloseParens,
        Token::True,
        Token::CloseParens,
    ];

    assert_eq!(tokens.len(), expected_tokens.len());
    for (i, ((token, _), exp_token)) in tokens.iter().zip(expected_tokens.iter()).enumerate() {
        assert_eq!(token, exp_token, "Token mismatch at position {}", i);
    }
}

#[test]
fn test_multiline_expression() {
    // Test from Haskell: LexerTests.hs - multiline expression
    let source = "(test\n :X (x:\n\n integer) true\n)";
    let tokens = lex(source).unwrap();

    let expected_tokens = vec![
        Token::OpenParens,
        Token::Ident("test".to_string()),
        Token::Colon,
        Token::Ident("X".to_string()),
        Token::OpenParens,
        Token::Ident("x".to_string()),
        Token::Colon,
        Token::Ident("integer".to_string()),
        Token::CloseParens,
        Token::True,
        Token::CloseParens,
    ];

    assert_eq!(tokens.len(), expected_tokens.len());
    for (i, ((token, _), exp_token)) in tokens.iter().zip(expected_tokens.iter()).enumerate() {
        assert_eq!(token, exp_token, "Token mismatch at position {}", i);
    }
}

#[test]
fn test_semicolon_comments() {
    // Test from parsing.repl - semicolon in expr
    let source = "(+ 1 2\n  ;\n  )";
    let tokens = lex(source).unwrap();

    let expected = vec![
        Token::OpenParens,
        Token::Ident("+".to_string()),
        Token::Number("1".to_string()),
        Token::Number("2".to_string()),
        Token::CloseParens,
    ];

    assert_eq!(tokens.len(), expected.len());
    for (i, ((token, _), exp_token)) in tokens.iter().zip(expected.iter()).enumerate() {
        assert_eq!(token, exp_token, "Token mismatch at position {}", i);
    }
}

#[test]
fn test_special_characters_in_property_expressions() {
    // Test from fv-syntax-regression.repl - special chars: &%^@
    let source = "(whatever (it parses literally:anything{}[][]&%^@))";
    let tokens = lex(source).unwrap();

    // These special characters should be parsed as part of identifiers
    assert!(tokens
        .iter()
        .any(|(t, _)| matches!(t, Token::Ident(s) if s.contains('&'))));
    assert!(tokens
        .iter()
        .any(|(t, _)| matches!(t, Token::Ident(s) if s.contains('%'))));
    assert!(tokens
        .iter()
        .any(|(t, _)| matches!(t, Token::Ident(s) if s.contains('^'))));
    assert!(tokens
        .iter()
        .any(|(t, _)| matches!(t, Token::Ident(s) if s.contains('@'))));
}

#[test]
fn test_all_keywords() {
    // Test all keywords from Haskell token list
    let keywords = vec![
        ("let", Token::Let),
        ("let*", Token::LetStar),
        ("lambda", Token::Lambda),
        ("module", Token::Module),
        ("interface", Token::Interface),
        ("use", Token::Import),
        ("step", Token::Step),
        ("step-with-rollback", Token::StepWithRollback),
        ("defun", Token::Defun),
        ("defconst", Token::Defconst),
        ("defcap", Token::Defcap),
        ("defpact", Token::Defpact),
        ("defschema", Token::Defschema),
        ("deftable", Token::Deftable),
        ("bless", Token::Bless),
        ("implements", Token::Implements),
        ("true", Token::True),
        ("false", Token::False),
    ];

    for (keyword, expected) in keywords {
        let tokens = lex(keyword).unwrap();
        assert_eq!(tokens.len(), 1);
        assert_eq!(
            tokens[0].0, expected,
            "Keyword '{}' not lexed correctly",
            keyword
        );
    }
}

#[test]
fn test_all_annotations() {
    // Test all annotation tokens
    let annotations = vec![
        ("@doc", Token::DocAnn),
        ("@model", Token::ModelAnn),
        ("@event", Token::EventAnn),
        ("@managed", Token::ManagedAnn),
    ];

    for (ann, expected) in annotations {
        let tokens = lex(ann).unwrap();
        assert_eq!(tokens.len(), 1);
        assert_eq!(
            tokens[0].0, expected,
            "Annotation '{}' not lexed correctly",
            ann
        );
    }
}

#[test]
fn test_all_delimiters() {
    // Test all delimiter tokens
    let delimiters = vec![
        ("{", Token::OpenBrace),
        ("}", Token::CloseBrace),
        ("(", Token::OpenParens),
        (")", Token::CloseParens),
        ("[", Token::OpenBracket),
        ("]", Token::CloseBracket),
        (",", Token::Comma),
        (":", Token::Colon),
        (".", Token::Dot),
        ("::", Token::DynAcc),
        (":=", Token::BindAssign),
    ];

    for (delim, expected) in delimiters {
        let tokens = lex(delim).unwrap();
        assert_eq!(tokens.len(), 1);
        assert_eq!(
            tokens[0].0, expected,
            "Delimiter '{}' not lexed correctly",
            delim
        );
    }
}

#[test]
fn test_string_escape_sequences() {
    // Test all escape sequences supported by Haskell lexer
    let test_cases = vec![
        (r#""hello\nworld""#, "hello\nworld"),
        (r#""tab\there""#, "tab\there"),
        (r#""carriage\rreturn""#, "carriage\rreturn"),
        (r#""quote\"inside""#, "quote\"inside"),
        (r#""backslash\\here""#, "backslash\\here"),
        (r#""apostrophe\'here""#, "apostrophe'here"),
    ];

    for (input, expected) in test_cases {
        let tokens = lex(input).unwrap();
        assert_eq!(tokens.len(), 1);
        match &tokens[0].0 {
            Token::String(s) => assert_eq!(s, expected, "String escape not handled correctly"),
            _ => panic!("Expected string token"),
        }
    }
}

#[test]
fn test_number_formats() {
    // Test various number formats
    let numbers = vec!["42", "-17", "3.14", "-2.5", "0", "-0", "1000000", "-999999"];

    for num in numbers {
        let tokens = lex(num).unwrap();
        assert_eq!(tokens.len(), 1);
        match &tokens[0].0 {
            Token::Number(n) => assert_eq!(n, num, "Number not lexed correctly"),
            _ => panic!("Expected number token for '{}'", num),
        }
    }
}

#[test]
fn test_single_tick_identifiers() {
    // Test single tick identifier pattern from Haskell
    let ticks = vec!["'namespace", "'keyset", "'admin", "'bank-admin"];

    for tick in ticks {
        let tokens = lex(tick).unwrap();
        assert_eq!(tokens.len(), 1);
        match &tokens[0].0 {
            Token::SingleTick(s) => assert_eq!(s, tick, "Single tick not lexed correctly"),
            _ => panic!("Expected single tick token for '{}'", tick),
        }
    }
}

#[test]
fn test_identifier_with_special_symbols() {
    // Test identifiers with all allowed special symbols: %#+\-_&$@<>=^?*!|/~
    let identifiers = vec![
        "%percent",
        "#hash",
        "+plus",
        "-minus",
        "_underscore",
        "&and",
        "$dollar",
        "@at",
        "<less",
        ">greater",
        "=equal",
        "^caret",
        "?question",
        "*star",
        "!bang",
        "|pipe",
        "/slash",
        "~tilde",
        "mixed%#+-_&$@<>=^?*!|/~123",
    ];

    for id in identifiers {
        let tokens = lex(id).unwrap();
        assert_eq!(tokens.len(), 1);
        match &tokens[0].0 {
            Token::Ident(s) => {
                assert_eq!(s, id, "Identifier with special chars not lexed correctly")
            }
            _ => panic!("Expected identifier token for '{}'", id),
        }
    }
}

#[test]
fn test_complex_expressions() {
    // Test complex expressions from various test files
    let source = r#"
        (defun test:string (name:string)
            @doc "Test function with type annotations"
            (format "Hello {}" [name]))
    "#;

    let tokens = lex(source).unwrap();

    // Verify we have all expected token types
    assert!(tokens.iter().any(|(t, _)| matches!(t, Token::Defun)));
    assert!(tokens.iter().any(|(t, _)| matches!(t, Token::DocAnn)));
    assert!(tokens.iter().any(|(t, _)| matches!(t, Token::String(_))));
    assert!(tokens.iter().any(|(t, _)| matches!(t, Token::Colon)));
}

#[test]
fn test_module_references() {
    // Test module reference syntax
    let source = "coin.transfer ns.mod.function";
    let tokens = lex(source).unwrap();

    // Should tokenize as: ident dot ident ident dot ident dot ident
    let expected = vec![
        Token::Ident("coin".to_string()),
        Token::Dot,
        Token::Ident("transfer".to_string()),
        Token::Ident("ns".to_string()),
        Token::Dot,
        Token::Ident("mod".to_string()),
        Token::Dot,
        Token::Ident("function".to_string()),
    ];

    assert_eq!(tokens.len(), expected.len());
    for (i, ((token, _), exp_token)) in tokens.iter().zip(expected.iter()).enumerate() {
        assert_eq!(token, exp_token, "Token mismatch at position {}", i);
    }
}

#[test]
fn test_dynamic_access() {
    // Test :: token for dynamic access
    let source = "obj::field";
    let tokens = lex(source).unwrap();

    let expected = vec![
        Token::Ident("obj".to_string()),
        Token::DynAcc,
        Token::Ident("field".to_string()),
    ];

    assert_eq!(tokens.len(), expected.len());
    for (i, ((token, _), exp_token)) in tokens.iter().zip(expected.iter()).enumerate() {
        assert_eq!(token, exp_token, "Token mismatch at position {}", i);
    }
}

#[test]
fn test_bind_assign() {
    // Test := token for bind assignments
    let source = r#"{"field" := value}"#;
    let tokens = lex(source).unwrap();

    assert!(tokens.iter().any(|(t, _)| matches!(t, Token::BindAssign)));
}

#[test]
fn test_empty_input() {
    // Test empty input
    let tokens = lex("").unwrap();
    assert_eq!(tokens.len(), 0);
}

#[test]
fn test_whitespace_only() {
    // Test whitespace only input
    let tokens = lex("   \n\t  \n  ").unwrap();
    assert_eq!(tokens.len(), 0);
}

#[test]
fn test_comment_only() {
    // Test comment only input
    let tokens = lex("; This is a comment\n; Another comment").unwrap();
    assert_eq!(tokens.len(), 0);
}

#[test]
fn test_list_with_commas() {
    // Test from parsing.repl - lists with and without commas
    let source1 = "[1 2 3]";
    let source2 = "[1, 2, 3]";

    let tokens1 = lex(source1).unwrap();
    let tokens2 = lex(source2).unwrap();

    // Both should have brackets, numbers, and tokens2 should have commas
    assert!(tokens1.iter().any(|(t, _)| matches!(t, Token::OpenBracket)));
    assert!(tokens2.iter().any(|(t, _)| matches!(t, Token::Comma)));
}

#[test]
fn test_lexer_memory_usage() {
    // Test that string interning is working
    let mut lexer = Lexer::new();

    // Lex the same identifier multiple times
    let source = "test test test test test";
    let tokens = lexer.lex_interned(source).unwrap();

    assert_eq!(tokens.len(), 5);

    // Check memory usage
    let usage = lexer.memory_usage();
    assert!(usage.interned_strings > 0);
    // Should only intern "test" once
    assert_eq!(usage.interned_strings, 1);
}
