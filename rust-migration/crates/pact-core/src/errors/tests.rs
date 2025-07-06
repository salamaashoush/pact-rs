//! Comprehensive tests for pact-errors
//!
//! Tests cover all error types, conversions, and display formatting.

use crate::*;
use std::path::PathBuf;

#[cfg(test)]
mod pact_error_tests {
    use super::*;

    #[test]
    fn test_runtime_error_creation() {
        let err = RuntimeError::InvalidArgument {
            function: "test".to_string(),
            message: "Bad argument".to_string(),
        };
        assert_eq!(err.to_string(), "Invalid argument: Bad argument");

        let err = RuntimeError::DatabaseError {
            message: "Table not found".to_string(),
        };
        assert_eq!(err.to_string(), "Database error: Table not found");

        let err = RuntimeError::DivisionByZero;
        assert_eq!(err.to_string(), "Division by zero");
    }

    #[test]
    fn test_type_error_creation() {
        let err = TypeError::TypeMismatch {
            expected: "string".to_string(),
            actual: "integer".to_string(),
        };
        assert_eq!(
            err.to_string(),
            "Type mismatch: expected string, got integer"
        );

        let err = TypeError::MissingField {
            field: "name".to_string(),
        };
        assert_eq!(err.to_string(), "Missing field: name");
    }

    #[test]
    fn test_parse_error_creation() {
        let err = ParseError::Syntax {
            message: "Unexpected token".to_string(),
        };
        assert_eq!(err.to_string(), "Syntax error: Unexpected token");
    }

    #[test]
    fn test_pact_error_variants() {
        let runtime_err = PactError::Runtime(RuntimeError::InvalidArgument {
            message: "test".to_string(),
            function: "test".to_string(),
        });
        assert!(matches!(runtime_err, PactError::Runtime(_)));

        let type_err = PactError::Type(TypeError::TypeMismatch {
            expected: "bool".to_string(),
            actual: "string".to_string(),
        });
        assert!(matches!(type_err, PactError::Type(_)));

        let parse_err = PactError::Parse(ParseError::Syntax {
            message: "test".to_string(),
        });
        assert!(matches!(parse_err, PactError::Parse(_)));
    }

    #[test]
    fn test_error_conversion() {
        let type_err = TypeError::MissingField {
            field: "test".to_string(),
        };
        let pact_err: PactError = type_err.into();
        assert!(matches!(pact_err, PactError::Type(_)));
    }

    #[test]
    fn test_error_display() {
        let err = PactError::Runtime(RuntimeError::UserError {
            message: "User validation failed".to_string(),
        });
        let display = err.to_string();
        assert!(display.contains("User error"));
        assert!(display.contains("User validation failed"));
    }

    #[test]
    fn test_all_runtime_error_variants() {
        // Test each RuntimeError variant
        let errors = vec![
            RuntimeError::InvalidArgument {
                message: "test".to_string(),
                function: "test".to_string(),
            },
            RuntimeError::DatabaseError {
                message: "db test".to_string(),
            },
            RuntimeError::TimeError {
                message: "time test".to_string(),
            },
            RuntimeError::CapabilityNotAcquired {
                message: "cap test".to_string(),
            },
            RuntimeError::GuardFailure {
                message: "guard test".to_string(),
            },
            RuntimeError::UserError {
                message: "user test".to_string(),
            },
            RuntimeError::InvalidOperation {
                operation: "add".to_string(),
                left_type: "string".to_string(),
                right_type: "number".to_string(),
            },
            RuntimeError::DivisionByZero,
        ];

        for err in errors {
            let pact_err = PactError::Runtime(err);
            // Ensure all errors have proper display strings
            assert!(!pact_err.to_string().is_empty());
        }
    }

    #[test]
    fn test_all_type_error_variants() {
        // Test each TypeError variant
        let errors = vec![
            TypeError::Mismatch {
                expected: "int".to_string(),
                actual: "string".to_string(),
            },
            TypeError::TypeMismatch {
                expected: "bool".to_string(),
                actual: "int".to_string(),
            },
            TypeError::RowMismatch {
                message: "Row structure mismatch".to_string(),
            },
            TypeError::MissingField {
                field: "required_field".to_string(),
            },
            TypeError::InfiniteType {
                var: "a".to_string(),
                ty: "List a".to_string(),
            },
            TypeError::NoInstance {
                class: "Num".to_string(),
                ty: "String".to_string(),
            },
            TypeError::RowMismatchFields {
                expected_fields: vec!["a".to_string(), "b".to_string()],
                actual_fields: vec!["a".to_string(), "c".to_string()],
            },
        ];

        for err in errors {
            let pact_err = PactError::Type(err);
            // Ensure all errors have proper display strings
            assert!(!pact_err.to_string().is_empty());
        }
    }

    #[test]
    fn test_error_serialization() {
        let err = RuntimeError::InvalidArgument {
            message: "test message".to_string(),
            function: "test".to_string(),
        };

        // Test that errors can be serialized and deserialized
        let serialized = serde_json::to_string(&err).unwrap();
        let deserialized: RuntimeError = serde_json::from_str(&serialized).unwrap();
        assert_eq!(err, deserialized);
    }

    #[test]
    fn test_result_type() {
        fn test_function(should_succeed: bool) -> PactResult<String> {
            if should_succeed {
                Ok("Success".to_string())
            } else {
                Err(PactError::Runtime(RuntimeError::UserError {
                    message: "Failed".to_string(),
                }))
            }
        }

        assert!(test_function(true).is_ok());
        assert!(test_function(false).is_err());

        match test_function(false) {
            Err(PactError::Runtime(RuntimeError::UserError { message })) => {
                assert_eq!(message, "Failed");
            }
            _ => panic!("Expected UserError"),
        }
    }
}

#[cfg(test)]
mod error_pattern_tests {
    use super::*;

    #[test]
    fn test_error_patterns_in_practice() {
        // Simulate a typical database operation error
        fn db_operation() -> PactResult<()> {
            Err(PactError::Runtime(RuntimeError::DatabaseError {
                message: "Table 'users' not found".to_string(),
            }))
        }

        let result = db_operation();
        assert!(result.is_err());

        // Pattern matching on specific errors
        match result {
            Err(PactError::Runtime(RuntimeError::DatabaseError { message })) => {
                assert!(message.contains("users"));
            }
            _ => panic!("Expected database error"),
        }
    }

    #[test]
    fn test_error_chaining() {
        // Test converting between error types
        fn type_check() -> Result<(), TypeError> {
            Err(TypeError::MissingField {
                field: "age".to_string(),
            })
        }

        fn higher_level_operation() -> PactResult<()> {
            type_check()?;
            Ok(())
        }

        let result = higher_level_operation();
        assert!(result.is_err());

        match result {
            Err(PactError::Type(TypeError::MissingField { field })) => {
                assert_eq!(field, "age");
            }
            _ => panic!("Expected type error"),
        }
    }

    #[test]
    fn test_custom_error_messages() {
        let err = RuntimeError::InvalidOperation {
            operation: "concat".to_string(),
            left_type: "number".to_string(),
            right_type: "bool".to_string(),
        };

        let message = err.to_string();
        assert!(message.contains("concat"));
        assert!(message.contains("number"));
        assert!(message.contains("bool"));
    }
}

#[cfg(test)]
mod error_equality_tests {
    use super::*;

    #[test]
    fn test_error_equality() {
        let err1 = RuntimeError::InvalidArgument {
            message: "test".to_string(),
            function: "test".to_string(),
        };
        let err2 = RuntimeError::InvalidArgument {
            message: "test".to_string(),
            function: "test".to_string(),
        };
        let err3 = RuntimeError::InvalidArgument {
            message: "different".to_string(),
            function: "test".to_string(),
        };

        assert_eq!(err1, err2);
        assert_ne!(err1, err3);
    }

    #[test]
    fn test_pact_error_equality() {
        let err1 = PactError::Runtime(RuntimeError::DivisionByZero);
        let err2 = PactError::Runtime(RuntimeError::DivisionByZero);
        let err3 = PactError::Type(TypeError::MissingField {
            field: "test".to_string(),
        });

        assert_eq!(err1, err2);
        assert_ne!(err1, err3);
    }

    #[test]
    fn test_error_clone() {
        let original = RuntimeError::UserError {
            message: "Original error".to_string(),
        };
        let cloned = original.clone();

        assert_eq!(original, cloned);
    }
}

#[cfg(test)]
mod info_tests {
    use super::*;

    #[test]
    fn test_pos_creation_and_display() {
        let pos = Pos::new(10, 5);
        assert_eq!(pos.line, 10);
        assert_eq!(pos.column, 5);
        assert_eq!(pos.to_string(), "10:5");
    }

    #[test]
    fn test_span_creation_and_display() {
        let start = Pos::new(1, 1);
        let end = Pos::new(1, 10);
        let span = Span::new(start.clone(), end);
        assert_eq!(span.to_string(), "1:1-1:10");

        let point_span = Span::point(Pos::new(5, 5));
        assert_eq!(point_span.to_string(), "5:5");
    }

    #[test]
    fn test_info_creation() {
        let span = Span::new(Pos::new(1, 1), Pos::new(1, 10));
        let info = Info::new(Some(PathBuf::from("test.pact")), span.clone());

        assert_eq!(info.file, Some(PathBuf::from("test.pact")));
        assert_eq!(info.span, span);
        assert_eq!(info.source, None);
    }

    #[test]
    fn test_info_with_source() {
        let span = Span::new(Pos::new(1, 1), Pos::new(1, 10));
        let info =
            Info::new(Some(PathBuf::from("test.pact")), span).with_source("let x = 42".to_string());

        assert_eq!(info.source, Some("let x = 42".to_string()));
    }

    #[test]
    fn test_info_display() {
        let span = Span::new(Pos::new(10, 5), Pos::new(10, 15));
        let info = Info::new(Some(PathBuf::from("module.pact")), span);
        assert_eq!(info.to_string(), "module.pact:10:5-10:15");

        let info_no_file = Info::new(None, Span::point(Pos::new(5, 10)));
        assert_eq!(info_no_file.to_string(), "5:10");
    }

    #[test]
    fn test_has_info_trait() {
        struct TestNode {
            info: Info,
        }

        impl HasInfo for TestNode {
            fn info(&self) -> &Info {
                &self.info
            }
        }

        let node = TestNode {
            info: Info::new(
                Some(PathBuf::from("test.pact")),
                Span::new(Pos::new(1, 1), Pos::new(2, 5)),
            ),
        };

        assert_eq!(node.span().start.line, 1);
        assert_eq!(node.file(), Some(&PathBuf::from("test.pact")));
    }

    #[test]
    fn test_error_context_formatting() {
        let source = "let x = 42\nlet y = x + \"hello\"\nlet z = y * 2";

        let mut ctx = ErrorInfo::new(
            Info::new(
                Some(PathBuf::from("test.pact")),
                Span::new(Pos::new(2, 13), Pos::new(2, 20)),
            )
            .with_source(source.to_string()),
        );

        ctx.add_context(
            "Type mismatch".to_string(),
            Info::new(
                Some(PathBuf::from("test.pact")),
                Span::new(Pos::new(1, 9), Pos::new(1, 11)),
            )
            .with_source(source.to_string()),
        );

        let formatted = ctx.format_with_source();
        assert!(formatted.contains("Error at test.pact:2:13-2:20"));
        assert!(formatted.contains("+ \"hello\""));
        assert!(formatted.contains("^^^^^^^"));
    }

    #[test]
    fn test_call_stack() {
        let mut error_info = ErrorInfo::new(Info::dummy());

        error_info.push_frame(CallFrame {
            function: "foo".to_string(),
            location: Info::new(
                Some(PathBuf::from("test.pact")),
                Span::point(Pos::new(10, 5)),
            ),
            args: Some(vec!["42".to_string(), "\"hello\"".to_string()]),
        });

        error_info.push_frame(CallFrame {
            function: "bar".to_string(),
            location: Info::new(
                Some(PathBuf::from("test.pact")),
                Span::point(Pos::new(15, 3)),
            ),
            args: None,
        });

        let formatted = error_info.format_with_source();
        assert!(formatted.contains("Call stack:"));
        assert!(formatted.contains("foo at test.pact:10:5"));
        assert!(formatted.contains("Arguments: 42, \"hello\""));
        assert!(formatted.contains("bar at test.pact:15:3"));
    }

    #[test]
    fn test_severity_display() {
        assert_eq!(Severity::Info.to_string(), "INFO");
        assert_eq!(Severity::Warning.to_string(), "WARNING");
        assert_eq!(Severity::Error.to_string(), "ERROR");
        assert_eq!(Severity::Fatal.to_string(), "FATAL");
    }

    #[test]
    fn test_severity_ordering() {
        assert!(Severity::Info < Severity::Warning);
        assert!(Severity::Warning < Severity::Error);
        assert!(Severity::Error < Severity::Fatal);
    }

    #[test]
    fn test_diagnostic() {
        let mut diag = Diagnostic::new(
            Severity::Error,
            "Type mismatch in function call".to_string(),
            ErrorInfo::new(Info::dummy()),
        );

        diag.add_suggestion("Check function signature".to_string());
        diag.add_suggestion("Convert argument types".to_string());

        let formatted = diag.format();
        assert!(formatted.contains("ERROR: Type mismatch"));
        assert!(formatted.contains("Suggestions:"));
        assert!(formatted.contains("Check function signature"));
        assert!(formatted.contains("Convert argument types"));
    }

    #[test]
    fn test_source_snippet_formatting() {
        let source = "line one\nline two has error here\nline three";
        let error_info = ErrorInfo::new(
            Info::new(None, Span::new(Pos::new(2, 10), Pos::new(2, 14)))
                .with_source(source.to_string()),
        );

        let formatted = error_info.format_with_source();
        assert!(formatted.contains("   1 | line one"));
        assert!(formatted.contains("   2 | line two has error here"));
        assert!(formatted.contains("     |          ^^^^"));
        assert!(formatted.contains("   3 | line three"));
    }

    #[test]
    fn test_multiline_span() {
        let source = "function foo() {\n  return\n    42\n}";
        let error_info = ErrorInfo::new(
            Info::new(None, Span::new(Pos::new(2, 3), Pos::new(3, 6)))
                .with_source(source.to_string()),
        );

        let formatted = error_info.format_with_source();
        assert!(formatted.contains("   2 |   return"));
        assert!(formatted.contains("     |   ^^^^^^"));
        assert!(formatted.contains("   3 |     42"));
        assert!(formatted.contains("     | ^^^^^"));
    }
}
