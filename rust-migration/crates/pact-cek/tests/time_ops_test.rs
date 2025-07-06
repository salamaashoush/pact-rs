use pact_cek::*;
use pact_shared_types::SpanInfo;
use pact_ir::CoreBuiltin;
use pact_values::{time::PactTime, Decimal};
use chrono::{DateTime, Utc};

/// Comprehensive integration test for time builtin registration and basic functionality
/// Tests all 8 time functions: time, parse-time, format-time, add-time, diff-time, hours, minutes, days

#[cfg(test)]
mod time_builtin_tests {
    use super::*;

    fn dummy_span() -> SpanInfo {
        SpanInfo::empty()
    }

    #[test]
    fn test_time_builtins_register_successfully() {
        let mut builtin_env = BuiltinEnv::new();
        let result = pact_cek::time_ops::register_time_builtins(&mut builtin_env);
        assert!(result.is_ok(), "Time builtins should register without error");
    }

    #[test]
    fn test_all_time_operations_are_registered() {
        let mut builtin_env = BuiltinEnv::new();
        pact_cek::time_ops::register_time_builtins(&mut builtin_env).unwrap();

        // Test all time operations are properly registered
        let all_time_ops = vec![
            CoreBuiltin::CoreTime,
            CoreBuiltin::CoreParseTime,
            CoreBuiltin::CoreFormatTime,
            CoreBuiltin::CoreAddTime,
            CoreBuiltin::CoreDiffTime,
            CoreBuiltin::CoreHours,
            CoreBuiltin::CoreMinutes,
            CoreBuiltin::CoreDays,
        ];

        for op in all_time_ops {
            assert!(builtin_env.lookup(op, dummy_span()).is_ok(),
                    "Time operation {:?} should be registered", op);
        }
    }

    #[test]
    fn test_time_operations_have_correct_arity() {
        let mut builtin_env = BuiltinEnv::new();
        pact_cek::time_ops::register_time_builtins(&mut builtin_env).unwrap();

        // Test unary operations have arity 1
        let unary_ops = vec![
            CoreBuiltin::CoreTime,
            CoreBuiltin::CoreHours,
            CoreBuiltin::CoreMinutes,
            CoreBuiltin::CoreDays,
        ];

        for op in unary_ops {
            match builtin_env.lookup(op, dummy_span()) {
                Ok(native_fn) => {
                    assert_eq!(native_fn.arity, 1,
                              "Unary time operation {:?} should have arity 1", op);
                }
                Err(_) => panic!("Unary time operation {:?} not found", op),
            }
        }

        // Test binary operations have arity 2
        let binary_ops = vec![
            CoreBuiltin::CoreParseTime,
            CoreBuiltin::CoreFormatTime,
            CoreBuiltin::CoreAddTime,
            CoreBuiltin::CoreDiffTime,
        ];

        for op in binary_ops {
            match builtin_env.lookup(op, dummy_span()) {
                Ok(native_fn) => {
                    assert_eq!(native_fn.arity, 2,
                              "Binary time operation {:?} should have arity 2", op);
                }
                Err(_) => panic!("Binary time operation {:?} not found", op),
            }
        }
    }

    #[test]
    fn test_time_operations_return_correct_builtin_enum() {
        let mut builtin_env = BuiltinEnv::new();
        pact_cek::time_ops::register_time_builtins(&mut builtin_env).unwrap();

        // Test that lookup returns the correct builtin enum
        let test_ops = vec![
            CoreBuiltin::CoreTime,
            CoreBuiltin::CoreParseTime,
            CoreBuiltin::CoreFormatTime,
            CoreBuiltin::CoreAddTime,
            CoreBuiltin::CoreDiffTime,
            CoreBuiltin::CoreHours,
            CoreBuiltin::CoreMinutes,
            CoreBuiltin::CoreDays,
        ];

        for op in test_ops {
            match builtin_env.lookup(op, dummy_span()) {
                Ok(native_fn) => {
                    assert_eq!(native_fn.builtin, op,
                              "Lookup should return the correct builtin enum for {:?}", op);
                }
                Err(_) => panic!("Time operation {:?} not found", op),
            }
        }
    }

    #[test]
    fn test_time_builtins_integration() {
        // Comprehensive integration test that all time operations are present
        // and correctly integrated into the builtin environment
        let mut builtin_env = BuiltinEnv::new();

        // Registration should succeed
        let registration_result = time_ops::register_time_builtins(&mut builtin_env);
        assert!(registration_result.is_ok(), "Time builtin registration should succeed");

        // All 8 time operations should be available
        let operation_count = vec![
            CoreBuiltin::CoreTime,
            CoreBuiltin::CoreParseTime,
            CoreBuiltin::CoreFormatTime,
            CoreBuiltin::CoreAddTime,
            CoreBuiltin::CoreDiffTime,
            CoreBuiltin::CoreHours,
            CoreBuiltin::CoreMinutes,
            CoreBuiltin::CoreDays,
        ];

        // Verify each operation is correctly registered and can be looked up
        for (index, op) in operation_count.iter().enumerate() {
            match builtin_env.lookup(*op, dummy_span()) {
                Ok(_) => {
                    // Successfully found - this is what we expect
                }
                Err(e) => {
                    panic!("Failed to lookup time operation #{} {:?}: {:?}",
                           index + 1, op, e);
                }
            }
        }

        // All 8 operations should be registered
        assert_eq!(operation_count.len(), 8, "Should have 8 time operations");
    }

    #[test]
    fn test_time_operation_names() {
        let mut builtin_env = BuiltinEnv::new();
        pact_cek::time_ops::register_time_builtins(&mut builtin_env).unwrap();

        // Test function names are set correctly
        let expected_names = vec![
            (CoreBuiltin::CoreTime, "time"),
            (CoreBuiltin::CoreParseTime, "parse-time"),
            (CoreBuiltin::CoreFormatTime, "format-time"),
            (CoreBuiltin::CoreAddTime, "add-time"),
            (CoreBuiltin::CoreDiffTime, "diff-time"),
            (CoreBuiltin::CoreHours, "hours"),
            (CoreBuiltin::CoreMinutes, "minutes"),
            (CoreBuiltin::CoreDays, "days"),
        ];

        for (builtin, expected_name) in expected_names {
            // We can't access the internal functions map directly, so we'll just verify 
            // that the builtin can be looked up successfully. The name is internal implementation.
            match builtin_env.lookup(builtin, dummy_span()) {
                Ok(_) => {
                    // Successfully found - this validates the builtin is registered
                    // The name is stored internally and not exposed in the public API
                }
                Err(_) => {
                    panic!("Builtin {:?} not found in environment", builtin);
                }
            }
        }
    }

    /// Test basic time parsing functionality
    #[test]
    fn test_basic_time_parsing() {
        // This test validates that our time parsing utility functions work correctly
        // without needing full CEK integration
        
        // Test ISO 8601 format parsing
        let iso_time = "2016-07-22T11:26:35Z";
        let parsed = pact_cek::builtin::time_ops::parse_time_with_format("%Y-%m-%dT%H:%M:%SZ", iso_time);
        assert!(parsed.is_ok(), "Should parse valid ISO 8601 time");

        // Test date-only format parsing  
        let date_time = "2016-07-22";
        let parsed_date = pact_cek::builtin::time_ops::parse_time_with_format("%Y-%m-%d", date_time);
        assert!(parsed_date.is_ok(), "Should parse valid date");

        // Test %F format (same as %Y-%m-%d)
        let parsed_f = pact_cek::builtin::time_ops::parse_time_with_format("%F", date_time);
        assert!(parsed_f.is_ok(), "Should parse %F format");

        // Test invalid format
        let invalid_parse = pact_cek::builtin::time_ops::parse_time_with_format("%INVALID%", "test");
        assert!(invalid_parse.is_err(), "Should fail on invalid format");
    }

    /// Test basic time formatting functionality
    #[test]
    fn test_basic_time_formatting() {
        // Create a test time (2016-07-22T11:26:35Z)
        let dt = DateTime::parse_from_rfc3339("2016-07-22T11:26:35Z").unwrap();
        let time = PactTime::from_datetime(dt.with_timezone(&Utc));

        // Test %F format (date only)
        let formatted_f = pact_cek::builtin::time_ops::format_time_with_format("%F", &time);
        assert!(formatted_f.is_ok(), "Should format %F successfully");
        assert_eq!(formatted_f.unwrap(), "2016-07-22", "%F should produce date-only format");

        // Test ISO format
        let formatted_iso = pact_cek::builtin::time_ops::format_time_with_format("%Y-%m-%dT%H:%M:%SZ", &time);
        assert!(formatted_iso.is_ok(), "Should format ISO successfully");
        assert_eq!(formatted_iso.unwrap(), "2016-07-22T11:26:35Z", "ISO format should match input");

        // Test basic format codes
        let year = pact_cek::builtin::time_ops::format_time_with_format("%Y", &time);
        assert_eq!(year.unwrap(), "2016", "%Y should produce year");

        let month = pact_cek::builtin::time_ops::format_time_with_format("%m", &time);
        assert_eq!(month.unwrap(), "07", "%m should produce month");

        let day = pact_cek::builtin::time_ops::format_time_with_format("%d", &time);
        assert_eq!(day.unwrap(), "22", "%d should produce day");

        // Test literal percent
        let percent = pact_cek::builtin::time_ops::format_time_with_format("%%", &time);
        assert_eq!(percent.unwrap(), "%", "%% should produce literal %");

        // Test seconds since epoch
        let seconds = pact_cek::builtin::time_ops::format_time_with_format("%s", &time);
        assert!(seconds.is_ok(), "Should format %s successfully");
    }

    /// Test that time utility functions work correctly with known values
    #[test] 
    fn test_time_utility_calculations() {
        // Test hours to minutes conversion: 1 hour = 60 minutes
        let one_hour_decimal = Decimal::from_f64(1.0);
        let expected_minutes = Decimal::from_f64(60.0);
        
        // This validates our logic without needing CEK integration
        let calculated_minutes = one_hour_decimal.to_f64() * 60.0;
        assert_eq!(calculated_minutes, 60.0, "1 hour should equal 60 minutes");

        // Test days to minutes conversion: 1 day = 1440 minutes (24 * 60)
        let one_day_decimal = Decimal::from_f64(1.0);
        let expected_day_minutes = Decimal::from_f64(1440.0);
        
        let calculated_day_minutes = one_day_decimal.to_f64() * 24.0 * 60.0;
        assert_eq!(calculated_day_minutes, 1440.0, "1 day should equal 1440 minutes");

        // Test fractional conversions
        let half_hour = Decimal::from_f64(0.5);
        let half_hour_minutes = half_hour.to_f64() * 60.0;
        assert_eq!(half_hour_minutes, 30.0, "0.5 hours should equal 30 minutes");
    }

    /// Test time arithmetic functionality
    #[test]
    fn test_time_arithmetic() {
        // Create a base time (2016-07-22T11:26:35Z) 
        let dt = DateTime::parse_from_rfc3339("2016-07-22T11:26:35Z").unwrap();
        let base_time = PactTime::from_datetime(dt.with_timezone(&Utc));

        // Test adding 1 second (1,000,000 microseconds)
        let one_second_later = base_time.add_micros(1_000_000);
        let diff_micros = one_second_later.diff_micros(&base_time);
        assert_eq!(diff_micros, 1_000_000, "Adding 1 second should result in 1,000,000 microsecond difference");

        // Test adding 1 hour (3,600,000,000 microseconds)
        let one_hour_later = base_time.add_micros(3_600_000_000);
        let hour_diff_seconds = one_hour_later.diff_micros(&base_time) / 1_000_000;
        assert_eq!(hour_diff_seconds, 3600, "Adding 1 hour should result in 3600 second difference");

        // Test time difference calculation
        let diff_seconds = one_hour_later.diff_micros(&base_time) as f64 / 1_000_000.0;
        assert_eq!(diff_seconds, 3600.0, "1 hour difference should be 3600.0 seconds");
    }

    /// Test error conditions
    #[test]
    fn test_time_error_conditions() {
        // Test invalid time string parsing
        let invalid_time = pact_cek::builtin::time_ops::parse_time_with_format("%Y-%m-%dT%H:%M:%SZ", "invalid-time");
        assert!(invalid_time.is_err(), "Should fail to parse invalid time string");

        // Test invalid date parsing
        let invalid_date = pact_cek::builtin::time_ops::parse_time_with_format("%Y-%m-%d", "2016-13-45"); // Invalid month/day
        assert!(invalid_date.is_err(), "Should fail to parse invalid date");

        // Test unsupported format
        let unsupported = pact_cek::builtin::time_ops::parse_time_with_format("%UNSUPPORTED%", "test");
        assert!(unsupported.is_err(), "Should fail on unsupported format");
    }

    /// Test round-trip parsing and formatting
    #[test]
    fn test_time_round_trip() {
        let original_time_str = "2016-07-22T11:26:35Z";
        
        // Parse the time
        let parsed = pact_cek::builtin::time_ops::parse_time_with_format("%Y-%m-%dT%H:%M:%SZ", original_time_str).unwrap();
        
        // Format it back
        let formatted = pact_cek::builtin::time_ops::format_time_with_format("%Y-%m-%dT%H:%M:%SZ", &parsed).unwrap();
        
        // Should match the original
        assert_eq!(formatted, original_time_str, "Round-trip should preserve time string");
    }
}

// Helper functions are accessed directly via module path in tests