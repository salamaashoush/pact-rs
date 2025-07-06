/// Comprehensive tests for format-time strftime codes
/// Tests all format codes supported by the Haskell implementation
use pact_cek::time_ops::format_time_with_format;
use pact_values::time::PactTime;
use chrono::{Utc, TimeZone};

#[cfg(test)]
mod format_time_tests {
    use super::*;

    fn create_test_time() -> PactTime {
        // Create a specific test time: 2023-06-15 14:30:45.123456 UTC
        let dt = Utc.with_ymd_and_hms(2023, 6, 15, 14, 30, 45).unwrap();
        let micros = dt.timestamp() * 1_000_000 + 123456;
        PactTime::from_micros(micros)
    }

    fn create_test_time_jan1() -> PactTime {
        // Create January 1st for week calculation tests
        let dt = Utc.with_ymd_and_hms(2023, 1, 1, 12, 0, 0).unwrap();
        PactTime::from_micros(dt.timestamp() * 1_000_000)
    }

    #[test]
    fn test_time_components() {
        let time = create_test_time();
        
        // Hour formats
        assert_eq!(format_time_with_format("%H", &time).unwrap(), "14");
        assert_eq!(format_time_with_format("%k", &time).unwrap(), "14");
        assert_eq!(format_time_with_format("%I", &time).unwrap(), "02");
        assert_eq!(format_time_with_format("%l", &time).unwrap(), " 2");
        
        // Minute and second
        assert_eq!(format_time_with_format("%M", &time).unwrap(), "30");
        assert_eq!(format_time_with_format("%S", &time).unwrap(), "45");
        
        // Microseconds and extensions
        assert_eq!(format_time_with_format("%v", &time).unwrap(), "123456");
        assert_eq!(format_time_with_format("%Q", &time).unwrap(), ".123456");
        assert_eq!(format_time_with_format("%q", &time).unwrap(), "123456000000");
        
        // Unix epoch seconds
        let expected_seconds = time.as_seconds().to_string();
        assert_eq!(format_time_with_format("%s", &time).unwrap(), expected_seconds);
    }

    #[test]
    fn test_date_components() {
        let time = create_test_time();
        
        // Year formats
        assert_eq!(format_time_with_format("%Y", &time).unwrap(), "2023");
        assert_eq!(format_time_with_format("%y", &time).unwrap(), "23");
        assert_eq!(format_time_with_format("%C", &time).unwrap(), "20");
        
        // Month formats
        assert_eq!(format_time_with_format("%B", &time).unwrap(), "June");
        assert_eq!(format_time_with_format("%b", &time).unwrap(), "Jun");
        assert_eq!(format_time_with_format("%h", &time).unwrap(), "Jun");
        assert_eq!(format_time_with_format("%m", &time).unwrap(), "06");
        
        // Day formats
        assert_eq!(format_time_with_format("%d", &time).unwrap(), "15");
        assert_eq!(format_time_with_format("%e", &time).unwrap(), "15");
        assert_eq!(format_time_with_format("%j", &time).unwrap(), "166"); // Day of year
    }

    #[test]
    fn test_week_date_format() {
        let time = create_test_time();
        
        // ISO week date formats
        assert_eq!(format_time_with_format("%G", &time).unwrap(), "2023");
        assert_eq!(format_time_with_format("%g", &time).unwrap(), "23");
        assert_eq!(format_time_with_format("%f", &time).unwrap(), "20");
        assert_eq!(format_time_with_format("%V", &time).unwrap(), "24"); // ISO week
        assert_eq!(format_time_with_format("%u", &time).unwrap(), "4"); // Thursday
    }

    #[test]
    fn test_day_of_week() {
        let time = create_test_time(); // June 15, 2023 is a Thursday
        
        assert_eq!(format_time_with_format("%a", &time).unwrap(), "Thu");
        assert_eq!(format_time_with_format("%A", &time).unwrap(), "Thursday");
        assert_eq!(format_time_with_format("%w", &time).unwrap(), "4"); // Sunday = 0
        
        // Test week of year calculations
        let jan1 = create_test_time_jan1(); // January 1, 2023 is a Sunday
        assert_eq!(format_time_with_format("%U", &jan1).unwrap(), "01"); // Week starts Sunday
        assert_eq!(format_time_with_format("%W", &jan1).unwrap(), "00"); // Week starts Monday
    }

    #[test]
    fn test_composite_formats() {
        let time = create_test_time();
        
        assert_eq!(format_time_with_format("%D", &time).unwrap(), "06/15/23");
        assert_eq!(format_time_with_format("%F", &time).unwrap(), "2023-06-15");
        assert_eq!(format_time_with_format("%x", &time).unwrap(), "06/15/23");
        assert_eq!(format_time_with_format("%N", &time).unwrap(), "2023-06-15T14:30:45.123456Z");
        assert_eq!(format_time_with_format("%Z", &time).unwrap(), "UTC");
    }

    #[test]
    fn test_special_cases() {
        let time = create_test_time();
        
        // Literal percent
        assert_eq!(format_time_with_format("%%", &time).unwrap(), "%");
        
        // Combined format strings
        assert_eq!(
            format_time_with_format("%Y-%m-%d %H:%M:%S", &time).unwrap(), 
            "2023-06-15 14:30:45"
        );
        
        // Format with text
        assert_eq!(
            format_time_with_format("Today is %A, %B %d, %Y", &time).unwrap(),
            "Today is Thursday, June 15, 2023"
        );
    }

    #[test]
    fn test_edge_cases() {
        let time = create_test_time();
        
        // Empty format string
        assert_eq!(format_time_with_format("", &time).unwrap(), "");
        
        // No format codes
        assert_eq!(format_time_with_format("Hello World", &time).unwrap(), "Hello World");
        
        // Multiple percent signs
        assert_eq!(format_time_with_format("%%%Y%%", &time).unwrap(), "%2023%");
    }

    #[test]
    fn test_12_hour_format() {
        // Test midnight (00:00)
        let midnight = Utc.with_ymd_and_hms(2023, 6, 15, 0, 0, 0).unwrap();
        let midnight_time = PactTime::from_micros(midnight.timestamp() * 1_000_000);
        assert_eq!(format_time_with_format("%I", &midnight_time).unwrap(), "12");
        assert_eq!(format_time_with_format("%l", &midnight_time).unwrap(), "12");
        
        // Test noon (12:00)
        let noon = Utc.with_ymd_and_hms(2023, 6, 15, 12, 0, 0).unwrap();
        let noon_time = PactTime::from_micros(noon.timestamp() * 1_000_000);
        assert_eq!(format_time_with_format("%I", &noon_time).unwrap(), "12");
        assert_eq!(format_time_with_format("%l", &noon_time).unwrap(), "12");
        
        // Test PM time (20:00 = 8 PM)
        let evening = Utc.with_ymd_and_hms(2023, 6, 15, 20, 0, 0).unwrap();
        let evening_time = PactTime::from_micros(evening.timestamp() * 1_000_000);
        assert_eq!(format_time_with_format("%I", &evening_time).unwrap(), "08");
        assert_eq!(format_time_with_format("%l", &evening_time).unwrap(), " 8");
    }

    #[test]
    fn test_fractional_seconds() {
        // Test with no fractional seconds
        let whole = Utc.with_ymd_and_hms(2023, 6, 15, 14, 30, 45).unwrap();
        let whole_time = PactTime::from_micros(whole.timestamp() * 1_000_000);
        assert_eq!(format_time_with_format("%Q", &whole_time).unwrap(), "");
        
        // Test with various fractional seconds
        let frac1 = PactTime::from_micros(whole.timestamp() * 1_000_000 + 100000);
        assert_eq!(format_time_with_format("%Q", &frac1).unwrap(), ".1");
        
        let frac2 = PactTime::from_micros(whole.timestamp() * 1_000_000 + 123000);
        assert_eq!(format_time_with_format("%Q", &frac2).unwrap(), ".123");
    }

    #[test]
    fn test_invalid_format_codes() {
        let time = create_test_time();
        
        // Test invalid format codes
        assert!(format_time_with_format("%X", &time).is_err());
        assert!(format_time_with_format("%9", &time).is_err());
        assert!(format_time_with_format("%@", &time).is_err());
    }

    #[test]
    fn test_space_padding() {
        // Test space padding for single-digit values
        let early = Utc.with_ymd_and_hms(2023, 1, 5, 9, 5, 5).unwrap();
        let early_time = PactTime::from_micros(early.timestamp() * 1_000_000);
        
        assert_eq!(format_time_with_format("%k", &early_time).unwrap(), " 9");
        assert_eq!(format_time_with_format("%e", &early_time).unwrap(), " 5");
        assert_eq!(format_time_with_format("%l", &early_time).unwrap(), " 9");
    }
}