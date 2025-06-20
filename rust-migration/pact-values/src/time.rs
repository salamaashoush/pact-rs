//! Time representation for Pact
//!
//! Provides microsecond-precision UTC timestamps matching Haskell Pact.

use serde::{Deserialize, Serialize};
use std::fmt;
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use chrono::{DateTime, Utc, TimeZone};

/// Time representation using microseconds since Unix epoch
/// Matches Haskell Pact's UTCTime with microsecond precision
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct PactTime(pub i64);

impl PactTime {
    /// Create a new PactTime from the current system time
    pub fn now() -> Self {
        let duration = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default();
        PactTime(duration.as_micros() as i64)
    }

    /// Create a PactTime from seconds since Unix epoch
    pub fn from_seconds(seconds: i64) -> Self {
        PactTime(seconds * 1_000_000)
    }

    /// Create a PactTime from milliseconds since Unix epoch
    pub fn from_millis(millis: i64) -> Self {
        PactTime(millis * 1_000)
    }

    /// Create a PactTime from microseconds since Unix epoch
    pub fn from_micros(micros: i64) -> Self {
        PactTime(micros)
    }

    /// Create a PactTime from a Duration since Unix epoch
    pub fn from_duration(duration: Duration) -> Self {
        PactTime(duration.as_micros() as i64)
    }

    /// Get seconds since Unix epoch
    pub fn as_seconds(&self) -> i64 {
        self.0 / 1_000_000
    }

    /// Get milliseconds since Unix epoch
    pub fn as_millis(&self) -> i64 {
        self.0 / 1_000
    }

    /// Get microseconds since Unix epoch
    pub fn as_micros(&self) -> i64 {
        self.0
    }

    /// Convert to Duration since Unix epoch
    pub fn as_duration(&self) -> Duration {
        Duration::from_micros(self.0 as u64)
    }

    /// Convert to SystemTime
    pub fn as_system_time(&self) -> SystemTime {
        UNIX_EPOCH + self.as_duration()
    }

    /// Add microseconds to this time
    pub fn add_micros(&self, micros: i64) -> Self {
        PactTime(self.0 + micros)
    }

    /// Add seconds to this time
    pub fn add_seconds(&self, seconds: i64) -> Self {
        self.add_micros(seconds * 1_000_000)
    }

    /// Subtract another time from this one, returning microseconds difference
    pub fn diff_micros(&self, other: &PactTime) -> i64 {
        self.0 - other.0
    }

    /// Subtract another time from this one, returning seconds difference
    pub fn diff_seconds(&self, other: &PactTime) -> i64 {
        self.diff_micros(other) / 1_000_000
    }

    /// Check if this time is before another
    pub fn is_before(&self, other: &PactTime) -> bool {
        self.0 < other.0
    }

    /// Check if this time is after another
    pub fn is_after(&self, other: &PactTime) -> bool {
        self.0 > other.0
    }

    /// Format as ISO 8601 string (approximate, for display purposes)
    pub fn to_iso_string(&self) -> String {
        // This is a simplified version - a full implementation would
        // properly format the timestamp with timezone info
        let seconds = self.as_seconds();
        let micros = self.0 % 1_000_000;
        format!(
            "{}T00:00:{:02}.{:06}Z",
            1970 + seconds / (365 * 24 * 3600), // Approximate year
            seconds % 60,
            micros
        )
    }

    /// Parse from seconds (floating point)
    pub fn from_seconds_f64(seconds: f64) -> Self {
        PactTime((seconds * 1_000_000.0) as i64)
    }

    /// Convert to seconds (floating point)
    pub fn as_seconds_f64(&self) -> f64 {
        self.0 as f64 / 1_000_000.0
    }

    /// Create from chrono DateTime
    pub fn from_datetime(dt: DateTime<Utc>) -> Self {
        let timestamp = dt.timestamp_micros();
        PactTime(timestamp)
    }

    /// Convert to chrono DateTime
    pub fn to_datetime(&self) -> DateTime<Utc> {
        Utc.timestamp_micros(self.0).unwrap()
    }
}

impl fmt::Display for PactTime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_iso_string())
    }
}

impl From<SystemTime> for PactTime {
    fn from(time: SystemTime) -> Self {
        let duration = time.duration_since(UNIX_EPOCH).unwrap_or_default();
        PactTime::from_duration(duration)
    }
}

impl From<Duration> for PactTime {
    fn from(duration: Duration) -> Self {
        PactTime::from_duration(duration)
    }
}

impl From<i64> for PactTime {
    fn from(micros: i64) -> Self {
        PactTime::from_micros(micros)
    }
}

/// Time arithmetic operations
impl std::ops::Add<i64> for PactTime {
    type Output = PactTime;

    fn add(self, micros: i64) -> Self::Output {
        self.add_micros(micros)
    }
}

impl std::ops::Sub<i64> for PactTime {
    type Output = PactTime;

    fn sub(self, micros: i64) -> Self::Output {
        self.add_micros(-micros)
    }
}

impl std::ops::Sub<PactTime> for PactTime {
    type Output = i64;

    fn sub(self, other: PactTime) -> Self::Output {
        self.diff_micros(&other)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_time_creation() {
        let time = PactTime::from_seconds(1234567890);
        assert_eq!(time.as_seconds(), 1234567890);
        assert_eq!(time.as_micros(), 1234567890 * 1_000_000);

        let time_millis = PactTime::from_millis(1234567890);
        assert_eq!(time_millis.as_millis(), 1234567890);

        let time_micros = PactTime::from_micros(1234567890);
        assert_eq!(time_micros.as_micros(), 1234567890);
    }

    #[test]
    fn test_time_arithmetic() {
        let time1 = PactTime::from_seconds(100);
        let time2 = PactTime::from_seconds(200);

        assert_eq!(time2.diff_seconds(&time1), 100);
        assert_eq!(time1.diff_seconds(&time2), -100);

        let time3 = time1.add_seconds(50);
        assert_eq!(time3.as_seconds(), 150);

        let time4 = time1 + 1_000_000; // Add 1 second in micros
        assert_eq!(time4.as_seconds(), 101);
    }

    #[test]
    fn test_time_comparison() {
        let time1 = PactTime::from_seconds(100);
        let time2 = PactTime::from_seconds(200);

        assert!(time1.is_before(&time2));
        assert!(time2.is_after(&time1));
        assert!(!time1.is_after(&time2));
        assert!(!time2.is_before(&time1));

        assert!(time1 < time2);
        assert!(time2 > time1);
    }

    #[test]
    fn test_time_conversion() {
        let system_time = SystemTime::now();
        let pact_time = PactTime::from(system_time);
        let back_to_system = pact_time.as_system_time();

        // Allow for some precision loss in conversion
        let diff = system_time
            .duration_since(back_to_system)
            .or_else(|_| back_to_system.duration_since(system_time))
            .unwrap();
        assert!(diff.as_millis() < 1);
    }

    #[test]
    fn test_time_formatting() {
        let time = PactTime::from_seconds(0);
        let iso_string = time.to_iso_string();
        assert!(iso_string.contains("1970"));
        assert!(iso_string.contains("T"));
        assert!(iso_string.contains("Z"));
    }

    #[test]
    fn test_floating_point_conversion() {
        let time = PactTime::from_seconds_f64(123.456789);
        let back = time.as_seconds_f64();

        // Should be close due to floating point precision
        assert!((back - 123.456789).abs() < 0.000001);
    }
}
