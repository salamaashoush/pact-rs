//! Utilities for attaching span information to errors during parsing

use pact_core::errors::PactError;
use pact_core::shared::SpanInfo;

/// Trait for attaching span information to errors
pub trait WithSpan {
    /// Attach span information to this error
    fn with_span(self, span: SpanInfo) -> SpanError;
}

/// An error with associated span information
#[derive(Debug, Clone)]
pub struct SpanError {
    pub error: PactError,
    pub span: SpanInfo,
}

impl WithSpan for PactError {
    fn with_span(self, span: SpanInfo) -> SpanError {
        SpanError { error: self, span }
    }
}

impl From<SpanError> for PactError {
    fn from(span_error: SpanError) -> Self {
        // For now, just return the inner error
        // In the future, we could enhance PactError to store span info
        span_error.error
    }
}

/// Create a parse error with span information
pub fn parse_error_with_span(message: &str, span: SpanInfo) -> SpanError {
    PactError::Parse(pact_core::errors::ParseError::Syntax {
        message: message.to_string(),
    }).with_span(span)
}

/// Create a type error with span information
pub fn type_error_with_span(expected: &str, actual: &str, span: SpanInfo) -> SpanError {
    PactError::Type(pact_core::errors::TypeError::TypeMismatch {
        expected: expected.to_string(),
        actual: actual.to_string(),
    }).with_span(span)
}