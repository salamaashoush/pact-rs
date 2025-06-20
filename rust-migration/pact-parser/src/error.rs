//! Parser error handling using the new error system

use pact_errors::{PactError, ParseError, PactErrorI};
use pact_shared_types::SpanInfo;

/// Result type for parser operations
pub type Result<T> = std::result::Result<T, PactErrorI>;

/// Helper to create a parse error with span
pub fn parse_error(message: String, span: SpanInfo) -> PactErrorI {
    PactError::PEParseError(
        ParseError::SyntaxError(message),
        span,
    )
}

/// Helper to create an unexpected token error
pub fn unexpected_token(expected: String, found: String, span: SpanInfo) -> PactErrorI {
    PactError::PEParseError(
        ParseError::UnexpectedToken { expected, found },
        span,
    )
}

/// Helper to create an invalid expression error
pub fn invalid_expression(message: String, span: SpanInfo) -> PactErrorI {
    PactError::PEParseError(
        ParseError::InvalidExpression(message),
        span,
    )
}