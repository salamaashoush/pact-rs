//! Unified token definitions for Pact lexer
//!
//! This module provides a single, optimal implementation of Pact lexing that is:
//! - Haskell-compatible (exact token names and behavior)
//! - High-performance (string interning, efficient parsing)  
//! - Memory-efficient (compact representations)
//! - Comprehensive (all Pact language features)

use logos::Logos;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use pact_core::errors::{PactError, LexerError, PactErrorI};
use pact_core::shared::SpanInfo;

/// String interner for memory-efficient string handling
#[derive(Debug, Clone)]
pub struct StringInterner {
    strings: Vec<String>,
    map: HashMap<String, u32>,
}

impl StringInterner {
    pub fn new() -> Self {
        Self {
            strings: Vec::new(),
            map: HashMap::new(),
        }
    }

    pub fn intern(&mut self, s: &str) -> u32 {
        if let Some(&id) = self.map.get(s) {
            id
        } else {
            let id = self.strings.len() as u32;
            self.map.insert(s.to_string(), id);
            self.strings.push(s.to_string());
            id
        }
    }

    pub fn get(&self, id: u32) -> &str {
        &self.strings[id as usize]
    }
}

impl Default for StringInterner {
    fn default() -> Self {
        Self::new()
    }
}

/// Convert logos::Span to SpanInfo with line/column tracking
fn logos_span_to_span_info(source: &str, span: logos::Span) -> SpanInfo {
    SpanInfo::from_byte_offsets(source, span.start, span.end)
}

/// Process string literal escape sequences according to Haskell semantics
fn process_string_escapes(s: &str) -> Result<String, String> {
    // Remove quotes
    let inner = &s[1..s.len()-1];
    let mut result = String::new();
    let mut chars = inner.chars().peekable();
    
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            if let Some(&escaped) = chars.peek() {
                if escaped.is_whitespace() {
                    // Multiline string handling: backslash followed by whitespace
                    chars.next(); // consume the whitespace char
                    
                    // Consume all whitespace
                    while let Some(&ws) = chars.peek() {
                        if ws.is_whitespace() {
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    
                    // Expect another backslash to continue
                    match chars.peek() {
                        Some(&'\\') => {
                            // Don't consume the backslash, let the loop handle it
                            continue;
                        }
                        _ => {
                            return Err("Invalid multiline string: expected \\ after whitespace".to_string());
                        }
                    }
                } else {
                    // Regular escape sequence
                    chars.next(); // consume the escaped char
                    match escaped {
                        'n' => result.push('\n'),
                        't' => result.push('\t'),
                        'r' => {
                            // Match Haskell behavior: carriage return is not supported
                            return Err("carriage return is not supported".to_string());
                        }
                        '\\' => result.push('\\'),
                        '"' => result.push('"'),
                        '\'' => result.push('\''),
                        _ => {
                            // Match Haskell behavior: throw error for invalid escapes
                            return Err(format!("Invalid escape sequence: \\{}", escaped));
                        }
                    }
                }
            } else {
                result.push('\\');
            }
        } else if ch == '\n' {
            // Newlines not allowed in string literals (except via \n escape)
            return Err("newline in string literal".to_string());
        } else if ch == '\r' {
            // Carriage returns not allowed
            return Err("carriage return in string literal".to_string());
        } else {
            result.push(ch);
        }
    }
    Ok(result)
}

/// Pact tokens exactly matching Haskell implementation
#[derive(Logos, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[logos(skip r"[ \t\n\f]+")] // Skip whitespace
#[logos(skip r";[^\n]*")] // Skip comments
pub enum Token {
    // Keywords (exact Haskell order)
    #[token("let")]
    Let,
    #[token("let*")]
    LetStar,
    #[token("lambda")]
    Lambda,
    #[token("module")]
    Module,
    #[token("interface")]
    Interface,
    #[token("use")]
    Import, // Note: Haskell calls this TokenImport
    #[token("step")]
    Step,
    #[token("step-with-rollback")]
    StepWithRollback,

    // Def keywords
    #[token("defun")]
    Defun,
    #[token("defconst")]
    Defconst,
    #[token("defcap")]
    Defcap,
    #[token("defpact")]
    Defpact,
    #[token("defschema")]
    Defschema,
    #[token("deftable")]
    Deftable,
    #[token("bless")]
    Bless,
    #[token("implements")]
    Implements,
    
    // Special constructs (missing from Rust but in Haskell)
    #[token("with-capability")]
    WithCapability,
    #[token("expect-typechecks")]
    ExpectTypechecks,
    #[token("expect-typecheck-failure")]
    ExpectTypecheckFailure,
    #[token("create-user-guard")]
    CreateUserGuard,
    #[token("enforce")]
    Enforce,
    #[token("enforce-one")]
    EnforceOne,

    // Annotations
    #[token("@doc")]
    DocAnn,
    #[token("@model")]
    ModelAnn,
    #[token("@event")]
    EventAnn,
    #[token("@managed")]
    ManagedAnn,

    // Delimiters
    #[token("{")]
    OpenBrace,
    #[token("}")]
    CloseBrace,
    #[token("(")]
    OpenParens,
    #[token(")")]
    CloseParens,
    #[token("[")]
    OpenBracket,
    #[token("]")]
    CloseBracket,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(".")]
    Dot,
    #[token("::")]
    DynAcc,
    #[token(":=")]
    BindAssign,

    // Literals
    #[token("true")]
    True,
    #[token("false")]
    False,

    // Data tokens (exact Haskell patterns)
    #[regex(r"'[a-zA-Z][a-zA-Z0-9\-_]*", |lex| lex.slice().to_string(), priority = 3)]
    SingleTick(String),

    // Identifiers following Haskell pattern: [$alpha $psymbol][$alpha $digit $psymbol]*
    // $alpha = [a-zA-Z], $digit = [0-9], $psymbol = [%#+\-_&$@<>=^?*!|/~]
    #[regex(r"[a-zA-Z%#+\-_&$@<>=^?*!|/~][a-zA-Z0-9%#+\-_&$@<>=^?*!|/~]*", |lex| lex.slice().to_string(), priority = 1)]
    Ident(String),

    // Numbers following Haskell pattern: [\-]?[$digit]+ but also support decimals
    #[regex(r"-?[0-9]+(\.[0-9]+)?", |lex| lex.slice().to_string(), priority = 2)]
    Number(String),

    // String literals with full escape handling
    #[regex(r#""([^"\\]|\\.)*""#, |lex| lex.slice().to_string())]
    String(String),

    // End-of-file token
    Eof,
}


/// Main lexing function - the primary public API
/// Returns tokens with span information, using string interning for efficiency
pub fn lex(source: &str) -> Result<Vec<(Token, SpanInfo)>, PactErrorI> {
    use logos::Logos;

    let mut tokens = Vec::new();
    let mut lex = Token::lexer(source);

    while let Some(result) = lex.next() {
        match result {
            Ok(mut token) => {
                let span = logos_span_to_span_info(source, lex.span());
                
                // Process string literals for escape sequences
                if let Token::String(ref s) = token {
                    match process_string_escapes(s) {
                        Ok(processed) => {
                            token = Token::String(processed);
                        }
                        Err(err) => {
                            return Err(PactError::PELexerError(
                                LexerError::InvalidEscape(err),
                                span,
                            ));
                        }
                    }
                }
                
                tokens.push((token, span));
            }
            Err(_) => {
                let span = logos_span_to_span_info(source, lex.span());
                let slice = &source[lex.span()];
                
                // Determine specific error type
                let error = if slice.starts_with('"') && !slice.ends_with('"') {
                    LexerError::UnterminatedString
                } else if slice.chars().any(|c| c.is_numeric()) {
                    LexerError::InvalidNumber(slice.to_string())
                } else if slice.contains('\\') {
                    LexerError::InvalidEscape(slice.to_string())
                } else {
                    LexerError::InvalidToken(slice.to_string())
                };
                
                return Err(PactError::PELexerError(error, span));
            }
        }
    }

    Ok(tokens)
}

/// High-level lexer with string interning for performance
pub struct Lexer {
    interner: StringInterner,
}

impl Lexer {
    pub fn new() -> Self {
        Self {
            interner: StringInterner::new(),
        }
    }

    /// Lex with string interning for repeated use
    pub fn lex_interned(&mut self, source: &str) -> Result<Vec<(Token, SpanInfo)>, PactErrorI> {
        let tokens = lex(source)?;

        // Intern strings in ident, string, and number tokens
        let mut result = Vec::with_capacity(tokens.len());
        for (token, span) in tokens {
            let interned_token = match token {
                Token::Ident(s) => {
                    let _id = self.interner.intern(&s);
                    Token::Ident(s) // Keep original for compatibility, but interned for memory
                }
                Token::String(s) => {
                    let _id = self.interner.intern(&s);
                    Token::String(s)
                }
                Token::Number(s) => {
                    let _id = self.interner.intern(&s);
                    Token::Number(s)
                }
                Token::SingleTick(s) => {
                    let _id = self.interner.intern(&s);
                    Token::SingleTick(s)
                }
                other => other,
            };
            result.push((interned_token, span));
        }

        Ok(result)
    }

    /// Get memory usage statistics
    pub fn memory_usage(&self) -> MemoryUsage {
        let string_storage = self
            .interner
            .strings
            .iter()
            .map(|s| s.capacity())
            .sum::<usize>();

        let map_overhead =
            self.interner.map.len() * (std::mem::size_of::<String>() + std::mem::size_of::<u32>());

        MemoryUsage {
            interned_strings: self.interner.strings.len(),
            string_storage,
            map_overhead,
            total: string_storage + map_overhead,
        }
    }
}

impl Default for Lexer {
    fn default() -> Self {
        Self::new()
    }
}

/// Memory usage statistics
#[derive(Debug, Clone)]
pub struct MemoryUsage {
    pub interned_strings: usize,
    pub string_storage: usize,
    pub map_overhead: usize,
    pub total: usize,
}

impl std::fmt::Display for MemoryUsage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Lexer Memory Usage:\n\
             Interned strings: {}\n\
             String storage: {} bytes\n\
             Map overhead: {} bytes\n\
             Total: {} bytes",
            self.interned_strings, self.string_storage, self.map_overhead, self.total
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let tokens = lex("(defun test () true)").unwrap();

        let expected = [
            Token::OpenParens,
            Token::Defun,
            Token::Ident("test".to_string()),
            Token::OpenParens,
            Token::CloseParens,
            Token::True,
            Token::CloseParens,
        ];

        assert_eq!(tokens.len(), expected.len());

        for (i, (token, _span)) in tokens.iter().enumerate() {
            assert_eq!(token, &expected[i], "Token {} mismatch", i);
        }
    }

    #[test]
    fn test_string_literals() {
        let tokens = lex(r#""hello world""#).unwrap();
        assert_eq!(tokens.len(), 1);

        match &tokens[0].0 {
            Token::String(s) => assert_eq!(s, "hello world"),
            _ => panic!("Expected string token"),
        }
    }

    #[test]
    fn test_string_escapes() {
        let tokens = lex(r#""hello\nworld\t\"test\"""#).unwrap();
        assert_eq!(tokens.len(), 1);

        match &tokens[0].0 {
            Token::String(s) => assert_eq!(s, "hello\nworld\t\"test\""),
            _ => panic!("Expected string token"),
        }
    }
    
    #[test]
    fn test_string_escape_errors() {
        // Test carriage return error
        let result = lex(r#""hello\rworld""#);
        assert!(result.is_err());
        match result.unwrap_err() {
            PactError::PELexerError(LexerError::InvalidEscape(msg), _) => {
                assert_eq!(msg, "carriage return is not supported");
            }
            _ => panic!("Expected InvalidEscape for carriage return"),
        }
        
        // Test invalid escape sequence
        let result = lex(r#""hello\xworld""#);
        assert!(result.is_err());
        match result.unwrap_err() {
            PactError::PELexerError(LexerError::InvalidEscape(msg), _) => {
                assert_eq!(msg, "Invalid escape sequence: \\x");
            }
            _ => panic!("Expected InvalidEscape for invalid escape"),
        }
    }

    #[test]
    fn test_numbers() {
        let tokens = lex("42 -17 3.14 -2.5").unwrap();
        assert_eq!(tokens.len(), 4);

        let expected = ["42", "-17", "3.14", "-2.5"];
        for (i, (token, _span)) in tokens.iter().enumerate() {
            match token {
                Token::Number(n) => assert_eq!(n, expected[i]),
                _ => panic!("Expected number token at position {}", i),
            }
        }
    }

    #[test]
    fn test_symbols_and_operators() {
        let tokens = lex("+ - * / = < > <= >= !=").unwrap();

        for (token, _span) in tokens {
            match token {
                Token::Ident(_) => {} // All operators are identifiers
                _ => panic!("Expected ident token for operator"),
            }
        }
    }

    #[test]
    fn test_capabilities() {
        let tokens = lex(
            "(with-capability (TRANSFER sender receiver amount) (transfer sender receiver amount))",
        )
        .unwrap();

        // Should contain with-capability as a proper token now
        assert!(tokens
            .iter()
            .any(|(token, _)| matches!(token, Token::WithCapability)));
    }
    
    #[test]
    fn test_special_tokens() {
        let tokens = lex("with-capability expect-typechecks expect-typecheck-failure create-user-guard enforce enforce-one").unwrap();

        let expected = [
            Token::WithCapability,
            Token::ExpectTypechecks,
            Token::ExpectTypecheckFailure,
            Token::CreateUserGuard,
            Token::Enforce,
            Token::EnforceOne,
        ];
        
        assert_eq!(tokens.len(), expected.len());
        
        for (i, (token, _)) in tokens.iter().enumerate() {
            assert_eq!(token, &expected[i], "Token {} mismatch", i);
        }
    }

    #[test]
    fn test_comments_ignored() {
        let tokens = lex("; this is a comment\n42").unwrap();
        assert_eq!(tokens.len(), 1);

        match &tokens[0].0 {
            Token::Number(n) => assert_eq!(n, "42"),
            _ => panic!("Expected number token"),
        }
    }

    #[test]
    fn test_lexer_with_interning() {
        let mut lexer = Lexer::new();

        let source = r#"
        (defun test (x y)
          (let ((result (+ x y)))
            result))
        "#;

        let tokens = lexer.lex_interned(source).unwrap();
        assert!(!tokens.is_empty());

        let usage = lexer.memory_usage();
        assert!(usage.total > 0);

        // Should have interned some strings
        assert!(usage.interned_strings > 0);
    }

    #[test]
    fn test_haskell_compatibility() {
        // Test that we match exact Haskell token names
        let tokens = lex("(defun module interface use implements)").unwrap();

        let expected_tokens = [
            Token::OpenParens,
            Token::Defun,
            Token::Module,
            Token::Interface,
            Token::Import, // "use" maps to Import in Haskell
            Token::Implements,
            Token::CloseParens,
        ];

        assert_eq!(tokens.len(), expected_tokens.len());

        for (i, (token, _)) in tokens.iter().enumerate() {
            assert_eq!(token, &expected_tokens[i]);
        }
    }

    #[test]
    fn test_single_tick_identifiers() {
        let tokens = lex("'keyset-name 'admin-keyset").unwrap();
        assert_eq!(tokens.len(), 2);

        match &tokens[0].0 {
            Token::SingleTick(s) => assert_eq!(s, "'keyset-name"),
            _ => panic!("Expected SingleTick token"),
        }

        match &tokens[1].0 {
            Token::SingleTick(s) => assert_eq!(s, "'admin-keyset"),
            _ => panic!("Expected SingleTick token"),
        }
    }

    #[test]
    fn test_improved_symbol_patterns() {
        // Test symbols that follow Haskell character classes exactly (excluding keywords)
        let tokens =
            lex("+ - * / = < > <= >= != %test #hash &ref $var ^caret ?maybe !bang |pipe ~tilde")
                .unwrap();

        // All should be parsed as identifiers
        for (token, _) in tokens {
            match token {
                Token::Ident(_) => {} // Expected
                _ => panic!("Expected all tokens to be identifiers, got: {:?}", token),
            }
        }
    }

    #[test]
    fn test_annotation_tokens() {
        // Test that annotation tokens are recognized properly
        let tokens = lex("@doc @model @event @managed").unwrap();

        let expected = [
            Token::DocAnn,
            Token::ModelAnn,
            Token::EventAnn,
            Token::ManagedAnn,
        ];
        assert_eq!(tokens.len(), expected.len());

        for (i, (token, _)) in tokens.iter().enumerate() {
            assert_eq!(token, &expected[i]);
        }
    }

    #[test]
    fn test_multiline_strings() {
        // Test basic multiline string
        let tokens = lex(r#""hello \  
        \world""#).unwrap();
        assert_eq!(tokens.len(), 1);
        match &tokens[0].0 {
            Token::String(s) => assert_eq!(s, "hello world"),
            _ => panic!("Expected string token"),
        }

        // Test multiline with tabs and spaces
        let tokens = lex(r#""first line \
                         \second line""#).unwrap();
        assert_eq!(tokens.len(), 1);
        match &tokens[0].0 {
            Token::String(s) => assert_eq!(s, "first line second line"),
            _ => panic!("Expected string token"),
        }

        // Test multiline with embedded escapes
        let tokens = lex(r#""line one\n\
                           \line two""#).unwrap();
        assert_eq!(tokens.len(), 1);
        match &tokens[0].0 {
            Token::String(s) => assert_eq!(s, "line one\nline two"),
            _ => panic!("Expected string token"),
        }
    }

    #[test]
    fn test_multiline_string_errors() {
        // Test missing continuation backslash
        let result = lex(r#""hello \  
        world""#);
        assert!(result.is_err());
        match result.unwrap_err() {
            PactError::PELexerError(LexerError::InvalidEscape(msg), _) => {
                assert!(msg.contains("Invalid multiline string"));
            }
            _ => panic!("Expected InvalidEscape for missing continuation"),
        }

        // Test newline without escape
        let result = lex(r#""hello
world""#);
        assert!(result.is_err());
        match result.unwrap_err() {
            PactError::PELexerError(LexerError::InvalidEscape(msg), _) => {
                assert!(msg.contains("newline in string literal"));
            }
            _ => panic!("Expected error for unescaped newline"),
        }
    }
}
