//! Enhanced diagnostics implementation with semantic analysis

use crate::semantic::{SemanticAnalyzer, SemanticAnalysis};
use tower_lsp::lsp_types::*;
use pact_compiler::compile_pact_source;
use pact_lexer::{Lexer, Token};
use pact_parser::parse;
use std::sync::Arc;

/// Comprehensive diagnostics analyzer
pub struct DiagnosticsAnalyzer {
    semantic_analyzer: Arc<SemanticAnalyzer>,
}

impl DiagnosticsAnalyzer {
    pub fn new() -> Self {
        Self {
            semantic_analyzer: Arc::new(SemanticAnalyzer::new()),
        }
    }
    
    /// Analyze document with multiple levels of diagnostics
    pub fn analyze(&self, uri: &Url, text: &str) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        
        // Level 1: Lexical analysis
        diagnostics.extend(self.analyze_lexical(text));
        
        // Level 2: Syntax analysis  
        diagnostics.extend(self.analyze_syntax(text));
        
        // Level 3: Semantic analysis
        let semantic_analysis = self.semantic_analyzer.analyze_document(uri, text);
        diagnostics.extend(semantic_analysis.diagnostics);
        
        // Level 4: Runtime analysis (if syntax is valid)
        if !self.has_syntax_errors(&diagnostics) {
            diagnostics.extend(self.analyze_runtime(text));
        }
        
        // Level 5: Style and best practices
        diagnostics.extend(self.analyze_style(text, uri));
        
        diagnostics
    }
    
    /// Analyze lexical errors
    fn analyze_lexical(&self, text: &str) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        
        match lex(text) {
            Ok(_tokens) => {
                // The lexer itself doesn't produce Error tokens in this implementation
                // So we don't check for them here
                
                // Additional lexical checks could be added here if needed
            }
            Err(e) => {
                // Extract span information from the error if available
                let span = e.span;
                let (start_line, start_char) = to_line_col(span.start, text);
                let (end_line, end_char) = to_line_col(span.end, text);
                
                diagnostics.push(Diagnostic {
                    range: Range {
                        start: Position {
                            line: start_line,
                            character: start_char,
                        },
                        end: Position {
                            line: end_line,
                            character: end_char,
                        },
                    },
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: Some(NumberOrString::String("LEX002".to_string())),
                    source: Some("pact-lexer".to_string()),
                    message: format!("Lexer error: {}", e),
                    related_information: None,
                    tags: None,
                    code_description: None,
                    data: None,
                });
            }
        }
        
        diagnostics
    }
    
    /// Analyze syntax errors
    fn analyze_syntax(&self, text: &str) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        
        match parse(text) {
            Ok(_) => {
                // Syntax is valid, check for syntax warnings
                self.check_syntax_warnings(text, &mut diagnostics);
            }
            Err(e) => {
                let error_message = e.to_string();
                let (line, character) = self.extract_error_position(&error_message);
                
                diagnostics.push(Diagnostic {
                    range: Range {
                        start: Position { line, character },
                        end: Position { line, character: character + 1 },
                    },
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: Some(NumberOrString::String("SYN001".to_string())),
                    source: Some("pact-parser".to_string()),
                    message: format!("Parse error: {}", e),
                    related_information: Some(vec![DiagnosticRelatedInformation {
                        location: Location {
                            uri: Url::parse("https://pact.kadena.io/docs/syntax").unwrap(),
                            range: Range::default(),
                        },
                        message: "See Pact syntax documentation".to_string(),
                    }]),
                    tags: None,
                    code_description: Some(CodeDescription {
                        href: Url::parse("https://pact.kadena.io/docs/syntax-errors").unwrap(),
                    }),
                    data: None,
                });
            }
        }
        
        diagnostics
    }
    
    /// Analyze runtime/evaluation errors and warnings
    fn analyze_runtime(&self, text: &str) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        
        match compile_pact_source(text) {
            Ok(_) => {
                // Compilation successful, no errors
            }
            Err(e) => {
                let error_message = e.to_string();
                let severity = if error_message.contains("warning") || error_message.contains("deprecated") {
                    DiagnosticSeverity::WARNING
                } else {
                    DiagnosticSeverity::ERROR
                };
                
                diagnostics.push(Diagnostic {
                    range: Range {
                        start: Position { line: 0, character: 0 },
                        end: Position { line: 0, character: 0 },
                    },
                    severity: Some(severity),
                    code: Some(NumberOrString::String("EVAL001".to_string())),
                    source: Some("pact-compiler".to_string()),
                    message: format!("Evaluation error: {}", e),
                    related_information: None,
                    tags: None,
                    code_description: None,
                    data: None,
                });
            }
        }
        
        diagnostics
    }
    
    /// Analyze style and best practices
    fn analyze_style(&self, text: &str, uri: &Url) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        
        // Check for style issues
        self.check_indentation(text, &mut diagnostics);
        self.check_line_length(text, &mut diagnostics);
        self.check_naming_conventions(text, &mut diagnostics);
        self.check_documentation(text, &mut diagnostics);
        self.check_security_patterns(text, &mut diagnostics);
        self.check_performance_hints(text, &mut diagnostics);
        
        diagnostics
    }
    
    /// Check for unterminated strings
    fn check_unterminated_strings(&self, tokens: &[pact_lexer::Token], text: &str, diagnostics: &mut Vec<Diagnostic>) {
        let lines: Vec<&str> = text.lines().collect();
        
        for (line_idx, line) in lines.iter().enumerate() {
            let mut in_string = false;
            let mut escape_next = false;
            
            for (char_idx, ch) in line.char_indices() {
                if escape_next {
                    escape_next = false;
                    continue;
                }
                
                match ch {
                    '"' => in_string = !in_string,
                    '\\' if in_string => escape_next = true,
                    _ => {}
                }
            }
            
            if in_string {
                diagnostics.push(Diagnostic {
                    range: Range {
                        start: Position { line: line_idx as u32, character: line.len() as u32 },
                        end: Position { line: line_idx as u32, character: line.len() as u32 },
                    },
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: Some(NumberOrString::String("LEX003".to_string())),
                    source: Some("pact-lexer".to_string()),
                    message: "Unterminated string literal".to_string(),
                    related_information: None,
                    tags: None,
                    code_description: None,
                    data: None,
                });
            }
        }
    }
    
    // TODO: Implement check_invalid_numbers when Token enum structure is finalized
    // This method was temporarily removed due to changes in the Token enum structure
    
    /// Check for syntax warnings (valid but potentially problematic)
    fn check_syntax_warnings(&self, text: &str, diagnostics: &mut Vec<Diagnostic>) {
        let lines: Vec<&str> = text.lines().collect();
        
        for (line_idx, line) in lines.iter().enumerate() {
            // Check for deeply nested expressions
            let paren_depth = line.chars().fold(0, |depth, ch| {
                match ch {
                    '(' => depth + 1,
                    ')' => (depth - 1).max(0),
                    _ => depth,
                }
            });
            
            if paren_depth > 5 {
                diagnostics.push(Diagnostic {
                    range: Range {
                        start: Position { line: line_idx as u32, character: 0 },
                        end: Position { line: line_idx as u32, character: line.len() as u32 },
                    },
                    severity: Some(DiagnosticSeverity::INFORMATION),
                    code: Some(NumberOrString::String("SYN002".to_string())),
                    source: Some("pact-parser".to_string()),
                    message: format!("Deeply nested expression (depth: {}). Consider refactoring.", paren_depth),
                    related_information: None,
                    tags: None,
                    code_description: None,
                    data: None,
                });
            }
        }
    }
    
    /// Check indentation consistency
    fn check_indentation(&self, text: &str, diagnostics: &mut Vec<Diagnostic>) {
        let lines: Vec<&str> = text.lines().collect();
        let mut expected_indent = 0;
        
        for (line_idx, line) in lines.iter().enumerate() {
            if line.trim().is_empty() {
                continue;
            }
            
            let actual_indent = line.len() - line.trim_start().len();
            
            // Simple indentation check for Lisp-style code
            if line.trim_start().starts_with('(') && !line.trim().starts_with(";;") {
                // Opening paren - should align properly
                if actual_indent != expected_indent && line_idx > 0 {
                    diagnostics.push(Diagnostic {
                        range: Range {
                            start: Position { line: line_idx as u32, character: 0 },
                            end: Position { line: line_idx as u32, character: actual_indent as u32 },
                        },
                        severity: Some(DiagnosticSeverity::INFORMATION),
                        code: Some(NumberOrString::String("STYLE001".to_string())),
                        source: Some("pact-style".to_string()),
                        message: format!("Inconsistent indentation. Expected {} spaces, got {}", expected_indent, actual_indent),
                        related_information: None,
                        tags: None,
                        code_description: None,
                        data: None,
                    });
                }
                expected_indent += 2; // Standard Lisp indentation
            }
        }
    }
    
    /// Check line length
    fn check_line_length(&self, text: &str, diagnostics: &mut Vec<Diagnostic>) {
        const MAX_LINE_LENGTH: usize = 100;
        
        for (line_idx, line) in text.lines().enumerate() {
            if line.len() > MAX_LINE_LENGTH {
                diagnostics.push(Diagnostic {
                    range: Range {
                        start: Position { line: line_idx as u32, character: MAX_LINE_LENGTH as u32 },
                        end: Position { line: line_idx as u32, character: line.len() as u32 },
                    },
                    severity: Some(DiagnosticSeverity::INFORMATION),
                    code: Some(NumberOrString::String("STYLE002".to_string())),
                    source: Some("pact-style".to_string()),
                    message: format!("Line too long ({} characters). Consider breaking into multiple lines.", line.len()),
                    related_information: None,
                    tags: None,
                    code_description: None,
                    data: None,
                });
            }
        }
    }
    
    /// Check naming conventions
    fn check_naming_conventions(&self, text: &str, diagnostics: &mut Vec<Diagnostic>) {
        // Check for kebab-case vs camelCase vs snake_case
        for (line_idx, line) in text.lines().enumerate() {
            if line.contains("defun") || line.contains("defcap") {
                // Extract function name and check convention
                if let Some(name_match) = self.extract_function_name(line) {
                    if name_match.contains('_') && name_match.contains('-') {
                        diagnostics.push(Diagnostic {
                            range: Range {
                                start: Position { line: line_idx as u32, character: 0 },
                                end: Position { line: line_idx as u32, character: line.len() as u32 },
                            },
                            severity: Some(DiagnosticSeverity::HINT),
                            code: Some(NumberOrString::String("STYLE003".to_string())),
                            source: Some("pact-style".to_string()),
                            message: "Mixed naming conventions. Prefer kebab-case for Pact functions.".to_string(),
                            related_information: None,
                            tags: None,
                            code_description: None,
                            data: None,
                        });
                    }
                }
            }
        }
    }
    
    /// Check for documentation
    fn check_documentation(&self, text: &str, diagnostics: &mut Vec<Diagnostic>) {
        for (line_idx, line) in text.lines().enumerate() {
            if line.contains("defun") && !text.lines().nth(line_idx.saturating_sub(1)).unwrap_or("").contains("@doc") {
                diagnostics.push(Diagnostic {
                    range: Range {
                        start: Position { line: line_idx as u32, character: 0 },
                        end: Position { line: line_idx as u32, character: line.len() as u32 },
                    },
                    severity: Some(DiagnosticSeverity::HINT),
                    code: Some(NumberOrString::String("DOC001".to_string())),
                    source: Some("pact-docs".to_string()),
                    message: "Function lacks documentation. Consider adding @doc annotation.".to_string(),
                    related_information: None,
                    tags: None,
                    code_description: None,
                    data: None,
                });
            }
        }
    }
    
    /// Check security patterns
    fn check_security_patterns(&self, text: &str, diagnostics: &mut Vec<Diagnostic>) {
        for (line_idx, line) in text.lines().enumerate() {
            // Check for potential security issues
            if line.contains("read") && !line.contains("with-capability") {
                diagnostics.push(Diagnostic {
                    range: Range {
                        start: Position { line: line_idx as u32, character: 0 },
                        end: Position { line: line_idx as u32, character: line.len() as u32 },
                    },
                    severity: Some(DiagnosticSeverity::WARNING),
                    code: Some(NumberOrString::String("SEC001".to_string())),
                    source: Some("pact-security".to_string()),
                    message: "Database read without capability check. Consider wrapping with appropriate capability.".to_string(),
                    related_information: None,
                    tags: None,
                    code_description: None,
                    data: None,
                });
            }
        }
    }
    
    /// Check performance hints
    fn check_performance_hints(&self, text: &str, diagnostics: &mut Vec<Diagnostic>) {
        for (line_idx, line) in text.lines().enumerate() {
            // Check for potentially expensive operations
            if line.contains("map") && line.contains("filter") {
                diagnostics.push(Diagnostic {
                    range: Range {
                        start: Position { line: line_idx as u32, character: 0 },
                        end: Position { line: line_idx as u32, character: line.len() as u32 },
                    },
                    severity: Some(DiagnosticSeverity::HINT),
                    code: Some(NumberOrString::String("PERF001".to_string())),
                    source: Some("pact-performance".to_string()),
                    message: "Consider combining map and filter operations for better performance.".to_string(),
                    related_information: None,
                    tags: None,
                    code_description: None,
                    data: None,
                });
            }
        }
    }
    
    /// Helper: Check if diagnostics contain syntax errors
    fn has_syntax_errors(&self, diagnostics: &[Diagnostic]) -> bool {
        diagnostics.iter().any(|d| {
            d.severity == Some(DiagnosticSeverity::ERROR) && 
            d.source.as_ref().map_or(false, |s| s.contains("parser") || s.contains("lexer"))
        })
    }
    
    /// Helper: Extract error position from error message
    fn extract_error_position(&self, error_message: &str) -> (u32, u32) {
        // Simple heuristic to extract line/column from error message
        // In a real implementation, the parser would provide structured error info
        (0, 0) // Default to start of file
    }
    
    /// Helper: Extract function name from defun line
    fn extract_function_name(&self, line: &str) -> Option<String> {
        if let Some(start) = line.find("defun") {
            let after_defun = &line[start + 5..];
            let trimmed = after_defun.trim_start();
            if let Some(space_idx) = trimmed.find(' ') {
                Some(trimmed[..space_idx].to_string())
            } else {
                None
            }
        } else {
            None
        }
    }
    
    /// Get semantic analyzer for other components
    pub fn semantic_analyzer(&self) -> Arc<SemanticAnalyzer> {
        self.semantic_analyzer.clone()
    }
}

/// Legacy function for backward compatibility
pub fn analyze(text: &str) -> Vec<Diagnostic> {
    let analyzer = DiagnosticsAnalyzer::new();
    let dummy_uri = Url::parse("file:///temp.pact").unwrap();
    analyzer.analyze(&dummy_uri, text)
}

/// Convert byte offset to line/column position
fn to_line_col(offset: usize, text: &str) -> (u32, u32) {
    let mut line = 0;
    let mut col = 0;
    
    for (i, ch) in text.char_indices() {
        if i == offset {
            return (line, col);
        }
        
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    
    (line, col)
}