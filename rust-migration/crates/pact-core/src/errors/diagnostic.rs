//! Diagnostic rendering for errors with source context
//!
//! This module provides pretty printing of errors with:
//! - Source code snippets
//! - Highlighted error locations
//! - Call stack traces
//! - Helpful suggestions

use super::{PactError, StackFrame};
use crate::shared::SpanInfo;
use std::fmt::Write;

/// Diagnostic renderer configuration
pub struct DiagnosticConfig {
    /// Number of context lines before/after error
    pub context_lines: usize,
    /// Use unicode characters
    pub use_unicode: bool,
    /// Use ANSI colors
    pub use_colors: bool,
    /// Show source snippets
    pub show_source: bool,
    /// Show call stack
    pub show_stack: bool,
}

impl Default for DiagnosticConfig {
    fn default() -> Self {
        DiagnosticConfig {
            context_lines: 2,
            use_unicode: true,
            use_colors: true,
            show_source: true,
            show_stack: true,
        }
    }
}

/// Source information for rendering
pub struct SourceInfo<'a> {
    /// Source file name/path
    pub filename: &'a str,
    /// Full source text
    pub source: &'a str,
}

/// Render a PactError with diagnostic information
pub fn render_diagnostic(
    error: &PactError<SpanInfo>,
    source: Option<&SourceInfo<'_>>,
    config: &DiagnosticConfig,
) -> String {
    let mut output = String::new();
    
    // Main error message
    write_error_header(&mut output, error, config);
    
    // Source snippet if available
    if let Some(src) = source {
        if config.show_source {
            write_source_snippet(&mut output, error.info(), src, config);
        }
    }
    
    // Call stack for execution errors
    match error {
        PactError::PEExecutionError(_, stack, _) |
        PactError::PEUserRecoverableError(_, stack, _) => {
            if config.show_stack && !stack.is_empty() {
                write_call_stack(&mut output, stack, config);
            }
        }
        _ => {}
    }
    
    // Suggestions
    if let Some(suggestion) = get_error_suggestion(error) {
        write_suggestion(&mut output, &suggestion, config);
    }
    
    output
}

/// Write the main error header
fn write_error_header(
    output: &mut String,
    error: &PactError<SpanInfo>,
    config: &DiagnosticConfig,
) {
    let error_type = match error {
        PactError::PELexerError(_, _) => "lexer error",
        PactError::PEParseError(_, _) => "parse error",
        PactError::PEDesugarError(_, _) => "desugar error",
        PactError::PEExecutionError(_, _, _) => "runtime error",
        PactError::PEUserRecoverableError(_, _, _) => "user error",
        PactError::PEVerifierError(_, _) => "verifier error",
    };
    
    if config.use_colors {
        use colored::*;
        writeln!(output, "{}: {}", 
            error_type.bright_red().bold(),
            error.to_string().bold()
        ).unwrap();
    } else {
        writeln!(output, "{}: {}", error_type, error).unwrap();
    }
}

/// Write source code snippet with error highlighting
fn write_source_snippet(
    output: &mut String,
    span: &SpanInfo,
    source_info: &SourceInfo<'_>,
    config: &DiagnosticConfig,
) {
    let lines: Vec<&str> = source_info.source.lines().collect();
    
    // Use line/column information directly from SpanInfo
    let start_line = span.start_line.saturating_sub(1); // Convert to 0-based
    let start_col = span.start_column;
    let end_line = span.end_line.saturating_sub(1); // Convert to 0-based
    let end_col = span.end_column;
    
    // Calculate line range to show
    let context_start = start_line.saturating_sub(config.context_lines);
    let context_end = (end_line + config.context_lines).min(lines.len() - 1);
    
    writeln!(output).unwrap();
    
    // File location
    if config.use_unicode {
        writeln!(output, "  ╭─[{}:{}:{}]", 
            source_info.filename, start_line + 1, start_col + 1).unwrap();
    } else {
        writeln!(output, "  --> {}:{}:{}", 
            source_info.filename, start_line + 1, start_col + 1).unwrap();
    }
    
    // Source lines
    for (_idx, line_num) in (context_start..=context_end).enumerate() {
        if line_num >= lines.len() {
            break;
        }
        
        let line = lines[line_num];
        let display_num = line_num + 1;
        
        // Line number and separator
        if config.use_unicode {
            write!(output, "  │ ").unwrap();
        } else {
            write!(output, "  | ").unwrap();
        }
        
        // Gutter with line number
        if line_num >= start_line && line_num <= end_line {
            // Error line - highlight line number
            if config.use_colors {
                use colored::*;
                let sep = if config.use_unicode { "│" } else { "|" };
                write!(output, "{:>4} {} ", display_num.to_string().bright_red().bold(), sep).unwrap();
            } else {
                let sep = if config.use_unicode { "│" } else { "|" };
                write!(output, "{:>4} {} ", display_num, sep).unwrap();
            }
        } else {
            // Context line
            let sep = if config.use_unicode { "│" } else { "|" };
            write!(output, "{:>4} {} ", display_num, sep).unwrap();
        }
        
        // Line content
        writeln!(output, "{}", line).unwrap();
        
        // Error underline
        if line_num >= start_line && line_num <= end_line {
            let sep = if config.use_unicode { "│" } else { "|" };
            write!(output, "  {}      ", sep).unwrap();
            
            let underline_start = if line_num == start_line { start_col } else { 0 };
            let underline_end = if line_num == end_line { end_col } else { line.len() };
            
            // Spaces before underline
            for _ in 0..underline_start {
                write!(output, " ").unwrap();
            }
            
            // Underline
            if config.use_colors {
                use colored::*;
                for _ in underline_start..underline_end {
                    write!(output, "{}", "^".bright_red().bold()).unwrap();
                }
            } else {
                for _ in underline_start..underline_end {
                    write!(output, "^").unwrap();
                }
            }
            
            writeln!(output).unwrap();
        }
    }
    
    // Bottom border
    if config.use_unicode {
        writeln!(output, "  ╰────").unwrap();
    } else {
        writeln!(output, "  ----").unwrap();
    }
}

/// Write call stack trace
fn write_call_stack(
    output: &mut String,
    stack: &[StackFrame<SpanInfo>],
    config: &DiagnosticConfig,
) {
    writeln!(output).unwrap();
    if config.use_colors {
        use colored::*;
        writeln!(output, "{}", "Call stack:".bright_cyan().bold()).unwrap();
    } else {
        writeln!(output, "Call stack:").unwrap();
    }
    
    for (_idx, frame) in stack.iter().enumerate() {
        let indent = "  ";
        let arrow = if config.use_unicode { "⮑" } else { "<-" };
        
        write!(output, "{}{} ", indent, arrow).unwrap();
        
        if config.use_colors {
            use colored::*;
            write!(output, "{} ", frame.fn_type.to_string().bright_green()).unwrap();
            write!(output, "{}", frame.name.to_string().bright_yellow().bold()).unwrap();
        } else {
            write!(output, "{} {}", frame.fn_type, frame.name).unwrap();
        }
        
        // Arguments
        if !frame.args.is_empty() {
            write!(output, "(").unwrap();
            for (i, arg) in frame.args.iter().enumerate() {
                if i > 0 {
                    write!(output, ", ").unwrap();
                }
                write!(output, "{}", arg).unwrap();
            }
            write!(output, ")").unwrap();
        }
        
        writeln!(output).unwrap();
    }
}

/// Write suggestion
fn write_suggestion(output: &mut String, suggestion: &str, config: &DiagnosticConfig) {
    writeln!(output).unwrap();
    if config.use_colors {
        use colored::*;
        writeln!(output, "{}: {}", "help".bright_cyan().bold(), suggestion).unwrap();
    } else {
        writeln!(output, "help: {}", suggestion).unwrap();
    }
}

/// Get helpful suggestion for an error
fn get_error_suggestion(error: &PactError<SpanInfo>) -> Option<String> {
    use super::{EvalError, ParseError, LexerError};
    
    match error {
        // Update to match new EvalError structure - these variants no longer exist in the same form
        // We can add more specific suggestions based on the complete error set later
        PactError::PEExecutionError(EvalError::InvariantFailure(_), _, _) => {
            Some("this indicates an internal consistency error - please report this".to_string())
        }
        PactError::PEExecutionError(EvalError::NativeArgumentsError { name, errors: _ }, _, _) => {
            Some(format!("check the argument types for native function '{}'", name))
        }
        PactError::PEExecutionError(EvalError::RunTimeTypecheckFailure { error: _, typ: _ }, _, _) => {
            Some("ensure the value matches the expected type".to_string())
        }
        PactError::PEParseError(ParseError::UnexpectedToken { expected, .. }, _) => {
            Some(format!("expected {}", expected))
        }
        PactError::PELexerError(LexerError::UnterminatedString, _) => {
            Some("add a closing quote (\") to terminate the string".to_string())
        }
        _ => None,
    }
}

/// Render multiple errors (useful for batch operations)
pub fn render_diagnostics(
    errors: &[PactError<SpanInfo>],
    source: Option<SourceInfo<'_>>,
    config: &DiagnosticConfig,
) -> String {
    let mut output = String::new();
    
    for (idx, error) in errors.iter().enumerate() {
        if idx > 0 {
            writeln!(output, "\n{}", "─".repeat(60)).unwrap();
        }
        output.push_str(&render_diagnostic(error, source.as_ref(), config));
    }
    
    output
}