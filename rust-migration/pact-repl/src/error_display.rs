//! Error display for the REPL using the new error system

use pact_errors::{PactErrorI, DiagnosticConfig, SourceInfo, render_diagnostic};

/// Display an error in the REPL with full diagnostics
pub fn display_error(error: &PactErrorI, source: &str) -> String {
    let config = DiagnosticConfig {
        context_lines: 2,
        use_unicode: true,
        use_colors: true,
        show_source: true,
        show_stack: true,
    };
    
    let source_info = SourceInfo {
        filename: "<repl>",
        source,
    };
    
    render_diagnostic(error, Some(&source_info), &config)
}