//! Code actions and refactoring support for Pact LSP
//!
//! This module provides intelligent code actions including quick fixes,
//! refactoring operations, and smart suggestions for Pact code.

use crate::semantic::SemanticAnalyzer;
use tower_lsp::lsp_types::*;
use std::sync::Arc;

/// Code action provider with refactoring capabilities
pub struct CodeActionProvider {
    semantic_analyzer: Arc<SemanticAnalyzer>,
}

impl CodeActionProvider {
    /// Create a new code action provider
    pub fn new(semantic_analyzer: Arc<SemanticAnalyzer>) -> Self {
        Self { semantic_analyzer }
    }
    
    /// Get available code actions for a range
    pub fn get_code_actions(
        &self,
        uri: &Url,
        text: &str,
        range: Range,
        context: &CodeActionContext,
    ) -> Vec<CodeActionOrCommand> {
        let mut actions = Vec::new();
        
        // Quick fixes for diagnostics
        for diagnostic in &context.diagnostics {
            if let Some(quick_fix) = self.create_quick_fix(uri, text, diagnostic) {
                actions.push(CodeActionOrCommand::CodeAction(quick_fix));
            }
        }
        
        // Refactoring actions
        actions.extend(self.get_refactoring_actions(uri, text, range));
        
        // Source actions (organize imports, format, etc.)
        if context.only.is_none() || context.only.as_ref().unwrap().contains(&CodeActionKind::SOURCE) {
            actions.extend(self.get_source_actions(uri, text));
        }
        
        actions
    }
    
    /// Create quick fix for a diagnostic
    fn create_quick_fix(&self, uri: &Url, text: &str, diagnostic: &Diagnostic) -> Option<CodeAction> {
        let source = diagnostic.source.as_ref()?;
        
        match source.as_str() {
            "pact-lexer" => self.create_lexer_quick_fix(uri, text, diagnostic),
            "pact-parser" => self.create_parser_quick_fix(uri, text, diagnostic),
            "pact-semantic" => self.create_semantic_quick_fix(uri, text, diagnostic),
            "pact-style" => self.create_style_quick_fix(uri, text, diagnostic),
            _ => None,
        }
    }
    
    /// Create quick fix for lexer errors
    fn create_lexer_quick_fix(&self, uri: &Url, text: &str, diagnostic: &Diagnostic) -> Option<CodeAction> {
        if diagnostic.message.contains("Unterminated string") {
            return Some(CodeAction {
                title: "Add closing quote".to_string(),
                kind: Some(CodeActionKind::QUICKFIX),
                diagnostics: Some(vec![diagnostic.clone()]),
                disabled: None,
                edit: Some(WorkspaceEdit {
                    changes: Some({
                        let mut changes = std::collections::HashMap::new();
                        changes.insert(uri.clone(), vec![TextEdit {
                            range: Range {
                                start: diagnostic.range.end,
                                end: diagnostic.range.end,
                            },
                            new_text: "\"".to_string(),
                        }]);
                        changes
                    }),
                    document_changes: None,
                    change_annotations: None,
                }),
                command: None,
                data: None,
                is_preferred: None,
            });
        }
        
        None
    }
    
    /// Create quick fix for parser errors
    fn create_parser_quick_fix(&self, uri: &Url, text: &str, diagnostic: &Diagnostic) -> Option<CodeAction> {
        if diagnostic.message.contains("missing closing parenthesis") {
            return Some(CodeAction {
                title: "Add closing parenthesis".to_string(),
                kind: Some(CodeActionKind::QUICKFIX),
                diagnostics: Some(vec![diagnostic.clone()]),
                disabled: None,
                edit: Some(WorkspaceEdit {
                    changes: Some({
                        let mut changes = std::collections::HashMap::new();
                        changes.insert(uri.clone(), vec![TextEdit {
                            range: Range {
                                start: diagnostic.range.end,
                                end: diagnostic.range.end,
                            },
                            new_text: ")".to_string(),
                        }]);
                        changes
                    }),
                    document_changes: None,
                    change_annotations: None,
                }),
                command: None,
                data: None,
                is_preferred: None,
            });
        }
        
        None
    }
    
    /// Create quick fix for semantic errors
    fn create_semantic_quick_fix(&self, uri: &Url, text: &str, diagnostic: &Diagnostic) -> Option<CodeAction> {
        if diagnostic.message.contains("Undefined symbol") {
            // Extract the undefined symbol name
            if let Some(symbol_name) = self.extract_symbol_from_message(&diagnostic.message) {
                // Get suggestions from related information
                if let Some(related) = &diagnostic.related_information {
                    for info in related {
                        if info.message.starts_with("Suggestion:") {
                            let suggestion = info.message.strip_prefix("Suggestion: ").unwrap_or("");
                            return Some(CodeAction {
                                title: format!("Replace with '{}'", suggestion),
                                kind: Some(CodeActionKind::QUICKFIX),
                                diagnostics: Some(vec![diagnostic.clone()]),
                                                disabled: None,
                                edit: Some(WorkspaceEdit {
                                    changes: Some({
                                        let mut changes = std::collections::HashMap::new();
                                        changes.insert(uri.clone(), vec![TextEdit {
                                            range: diagnostic.range,
                                            new_text: suggestion.to_string(),
                                        }]);
                                        changes
                                    }),
                                    document_changes: None,
                                    change_annotations: None,
                                }),
                                command: None,
                                data: None,
                                is_preferred: None,
                            });
                        }
                    }
                }
            }
        }
        
        None
    }
    
    /// Create quick fix for style issues
    fn create_style_quick_fix(&self, uri: &Url, text: &str, diagnostic: &Diagnostic) -> Option<CodeAction> {
        if diagnostic.message.contains("Function lacks documentation") {
            // Extract function name from the line
            let lines: Vec<&str> = text.lines().collect();
            let line_idx = diagnostic.range.start.line as usize;
            
            if line_idx < lines.len() {
                let line = lines[line_idx];
                if let Some(func_name) = self.extract_function_name(line) {
                    let doc_comment = format!("@doc \"TODO: Document the {} function\"\n", func_name);
                    
                    return Some(CodeAction {
                        title: "Add documentation comment".to_string(),
                        kind: Some(CodeActionKind::QUICKFIX),
                        diagnostics: Some(vec![diagnostic.clone()]),
                                disabled: None,
                        edit: Some(WorkspaceEdit {
                            changes: Some({
                                let mut changes = std::collections::HashMap::new();
                                changes.insert(uri.clone(), vec![TextEdit {
                                    range: Range {
                                        start: Position { line: diagnostic.range.start.line, character: 0 },
                                        end: Position { line: diagnostic.range.start.line, character: 0 },
                                    },
                                    new_text: doc_comment,
                                }]);
                                changes
                            }),
                            document_changes: None,
                            change_annotations: None,
                        }),
                        command: None,
                        data: None,
                        is_preferred: None,
                    });
                }
            }
        }
        
        None
    }
    
    /// Get refactoring actions for a range
    fn get_refactoring_actions(&self, uri: &Url, text: &str, range: Range) -> Vec<CodeActionOrCommand> {
        let mut actions = Vec::new();
        
        // Extract to function
        if !range.start.eq(&range.end) {
            actions.push(CodeActionOrCommand::CodeAction(self.create_extract_function_action(uri, text, range)));
        }
        
        // Inline variable/function
        if let Some(symbol_name) = self.get_symbol_at_range(text, range) {
            if let Some(action) = self.create_inline_action(uri, text, &symbol_name, range) {
                actions.push(CodeActionOrCommand::CodeAction(action));
            }
        }
        
        // Rename symbol
        if let Some(symbol_name) = self.get_symbol_at_range(text, range) {
            actions.push(CodeActionOrCommand::CodeAction(self.create_rename_action(&symbol_name, range)));
        }
        
        // Convert to/from lambda
        if let Some(action) = self.create_lambda_conversion_action(uri, text, range) {
            actions.push(CodeActionOrCommand::CodeAction(action));
        }
        
        // Simplify boolean expressions
        if let Some(action) = self.create_boolean_simplification_action(uri, text, range) {
            actions.push(CodeActionOrCommand::CodeAction(action));
        }
        
        actions
    }
    
    /// Get source actions (organize imports, format, etc.)
    fn get_source_actions(&self, uri: &Url, text: &str) -> Vec<CodeActionOrCommand> {
        let mut actions = Vec::new();
        
        // Organize imports
        actions.push(CodeActionOrCommand::CodeAction(CodeAction {
            title: "Organize imports".to_string(),
            kind: Some(CodeActionKind::SOURCE_ORGANIZE_IMPORTS),
            diagnostics: None,
            disabled: None,
            edit: Some(self.create_organize_imports_edit(uri, text)),
            command: None,
            data: None,
            is_preferred: None,
        }));
        
        // Add missing imports
        if let Some(edit) = self.create_add_missing_imports_edit(uri, text) {
            actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                title: "Add missing imports".to_string(),
                kind: Some(CodeActionKind::SOURCE_FIX_ALL),
                diagnostics: None,
                disabled: None,
                edit: Some(edit),
                command: None,
                data: None,
                is_preferred: None,
            }));
        }
        
        // Remove unused imports
        if let Some(edit) = self.create_remove_unused_imports_edit(uri, text) {
            actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                title: "Remove unused imports".to_string(),
                kind: Some(CodeActionKind::SOURCE_FIX_ALL),
                diagnostics: None,
                disabled: None,
                edit: Some(edit),
                command: None,
                data: None,
                is_preferred: None,
            }));
        }
        
        actions
    }
    
    /// Create extract function action
    fn create_extract_function_action(&self, uri: &Url, text: &str, range: Range) -> CodeAction {
        let lines: Vec<&str> = text.lines().collect();
        let start_line = range.start.line as usize;
        let end_line = range.end.line as usize;
        
        // Extract the selected text
        let mut selected_text = String::new();
        for i in start_line..=end_line.min(lines.len() - 1) {
            if i > start_line {
                selected_text.push('\n');
            }
            
            let line = lines[i];
            if i == start_line && i == end_line {
                // Single line selection
                let start_char = range.start.character as usize;
                let end_char = range.end.character as usize;
                if start_char < line.len() && end_char <= line.len() {
                    selected_text.push_str(&line[start_char..end_char]);
                }
            } else if i == start_line {
                // First line of multi-line selection
                let start_char = range.start.character as usize;
                if start_char < line.len() {
                    selected_text.push_str(&line[start_char..]);
                }
            } else if i == end_line {
                // Last line of multi-line selection
                let end_char = range.end.character as usize;
                if end_char <= line.len() {
                    selected_text.push_str(&line[..end_char]);
                }
            } else {
                // Middle lines
                selected_text.push_str(line);
            }
        }
        
        let function_name = "extracted-function";
        let function_def = format!("(defun {} ()\n  {})\n\n", function_name, selected_text);
        let function_call = format!("({})", function_name);
        
        // Find a good place to insert the function (before current function)
        let insert_position = self.find_function_insert_position(&lines, start_line);
        
        CodeAction {
            title: "Extract to function".to_string(),
            kind: Some(CodeActionKind::REFACTOR_EXTRACT),
            diagnostics: None,
            disabled: None,
            edit: Some(WorkspaceEdit {
                changes: Some({
                    let mut changes = std::collections::HashMap::new();
                    changes.insert(uri.clone(), vec![
                        // Insert function definition
                        TextEdit {
                            range: Range {
                                start: Position { line: insert_position, character: 0 },
                                end: Position { line: insert_position, character: 0 },
                            },
                            new_text: function_def,
                        },
                        // Replace selected text with function call
                        TextEdit {
                            range,
                            new_text: function_call,
                        },
                    ]);
                    changes
                }),
                document_changes: None,
                change_annotations: None,
            }),
            command: None,
            data: None,
            is_preferred: None,
        }
    }
    
    /// Create inline action
    fn create_inline_action(&self, uri: &Url, text: &str, symbol_name: &str, range: Range) -> Option<CodeAction> {
        // For now, create a simple inline action
        // In a real implementation, this would analyze the symbol definition and inline it
        Some(CodeAction {
            title: format!("Inline '{}'", symbol_name),
            kind: Some(CodeActionKind::REFACTOR_INLINE),
            diagnostics: None,
            disabled: None,
            edit: None,
            command: None,
            data: None,
            is_preferred: None,
        })
    }
    
    /// Create rename action
    fn create_rename_action(&self, symbol_name: &str, range: Range) -> CodeAction {
        CodeAction {
            title: format!("Rename '{}'", symbol_name),
            kind: Some(CodeActionKind::REFACTOR),
            diagnostics: None,
            disabled: None,
            edit: None,
            command: Some(Command {
                title: "Rename Symbol".to_string(),
                command: "editor.action.rename".to_string(),
                arguments: None,
            }),
            data: None,
            is_preferred: None,
        }
    }
    
    /// Create lambda conversion action
    fn create_lambda_conversion_action(&self, uri: &Url, text: &str, range: Range) -> Option<CodeAction> {
        // Detect if the selection contains a function that can be converted to lambda
        let lines: Vec<&str> = text.lines().collect();
        let line_idx = range.start.line as usize;
        
        if line_idx < lines.len() {
            let line = lines[line_idx];
            if line.trim().starts_with("(defun") {
                return Some(CodeAction {
                    title: "Convert to lambda".to_string(),
                    kind: Some(CodeActionKind::REFACTOR),
                    diagnostics: None,
                        disabled: None,
                    edit: None,
                    command: None,
                    data: None,
                    is_preferred: None,
                });
            }
        }
        
        None
    }
    
    /// Create boolean simplification action
    fn create_boolean_simplification_action(&self, uri: &Url, text: &str, range: Range) -> Option<CodeAction> {
        // Detect boolean expressions that can be simplified
        let lines: Vec<&str> = text.lines().collect();
        let line_idx = range.start.line as usize;
        
        if line_idx < lines.len() {
            let line = lines[line_idx];
            if line.contains("(not (not ") {
                // Double negation
                return Some(CodeAction {
                    title: "Simplify double negation".to_string(),
                    kind: Some(CodeActionKind::REFACTOR),
                    diagnostics: None,
                        disabled: None,
                    edit: Some(WorkspaceEdit {
                        changes: Some({
                            let mut changes = std::collections::HashMap::new();
                            // This is a simplified example - real implementation would parse the expression
                            let simplified = line.replace("(not (not ", "(").replace("))", ")");
                            changes.insert(uri.clone(), vec![TextEdit {
                                range: Range {
                                    start: Position { line: line_idx as u32, character: 0 },
                                    end: Position { line: line_idx as u32, character: line.len() as u32 },
                                },
                                new_text: simplified,
                            }]);
                            changes
                        }),
                        document_changes: None,
                        change_annotations: None,
                    }),
                    command: None,
                    data: None,
                    is_preferred: None,
                });
            }
        }
        
        None
    }
    
    /// Create organize imports edit
    fn create_organize_imports_edit(&self, uri: &Url, text: &str) -> WorkspaceEdit {
        // For now, just return an empty edit
        // Real implementation would sort and organize use statements
        WorkspaceEdit {
            changes: Some(std::collections::HashMap::new()),
            document_changes: None,
            change_annotations: None,
        }
    }
    
    /// Create add missing imports edit
    fn create_add_missing_imports_edit(&self, uri: &Url, text: &str) -> Option<WorkspaceEdit> {
        // Analyze undefined symbols and suggest imports
        // For now, return None
        None
    }
    
    /// Create remove unused imports edit
    fn create_remove_unused_imports_edit(&self, uri: &Url, text: &str) -> Option<WorkspaceEdit> {
        // Analyze which imports are unused
        // For now, return None
        None
    }
    
    // Helper methods
    
    /// Extract symbol name from diagnostic message
    fn extract_symbol_from_message(&self, message: &str) -> Option<String> {
        if let Some(start) = message.find("Undefined symbol: ") {
            let symbol_part = &message[start + 18..];
            if let Some(end) = symbol_part.find(' ') {
                Some(symbol_part[..end].to_string())
            } else {
                Some(symbol_part.to_string())
            }
        } else {
            None
        }
    }
    
    /// Extract function name from a line
    fn extract_function_name(&self, line: &str) -> Option<String> {
        if let Some(start) = line.find("defun ") {
            let after_defun = &line[start + 6..];
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
    
    /// Get symbol at range
    fn get_symbol_at_range(&self, text: &str, range: Range) -> Option<String> {
        let lines: Vec<&str> = text.lines().collect();
        let line_idx = range.start.line as usize;
        
        if line_idx >= lines.len() {
            return None;
        }
        
        let line = lines[line_idx];
        let start_char = range.start.character as usize;
        let end_char = range.end.character as usize;
        
        if start_char < line.len() && end_char <= line.len() && start_char < end_char {
            Some(line[start_char..end_char].to_string())
        } else {
            None
        }
    }
    
    /// Find good position to insert a function
    fn find_function_insert_position(&self, lines: &[&str], current_line: usize) -> u32 {
        // Look backwards for the start of the current function
        for i in (0..current_line).rev() {
            if lines[i].trim().starts_with("(defun") {
                return i as u32;
            }
        }
        
        // Default to beginning of file
        0
    }
}

impl Default for CodeActionProvider {
    fn default() -> Self {
        Self::new(Arc::new(crate::semantic::SemanticAnalyzer::new()))
    }
}

/// Legacy function for backward compatibility
pub fn get_code_actions(text: &str, range: Range, context: &CodeActionContext) -> Vec<CodeActionOrCommand> {
    let provider = CodeActionProvider::default();
    let dummy_uri = Url::parse("file:///temp.pact").unwrap();
    provider.get_code_actions(&dummy_uri, text, range, context)
}