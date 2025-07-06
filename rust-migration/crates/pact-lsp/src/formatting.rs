//! Code formatting implementation for Pact
//!
//! This module provides intelligent code formatting that follows Pact
//! conventions and best practices for smart contract development.

use tower_lsp::lsp_types::*;
use pact_syntax::{lex, Token};
use pact_syntax::{parse, ParsedTopLevel, ParsedDef, ParsedExpr, Binder, MArg, Type, Field, ParsedModule, ParsedInterface, Import, Governance};
use std::collections::VecDeque;

/// Pact code formatter with configurable options
pub struct PactFormatter {
    config: FormattingConfig,
}

/// Configuration options for formatting
#[derive(Debug, Clone)]
pub struct FormattingConfig {
    /// Number of spaces for indentation (default: 2)
    pub indent_size: usize,
    /// Use spaces instead of tabs (default: true)
    pub use_spaces: bool,
    /// Maximum line length before wrapping (default: 100)
    pub max_line_length: usize,
    /// Add trailing commas in lists/objects (default: false)
    pub trailing_commas: bool,
    /// Align function parameters vertically (default: true)
    pub align_parameters: bool,
    /// Keep empty lines between definitions (default: true)
    pub preserve_blank_lines: bool,
    /// Sort module imports (default: true)
    pub sort_imports: bool,
}

impl Default for FormattingConfig {
    fn default() -> Self {
        Self {
            indent_size: 2,
            use_spaces: true,
            max_line_length: 100,
            trailing_commas: false,
            align_parameters: true,
            preserve_blank_lines: true,
            sort_imports: true,
        }
    }
}

impl PactFormatter {
    /// Create a new formatter with default configuration
    pub fn new() -> Self {
        Self {
            config: FormattingConfig::default(),
        }
    }
    
    /// Create a new formatter with custom configuration
    pub fn with_config(config: FormattingConfig) -> Self {
        Self { config }
    }
    
    /// Format entire document
    pub fn format_document(&self, text: &str) -> Result<Vec<TextEdit>, String> {
        // Parse the document to understand structure
        match parse(text) {
            Ok(program) => {
                let formatted = self.format_program(text, &program)?;
                if formatted == text {
                    // No changes needed
                    Ok(vec![])
                } else {
                    // Replace entire document
                    let lines: Vec<&str> = text.lines().collect();
                    let end_line = lines.len().saturating_sub(1) as u32;
                    let end_char = lines.last().map(|l| l.len()).unwrap_or(0) as u32;
                    
                    Ok(vec![TextEdit {
                        range: Range {
                            start: Position { line: 0, character: 0 },
                            end: Position { line: end_line, character: end_char },
                        },
                        new_text: formatted,
                    }])
                }
            }
            Err(e) => {
                // If parsing fails, try basic formatting
                Ok(self.format_basic(text))
            }
        }
    }
    
    /// Format a range of text
    pub fn format_range(&self, text: &str, range: Range) -> Result<Vec<TextEdit>, String> {
        let lines: Vec<&str> = text.lines().collect();
        let start_line = range.start.line as usize;
        let end_line = range.end.line as usize;
        
        if start_line >= lines.len() || end_line >= lines.len() {
            return Ok(vec![]);
        }
        
        // Extract the range text
        let range_lines = &lines[start_line..=end_line];
        let range_text = range_lines.join("\n");
        
        // Format the extracted range
        let formatted = self.format_text_block(&range_text)?;
        
        if formatted == range_text {
            Ok(vec![])
        } else {
            Ok(vec![TextEdit {
                range,
                new_text: formatted,
            }])
        }
    }
    
    /// Format a complete program AST
    fn format_program(&self, original_text: &str, program: &Program) -> Result<String, String> {
        let mut output = String::new();
        let mut first_item = true;
        
        for item in &program.items {
            if !first_item && self.config.preserve_blank_lines {
                output.push_str("\n\n");
            }
            
            output.push_str(&self.format_top_level_item(item)?);
            first_item = false;
        }
        
        // Ensure file ends with newline
        if !output.ends_with('\n') {
            output.push('\n');
        }
        
        Ok(output)
    }
    
    /// Format a top-level item
    fn format_top_level_item(&self, item: &TopLevel) -> Result<String, String> {
        match item {
            TopLevel::Module(module) => self.format_module(module),
            TopLevel::Interface(interface) => self.format_interface(interface),
            TopLevel::Expression(expr) => self.format_expression(expr, 0),
            TopLevel::Use(use_stmt) => self.format_use_statement(use_stmt),
            TopLevel::Declaration(decl) => self.format_declaration(decl, 0),
        }
    }
    
    /// Format a module definition
    fn format_module(&self, module: &Module) -> Result<String, String> {
        let mut output = String::new();
        
        // Module header
        output.push_str("(module ");
        output.push_str(&module.name);
        output.push(' ');
        output.push_str(&self.format_governance(&module.governance)?);
        
        // Module metadata
        if let Some(meta) = &module.meta {
            if let Some(docs) = &meta.docs {
                output.push_str(&format!("\n  @doc \"{}\"", docs));
            }
            for model in &meta.model {
                output.push_str(&format!("\n  @model [{}]", model));
            }
        }
        
        output.push('\n');
        
        // Module body
        for (i, declaration) in module.declarations.iter().enumerate() {
            if i > 0 {
                output.push('\n');
            }
            output.push_str(&self.format_declaration(declaration, 1)?);
        }
        
        output.push_str("\n)");
        Ok(output)
    }
    
    /// Format module governance
    fn format_governance(&self, governance: &Governance) -> Result<String, String> {
        match governance {
            Governance::Keyset(name) => Ok(format!("'{}", name)),
            Governance::Capability(name) => Ok(format!("({})", name)),
        }
    }
    
    /// Format an interface definition
    fn format_interface(&self, interface: &Interface) -> Result<String, String> {
        let mut output = String::new();
        
        output.push_str("(interface ");
        output.push_str(&interface.name);
        output.push('\n');
        
        for (i, declaration) in interface.declarations.iter().enumerate() {
            if i > 0 {
                output.push('\n');
            }
            output.push_str(&self.format_declaration(declaration, 1)?);
        }
        
        output.push_str("\n)");
        Ok(output)
    }
    
    /// Format a use statement
    fn format_use_statement(&self, use_stmt: &Use) -> Result<String, String> {
        let mut output = String::new();
        output.push_str("(use ");
        output.push_str(&use_stmt.module);
        
        if let Some(imports) = &use_stmt.imports {
            output.push_str(" [");
            if self.config.sort_imports {
                let mut sorted_imports = imports.clone();
                sorted_imports.sort();
                for (i, import) in sorted_imports.iter().enumerate() {
                    if i > 0 {
                        output.push(' ');
                    }
                    output.push_str(import);
                }
            } else {
                for (i, import) in imports.iter().enumerate() {
                    if i > 0 {
                        output.push(' ');
                    }
                    output.push_str(import);
                }
            }
            output.push(']');
        }
        
        output.push(')');
        Ok(output)
    }
    
    /// Format a declaration
    fn format_declaration(&self, declaration: &Declaration, indent_level: usize) -> Result<String, String> {
        let indent = self.get_indent(indent_level);
        
        match declaration {
            Declaration::Defun(defun) => {
                let mut output = String::new();
                
                // Documentation
                if let Some(docs) = &defun.docs {
                    output.push_str(&format!("{}@doc \"{}\"\n", indent, docs));
                }
                
                // Function signature
                output.push_str(&format!("{}(defun {}", indent, defun.name));
                
                // Parameters
                if defun.params.is_empty() {
                    output.push_str(" ()");
                } else if self.config.align_parameters && self.should_wrap_parameters(&defun.params) {
                    output.push_str(" (");
                    for (i, param) in defun.params.iter().enumerate() {
                        if i > 0 {
                            output.push('\n');
                            output.push_str(&self.get_indent(indent_level + 1));
                        }
                        output.push_str(&self.format_parameter(param)?);
                    }
                    output.push(')');
                } else {
                    output.push_str(" (");
                    for (i, param) in defun.params.iter().enumerate() {
                        if i > 0 {
                            output.push(' ');
                        }
                        output.push_str(&self.format_parameter(param)?);
                    }
                    output.push(')');
                }
                
                // Return type
                if let Some(return_type) = &defun.return_type {
                    output.push_str(&format!(": {}", self.format_type(return_type)?));
                }
                
                // Function body
                for expr in &defun.body {
                    output.push('\n');
                    output.push_str(&self.format_expression(expr, indent_level + 1)?);
                }
                
                output.push(')');
                Ok(output)
            }
            Declaration::Defcap(defcap) => {
                let mut output = String::new();
                
                if let Some(docs) = &defcap.docs {
                    output.push_str(&format!("{}@doc \"{}\"\n", indent, docs));
                }
                
                output.push_str(&format!("{}(defcap {}", indent, defcap.name));
                
                // Parameters
                if defcap.params.is_empty() {
                    output.push_str(" ()");
                } else {
                    output.push_str(" (");
                    for (i, param) in defcap.params.iter().enumerate() {
                        if i > 0 {
                            output.push(' ');
                        }
                        output.push_str(&self.format_parameter(param)?);
                    }
                    output.push(')');
                }
                
                // Capability body
                for expr in &defcap.body {
                    output.push('\n');
                    output.push_str(&self.format_expression(expr, indent_level + 1)?);
                }
                
                output.push(')');
                Ok(output)
            }
            Declaration::Defconst(defconst) => {
                let mut output = String::new();
                
                if let Some(docs) = &defconst.docs {
                    output.push_str(&format!("{}@doc \"{}\"\n", indent, docs));
                }
                
                output.push_str(&format!("{}(defconst {}", indent, defconst.name));
                
                if let Some(type_ann) = &defconst.type_ann {
                    output.push_str(&format!(": {}", self.format_type(type_ann)?));
                }
                
                output.push('\n');
                output.push_str(&self.format_expression(&defconst.value, indent_level + 1)?);
                output.push(')');
                Ok(output)
            }
            Declaration::Defschema(defschema) => {
                let mut output = String::new();
                
                if let Some(docs) = &defschema.docs {
                    output.push_str(&format!("{}@doc \"{}\"\n", indent, docs));
                }
                
                output.push_str(&format!("{}(defschema {}", indent, defschema.name));
                
                for field in &defschema.fields {
                    output.push('\n');
                    output.push_str(&format!("{}{}: {}", 
                        self.get_indent(indent_level + 1),
                        field.name,
                        self.format_type(&field.ty)?
                    ));
                }
                
                output.push(')');
                Ok(output)
            }
            Declaration::Deftable(deftable) => {
                let mut output = String::new();
                
                if let Some(docs) = &deftable.docs {
                    output.push_str(&format!("{}@doc \"{}\"\n", indent, docs));
                }
                
                output.push_str(&format!("{}(deftable {}:{}", 
                    indent, 
                    deftable.name,
                    deftable.schema
                ));
                output.push(')');
                Ok(output)
            }
            Declaration::Defpact(defpact) => {
                let mut output = String::new();
                
                if let Some(docs) = &defpact.docs {
                    output.push_str(&format!("{}@doc \"{}\"\n", indent, docs));
                }
                
                output.push_str(&format!("{}(defpact {}", indent, defpact.name));
                
                // Parameters
                if defpact.params.is_empty() {
                    output.push_str(" ()");
                } else {
                    output.push_str(" (");
                    for (i, param) in defpact.params.iter().enumerate() {
                        if i > 0 {
                            output.push(' ');
                        }
                        output.push_str(&self.format_parameter(param)?);
                    }
                    output.push(')');
                }
                
                // Pact steps
                for step in &defpact.steps {
                    output.push('\n');
                    if let Some(name) = &step.name {
                        output.push_str(&format!("{}(step {})", self.get_indent(indent_level + 1), name));
                    }
                    for expr in &step.body {
                        output.push('\n');
                        output.push_str(&self.format_expression(expr, indent_level + 1)?);
                    }
                }
                
                output.push(')');
                Ok(output)
            }
            Declaration::Implements(interface_name) => {
                Ok(format!("{}(implements {})", indent, interface_name))
            }
        }
    }
    
    /// Format an expression
    fn format_expression(&self, expr: &Expr, indent_level: usize) -> Result<String, String> {
        let indent = self.get_indent(indent_level);
        
        match expr {
            Expr::Integer(n, _) => Ok(format!("{}{}", indent, n)),
            Expr::Decimal(d, _) => Ok(format!("{}{}", indent, d)),
            Expr::String(s, _) => Ok(format!("{}\"{}\"", indent, s)),
            Expr::Bool(b, _) => Ok(format!("{}{}", indent, b)),
            Expr::Var(name, _) => Ok(format!("{}{}", indent, name)),
            Expr::QualifiedVar { module, name, .. } => Ok(format!("{}{}.{}", indent, module, name)),
            
            Expr::App { func, args, .. } => {
                let mut output = String::new();
                output.push_str(&format!("{}(", indent));
                
                // Function name
                let func_str = self.format_expression_inline(func)?;
                output.push_str(&func_str);
                
                // Arguments
                if args.is_empty() {
                    output.push(')');
                } else if self.should_wrap_arguments(&func_str, args) {
                    // Multi-line arguments
                    for arg in args {
                        output.push('\n');
                        output.push_str(&self.format_expression(arg, indent_level + 1)?);
                    }
                    output.push(')');
                } else {
                    // Single-line arguments
                    for arg in args {
                        output.push(' ');
                        output.push_str(&self.format_expression_inline(arg)?);
                    }
                    output.push(')');
                }
                
                Ok(output)
            }
            
            Expr::Let { bindings, body, .. } => {
                let mut output = String::new();
                output.push_str(&format!("{}(let (", indent));
                
                for (i, binding) in bindings.iter().enumerate() {
                    if i > 0 {
                        output.push('\n');
                        output.push_str(&self.get_indent(indent_level + 2));
                    }
                    output.push_str(&format!("({} {})", 
                        binding.name,
                        self.format_expression_inline(&binding.value)?
                    ));
                }
                
                output.push(')');
                
                for expr in body {
                    output.push('\n');
                    output.push_str(&self.format_expression(expr, indent_level + 1)?);
                }
                
                output.push(')');
                Ok(output)
            }
            
            Expr::If { cond, then_expr, else_expr, .. } => {
                let mut output = String::new();
                output.push_str(&format!("{}(if ", indent));
                output.push_str(&self.format_expression_inline(cond)?);
                output.push('\n');
                output.push_str(&self.format_expression(then_expr, indent_level + 1)?);
                
                if let Some(else_e) = else_expr {
                    output.push('\n');
                    output.push_str(&self.format_expression(else_e, indent_level + 1)?);
                }
                
                output.push(')');
                Ok(output)
            }
            
            Expr::List(elements, _) => {
                let mut output = String::new();
                output.push_str(&format!("{}[", indent));
                
                if elements.is_empty() {
                    output.push(']');
                } else if self.should_wrap_list(elements) {
                    for (i, element) in elements.iter().enumerate() {
                        if i > 0 {
                            output.push('\n');
                        }
                        output.push_str(&self.format_expression(element, indent_level + 1)?);
                    }
                    output.push(']');
                } else {
                    for (i, element) in elements.iter().enumerate() {
                        if i > 0 {
                            output.push(' ');
                        }
                        output.push_str(&self.format_expression_inline(element)?);
                    }
                    output.push(']');
                }
                
                Ok(output)
            }
            
            Expr::Object(fields, _) => {
                let mut output = String::new();
                output.push_str(&format!("{}{{", indent));
                
                if fields.is_empty() {
                    output.push('}');
                } else {
                    for (i, (key, value)) in fields.iter().enumerate() {
                        if i > 0 {
                            output.push(',');
                        }
                        output.push('\n');
                        output.push_str(&format!("{}\"{}\": {}", 
                            self.get_indent(indent_level + 1),
                            key,
                            self.format_expression_inline(value)?
                        ));
                    }
                    
                    if self.config.trailing_commas && !fields.is_empty() {
                        output.push(',');
                    }
                    
                    output.push('\n');
                    output.push_str(&indent);
                    output.push('}');
                }
                
                Ok(output)
            }
            
            // Handle other expression types with basic formatting
            _ => {
                // For now, just return the expression as-is for unhandled types
                Ok(format!("{}({})", indent, "..."))
            }
        }
    }
    
    /// Format expression as inline (single-line)
    fn format_expression_inline(&self, expr: &Expr) -> Result<String, String> {
        match expr {
            Expr::Integer(n, _) => Ok(n.to_string()),
            Expr::Decimal(d, _) => Ok(d.clone()),
            Expr::String(s, _) => Ok(format!("\"{}\"", s)),
            Expr::Bool(b, _) => Ok(b.to_string()),
            Expr::Var(name, _) => Ok(name.clone()),
            Expr::QualifiedVar { module, name, .. } => Ok(format!("{}.{}", module, name)),
            
            Expr::App { func, args, .. } => {
                let mut output = String::new();
                output.push('(');
                output.push_str(&self.format_expression_inline(func)?);
                
                for arg in args {
                    output.push(' ');
                    output.push_str(&self.format_expression_inline(arg)?);
                }
                
                output.push(')');
                Ok(output)
            }
            
            Expr::List(elements, _) => {
                let mut output = String::new();
                output.push('[');
                
                for (i, element) in elements.iter().enumerate() {
                    if i > 0 {
                        output.push(' ');
                    }
                    output.push_str(&self.format_expression_inline(element)?);
                }
                
                output.push(']');
                Ok(output)
            }
            
            _ => {
                // For complex expressions, fall back to multi-line formatting
                // but strip the leading indent
                let formatted = self.format_expression(expr, 0)?;
                Ok(formatted.trim_start().to_string())
            }
        }
    }
    
    
    /// Format a parameter
    fn format_parameter(&self, param: &Parameter) -> Result<String, String> {
        let mut output = param.name.clone();
        if let Some(param_type) = &param.ty {
            output.push(':');
            output.push_str(&self.format_type(param_type)?);
        }
        Ok(output)
    }
    
    /// Format a type
    fn format_type(&self, type_ref: &Type) -> Result<String, String> {
        match type_ref {
            Type::String => Ok("string".to_string()),
            Type::Integer => Ok("integer".to_string()),
            Type::Decimal => Ok("decimal".to_string()),
            Type::Bool => Ok("bool".to_string()),
            Type::Time => Ok("time".to_string()),
            Type::Keyset => Ok("keyset".to_string()),
            Type::Guard => Ok("guard".to_string()),
            Type::List(inner) => Ok(format!("[{}]", self.format_type(inner)?)),
            Type::Object(obj_type) => {
                match obj_type {
                    ObjectType::Schema(name) => Ok(name.clone()),
                    ObjectType::Fields(fields) => {
                        let mut output = String::new();
                        output.push('{');
                        
                        for (i, field) in fields.iter().enumerate() {
                            if i > 0 {
                                output.push_str(", ");
                            }
                            output.push_str(&format!("{}: {}", field.name, self.format_type(&field.ty)?));
                        }
                        
                        output.push('}');
                        Ok(output)
                    }
                }
            }
            Type::ModuleRef(name) => Ok(format!("module<{}>", name)),
            Type::TypeVar(name) => Ok(name.clone()),
        }
    }
    
    /// Basic formatting for when parsing fails
    fn format_basic(&self, text: &str) -> Vec<TextEdit> {
        let mut edits = Vec::new();
        let lines: Vec<&str> = text.lines().collect();
        
        for (line_idx, line) in lines.iter().enumerate() {
            // Fix indentation
            let trimmed = line.trim_start();
            if trimmed.is_empty() {
                continue;
            }
            
            let expected_indent = self.calculate_basic_indent(line_idx, &lines);
            let current_indent = line.len() - trimmed.len();
            
            if current_indent != expected_indent {
                let new_indent = self.get_indent(expected_indent / self.config.indent_size);
                edits.push(TextEdit {
                    range: Range {
                        start: Position { line: line_idx as u32, character: 0 },
                        end: Position { line: line_idx as u32, character: current_indent as u32 },
                    },
                    new_text: new_indent,
                });
            }
        }
        
        edits
    }
    
    /// Format a block of text
    fn format_text_block(&self, text: &str) -> Result<String, String> {
        // Simple line-by-line formatting
        let lines: Vec<&str> = text.lines().collect();
        let mut formatted_lines = Vec::new();
        
        for (i, line) in lines.iter().enumerate() {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                formatted_lines.push(String::new());
                continue;
            }
            
            let indent_level = self.calculate_basic_indent(i, &lines) / self.config.indent_size;
            let formatted_line = format!("{}{}", self.get_indent(indent_level), trimmed);
            formatted_lines.push(formatted_line);
        }
        
        Ok(formatted_lines.join("\n"))
    }
    
    // Helper methods
    
    fn get_indent(&self, level: usize) -> String {
        if self.config.use_spaces {
            " ".repeat(level * self.config.indent_size)
        } else {
            "\t".repeat(level)
        }
    }
    
    fn should_wrap_parameters(&self, params: &[Parameter]) -> bool {
        if params.len() > 3 {
            return true;
        }
        
        let total_length: usize = params.iter()
            .map(|p| p.name.len() + p.ty.as_ref().map(|_| 10).unwrap_or(0))
            .sum();
        
        total_length > 40
    }
    
    fn should_wrap_arguments(&self, func_name: &str, args: &[Expr]) -> bool {
        if args.len() > 2 {
            return true;
        }
        
        // Special cases for certain functions
        match func_name {
            "if" | "let" | "with-capability" | "defun" | "defcap" => true,
            _ => {
                // Estimate line length
                let estimated_length = func_name.len() + args.len() * 15; // rough estimate
                estimated_length > 60
            }
        }
    }
    
    fn should_wrap_list(&self, elements: &[Expr]) -> bool {
        elements.len() > 5
    }
    
    fn calculate_basic_indent(&self, line_idx: usize, lines: &[&str]) -> usize {
        let line = lines[line_idx].trim();
        
        if line.is_empty() {
            return 0;
        }
        
        // Simple heuristic based on parentheses
        let mut indent = 0;
        
        for i in 0..line_idx {
            let prev_line = lines[i].trim();
            let open_parens = prev_line.chars().filter(|&c| c == '(').count();
            let close_parens = prev_line.chars().filter(|&c| c == ')').count();
            indent += open_parens * self.config.indent_size;
            indent = indent.saturating_sub(close_parens * self.config.indent_size);
        }
        
        // If current line starts with closing paren, reduce indent
        if line.starts_with(')') {
            indent = indent.saturating_sub(self.config.indent_size);
        }
        
        indent
    }
}

impl Default for PactFormatter {
    fn default() -> Self {
        Self::new()
    }
}

/// Legacy function for backward compatibility
pub fn format_document(text: &str) -> Vec<TextEdit> {
    let formatter = PactFormatter::new();
    formatter.format_document(text).unwrap_or_else(|_| vec![])
}

/// Format a range of text
pub fn format_range(text: &str, range: Range) -> Vec<TextEdit> {
    let formatter = PactFormatter::new();
    formatter.format_range(text, range).unwrap_or_else(|_| vec![])
}