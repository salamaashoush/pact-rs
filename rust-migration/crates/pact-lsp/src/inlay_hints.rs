//! Inlay hints support for Pact LSP
//!
//! This module provides inline type hints and parameter hints to improve
//! code readability and understanding by showing inferred types and function signatures.

use crate::semantic::SemanticAnalyzer;
use tower_lsp::lsp_types::*;
use pact_eval::TypeCheckContext;
use pact_syntax::{parse, ParsedDef, ParsedExpr, ParsedTopLevel, MArg, Type};
use pact_schema::inference::{TypeInferencer, TypeVarGen};
use pact_schema::{ConcreteType, TypeScheme as ConcreteTypeScheme};
pub type SchemaType = pact_schema::ConcreteType;
use pact_core::values::values::PactValue;
use std::collections::HashMap;
use std::sync::Arc;

/// Inlay hint provider for type and parameter hints
pub struct InlayHintProvider {
    semantic_analyzer: Arc<SemanticAnalyzer>,
    type_inferencer: ExprInferencer,
    schema_inferencer: TypeInferencer,
}

/// Different kinds of inlay hints we support
#[derive(Debug, Clone, PartialEq)]
pub enum PactInlayHintKind {
    /// Type hint for variables and expressions
    Type,
    /// Parameter name hint for function calls
    Parameter,
    /// Return type hint for functions
    ReturnType,
    /// Enum variant hint
    EnumMember,
    /// Capability requirement hint
    Capability,
}

impl PactInlayHintKind {
    pub fn to_lsp_kind(self) -> InlayHintKind {
        match self {
            Self::Type => InlayHintKind::TYPE,
            Self::Parameter => InlayHintKind::PARAMETER,
            Self::ReturnType => InlayHintKind::TYPE,
            Self::EnumMember => InlayHintKind::TYPE,
            Self::Capability => InlayHintKind::PARAMETER,
        }
    }
}

impl InlayHintProvider {
    /// Create a new inlay hint provider
    pub fn new(semantic_analyzer: Arc<SemanticAnalyzer>) -> Self {
        let type_inferencer = ExprInferencer::new();
        let schema_inferencer = TypeInferencer::new();
        
        Self {
            semantic_analyzer,
            type_inferencer,
            schema_inferencer,
        }
    }
    
    /// Get inlay hints for a document range
    pub fn get_inlay_hints(&self, uri: &Url, text: &str, range: Range) -> Result<Vec<InlayHint>, String> {
        let mut hints = Vec::new();
        
        // Parse the document
        match parse(text) {
            Ok(program) => {
                let lines: Vec<&str> = text.lines().collect();
                
                // Create environment for type inference
                let mut env = Environment::new();
                self.populate_builtin_types(&mut env);
                
                // Get hints from top-level items
                for item in &program.items {
                    self.analyze_top_level_for_hints(&mut hints, item, &lines, range, &env)?;
                }
                
                Ok(hints)
            },
            Err(_) => {
                // If parsing fails, return empty hints
                Ok(hints)
            }
        }
    }
    
    /// Analyze top-level items for inlay hints
    fn analyze_top_level_for_hints(
        &self,
        hints: &mut Vec<InlayHint>,
        item: &TopLevel,
        lines: &[&str],
        range: Range,
        env: &Environment,
    ) -> Result<(), String> {
        match item {
            TopLevel::Module(module) => {
                for decl in &module.declarations {
                    self.analyze_declaration_for_hints(hints, decl, lines, range, env)?;
                }
            },
            TopLevel::Interface(interface) => {
                for decl in &interface.declarations {
                    self.analyze_declaration_for_hints(hints, decl, lines, range, env)?;
                }
            },
            TopLevel::Declaration(decl) => {
                self.analyze_declaration_for_hints(hints, decl, lines, range, env)?;
            },
            TopLevel::Expression(expr) => {
                self.analyze_expression_for_hints(hints, expr, lines, range, env)?;
            },
            TopLevel::Use(_) => {
                // No hints needed for use statements
            },
        }
        
        Ok(())
    }
    
    /// Analyze declarations for inlay hints
    fn analyze_declaration_for_hints(
        &self,
        hints: &mut Vec<InlayHint>,
        decl: &Declaration,
        lines: &[&str],
        range: Range,
        env: &Environment,
    ) -> Result<(), String> {
        match decl {
            Declaration::Defun(defun) => {
                // Add return type hint if not explicitly specified
                if defun.return_type.is_none() {
                    if let Some(inferred_type) = self.infer_function_return_type(&defun.body, env) {
                        if let Some(position) = self.find_function_return_position(&defun.name, &defun.span, lines) {
                            if self.position_in_range(position, range) {
                                hints.push(InlayHint {
                                    position,
                                    label: InlayHintLabel::String(format!(": {}", inferred_type)),
                                    kind: Some(PactInlayHintKind::ReturnType.to_lsp_kind()),
                                    text_edits: None,
                                    tooltip: Some(InlayHintTooltip::String(
                                        "Inferred return type".to_string()
                                    )),
                                    padding_left: Some(false),
                                    padding_right: Some(true),
                                    data: None,
                                });
                            }
                        }
                    }
                }
                
                // Add parameter type hints for untyped parameters
                for param in &defun.params {
                    if param.ty.is_none() {
                        if let Some(inferred_type) = self.infer_parameter_type(&param.name, &defun.body, env) {
                            if let Some(position) = self.find_parameter_position(&param.name, &defun.span, lines) {
                                if self.position_in_range(position, range) {
                                    hints.push(InlayHint {
                                        position,
                                        label: InlayHintLabel::String(format!(": {}", inferred_type)),
                                        kind: Some(PactInlayHintKind::Type.to_lsp_kind()),
                                        text_edits: None,
                                        tooltip: Some(InlayHintTooltip::String(
                                            "Inferred parameter type".to_string()
                                        )),
                                        padding_left: Some(false),
                                        padding_right: Some(true),
                                        data: None,
                                    });
                                }
                            }
                        }
                    }
                }
                
                // Analyze function body
                for expr in &defun.body {
                    self.analyze_expression_for_hints(hints, expr, lines, range, env)?;
                }
            },
            Declaration::Defcap(defcap) => {
                // Analyze capability body
                for expr in &defcap.body {
                    self.analyze_expression_for_hints(hints, expr, lines, range, env)?;
                }
            },
            Declaration::Defconst(defconst) => {
                // Add type hint if not specified
                if defconst.type_ann.is_none() {
                    if let Some(inferred_type) = self.infer_expression_type(&defconst.value, env) {
                        if let Some(position) = self.find_constant_type_position(&defconst.name, &defconst.span, lines) {
                            if self.position_in_range(position, range) {
                                hints.push(InlayHint {
                                    position,
                                    label: InlayHintLabel::String(format!(": {}", inferred_type)),
                                    kind: Some(PactInlayHintKind::Type.to_lsp_kind()),
                                    text_edits: None,
                                    tooltip: Some(InlayHintTooltip::String(
                                        "Inferred constant type".to_string()
                                    )),
                                    padding_left: Some(false),
                                    padding_right: Some(true),
                                    data: None,
                                });
                            }
                        }
                    }
                }
                
                // Analyze constant value
                self.analyze_expression_for_hints(hints, &defconst.value, lines, range, env)?;
            },
            _ => {
                // Handle other declaration types as needed
            }
        }
        
        Ok(())
    }
    
    /// Analyze expressions for inlay hints
    fn analyze_expression_for_hints(
        &self,
        hints: &mut Vec<InlayHint>,
        expr: &Expr,
        lines: &[&str],
        range: Range,
        env: &Environment,
    ) -> Result<(), String> {
        match expr {
            Expr::App { func, args, span } => {
                // Add parameter name hints for function calls
                if let Some(func_name) = self.extract_function_name(func.as_ref()) {
                    if let Some(signature) = self.get_function_signature(&func_name, env) {
                        self.add_parameter_hints(hints, &signature, args, span, lines, range)?;
                    }
                }
                
                // Analyze function and arguments recursively
                self.analyze_expression_for_hints(hints, func.as_ref(), lines, range, env)?;
                for arg in args {
                    self.analyze_expression_for_hints(hints, arg, lines, range, env)?;
                }
            },
            Expr::Let { bindings, body, .. } => {
                // Add type hints for let bindings without explicit types
                for binding in bindings {
                    if let Some(inferred_type) = self.infer_expression_type(&binding.value, env) {
                        if let Some(position) = self.find_binding_type_position(&binding.name, &binding.value.span(), lines) {
                            if self.position_in_range(position, range) {
                                hints.push(InlayHint {
                                    position,
                                    label: InlayHintLabel::String(format!(": {}", inferred_type)),
                                    kind: Some(PactInlayHintKind::Type.to_lsp_kind()),
                                    text_edits: None,
                                    tooltip: Some(InlayHintTooltip::String(
                                        "Inferred binding type".to_string()
                                    )),
                                    padding_left: Some(false),
                                    padding_right: Some(true),
                                    data: None,
                                });
                            }
                        }
                    }
                    
                    // Analyze binding value
                    self.analyze_expression_for_hints(hints, &binding.value, lines, range, env)?;
                }
                
                // Analyze body expressions
                for expr in body {
                    self.analyze_expression_for_hints(hints, expr, lines, range, env)?;
                }
            },
            Expr::If { cond, then_expr, else_expr, .. } => {
                self.analyze_expression_for_hints(hints, cond.as_ref(), lines, range, env)?;
                self.analyze_expression_for_hints(hints, then_expr.as_ref(), lines, range, env)?;
                if let Some(else_e) = else_expr {
                    self.analyze_expression_for_hints(hints, else_e.as_ref(), lines, range, env)?;
                }
            },
            Expr::Lambda { params, body, .. } => {
                // Add type hints for lambda parameters
                for param in params {
                    if param.ty.is_none() {
                        if let Some(inferred_type) = self.infer_lambda_parameter_type(&param.name, body, env) {
                            if let Some(position) = self.find_parameter_position(&param.name, &expr.span(), lines) {
                                if self.position_in_range(position, range) {
                                    hints.push(InlayHint {
                                        position,
                                        label: InlayHintLabel::String(format!(": {}", inferred_type)),
                                        kind: Some(PactInlayHintKind::Type.to_lsp_kind()),
                                        text_edits: None,
                                        tooltip: Some(InlayHintTooltip::String(
                                            "Inferred lambda parameter type".to_string()
                                        )),
                                        padding_left: Some(false),
                                        padding_right: Some(true),
                                        data: None,
                                    });
                                }
                            }
                        }
                    }
                }
                
                // Analyze lambda body
                for expr in body {
                    self.analyze_expression_for_hints(hints, expr, lines, range, env)?;
                }
            },
            Expr::List(elements, _) => {
                for element in elements {
                    self.analyze_expression_for_hints(hints, element, lines, range, env)?;
                }
            },
            Expr::Object(fields, _) => {
                for (_, value) in fields {
                    self.analyze_expression_for_hints(hints, value, lines, range, env)?;
                }
            },
            _ => {
                // Handle other expression types as needed
            }
        }
        
        Ok(())
    }
    
    /// Add parameter name hints for function calls
    fn add_parameter_hints(
        &self,
        hints: &mut Vec<InlayHint>,
        signature: &FunctionSignature,
        args: &[Expr],
        span: &pact_lexer::Span,
        lines: &[&str],
        range: Range,
    ) -> Result<(), String> {
        for (i, arg) in args.iter().enumerate() {
            if let Some(param_name) = signature.parameters.get(i) {
                if let Some(position) = self.find_argument_position(i, span, lines) {
                    if self.position_in_range(position, range) {
                        hints.push(InlayHint {
                            position,
                            label: InlayHintLabel::String(format!("{}:", param_name)),
                            kind: Some(PactInlayHintKind::Parameter.to_lsp_kind()),
                            text_edits: None,
                            tooltip: Some(InlayHintTooltip::String(
                                format!("Parameter: {}", param_name)
                            )),
                            padding_left: Some(false),
                            padding_right: Some(true),
                            data: None,
                        });
                    }
                }
            }
        }
        
        Ok(())
    }
    
    // Type inference methods
    
    /// Infer the return type of a function
    fn infer_function_return_type(&self, body: &[Expr], env: &Environment) -> Option<String> {
        if let Some(last_expr) = body.last() {
            self.infer_expression_type(last_expr, env)
        } else {
            Some("unit".to_string())
        }
    }
    
    /// Infer the type of a parameter from its usage
    fn infer_parameter_type(&self, param_name: &str, body: &[Expr], env: &Environment) -> Option<String> {
        // Analyze how the parameter is used in the function body
        for expr in body {
            if let Some(inferred_type) = self.infer_parameter_usage_type(param_name, expr, env) {
                return Some(inferred_type);
            }
        }
        
        // Default to generic type if we can't infer
        Some("a".to_string())
    }
    
    /// Infer the type of an expression
    fn infer_expression_type(&self, expr: &Expr, env: &Environment) -> Option<String> {
        match expr {
            Expr::Integer(_, _) => Some("integer".to_string()),
            Expr::Decimal(_, _) => Some("decimal".to_string()),
            Expr::String(_, _) => Some("string".to_string()),
            Expr::Bool(_, _) => Some("bool".to_string()),
            Expr::List(elements, _) => {
                if let Some(first) = elements.first() {
                    if let Some(element_type) = self.infer_expression_type(first, env) {
                        return Some(format!("[{}]", element_type));
                    }
                }
                Some("[a]".to_string())
            },
            Expr::Object(fields, _) => {
                if fields.is_empty() {
                    Some("{}".to_string())
                } else {
                    // Try to infer object schema
                    Some("object".to_string())
                }
            },
            Expr::Var(name, _) => {
                // Look up variable type in environment or semantic analyzer
                if let Some(var_type) = self.semantic_analyzer.get_variable_type(name) {
                    Some(var_type)
                } else {
                    // Check if it's a built-in function
                    if let Some(signature) = self.get_builtin_function_type(name) {
                        Some(signature)
                    } else {
                        None
                    }
                }
            },
            Expr::App { func, args, .. } => {
                // Infer return type based on function
                if let Some(func_name) = self.extract_function_name(func.as_ref()) {
                    self.get_function_return_type(&func_name, args, env)
                } else {
                    None
                }
            },
            _ => None,
        }
    }
    
    /// Infer parameter type from its usage
    fn infer_parameter_usage_type(&self, param_name: &str, expr: &Expr, env: &Environment) -> Option<String> {
        match expr {
            Expr::App { func, args, .. } => {
                if let Some(func_name) = self.extract_function_name(func.as_ref()) {
                    // Check if parameter is used in a typed context
                    for (i, arg) in args.iter().enumerate() {
                        if self.expression_uses_variable(arg, param_name) {
                            if let Some(param_type) = self.get_function_parameter_type(&func_name, i) {
                                return Some(param_type);
                            }
                        }
                    }
                }
                
                // Recursively check arguments
                self.expression_uses_variable(func.as_ref(), param_name);
                for arg in args {
                    if let Some(inferred_type) = self.infer_parameter_usage_type(param_name, arg, env) {
                        return Some(inferred_type);
                    }
                }
            },
            Expr::Let { bindings, body, .. } => {
                for binding in bindings {
                    if let Some(inferred_type) = self.infer_parameter_usage_type(param_name, &binding.value, env) {
                        return Some(inferred_type);
                    }
                }
                for expr in body {
                    if let Some(inferred_type) = self.infer_parameter_usage_type(param_name, expr, env) {
                        return Some(inferred_type);
                    }
                }
            },
            _ => {}
        }
        
        None
    }
    
    /// Infer lambda parameter type
    fn infer_lambda_parameter_type(&self, param_name: &str, body: &[Expr], env: &Environment) -> Option<String> {
        for expr in body {
            if let Some(inferred_type) = self.infer_parameter_usage_type(param_name, expr, env) {
                return Some(inferred_type);
            }
        }
        
        Some("a".to_string())
    }
    
    // Helper methods
    
    /// Extract function name from expression
    fn extract_function_name(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::Var(name, _) => Some(name.clone()),
            Expr::QualifiedVar { name, .. } => Some(name.clone()),
            _ => None,
        }
    }
    
    /// Check if expression uses a variable
    fn expression_uses_variable(&self, expr: &Expr, var_name: &str) -> bool {
        match expr {
            Expr::Var(name, _) => name == var_name,
            Expr::App { func, args, .. } => {
                self.expression_uses_variable(func.as_ref(), var_name) ||
                args.iter().any(|arg| self.expression_uses_variable(arg, var_name))
            },
            Expr::Let { bindings, body, .. } => {
                bindings.iter().any(|b| self.expression_uses_variable(&b.value, var_name)) ||
                body.iter().any(|e| self.expression_uses_variable(e, var_name))
            },
            _ => false,
        }
    }
    
    /// Get function signature from environment
    fn get_function_signature(&self, func_name: &str, env: &Environment) -> Option<FunctionSignature> {
        // Check built-in functions first
        if let Some(signature) = self.get_builtin_function_signature(func_name) {
            return Some(signature);
        }
        
        // Check user-defined functions in environment
        // This would require integration with the environment's function storage
        None
    }
    
    /// Get built-in function signature
    fn get_builtin_function_signature(&self, func_name: &str) -> Option<FunctionSignature> {
        match func_name {
            "+" => Some(FunctionSignature {
                parameters: vec!["x".to_string(), "y".to_string()],
                return_type: "integer".to_string(),
            }),
            "-" => Some(FunctionSignature {
                parameters: vec!["x".to_string(), "y".to_string()],
                return_type: "integer".to_string(),
            }),
            "*" => Some(FunctionSignature {
                parameters: vec!["x".to_string(), "y".to_string()],
                return_type: "integer".to_string(),
            }),
            "/" => Some(FunctionSignature {
                parameters: vec!["x".to_string(), "y".to_string()],
                return_type: "decimal".to_string(),
            }),
            "=" => Some(FunctionSignature {
                parameters: vec!["x".to_string(), "y".to_string()],
                return_type: "bool".to_string(),
            }),
            ">" => Some(FunctionSignature {
                parameters: vec!["x".to_string(), "y".to_string()],
                return_type: "bool".to_string(),
            }),
            "<" => Some(FunctionSignature {
                parameters: vec!["x".to_string(), "y".to_string()],
                return_type: "bool".to_string(),
            }),
            "length" => Some(FunctionSignature {
                parameters: vec!["list".to_string()],
                return_type: "integer".to_string(),
            }),
            "str-to-int" => Some(FunctionSignature {
                parameters: vec!["str".to_string()],
                return_type: "integer".to_string(),
            }),
            "int-to-str" => Some(FunctionSignature {
                parameters: vec!["int".to_string()],
                return_type: "string".to_string(),
            }),
            "read" => Some(FunctionSignature {
                parameters: vec!["table".to_string(), "key".to_string()],
                return_type: "object".to_string(),
            }),
            "write" => Some(FunctionSignature {
                parameters: vec!["table".to_string(), "key".to_string(), "object".to_string()],
                return_type: "string".to_string(),
            }),
            _ => None,
        }
    }
    
    /// Get built-in function type
    fn get_builtin_function_type(&self, func_name: &str) -> Option<String> {
        if let Some(signature) = self.get_builtin_function_signature(func_name) {
            let param_types = signature.parameters.join(" -> ");
            Some(format!("{} -> {}", param_types, signature.return_type))
        } else {
            None
        }
    }
    
    /// Get function return type
    fn get_function_return_type(&self, func_name: &str, args: &[Expr], env: &Environment) -> Option<String> {
        if let Some(signature) = self.get_builtin_function_signature(func_name) {
            Some(signature.return_type)
        } else {
            None
        }
    }
    
    /// Get function parameter type
    fn get_function_parameter_type(&self, func_name: &str, param_index: usize) -> Option<String> {
        match func_name {
            "+" | "-" | "*" => {
                if param_index < 2 {
                    Some("integer".to_string())
                } else {
                    None
                }
            },
            "/" => {
                if param_index < 2 {
                    Some("decimal".to_string())
                } else {
                    None
                }
            },
            "str-to-int" => {
                if param_index == 0 {
                    Some("string".to_string())
                } else {
                    None
                }
            },
            "int-to-str" => {
                if param_index == 0 {
                    Some("integer".to_string())
                } else {
                    None
                }
            },
            _ => None,
        }
    }
    
    /// Populate environment with built-in types
    fn populate_builtin_types(&self, env: &mut Environment) {
        // This would populate the environment with built-in type information
        // For now, this is a placeholder
    }
    
    // Position finding methods
    
    /// Find function return type position
    fn find_function_return_position(&self, func_name: &str, span: &pact_lexer::Span, lines: &[&str]) -> Option<Position> {
        // Find the position after the parameter list where return type would go
        self.find_position_after_text(&format!("defun {}", func_name), span, lines)
    }
    
    /// Find parameter type position
    fn find_parameter_position(&self, param_name: &str, span: &pact_lexer::Span, lines: &[&str]) -> Option<Position> {
        self.find_position_after_text(param_name, span, lines)
    }
    
    /// Find constant type position
    fn find_constant_type_position(&self, const_name: &str, span: &pact_lexer::Span, lines: &[&str]) -> Option<Position> {
        self.find_position_after_text(&format!("defconst {}", const_name), span, lines)
    }
    
    /// Find binding type position
    fn find_binding_type_position(&self, binding_name: &str, span: &pact_lexer::Span, lines: &[&str]) -> Option<Position> {
        self.find_position_after_text(binding_name, span, lines)
    }
    
    /// Find argument position for parameter hints
    fn find_argument_position(&self, arg_index: usize, span: &pact_lexer::Span, lines: &[&str]) -> Option<Position> {
        // This would find the position before the nth argument
        // For now, return a dummy position
        Some(Position { line: 0, character: 0 })
    }
    
    /// Find position after specific text
    fn find_position_after_text(&self, text: &str, span: &pact_lexer::Span, lines: &[&str]) -> Option<Position> {
        let mut char_count = 0;
        
        for (line_idx, line) in lines.iter().enumerate() {
            let line_start = char_count;
            let line_end = char_count + line.len();
            
            if span.start >= line_start && span.start <= line_end {
                if let Some(text_pos) = line.find(text) {
                    let after_pos = text_pos + text.len();
                    return Some(Position {
                        line: line_idx as u32,
                        character: after_pos as u32,
                    });
                }
            }
            
            char_count = line_end + 1;
        }
        
        None
    }
    
    /// Check if position is within range
    fn position_in_range(&self, position: Position, range: Range) -> bool {
        (position.line > range.start.line || 
         (position.line == range.start.line && position.character >= range.start.character)) &&
        (position.line < range.end.line || 
         (position.line == range.end.line && position.character <= range.end.character))
    }
}

/// Function signature for hint generation
#[derive(Debug, Clone)]
struct FunctionSignature {
    parameters: Vec<String>,
    return_type: String,
}