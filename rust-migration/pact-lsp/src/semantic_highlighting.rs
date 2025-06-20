//! Semantic highlighting support for Pact LSP
//!
//! This module provides semantic token analysis that goes beyond syntax highlighting
//! to provide type-aware, context-sensitive highlighting for better code understanding.

use crate::semantic::SemanticAnalyzer;
use tower_lsp::lsp_types::*;
use pact_eval::TypeCheckContext;
use pact_lexer::{Lexer, Token};
use pact_parser::{parse, ParsedDef, ParsedExpr, ParsedTopLevel, MArg, Type};
use pact_schema::inference::TypeInferencer;
use pact_values::values::PactValue;
use std::collections::HashMap;
use std::sync::Arc;

/// Semantic token provider for enhanced highlighting
pub struct SemanticTokenProvider {
    semantic_analyzer: Arc<SemanticAnalyzer>,
    type_inferencer: ExprInferencer,
}

/// Semantic token types supported by Pact LSP
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PactSemanticTokenType {
    // Variables and identifiers
    Variable,
    Parameter,
    Function,
    Method,
    
    // Types and schemas
    Type,
    TypeParameter,
    Schema,
    Interface,
    
    // Language constructs
    Keyword,
    Operator,
    String,
    Number,
    Boolean,
    
    // Pact-specific
    Capability,
    Table,
    Module,
    Namespace,
    Guard,
    
    // Comments and documentation
    Comment,
    Documentation,
    
    // Error indicators
    Error,
    Warning,
    Deprecated,
}

impl PactSemanticTokenType {
    /// Convert to LSP semantic token type index
    pub fn to_lsp_type(self) -> u32 {
        match self {
            Self::Variable => 0,
            Self::Parameter => 1,
            Self::Function => 2,
            Self::Method => 3,
            Self::Type => 4,
            Self::TypeParameter => 5,
            Self::Schema => 6,
            Self::Interface => 7,
            Self::Keyword => 8,
            Self::Operator => 9,
            Self::String => 10,
            Self::Number => 11,
            Self::Boolean => 8, // Same as keyword
            Self::Capability => 12,
            Self::Table => 13,
            Self::Module => 14,
            Self::Namespace => 14,
            Self::Guard => 12,
            Self::Comment => 15,
            Self::Documentation => 15,
            Self::Error => 0, // Same as variable
            Self::Warning => 0,
            Self::Deprecated => 0,
        }
    }
    
    /// Convert to LSP semantic token type (for legend)
    pub fn to_semantic_token_type(self) -> SemanticTokenType {
        match self {
            Self::Variable => SemanticTokenType::VARIABLE,
            Self::Parameter => SemanticTokenType::PARAMETER,
            Self::Function => SemanticTokenType::FUNCTION,
            Self::Method => SemanticTokenType::METHOD,
            Self::Type => SemanticTokenType::TYPE,
            Self::TypeParameter => SemanticTokenType::TYPE_PARAMETER,
            Self::Schema => SemanticTokenType::STRUCT,
            Self::Interface => SemanticTokenType::INTERFACE,
            Self::Keyword => SemanticTokenType::KEYWORD,
            Self::Operator => SemanticTokenType::OPERATOR,
            Self::String => SemanticTokenType::STRING,
            Self::Number => SemanticTokenType::NUMBER,
            Self::Boolean => SemanticTokenType::KEYWORD,
            Self::Capability => SemanticTokenType::DECORATOR,
            Self::Table => SemanticTokenType::PROPERTY,
            Self::Module => SemanticTokenType::NAMESPACE,
            Self::Namespace => SemanticTokenType::NAMESPACE,
            Self::Guard => SemanticTokenType::DECORATOR,
            Self::Comment => SemanticTokenType::COMMENT,
            Self::Documentation => SemanticTokenType::COMMENT,
            Self::Error => SemanticTokenType::VARIABLE, // No specific error type in LSP
            Self::Warning => SemanticTokenType::VARIABLE,
            Self::Deprecated => SemanticTokenType::VARIABLE,
        }
    }
}

/// Semantic token modifiers for additional highlighting context
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PactSemanticTokenModifier {
    Declaration,
    Definition,
    ReadOnly,
    Static,
    Deprecated,
    Abstract,
    Async,
    Modification,
    Documentation,
    DefaultLibrary,
}

impl PactSemanticTokenModifier {
    /// Convert to LSP semantic token modifier
    pub fn to_lsp_modifier(self) -> SemanticTokenModifier {
        match self {
            Self::Declaration => SemanticTokenModifier::DECLARATION,
            Self::Definition => SemanticTokenModifier::DEFINITION,
            Self::ReadOnly => SemanticTokenModifier::READONLY,
            Self::Static => SemanticTokenModifier::STATIC,
            Self::Deprecated => SemanticTokenModifier::DEPRECATED,
            Self::Abstract => SemanticTokenModifier::ABSTRACT,
            Self::Async => SemanticTokenModifier::ASYNC,
            Self::Modification => SemanticTokenModifier::MODIFICATION,
            Self::Documentation => SemanticTokenModifier::DOCUMENTATION,
            Self::DefaultLibrary => SemanticTokenModifier::DEFAULT_LIBRARY,
        }
    }
}

/// Represents a semantic token with position and type information
#[derive(Debug, Clone)]
pub struct SemanticToken {
    pub line: u32,
    pub start: u32,
    pub length: u32,
    pub token_type: PactSemanticTokenType,
    pub modifiers: Vec<PactSemanticTokenModifier>,
}

impl SemanticTokenProvider {
    /// Create a new semantic token provider
    pub fn new(semantic_analyzer: Arc<SemanticAnalyzer>) -> Self {
        let type_inferencer = ExprInferencer::new();
        Self {
            semantic_analyzer,
            type_inferencer,
        }
    }
    
    /// Get semantic tokens for the entire document
    pub fn get_semantic_tokens(&self, uri: &Url, text: &str) -> Result<Vec<SemanticToken>, String> {
        let mut tokens = Vec::new();
        
        // First, get lexical tokens for basic highlighting
        let lexer_tokens = match lex(text) {
            Ok(tokens) => tokens,
            Err(_) => Vec::new(), // Continue with empty tokens on error
        };
        
        // Add lexical token highlighting
        self.add_lexical_tokens(&mut tokens, &lexer_tokens, text)?;
        
        // Parse AST for semantic analysis
        match parse(text) {
            Ok(program) => {
                // Add semantic tokens from AST analysis
                self.add_semantic_tokens(&mut tokens, &program, text)?;
                
                // Add type-based highlighting
                self.add_type_based_tokens(&mut tokens, &program, text)?;
                
                // Sort tokens by position
                tokens.sort_by(|a, b| {
                    a.line.cmp(&b.line).then(a.start.cmp(&b.start))
                });
                
                Ok(tokens)
            },
            Err(_) => {
                // If parsing fails, return lexical tokens only
                Ok(tokens)
            }
        }
    }
    
    /// Add basic lexical tokens
    fn add_lexical_tokens(&self, tokens: &mut Vec<SemanticToken>, lexer_tokens: &[(Token, pact_lexer::Span)], text: &str) -> Result<(), String> {
        let lines: Vec<&str> = text.lines().collect();
        
        for (token, span) in lexer_tokens {
            if let Some((line_idx, col_idx)) = self.span_to_position(span, &lines) {
                let token_type = match token {
                    Token::String(_) => PactSemanticTokenType::String,
                    Token::Number(_) => PactSemanticTokenType::Number,
                    Token::True | Token::False => PactSemanticTokenType::Boolean,
                    Token::Defun | Token::Defcap | Token::Defconst | 
                    Token::Defschema | Token::Deftable | Token::Defpact => PactSemanticTokenType::Keyword,
                    Token::If | Token::Let | Token::Lambda | Token::Module => PactSemanticTokenType::Keyword,
                    Token::Plus | Token::Minus | Token::Star | Token::Slash => PactSemanticTokenType::Operator,
                    Token::Eq | Token::Neq | Token::Lt | Token::Gt => PactSemanticTokenType::Operator,
                    Token::And | Token::Or | Token::Not => PactSemanticTokenType::Operator,
                    _ => continue, // Skip other tokens for now
                };
                
                let length = span.end.saturating_sub(span.start) as u32;
                
                tokens.push(SemanticToken {
                    line: line_idx as u32,
                    start: col_idx as u32,
                    length,
                    token_type,
                    modifiers: vec![],
                });
            }
        }
        
        Ok(())
    }
    
    /// Add semantic tokens from AST analysis
    fn add_semantic_tokens(&self, tokens: &mut Vec<SemanticToken>, program: &pact_parser::Program, text: &str) -> Result<(), String> {
        let lines: Vec<&str> = text.lines().collect();
        
        for item in &program.items {
            self.analyze_top_level_item(tokens, item, &lines)?;
        }
        
        Ok(())
    }
    
    /// Analyze top-level items for semantic tokens
    fn analyze_top_level_item(&self, tokens: &mut Vec<SemanticToken>, item: &TopLevel, lines: &[&str]) -> Result<(), String> {
        match item {
            TopLevel::Module(module) => {
                // Highlight module name
                if let Some((line, col)) = self.find_identifier_position(&module.name, &module.span, lines) {
                    tokens.push(SemanticToken {
                        line: line as u32,
                        start: col as u32,
                        length: module.name.len() as u32,
                        token_type: PactSemanticTokenType::Module,
                        modifiers: vec![PactSemanticTokenModifier::Declaration],
                    });
                }
                
                // Analyze module declarations
                for decl in &module.declarations {
                    self.analyze_declaration(tokens, decl, lines)?;
                }
            },
            TopLevel::Interface(interface) => {
                // Highlight interface name
                if let Some((line, col)) = self.find_identifier_position(&interface.name, &interface.span, lines) {
                    tokens.push(SemanticToken {
                        line: line as u32,
                        start: col as u32,
                        length: interface.name.len() as u32,
                        token_type: PactSemanticTokenType::Interface,
                        modifiers: vec![PactSemanticTokenModifier::Declaration],
                    });
                }
                
                // Analyze interface declarations
                for decl in &interface.declarations {
                    self.analyze_declaration(tokens, decl, lines)?;
                }
            },
            TopLevel::Use(use_stmt) => {
                // Highlight module reference
                if let Some((line, col)) = self.find_identifier_position(&use_stmt.module, &use_stmt.span, lines) {
                    tokens.push(SemanticToken {
                        line: line as u32,
                        start: col as u32,
                        length: use_stmt.module.len() as u32,
                        token_type: PactSemanticTokenType::Module,
                        modifiers: vec![],
                    });
                }
            },
            TopLevel::Declaration(decl) => {
                self.analyze_declaration(tokens, decl, lines)?;
            },
            TopLevel::Expression(expr) => {
                self.analyze_expression(tokens, expr, lines)?;
            },
        }
        
        Ok(())
    }
    
    /// Analyze declarations for semantic tokens
    fn analyze_declaration(&self, tokens: &mut Vec<SemanticToken>, decl: &Declaration, lines: &[&str]) -> Result<(), String> {
        match decl {
            Declaration::Defun(defun) => {
                // Highlight function name
                if let Some((line, col)) = self.find_identifier_position(&defun.name, &defun.span, lines) {
                    tokens.push(SemanticToken {
                        line: line as u32,
                        start: col as u32,
                        length: defun.name.len() as u32,
                        token_type: PactSemanticTokenType::Function,
                        modifiers: vec![PactSemanticTokenModifier::Declaration],
                    });
                }
                
                // Highlight parameters
                for param in &defun.params {
                    self.analyze_parameter(tokens, param, &defun.span, lines)?;
                }
                
                // Highlight return type
                if let Some(return_type) = &defun.return_type {
                    self.analyze_type_annotation(tokens, return_type, &defun.span, lines)?;
                }
                
                // Analyze function body
                for expr in &defun.body {
                    self.analyze_expression(tokens, expr, lines)?;
                }
            },
            Declaration::Defcap(defcap) => {
                // Highlight capability name
                if let Some((line, col)) = self.find_identifier_position(&defcap.name, &defcap.span, lines) {
                    tokens.push(SemanticToken {
                        line: line as u32,
                        start: col as u32,
                        length: defcap.name.len() as u32,
                        token_type: PactSemanticTokenType::Capability,
                        modifiers: vec![PactSemanticTokenModifier::Declaration],
                    });
                }
                
                // Highlight parameters
                for param in &defcap.params {
                    self.analyze_parameter(tokens, param, &defcap.span, lines)?;
                }
                
                // Analyze capability body
                for expr in &defcap.body {
                    self.analyze_expression(tokens, expr, lines)?;
                }
            },
            Declaration::Defconst(defconst) => {
                // Highlight constant name
                if let Some((line, col)) = self.find_identifier_position(&defconst.name, &defconst.span, lines) {
                    let modifiers = vec![
                        PactSemanticTokenModifier::Declaration,
                        PactSemanticTokenModifier::ReadOnly,
                    ];
                    
                    tokens.push(SemanticToken {
                        line: line as u32,
                        start: col as u32,
                        length: defconst.name.len() as u32,
                        token_type: PactSemanticTokenType::Variable,
                        modifiers,
                    });
                }
                
                // Highlight type annotation
                if let Some(type_ann) = &defconst.type_ann {
                    self.analyze_type_annotation(tokens, type_ann, &defconst.span, lines)?;
                }
                
                // Analyze value expression
                self.analyze_expression(tokens, &defconst.value, lines)?;
            },
            Declaration::Defschema(defschema) => {
                // Highlight schema name
                if let Some((line, col)) = self.find_identifier_position(&defschema.name, &defschema.span, lines) {
                    tokens.push(SemanticToken {
                        line: line as u32,
                        start: col as u32,
                        length: defschema.name.len() as u32,
                        token_type: PactSemanticTokenType::Schema,
                        modifiers: vec![PactSemanticTokenModifier::Declaration],
                    });
                }
                
                // Highlight schema fields
                for field in &defschema.fields {
                    if let Some((line, col)) = self.find_identifier_position(&field.name, &defschema.span, lines) {
                        tokens.push(SemanticToken {
                            line: line as u32,
                            start: col as u32,
                            length: field.name.len() as u32,
                            token_type: PactSemanticTokenType::Variable,
                            modifiers: vec![PactSemanticTokenModifier::Declaration],
                        });
                    }
                    
                    self.analyze_type_annotation(tokens, &field.ty, &defschema.span, lines)?;
                }
            },
            Declaration::Deftable(deftable) => {
                // Highlight table name
                if let Some((line, col)) = self.find_identifier_position(&deftable.name, &deftable.span, lines) {
                    tokens.push(SemanticToken {
                        line: line as u32,
                        start: col as u32,
                        length: deftable.name.len() as u32,
                        token_type: PactSemanticTokenType::Table,
                        modifiers: vec![PactSemanticTokenModifier::Declaration],
                    });
                }
                
                // Highlight schema reference
                if let Some((line, col)) = self.find_identifier_position(&deftable.schema, &deftable.span, lines) {
                    tokens.push(SemanticToken {
                        line: line as u32,
                        start: col as u32,
                        length: deftable.schema.len() as u32,
                        token_type: PactSemanticTokenType::Schema,
                        modifiers: vec![],
                    });
                }
            },
            Declaration::Defpact(defpact) => {
                // Highlight pact name
                if let Some((line, col)) = self.find_identifier_position(&defpact.name, &defpact.span, lines) {
                    tokens.push(SemanticToken {
                        line: line as u32,
                        start: col as u32,
                        length: defpact.name.len() as u32,
                        token_type: PactSemanticTokenType::Function,
                        modifiers: vec![PactSemanticTokenModifier::Declaration, PactSemanticTokenModifier::Async],
                    });
                }
                
                // Highlight parameters
                for param in &defpact.params {
                    self.analyze_parameter(tokens, param, &defpact.span, lines)?;
                }
            },
            Declaration::Implements(interface_name) => {
                // Highlight interface reference
                if let Some((line, col)) = self.find_text_in_span(interface_name, &pact_lexer::Span { start: 0, end: 0 }, lines) {
                    tokens.push(SemanticToken {
                        line: line as u32,
                        start: col as u32,
                        length: interface_name.len() as u32,
                        token_type: PactSemanticTokenType::Interface,
                        modifiers: vec![],
                    });
                }
            },
        }
        
        Ok(())
    }
    
    /// Analyze expressions for semantic tokens
    fn analyze_expression(&self, tokens: &mut Vec<SemanticToken>, expr: &Expr, lines: &[&str]) -> Result<(), String> {
        match expr {
            Expr::Var(name, span) => {
                if let Some((line, col)) = self.span_to_position(span, lines) {
                    // Determine if this is a built-in function or user variable
                    let token_type = if self.semantic_analyzer.is_builtin_function(name) {
                        PactSemanticTokenType::Function
                    } else {
                        PactSemanticTokenType::Variable
                    };
                    
                    let mut modifiers = vec![];
                    if self.semantic_analyzer.is_builtin_function(name) {
                        modifiers.push(PactSemanticTokenModifier::DefaultLibrary);
                    }
                    
                    tokens.push(SemanticToken {
                        line: line as u32,
                        start: col as u32,
                        length: name.len() as u32,
                        token_type,
                        modifiers,
                    });
                }
            },
            Expr::QualifiedVar { module, name, span } => {
                if let Some((line, col)) = self.span_to_position(span, lines) {
                    // Highlight module part
                    tokens.push(SemanticToken {
                        line: line as u32,
                        start: col as u32,
                        length: module.len() as u32,
                        token_type: PactSemanticTokenType::Module,
                        modifiers: vec![],
                    });
                    
                    // Highlight name part (offset by module.len() + 1 for the dot)
                    tokens.push(SemanticToken {
                        line: line as u32,
                        start: col as u32 + module.len() as u32 + 1,
                        length: name.len() as u32,
                        token_type: PactSemanticTokenType::Function,
                        modifiers: vec![],
                    });
                }
            },
            Expr::App { func, args, .. } => {
                // Analyze function expression
                self.analyze_expression(tokens, func.as_ref(), lines)?;
                
                // Analyze arguments
                for arg in args {
                    self.analyze_expression(tokens, arg, lines)?;
                }
            },
            Expr::Let { bindings, body, .. } => {
                // Analyze bindings
                for binding in bindings {
                    // Highlight binding name
                    if let Some((line, col)) = self.find_text_in_span(&binding.name, &binding.value.span(), lines) {
                        tokens.push(SemanticToken {
                            line: line as u32,
                            start: col as u32,
                            length: binding.name.len() as u32,
                            token_type: PactSemanticTokenType::Variable,
                            modifiers: vec![PactSemanticTokenModifier::Declaration],
                        });
                    }
                    
                    // Analyze binding value
                    self.analyze_expression(tokens, &binding.value, lines)?;
                }
                
                // Analyze body expressions
                for expr in body {
                    self.analyze_expression(tokens, expr, lines)?;
                }
            },
            Expr::If { cond, then_expr, else_expr, .. } => {
                self.analyze_expression(tokens, cond.as_ref(), lines)?;
                self.analyze_expression(tokens, then_expr.as_ref(), lines)?;
                if let Some(else_e) = else_expr {
                    self.analyze_expression(tokens, else_e.as_ref(), lines)?;
                }
            },
            Expr::Lambda { params, body, .. } => {
                // Highlight parameters
                for param in params {
                    self.analyze_parameter(tokens, param, &expr.span(), lines)?;
                }
                
                // Analyze body
                for expr in body {
                    self.analyze_expression(tokens, expr, lines)?;
                }
            },
            Expr::WithCapability { cap, body, .. } => {
                self.analyze_expression(tokens, cap.as_ref(), lines)?;
                for expr in body {
                    self.analyze_expression(tokens, expr, lines)?;
                }
            },
            Expr::List(elements, _) => {
                for element in elements {
                    self.analyze_expression(tokens, element, lines)?;
                }
            },
            Expr::Object(fields, _) => {
                for (_, value) in fields {
                    self.analyze_expression(tokens, value, lines)?;
                }
            },
            // Handle other expression types...
            _ => {
                // For now, skip other expression types
            }
        }
        
        Ok(())
    }
    
    /// Analyze parameter for semantic tokens
    fn analyze_parameter(&self, tokens: &mut Vec<SemanticToken>, param: &Parameter, span: &pact_lexer::Span, lines: &[&str]) -> Result<(), String> {
        if let Some((line, col)) = self.find_identifier_position(&param.name, span, lines) {
            tokens.push(SemanticToken {
                line: line as u32,
                start: col as u32,
                length: param.name.len() as u32,
                token_type: PactSemanticTokenType::Parameter,
                modifiers: vec![PactSemanticTokenModifier::Declaration],
            });
        }
        
        // Highlight type annotation if present
        if let Some(param_type) = &param.ty {
            self.analyze_type_annotation(tokens, param_type, span, lines)?;
        }
        
        Ok(())
    }
    
    /// Analyze type annotations for semantic tokens
    fn analyze_type_annotation(&self, tokens: &mut Vec<SemanticToken>, type_ref: &Type, span: &pact_lexer::Span, lines: &[&str]) -> Result<(), String> {
        let type_name = self.type_to_string(type_ref);
        
        if let Some((line, col)) = self.find_text_in_span(&type_name, span, lines) {
            tokens.push(SemanticToken {
                line: line as u32,
                start: col as u32,
                length: type_name.len() as u32,
                token_type: PactSemanticTokenType::Type,
                modifiers: vec![],
            });
        }
        
        Ok(())
    }
    
    /// Add type-based highlighting using type inference
    fn add_type_based_tokens(&self, tokens: &mut Vec<SemanticToken>, program: &pact_parser::Program, text: &str) -> Result<(), String> {
        // For now, this is a placeholder for future type inference integration
        // We would use the ExprInferencer here to add type-based highlighting
        
        // Example: Highlight type mismatches, inferred types, etc.
        // This would require integration with the type inference engine
        
        Ok(())
    }
    
    // Helper methods
    
    /// Convert type to string representation
    fn type_to_string(&self, type_ref: &Type) -> String {
        match type_ref {
            Type::String => "string".to_string(),
            Type::Integer => "integer".to_string(),
            Type::Decimal => "decimal".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Time => "time".to_string(),
            Type::Keyset => "keyset".to_string(),
            Type::Guard => "guard".to_string(),
            Type::List(inner) => format!("[{}]", self.type_to_string(inner)),
            Type::Object(obj_type) => {
                match obj_type {
                    pact_parser::ObjectType::Schema(name) => name.clone(),
                    pact_parser::ObjectType::Fields(_) => "object".to_string(),
                }
            },
            Type::ModuleRef(name) => format!("module<{}>", name),
            Type::TypeVar(name) => name.clone(),
        }
    }
    
    /// Convert span to line/column position
    fn span_to_position(&self, span: &pact_lexer::Span, lines: &[&str]) -> Option<(usize, usize)> {
        let mut char_count = 0;
        
        for (line_idx, line) in lines.iter().enumerate() {
            let line_start = char_count;
            let line_end = char_count + line.len();
            
            if span.start >= line_start && span.start <= line_end {
                let col = span.start - line_start;
                return Some((line_idx, col));
            }
            
            char_count = line_end + 1; // +1 for newline
        }
        
        None
    }
    
    /// Find identifier position within a span
    fn find_identifier_position(&self, identifier: &str, span: &pact_lexer::Span, lines: &[&str]) -> Option<(usize, usize)> {
        self.find_text_in_span(identifier, span, lines)
    }
    
    /// Find text within a span
    fn find_text_in_span(&self, text: &str, span: &pact_lexer::Span, lines: &[&str]) -> Option<(usize, usize)> {
        let mut char_count = 0;
        
        for (line_idx, line) in lines.iter().enumerate() {
            let line_start = char_count;
            let line_end = char_count + line.len();
            
            if span.start >= line_start && span.start <= line_end {
                // Look for the text in this line
                if let Some(col_offset) = line.find(text) {
                    return Some((line_idx, col_offset));
                }
            }
            
            char_count = line_end + 1; // +1 for newline
        }
        
        None
    }
}

/// Get semantic token legend for client capabilities
pub fn get_semantic_token_legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: vec![
            SemanticTokenType::VARIABLE,        // 0
            SemanticTokenType::PARAMETER,       // 1
            SemanticTokenType::FUNCTION,        // 2
            SemanticTokenType::METHOD,          // 3
            SemanticTokenType::TYPE,            // 4
            SemanticTokenType::TYPE_PARAMETER,  // 5
            SemanticTokenType::STRUCT,          // 6
            SemanticTokenType::INTERFACE,       // 7
            SemanticTokenType::KEYWORD,         // 8
            SemanticTokenType::OPERATOR,        // 9
            SemanticTokenType::STRING,          // 10
            SemanticTokenType::NUMBER,          // 11
            SemanticTokenType::DECORATOR,       // 12
            SemanticTokenType::PROPERTY,        // 13
            SemanticTokenType::NAMESPACE,       // 14
            SemanticTokenType::COMMENT,         // 15
        ],
        token_modifiers: vec![
            SemanticTokenModifier::DECLARATION,
            SemanticTokenModifier::DEFINITION,
            SemanticTokenModifier::READONLY,
            SemanticTokenModifier::STATIC,
            SemanticTokenModifier::DEPRECATED,
            SemanticTokenModifier::ABSTRACT,
            SemanticTokenModifier::ASYNC,
            SemanticTokenModifier::MODIFICATION,
            SemanticTokenModifier::DOCUMENTATION,
            SemanticTokenModifier::DEFAULT_LIBRARY,
        ],
    }
}

/// Convert semantic tokens to LSP format
pub fn tokens_to_lsp_format(tokens: &[SemanticToken]) -> Vec<SemanticToken> {
    // For now, return as-is
    // In a full implementation, we would convert to the relative encoding expected by LSP
    tokens.to_vec()
}