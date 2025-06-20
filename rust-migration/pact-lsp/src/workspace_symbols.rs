//! Workspace symbol search and project navigation for Pact LSP
//!
//! This module provides project-wide symbol search and navigation capabilities,
//! allowing users to quickly find functions, types, tables, and other symbols
//! across the entire workspace.

use crate::semantic::SemanticAnalyzer;
use tower_lsp::lsp_types::*;
use pact_parser::{parse, Declaration, TopLevel, Expr};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// Workspace symbol provider for project-wide navigation
pub struct WorkspaceSymbolProvider {
    semantic_analyzer: Arc<SemanticAnalyzer>,
    symbol_cache: HashMap<Url, Vec<WorkspaceSymbol>>,
    file_cache: HashMap<Url, String>,
}

/// Symbol information with enhanced metadata
#[derive(Debug, Clone)]
pub struct PactSymbolInfo {
    pub name: String,
    pub kind: SymbolKind,
    pub location: Location,
    pub container_name: Option<String>,
    pub detail: Option<String>,
    pub documentation: Option<String>,
    pub deprecated: bool,
    pub tags: Vec<SymbolTag>,
}

impl WorkspaceSymbolProvider {
    /// Create a new workspace symbol provider
    pub fn new(semantic_analyzer: Arc<SemanticAnalyzer>) -> Self {
        Self {
            semantic_analyzer,
            symbol_cache: HashMap::new(),
            file_cache: HashMap::new(),
        }
    }
    
    /// Index a file for workspace symbols
    pub fn index_file(&mut self, uri: Url, content: String) -> Result<(), String> {
        // Store file content
        self.file_cache.insert(uri.clone(), content.clone());
        
        // Parse and extract symbols
        let symbols = self.extract_symbols_from_content(&uri, &content)?;
        
        // Update symbol cache
        self.symbol_cache.insert(uri, symbols);
        
        Ok(())
    }
    
    /// Remove file from index
    pub fn remove_file(&mut self, uri: &Url) {
        self.symbol_cache.remove(uri);
        self.file_cache.remove(uri);
    }
    
    /// Search for workspace symbols
    pub fn search_symbols(&self, query: &str) -> Vec<WorkspaceSymbol> {
        let mut results = Vec::new();
        let query_lower = query.to_lowercase();
        
        // Search through all cached symbols
        for symbols in self.symbol_cache.values() {
            for symbol in symbols {
                if self.symbol_matches(&symbol.name, &query_lower) {
                    results.push(symbol.clone());
                }
            }
        }
        
        // Sort results by relevance
        results.sort_by(|a, b| {
            self.calculate_relevance(&a.name, &query_lower)
                .cmp(&self.calculate_relevance(&b.name, &query_lower))
                .reverse()
        });
        
        // Limit results to reasonable number
        results.truncate(100);
        
        results
    }
    
    /// Get all symbols in a specific file
    pub fn get_file_symbols(&self, uri: &Url) -> Vec<WorkspaceSymbol> {
        self.symbol_cache.get(uri).cloned().unwrap_or_default()
    }
    
    /// Get symbols by kind
    pub fn get_symbols_by_kind(&self, kind: SymbolKind) -> Vec<WorkspaceSymbol> {
        let mut results = Vec::new();
        
        for symbols in self.symbol_cache.values() {
            for symbol in symbols {
                if symbol.kind == kind {
                    results.push(symbol.clone());
                }
            }
        }
        
        results
    }
    
    /// Get all function symbols
    pub fn get_functions(&self) -> Vec<WorkspaceSymbol> {
        self.get_symbols_by_kind(SymbolKind::FUNCTION)
    }
    
    /// Get all module symbols
    pub fn get_modules(&self) -> Vec<WorkspaceSymbol> {
        self.get_symbols_by_kind(SymbolKind::MODULE)
    }
    
    /// Get all table symbols
    pub fn get_tables(&self) -> Vec<WorkspaceSymbol> {
        self.get_symbols_by_kind(SymbolKind::FIELD) // Using FIELD for tables
    }
    
    /// Get all capability symbols
    pub fn get_capabilities(&self) -> Vec<WorkspaceSymbol> {
        self.get_symbols_by_kind(SymbolKind::METHOD) // Using METHOD for capabilities
    }
    
    /// Get all schema symbols
    pub fn get_schemas(&self) -> Vec<WorkspaceSymbol> {
        self.get_symbols_by_kind(SymbolKind::STRUCT)
    }
    
    /// Get all interface symbols
    pub fn get_interfaces(&self) -> Vec<WorkspaceSymbol> {
        self.get_symbols_by_kind(SymbolKind::INTERFACE)
    }
    
    /// Extract symbols from file content
    fn extract_symbols_from_content(&self, uri: &Url, content: &str) -> Result<Vec<WorkspaceSymbol>, String> {
        let mut symbols = Vec::new();
        
        match parse(content) {
            Ok(program) => {
                let lines: Vec<&str> = content.lines().collect();
                
                for item in &program.items {
                    self.extract_symbols_from_top_level(&mut symbols, item, uri, &lines)?;
                }
                
                Ok(symbols)
            },
            Err(_) => {
                // If parsing fails, return empty symbols
                Ok(symbols)
            }
        }
    }
    
    /// Extract symbols from top-level items
    fn extract_symbols_from_top_level(
        &self,
        symbols: &mut Vec<WorkspaceSymbol>,
        item: &TopLevel,
        uri: &Url,
        lines: &[&str],
    ) -> Result<(), String> {
        match item {
            TopLevel::Module(module) => {
                // Add module symbol
                if let Some(location) = self.create_location(uri, &module.span, lines) {
                    symbols.push(WorkspaceSymbol {
                        name: module.name.clone(),
                        kind: SymbolKind::MODULE,
                        tags: None,
                        container_name: None,
                        location: OneOf::Left(location),
                        data: None,
                    });
                }
                
                // Extract symbols from module declarations
                for decl in &module.declarations {
                    self.extract_symbols_from_declaration(
                        symbols,
                        decl,
                        uri,
                        lines,
                        Some(&module.name),
                    )?;
                }
            },
            TopLevel::Interface(interface) => {
                // Add interface symbol
                if let Some(location) = self.create_location(uri, &interface.span, lines) {
                    symbols.push(WorkspaceSymbol {
                        name: interface.name.clone(),
                        kind: SymbolKind::INTERFACE,
                        tags: None,
                        container_name: None,
                        location: OneOf::Left(location),
                        data: None,
                    });
                }
                
                // Extract symbols from interface declarations
                for decl in &interface.declarations {
                    self.extract_symbols_from_declaration(
                        symbols,
                        decl,
                        uri,
                        lines,
                        Some(&interface.name),
                    )?;
                }
            },
            TopLevel::Use(use_stmt) => {
                // Add use statement as import symbol
                if let Some(location) = self.create_location(uri, &use_stmt.span, lines) {
                    symbols.push(WorkspaceSymbol {
                        name: format!("use {}", use_stmt.module),
                        kind: SymbolKind::NAMESPACE,
                        tags: None,
                        container_name: None,
                        location: OneOf::Left(location),
                        data: None,
                    });
                }
            },
            TopLevel::Declaration(decl) => {
                self.extract_symbols_from_declaration(symbols, decl, uri, lines, None)?;
            },
            TopLevel::Expression(_) => {
                // Top-level expressions don't create workspace symbols
            },
        }
        
        Ok(())
    }
    
    /// Extract symbols from declarations
    fn extract_symbols_from_declaration(
        &self,
        symbols: &mut Vec<WorkspaceSymbol>,
        decl: &Declaration,
        uri: &Url,
        lines: &[&str],
        container_name: Option<&str>,
    ) -> Result<(), String> {
        match decl {
            Declaration::Defun(defun) => {
                if let Some(location) = self.create_location(uri, &defun.span, lines) {
                    let detail = self.create_function_detail(&defun.name, &defun.params, &defun.return_type);
                    
                    symbols.push(WorkspaceSymbol {
                        name: defun.name.clone(),
                        kind: SymbolKind::FUNCTION,
                        tags: None,
                        container_name: container_name.map(|s| s.to_string()),
                        location: OneOf::Left(location),
                        data: Some(serde_json::json!({
                            "detail": detail,
                            "documentation": defun.docs,
                            "parameters": defun.params.len(),
                            "return_type": defun.return_type.as_ref().map(|t| self.type_to_string(t)),
                        })),
                    });
                }
            },
            Declaration::Defcap(defcap) => {
                if let Some(location) = self.create_location(uri, &defcap.span, lines) {
                    let detail = self.create_capability_detail(&defcap.name, &defcap.params);
                    
                    symbols.push(WorkspaceSymbol {
                        name: defcap.name.clone(),
                        kind: SymbolKind::METHOD, // Using METHOD for capabilities
                        tags: if defcap.event { Some(vec![SymbolTag::DEPRECATED]) } else { None }, // Just an example
                        container_name: container_name.map(|s| s.to_string()),
                        location: OneOf::Left(location),
                        data: Some(serde_json::json!({
                            "detail": detail,
                            "documentation": defcap.docs,
                            "parameters": defcap.params.len(),
                            "managed": defcap.managed.is_some(),
                            "event": defcap.event,
                        })),
                    });
                }
            },
            Declaration::Defconst(defconst) => {
                if let Some(location) = self.create_location(uri, &defconst.span, lines) {
                    let detail = self.create_constant_detail(&defconst.name, &defconst.type_ann);
                    
                    symbols.push(WorkspaceSymbol {
                        name: defconst.name.clone(),
                        kind: SymbolKind::CONSTANT,
                        tags: None,
                        container_name: container_name.map(|s| s.to_string()),
                        location: OneOf::Left(location),
                        data: Some(serde_json::json!({
                            "detail": detail,
                            "documentation": defconst.docs,
                            "type": defconst.type_ann.as_ref().map(|t| self.type_to_string(t)),
                        })),
                    });
                }
            },
            Declaration::Defschema(defschema) => {
                if let Some(location) = self.create_location(uri, &defschema.span, lines) {
                    let detail = self.create_schema_detail(&defschema.name, &defschema.fields);
                    
                    symbols.push(WorkspaceSymbol {
                        name: defschema.name.clone(),
                        kind: SymbolKind::STRUCT,
                        tags: None,
                        container_name: container_name.map(|s| s.to_string()),
                        location: OneOf::Left(location),
                        data: Some(serde_json::json!({
                            "detail": detail,
                            "documentation": defschema.docs,
                            "fields": defschema.fields.len(),
                        })),
                    });
                }
                
                // Add field symbols
                for field in &defschema.fields {
                    if let Some(location) = self.create_field_location(uri, &defschema.span, &field.name, lines) {
                        symbols.push(WorkspaceSymbol {
                            name: field.name.clone(),
                            kind: SymbolKind::FIELD,
                            tags: None,
                            container_name: Some(defschema.name.clone()),
                            location: OneOf::Left(location),
                            data: Some(serde_json::json!({
                                "type": self.type_to_string(&field.ty),
                                "schema": defschema.name,
                            })),
                        });
                    }
                }
            },
            Declaration::Deftable(deftable) => {
                if let Some(location) = self.create_location(uri, &deftable.span, lines) {
                    let detail = self.create_table_detail(&deftable.name, &deftable.schema);
                    
                    symbols.push(WorkspaceSymbol {
                        name: deftable.name.clone(),
                        kind: SymbolKind::FIELD, // Using FIELD for tables
                        tags: None,
                        container_name: container_name.map(|s| s.to_string()),
                        location: OneOf::Left(location),
                        data: Some(serde_json::json!({
                            "detail": detail,
                            "documentation": deftable.docs,
                            "schema": deftable.schema,
                        })),
                    });
                }
            },
            Declaration::Defpact(defpact) => {
                if let Some(location) = self.create_location(uri, &defpact.span, lines) {
                    let detail = self.create_pact_detail(&defpact.name, &defpact.params, defpact.steps.len());
                    
                    symbols.push(WorkspaceSymbol {
                        name: defpact.name.clone(),
                        kind: SymbolKind::FUNCTION,
                        tags: None,
                        container_name: container_name.map(|s| s.to_string()),
                        location: OneOf::Left(location),
                        data: Some(serde_json::json!({
                            "detail": detail,
                            "documentation": defpact.docs,
                            "parameters": defpact.params.len(),
                            "steps": defpact.steps.len(),
                        })),
                    });
                }
            },
            Declaration::Implements(interface_name) => {
                // Add implements reference
                symbols.push(WorkspaceSymbol {
                    name: format!("implements {}", interface_name),
                    kind: SymbolKind::INTERFACE,
                    tags: Some(vec![]),
                    container_name: container_name.map(|s| s.to_string()),
                    location: OneOf::Left(Location {
                        uri: uri.clone(),
                        range: Range {
                            start: Position { line: 0, character: 0 },
                            end: Position { line: 0, character: 0 },
                        },
                    }),
                    data: Some(serde_json::json!({
                        "interface": interface_name,
                    })),
                });
            },
        }
        
        Ok(())
    }
    
    // Symbol matching and relevance calculation
    
    /// Check if symbol matches query
    fn symbol_matches(&self, symbol_name: &str, query: &str) -> bool {
        let symbol_lower = symbol_name.to_lowercase();
        
        // Exact match
        if symbol_lower == query {
            return true;
        }
        
        // Starts with match
        if symbol_lower.starts_with(query) {
            return true;
        }
        
        // Contains match
        if symbol_lower.contains(query) {
            return true;
        }
        
        // Fuzzy match (check if all characters of query appear in order)
        self.fuzzy_match(&symbol_lower, query)
    }
    
    /// Fuzzy matching algorithm
    fn fuzzy_match(&self, text: &str, pattern: &str) -> bool {
        let mut text_chars = text.chars();
        let mut pattern_chars = pattern.chars();
        
        if let Some(mut pattern_char) = pattern_chars.next() {
            for text_char in text_chars {
                if text_char == pattern_char {
                    if let Some(next_pattern_char) = pattern_chars.next() {
                        pattern_char = next_pattern_char;
                    } else {
                        return true; // All pattern characters matched
                    }
                }
            }
        }
        
        false
    }
    
    /// Calculate relevance score for sorting
    fn calculate_relevance(&self, symbol_name: &str, query: &str) -> i32 {
        let symbol_lower = symbol_name.to_lowercase();
        
        // Exact match - highest score
        if symbol_lower == query {
            return 1000;
        }
        
        // Starts with match - high score
        if symbol_lower.starts_with(query) {
            return 800;
        }
        
        // Word boundary match - medium-high score
        if symbol_lower.split('_').any(|part| part.starts_with(query)) ||
           symbol_lower.split('-').any(|part| part.starts_with(query)) {
            return 600;
        }
        
        // Contains match - medium score
        if symbol_lower.contains(query) {
            return 400;
        }
        
        // Fuzzy match - lower score based on distance
        if self.fuzzy_match(&symbol_lower, query) {
            let distance = self.levenshtein_distance(&symbol_lower, query);
            return 200 - distance.min(200);
        }
        
        0
    }
    
    /// Calculate Levenshtein distance
    fn levenshtein_distance(&self, s1: &str, s2: &str) -> i32 {
        let len1 = s1.chars().count();
        let len2 = s2.chars().count();
        
        if len1 == 0 { return len2 as i32; }
        if len2 == 0 { return len1 as i32; }
        
        let mut matrix = vec![vec![0; len2 + 1]; len1 + 1];
        
        for i in 0..=len1 {
            matrix[i][0] = i;
        }
        for j in 0..=len2 {
            matrix[0][j] = j;
        }
        
        let s1_chars: Vec<char> = s1.chars().collect();
        let s2_chars: Vec<char> = s2.chars().collect();
        
        for i in 1..=len1 {
            for j in 1..=len2 {
                let cost = if s1_chars[i - 1] == s2_chars[j - 1] { 0 } else { 1 };
                matrix[i][j] = (matrix[i - 1][j] + 1)
                    .min(matrix[i][j - 1] + 1)
                    .min(matrix[i - 1][j - 1] + cost);
            }
        }
        
        matrix[len1][len2] as i32
    }
    
    // Helper methods for creating symbols
    
    /// Create location from span
    fn create_location(&self, uri: &Url, span: &pact_lexer::Span, lines: &[&str]) -> Option<Location> {
        let (start_line, start_char) = self.span_to_position(span.start, lines)?;
        let (end_line, end_char) = self.span_to_position(span.end, lines)?;
        
        Some(Location {
            uri: uri.clone(),
            range: Range {
                start: Position {
                    line: start_line as u32,
                    character: start_char as u32,
                },
                end: Position {
                    line: end_line as u32,
                    character: end_char as u32,
                },
            },
        })
    }
    
    /// Create location for a field within a schema
    fn create_field_location(&self, uri: &Url, schema_span: &pact_lexer::Span, field_name: &str, lines: &[&str]) -> Option<Location> {
        // For now, use the schema span
        // In a full implementation, we would find the exact field location
        self.create_location(uri, schema_span, lines)
    }
    
    /// Convert span position to line/character
    fn span_to_position(&self, pos: usize, lines: &[&str]) -> Option<(usize, usize)> {
        let mut char_count = 0;
        
        for (line_idx, line) in lines.iter().enumerate() {
            let line_start = char_count;
            let line_end = char_count + line.len();
            
            if pos >= line_start && pos <= line_end {
                let char_pos = pos - line_start;
                return Some((line_idx, char_pos));
            }
            
            char_count = line_end + 1; // +1 for newline
        }
        
        None
    }
    
    /// Create function detail string
    fn create_function_detail(&self, name: &str, params: &[pact_parser::Parameter], return_type: &Option<pact_parser::Type>) -> String {
        let param_list = params.iter()
            .map(|p| {
                if let Some(ty) = &p.ty {
                    format!("{}: {}", p.name, self.type_to_string(ty))
                } else {
                    p.name.clone()
                }
            })
            .collect::<Vec<_>>()
            .join(", ");
        
        if let Some(ret_ty) = return_type {
            format!("(defun {} ({}) -> {})", name, param_list, self.type_to_string(ret_ty))
        } else {
            format!("(defun {} ({}))", name, param_list)
        }
    }
    
    /// Create capability detail string
    fn create_capability_detail(&self, name: &str, params: &[pact_parser::Parameter]) -> String {
        let param_list = params.iter()
            .map(|p| {
                if let Some(ty) = &p.ty {
                    format!("{}: {}", p.name, self.type_to_string(ty))
                } else {
                    p.name.clone()
                }
            })
            .collect::<Vec<_>>()
            .join(", ");
        
        format!("(defcap {} ({}))", name, param_list)
    }
    
    /// Create constant detail string
    fn create_constant_detail(&self, name: &str, type_ann: &Option<pact_parser::Type>) -> String {
        if let Some(ty) = type_ann {
            format!("(defconst {} : {})", name, self.type_to_string(ty))
        } else {
            format!("(defconst {})", name)
        }
    }
    
    /// Create schema detail string
    fn create_schema_detail(&self, name: &str, fields: &[pact_parser::Field]) -> String {
        format!("(defschema {} <{} fields>)", name, fields.len())
    }
    
    /// Create table detail string
    fn create_table_detail(&self, name: &str, schema: &str) -> String {
        format!("(deftable {}:{})", name, schema)
    }
    
    /// Create pact detail string
    fn create_pact_detail(&self, name: &str, params: &[pact_parser::Parameter], step_count: usize) -> String {
        let param_list = params.iter()
            .map(|p| {
                if let Some(ty) = &p.ty {
                    format!("{}: {}", p.name, self.type_to_string(ty))
                } else {
                    p.name.clone()
                }
            })
            .collect::<Vec<_>>()
            .join(", ");
        
        format!("(defpact {} ({}) <{} steps>)", name, param_list, step_count)
    }
    
    /// Convert type to string
    fn type_to_string(&self, ty: &pact_parser::Type) -> String {
        match ty {
            pact_parser::Type::String => "string".to_string(),
            pact_parser::Type::Integer => "integer".to_string(),
            pact_parser::Type::Decimal => "decimal".to_string(),
            pact_parser::Type::Bool => "bool".to_string(),
            pact_parser::Type::Time => "time".to_string(),
            pact_parser::Type::Keyset => "keyset".to_string(),
            pact_parser::Type::Guard => "guard".to_string(),
            pact_parser::Type::List(inner) => format!("[{}]", self.type_to_string(inner)),
            pact_parser::Type::Object(obj_type) => {
                match obj_type {
                    pact_parser::ObjectType::Schema(name) => name.clone(),
                    pact_parser::ObjectType::Fields(_) => "object".to_string(),
                }
            },
            pact_parser::Type::ModuleRef(name) => format!("module<{}>", name),
            pact_parser::Type::TypeVar(name) => name.clone(),
        }
    }
}