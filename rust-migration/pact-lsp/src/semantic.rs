//! Semantic analysis for Pact LSP

use crate::document::Document;
use dashmap::DashMap;
use tower_lsp::lsp_types::*;
use pact_lexer::Token;
use pact_parser::{parse, ParsedExpr, Binder, ParsedDef, ParsedTopLevel};
use pact_values::PactValue;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

/// Symbol information for semantic analysis
#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub range: Range,
    pub detail: Option<String>,
    pub documentation: Option<String>,
    pub definition_uri: Option<Url>,
    pub definition_range: Option<Range>,
    pub symbol_type: Option<String>,
    pub scope: SymbolScope,
}

/// Symbol scope information
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolScope {
    Global,
    Module(String),
    Function(String),
    Let(String),
    Parameter(String),
}

/// Type information for symbols
#[derive(Debug, Clone)]
pub struct TypeInfo {
    pub type_name: String,
    pub is_optional: bool,
    pub constraints: Vec<String>,
    pub documentation: Option<String>,
}

/// Semantic analysis context
#[derive(Debug)]
pub struct SemanticContext {
    symbols: HashMap<String, Symbol>,
    scopes: Vec<SymbolScope>,
    current_module: Option<String>,
    current_function: Option<String>,
    type_environment: HashMap<String, TypeInfo>,
    imports: HashMap<String, String>, // alias -> full_name
    capabilities: HashSet<String>,
    errors: Vec<SemanticError>,
    warnings: Vec<SemanticWarning>,
}

/// Semantic error types
#[derive(Debug, Clone)]
pub struct SemanticError {
    pub range: Range,
    pub message: String,
    pub error_type: SemanticErrorType,
    pub suggestions: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum SemanticErrorType {
    UndefinedSymbol,
    TypeMismatch,
    InvalidArity,
    UnresolvedImport,
    DuplicateDefinition,
    MissingCapability,
    InvalidScope,
    CircularDependency,
}

/// Semantic warning types
#[derive(Debug, Clone)]
pub struct SemanticWarning {
    pub range: Range,
    pub message: String,
    pub warning_type: SemanticWarningType,
}

#[derive(Debug, Clone)]
pub enum SemanticWarningType {
    UnusedVariable,
    UnusedImport,
    DeadCode,
    DeprecatedFunction,
    PerformanceHint,
    StyleSuggestion,
}

/// Workspace semantic analyzer
pub struct SemanticAnalyzer {
    documents: Arc<DashMap<Url, Document>>,
    symbols: Arc<DashMap<Url, Vec<Symbol>>>,
    global_symbols: Arc<DashMap<String, Symbol>>,
    builtin_symbols: HashMap<String, Symbol>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let mut builtin_symbols = HashMap::new();

        // Initialize builtin function symbols
        builtin_symbols.insert("+".to_string(), Symbol {
            name: "+".to_string(),
            kind: SymbolKind::FUNCTION,
            range: Range::default(),
            detail: Some("(+ x y) -> number".to_string()),
            documentation: Some("Addition operator. Works with integers and decimals.".to_string()),
            definition_uri: None,
            definition_range: None,
            symbol_type: Some("(integer|decimal, integer|decimal) -> integer|decimal".to_string()),
            scope: SymbolScope::Global,
        });

        builtin_symbols.insert("defun".to_string(), Symbol {
            name: "defun".to_string(),
            kind: SymbolKind::FUNCTION,
            range: Range::default(),
            detail: Some("(defun name (params...) body)".to_string()),
            documentation: Some("Define a function with parameters and body.".to_string()),
            definition_uri: None,
            definition_range: None,
            symbol_type: Some("keyword".to_string()),
            scope: SymbolScope::Global,
        });

        builtin_symbols.insert("let".to_string(), Symbol {
            name: "let".to_string(),
            kind: SymbolKind::FUNCTION,
            range: Range::default(),
            detail: Some("(let ((var val) ...) body)".to_string()),
            documentation: Some("Bind variables in local scope.".to_string()),
            definition_uri: None,
            definition_range: None,
            symbol_type: Some("keyword".to_string()),
            scope: SymbolScope::Global,
        });

        // Add more builtins...
        Self::add_common_builtins(&mut builtin_symbols);

        Self {
            documents: Arc::new(DashMap::new()),
            symbols: Arc::new(DashMap::new()),
            global_symbols: Arc::new(DashMap::new()),
            builtin_symbols,
        }
    }

    /// Add common builtin functions
    fn add_common_builtins(builtins: &mut HashMap<String, Symbol>) {
        let common_functions = vec![
            ("-", "(- x y) -> number", "Subtraction operator"),
            ("*", "(* x y) -> number", "Multiplication operator"),
            ("/", "(/ x y) -> number", "Division operator"),
            ("=", "(= x y) -> bool", "Equality comparison"),
            (">", "(> x y) -> bool", "Greater than comparison"),
            ("<", "(< x y) -> bool", "Less than comparison"),
            ("and", "(and x y) -> bool", "Logical AND"),
            ("or", "(or x y) -> bool", "Logical OR"),
            ("not", "(not x) -> bool", "Logical NOT"),
            ("if", "(if condition then else) -> any", "Conditional expression"),
            ("length", "(length collection) -> integer", "Get length of string or list"),
            ("at", "(at index collection) -> any", "Get element at index"),
            ("contains", "(contains item collection) -> bool", "Check if collection contains item"),
            ("map", "(map function list) -> list", "Apply function to each element"),
            ("filter", "(filter predicate list) -> list", "Filter list by predicate"),
            ("fold", "(fold function initial list) -> any", "Reduce list with function"),
            ("enforce", "(enforce condition message) -> bool", "Assert condition or fail"),
            ("with-capability", "(with-capability cap body) -> any", "Execute with capability"),
            ("module", "(module name keyset body) -> module", "Define a module"),
            ("defcap", "(defcap name params body) -> capability", "Define a capability"),
            ("defconst", "(defconst name value) -> constant", "Define a constant"),
            ("defschema", "(defschema name fields) -> schema", "Define a schema"),
            ("deftable", "(deftable name schema) -> table", "Define a table"),
        ];

        for (name, detail, doc) in common_functions {
            builtins.insert(name.to_string(), Symbol {
                name: name.to_string(),
                kind: SymbolKind::FUNCTION,
                range: Range::default(),
                detail: Some(detail.to_string()),
                documentation: Some(doc.to_string()),
                definition_uri: None,
                definition_range: None,
                symbol_type: Some("builtin".to_string()),
                scope: SymbolScope::Global,
            });
        }
    }

    /// Analyze a document and return semantic information
    pub fn analyze_document(&self, uri: &Url, text: &str) -> SemanticAnalysis {
        let mut context = SemanticContext::new();

        // Parse the document
        match parse(text) {
            Ok(program) => {
                for item in program.items {
                    self.analyze_top_level(&item, &mut context, text, uri);
                }
            }
            Err(e) => {
                context.errors.push(SemanticError {
                    range: Range {
                        start: Position { line: 0, character: 0 },
                        end: Position { line: 0, character: 0 },
                    },
                    message: format!("Parse error: {}", e),
                    error_type: SemanticErrorType::InvalidScope,
                    suggestions: vec!["Check syntax and fix parsing errors".to_string()],
                });
            }
        }

        // Store symbols for this document
        let symbols: Vec<Symbol> = context.symbols.values().cloned().collect();
        self.symbols.insert(uri.clone(), symbols.clone());

        SemanticAnalysis {
            symbols,
            diagnostics: self.create_diagnostics(&context),
            type_information: context.type_environment,
        }
    }

    /// Analyze a top-level item
    fn analyze_top_level(&self, item: &TopLevel, context: &mut SemanticContext, text: &str, uri: &Url) {
        match item {
            TopLevel::Module(module) => {
                // Enter module scope
                context.enter_scope(SymbolScope::Module(module.name.clone()));
                context.current_module = Some(module.name.clone());

                // Add module symbol
                context.add_symbol(Symbol {
                    name: module.name.clone(),
                    kind: SymbolKind::MODULE,
                    range: Range::default(),
                    detail: Some(format!("module {}", module.name)),
                    documentation: module.meta.as_ref().and_then(|m| m.docs.clone()),
                    definition_uri: Some(uri.clone()),
                    definition_range: None,
                    symbol_type: Some("module".to_string()),
                    scope: SymbolScope::Global,
                });

                // Analyze module declarations
                for declaration in &module.declarations {
                    self.analyze_declaration(declaration, context, text, uri);
                }

                // Exit module scope
                context.exit_scope();
                context.current_module = None;
            }
            TopLevel::Interface(interface) => {
                // Analyze interface declarations
                for declaration in &interface.declarations {
                    self.analyze_declaration(declaration, context, text, uri);
                }
            }
            TopLevel::Expression(expr) => {
                self.analyze_expression(expr, context, text, uri);
            }
            TopLevel::Use(_use_stmt) => {
                // Handle imports - could track module dependencies
            }
            TopLevel::Declaration(declaration) => {
                self.analyze_declaration(declaration, context, text, uri);
            }
        }
    }

    /// Check if a function is a builtin
    pub fn is_builtin_function(&self, name: &str) -> bool {
        self.builtin_symbols.contains_key(name)
    }

    /// Get variable type if known
    pub fn get_variable_type(&self, name: &str) -> Option<String> {
        // Check builtin symbols first
        if let Some(symbol) = self.builtin_symbols.get(name) {
            return symbol.symbol_type.clone();
        }
        
        // Check global symbols
        if let Some(symbol) = self.global_symbols.get(name) {
            return symbol.symbol_type.clone();
        }
        
        None
    }

    /// Get symbol at position
    pub fn get_symbol_at_position(&self, uri: &Url, position: Position) -> Option<Symbol> {
        if let Some(symbols) = self.symbols.get(uri) {
            for symbol in symbols.iter() {
                if self.position_in_range(position, symbol.range) {
                    return Some(symbol.clone());
                }
            }
        }
        None
    }

    /// Find references to a symbol
    pub fn find_references(&self, symbol_name: &str) -> Vec<Location> {
        let mut references = Vec::new();
        
        for entry in self.symbols.iter() {
            let uri = entry.key();
            let symbols = entry.value();
            
            for symbol in symbols.iter() {
                if symbol.name == symbol_name {
                    references.push(Location {
                        uri: uri.clone(),
                        range: symbol.range,
                    });
                }
            }
        }
        
        references
    }

    /// Check if position is in range
    fn position_in_range(&self, position: Position, range: Range) -> bool {
        (position.line > range.start.line || 
         (position.line == range.start.line && position.character >= range.start.character)) &&
        (position.line < range.end.line || 
         (position.line == range.end.line && position.character <= range.end.character))
    }

    /// Analyze an expression recursively
    fn analyze_expression(&self, expr: &Expr, context: &mut SemanticContext, text: &str, uri: &Url) {
        match expr {
            Expr::Integer(_, _) | Expr::Decimal(_, _) | Expr::String(_, _) | Expr::Bool(_, _) => {
                // Literals don't need semantic analysis
            }

            Expr::Var(name, _) => {
                self.analyze_variable_reference(name, context);
            }

            Expr::App { func, args, .. } => {
                // Analyze function call
                self.analyze_expression(func.as_ref(), context, text, uri);

                // Recursively analyze arguments
                for arg in args {
                    self.analyze_expression(arg, context, text, uri);
                }
            }

            Expr::Let { bindings, body, span } => {
                // Enter new scope
                context.enter_scope(SymbolScope::Let("let".to_string()));

                // Analyze bindings
                for binding in bindings {
                    self.analyze_binding(binding, context, text, uri);
                }

                // Analyze body
                for expr in body {
                    self.analyze_expression(expr, context, text, uri);
                }

                // Exit scope
                context.exit_scope();
            }

            Expr::If { cond, then_expr, else_expr, span } => {
                self.analyze_expression(cond.as_ref(), context, text, uri);
                self.analyze_expression(then_expr.as_ref(), context, text, uri);
                if let Some(else_e) = else_expr {
                    self.analyze_expression(else_e.as_ref(), context, text, uri);
                }
            }

            // Definitions are now handled as separate Declaration variants
            // This is handled in the top-level parsing

            // Module expressions are handled at top-level

            Expr::List(elements, _) => {
                for element in elements {
                    self.analyze_expression(element, context, text, uri);
                }
            }

            Expr::Object(fields, _) => {
                for (_, value) in fields {
                    self.analyze_expression(value, context, text, uri);
                }
            }

            // Handle all other expression types with basic traversal
            _ => {
                // For now, we'll handle other expression types with minimal analysis
                // This can be expanded later as needed
            }
        }
    }

    /// Analyze variable reference
    fn analyze_variable_reference(&self, name: &str, context: &mut SemanticContext) {
        // Check if variable is defined in current scope
        if !context.is_symbol_defined(name) && !self.builtin_symbols.contains_key(name) {
            context.errors.push(SemanticError {
                range: Range::default(), // TODO: Get actual range from AST
                message: format!("Undefined symbol: {}", name),
                error_type: SemanticErrorType::UndefinedSymbol,
                suggestions: self.suggest_similar_symbols(name, context),
            });
        }
    }

    /// Analyze function call
    fn analyze_function_call(&self, function: &Expr, arguments: &[Expr], context: &mut SemanticContext, _text: &str, _uri: &Url) {
        if let Expr::Var(func_name, _) = function {
            // Check if function exists
            if let Some(symbol) = context.get_symbol(func_name).or_else(|| self.builtin_symbols.get(func_name)) {
                // Validate arity if known
                self.validate_function_arity(func_name, arguments.len(), context);

                // Check capability requirements
                if self.requires_capability(func_name) {
                    self.check_capability_available(func_name, context);
                }
            } else {
                context.errors.push(SemanticError {
                    range: Range::default(),
                    message: format!("Unknown function: {}", func_name),
                    error_type: SemanticErrorType::UndefinedSymbol,
                    suggestions: self.suggest_similar_functions(func_name),
                });
            }
        }
    }

    /// Analyze let binding
    fn analyze_binding(&self, binding: &Binding, context: &mut SemanticContext, text: &str, uri: &Url) {
        // Add symbol to current scope
        context.add_symbol(Symbol {
            name: binding.name.clone(),
            kind: SymbolKind::VARIABLE,
            range: Range::default(), // TODO: Get from AST
            detail: Some("local variable".to_string()),
            documentation: None,
            definition_uri: Some(uri.clone()),
            definition_range: None,
            symbol_type: None, // TODO: Infer type
            scope: context.current_scope().clone(),
        });

        // Analyze the value expression
        self.analyze_expression(&binding.value, context, text, uri);
    }

    /// Analyze declaration
    fn analyze_declaration(&self, declaration: &Declaration, context: &mut SemanticContext, text: &str, uri: &Url) {
        match declaration {
            Declaration::Defun(defun) => {
                let symbol = Symbol {
                    name: defun.name.clone(),
                    kind: SymbolKind::FUNCTION,
                    range: Range::default(), // TODO: Get from AST
                    detail: Some(format!("(defun {})", defun.name)),
                    documentation: defun.docs.clone(),
                    definition_uri: Some(uri.clone()),
                    definition_range: None,
                    symbol_type: None, // TODO: Build function type
                    scope: context.current_scope().clone(),
                };

                context.add_symbol(symbol.clone());
                self.global_symbols.insert(defun.name.clone(), symbol);

                // Enter function scope and analyze body
                context.enter_scope(SymbolScope::Function(defun.name.clone()));
                for expr in &defun.body {
                    self.analyze_expression(expr, context, text, uri);
                }
                context.exit_scope();
            }
            Declaration::Defcap(defcap) => {
                let symbol = Symbol {
                    name: defcap.name.clone(),
                    kind: SymbolKind::FUNCTION, // Capabilities are functions with special semantics
                    range: Range::default(),
                    detail: Some(format!("(defcap {})", defcap.name)),
                    documentation: defcap.docs.clone(),
                    definition_uri: Some(uri.clone()),
                    definition_range: None,
                    symbol_type: Some("capability".to_string()),
                    scope: context.current_scope().clone(),
                };

                context.add_symbol(symbol.clone());
                self.global_symbols.insert(defcap.name.clone(), symbol);

                // Enter capability scope and analyze body
                context.enter_scope(SymbolScope::Function(defcap.name.clone()));
                for expr in &defcap.body {
                    self.analyze_expression(expr, context, text, uri);
                }
                context.exit_scope();
            }
            Declaration::Defconst(defconst) => {
                let symbol = Symbol {
                    name: defconst.name.clone(),
                    kind: SymbolKind::CONSTANT,
                    range: Range::default(),
                    detail: Some(format!("(defconst {})", defconst.name)),
                    documentation: defconst.docs.clone(),
                    definition_uri: Some(uri.clone()),
                    definition_range: None,
                    symbol_type: None,
                    scope: context.current_scope().clone(),
                };

                context.add_symbol(symbol.clone());
                self.global_symbols.insert(defconst.name.clone(), symbol);
            }
            Declaration::Defschema(defschema) => {
                let symbol = Symbol {
                    name: defschema.name.clone(),
                    kind: SymbolKind::STRUCT,
                    range: Range::default(),
                    detail: Some(format!("(defschema {})", defschema.name)),
                    documentation: defschema.docs.clone(),
                    definition_uri: Some(uri.clone()),
                    definition_range: None,
                    symbol_type: Some("schema".to_string()),
                    scope: context.current_scope().clone(),
                };

                context.add_symbol(symbol.clone());
                self.global_symbols.insert(defschema.name.clone(), symbol);
            }
            Declaration::Deftable(deftable) => {
                let symbol = Symbol {
                    name: deftable.name.clone(),
                    kind: SymbolKind::OBJECT,
                    range: Range::default(),
                    detail: Some(format!("(deftable {})", deftable.name)),
                    documentation: deftable.docs.clone(),
                    definition_uri: Some(uri.clone()),
                    definition_range: None,
                    symbol_type: Some("table".to_string()),
                    scope: context.current_scope().clone(),
                };

                context.add_symbol(symbol.clone());
                self.global_symbols.insert(deftable.name.clone(), symbol);
            }
            Declaration::Defpact(defpact) => {
                let symbol = Symbol {
                    name: defpact.name.clone(),
                    kind: SymbolKind::FUNCTION,
                    range: Range::default(),
                    detail: Some(format!("(defpact {})", defpact.name)),
                    documentation: defpact.docs.clone(),
                    definition_uri: Some(uri.clone()),
                    definition_range: None,
                    symbol_type: Some("pact".to_string()),
                    scope: context.current_scope().clone(),
                };

                context.add_symbol(symbol.clone());
                self.global_symbols.insert(defpact.name.clone(), symbol);
            }
            Declaration::Implements(_interface_name) => {
                // Interface implementation doesn't create symbols
            }
        }
    }


    /// Validate function arity
    fn validate_function_arity(&self, func_name: &str, actual_arity: usize, context: &mut SemanticContext) {
        let expected_arity = match func_name {
            "+" | "-" | "*" | "/" | "=" | ">" | "<" | ">=" | "<=" | "!=" => Some(2),
            "not" | "length" => Some(1),
            "if" => Some(3),
            "at" => Some(2),
            _ => None, // Unknown or variadic
        };

        if let Some(expected) = expected_arity {
            if actual_arity != expected {
                context.errors.push(SemanticError {
                    range: Range::default(),
                    message: format!("Function '{}' expects {} arguments, got {}", func_name, expected, actual_arity),
                    error_type: SemanticErrorType::InvalidArity,
                    suggestions: vec![format!("Provide exactly {} arguments", expected)],
                });
            }
        }
    }

    /// Check if function requires capability
    fn requires_capability(&self, func_name: &str) -> bool {
        matches!(func_name, "transfer" | "read" | "write" | "update" | "insert")
    }

    /// Check if required capability is available
    fn check_capability_available(&self, func_name: &str, context: &mut SemanticContext) {
        // This is a simplified check - in reality would need to track capability stack
        if !context.capabilities.contains(func_name) {
            context.warnings.push(SemanticWarning {
                range: Range::default(),
                message: format!("Function '{}' may require capabilities", func_name),
                warning_type: SemanticWarningType::PerformanceHint,
            });
        }
    }

    /// Suggest similar symbols for typos
    fn suggest_similar_symbols(&self, name: &str, context: &SemanticContext) -> Vec<String> {
        let mut suggestions = Vec::new();

        // Check defined symbols
        for symbol_name in context.symbols.keys() {
            if self.is_similar(name, symbol_name) {
                suggestions.push(symbol_name.clone());
            }
        }

        // Check builtins
        for builtin_name in self.builtin_symbols.keys() {
            if self.is_similar(name, builtin_name) {
                suggestions.push(builtin_name.clone());
            }
        }

        suggestions.truncate(3); // Limit suggestions
        suggestions
    }

    /// Suggest similar function names
    fn suggest_similar_functions(&self, name: &str) -> Vec<String> {
        let mut suggestions = Vec::new();

        for builtin_name in self.builtin_symbols.keys() {
            if self.is_similar(name, builtin_name) {
                suggestions.push(builtin_name.clone());
            }
        }

        suggestions.truncate(3);
        suggestions
    }

    /// Simple string similarity check
    fn is_similar(&self, a: &str, b: &str) -> bool {
        // Simple Levenshtein distance check
        let max_len = a.len().max(b.len());
        if max_len == 0 { return true; }

        let distance = self.levenshtein_distance(a, b);
        (distance as f64 / max_len as f64) < 0.4 // 40% similarity threshold
    }

    /// Calculate Levenshtein distance
    fn levenshtein_distance(&self, a: &str, b: &str) -> usize {
        let a_chars: Vec<char> = a.chars().collect();
        let b_chars: Vec<char> = b.chars().collect();
        let a_len = a_chars.len();
        let b_len = b_chars.len();

        let mut dp = vec![vec![0; b_len + 1]; a_len + 1];

        for i in 0..=a_len {
            dp[i][0] = i;
        }
        for j in 0..=b_len {
            dp[0][j] = j;
        }

        for i in 1..=a_len {
            for j in 1..=b_len {
                if a_chars[i-1] == b_chars[j-1] {
                    dp[i][j] = dp[i-1][j-1];
                } else {
                    dp[i][j] = 1 + dp[i-1][j].min(dp[i][j-1]).min(dp[i-1][j-1]);
                }
            }
        }

        dp[a_len][b_len]
    }

    /// Create LSP diagnostics from semantic analysis
    fn create_diagnostics(&self, context: &SemanticContext) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        // Add errors
        for error in &context.errors {
            diagnostics.push(Diagnostic {
                range: error.range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String(format!("{:?}", error.error_type))),
                source: Some("pact-semantic".to_string()),
                message: error.message.clone(),
                related_information: if error.suggestions.is_empty() {
                    None
                } else {
                    Some(error.suggestions.iter().map(|s| DiagnosticRelatedInformation {
                        location: Location {
                            uri: Url::parse("file:///suggestion").unwrap(),
                            range: Range::default(),
                        },
                        message: format!("Suggestion: {}", s),
                    }).collect())
                },
                tags: None,
                code_description: None,
                data: None,
            });
        }

        // Add warnings
        for warning in &context.warnings {
            diagnostics.push(Diagnostic {
                range: warning.range,
                severity: Some(DiagnosticSeverity::WARNING),
                code: Some(NumberOrString::String(format!("{:?}", warning.warning_type))),
                source: Some("pact-semantic".to_string()),
                message: warning.message.clone(),
                related_information: None,
                tags: None,
                code_description: None,
                data: None,
            });
        }

        diagnostics
    }

}

/// Result of semantic analysis
pub struct SemanticAnalysis {
    pub symbols: Vec<Symbol>,
    pub diagnostics: Vec<Diagnostic>,
    pub type_information: HashMap<String, TypeInfo>,
}

impl SemanticContext {
    fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            scopes: vec![SymbolScope::Global],
            current_module: None,
            current_function: None,
            type_environment: HashMap::new(),
            imports: HashMap::new(),
            capabilities: HashSet::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    fn enter_scope(&mut self, scope: SymbolScope) {
        self.scopes.push(scope);
    }

    fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    fn current_scope(&self) -> &SymbolScope {
        self.scopes.last().unwrap()
    }

    fn add_symbol(&mut self, symbol: Symbol) {
        self.symbols.insert(symbol.name.clone(), symbol);
    }

    fn get_symbol(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }

    fn is_symbol_defined(&self, name: &str) -> bool {
        self.symbols.contains_key(name)
    }
}
