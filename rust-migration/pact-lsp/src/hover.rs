//! Hover information provider with rich type hints and documentation

use crate::semantic::{SemanticAnalyzer, Symbol, SymbolScope};
use tower_lsp::lsp_types::*;
use pact_lexer::{lex, Token};
use pact_parser::parse;
use std::sync::Arc;

/// Hover information provider with semantic analysis integration
pub struct HoverProvider {
    semantic_analyzer: Arc<SemanticAnalyzer>,
}

impl HoverProvider {
    pub fn new(semantic_analyzer: Arc<SemanticAnalyzer>) -> Self {
        Self { semantic_analyzer }
    }
    
    /// Get hover information at a position
    pub fn get_hover(&self, uri: &Url, text: &str, position: Position) -> Option<Hover> {
        // First, try to get symbol information from semantic analysis
        if let Some(symbol) = self.semantic_analyzer.get_symbol_at_position(uri, position) {
            return Some(self.create_symbol_hover(&symbol));
        }
        
        // If no symbol found, try to extract identifier at position
        if let Some(identifier) = self.extract_identifier_at_position(text, position) {
            return self.create_hover_for_identifier(&identifier, text, position);
        }
        
        None
    }
    
    /// Create hover information for a known symbol
    fn create_symbol_hover(&self, symbol: &Symbol) -> Hover {
        let mut contents = Vec::new();
        
        // Add symbol signature/detail
        if let Some(detail) = &symbol.detail {
            contents.push(MarkedString::String(format!("```pact\n{}\n```", detail)));
        } else {
            let kind_str = self.symbol_kind_to_string(symbol.kind);
            contents.push(MarkedString::String(format!("```pact\n{} {}\n```", kind_str, symbol.name)));
        }
        
        // Add type information if available
        if let Some(symbol_type) = &symbol.symbol_type {
            contents.push(MarkedString::String(format!("**Type**: `{}`", symbol_type)));
        }
        
        // Add scope information
        let scope_info = self.format_scope_info(&symbol.scope);
        if !scope_info.is_empty() {
            contents.push(MarkedString::String(format!("**Scope**: {}", scope_info)));
        }
        
        // Add documentation if available
        if let Some(doc) = &symbol.documentation {
            contents.push(MarkedString::String(format!("---\n{}", doc)));
        }
        
        // Add definition location if available
        if let Some(def_uri) = &symbol.definition_uri {
            let file_name = def_uri.path_segments()
                .and_then(|segments| segments.last())
                .unwrap_or("unknown");
            contents.push(MarkedString::String(format!("*Defined in: {}*", file_name)));
        }
        
        Hover {
            contents: HoverContents::Array(contents),
            range: Some(symbol.range),
        }
    }
    
    /// Create hover for an identifier that might be a builtin or unknown symbol
    fn create_hover_for_identifier(&self, identifier: &str, _text: &str, _position: Position) -> Option<Hover> {
        // Check if it's a builtin function
        if let Some(builtin_info) = self.get_builtin_info(identifier) {
            let mut contents = Vec::new();
            
            contents.push(MarkedString::String(format!("```pact\n{}\n```", builtin_info.signature)));
            
            if !builtin_info.category.is_empty() {
                contents.push(MarkedString::String(format!("**Category**: {}", builtin_info.category)));
            }
            
            contents.push(MarkedString::String(format!("---\n{}", builtin_info.description)));
            
            if !builtin_info.examples.is_empty() {
                contents.push(MarkedString::String("**Examples**:".to_string()));
                for example in &builtin_info.examples {
                    contents.push(MarkedString::String(format!("```pact\n{}\n```", example)));
                }
            }
            
            if !builtin_info.see_also.is_empty() {
                let see_also = builtin_info.see_also.join(", ");
                contents.push(MarkedString::String(format!("**See also**: {}", see_also)));
            }
            
            return Some(Hover {
                contents: HoverContents::Array(contents),
                range: None,
            });
        }
        
        // Check if it's a type name
        if let Some(type_info) = self.get_type_info(identifier) {
            return Some(Hover {
                contents: HoverContents::Array(vec![
                    MarkedString::String(format!("```pact\ntype {}\n```", identifier)),
                    MarkedString::String(format!("**Built-in type**: {}", type_info)),
                ]),
                range: None,
            });
        }
        
        None
    }
    
    /// Extract identifier at the given position
    fn extract_identifier_at_position(&self, text: &str, position: Position) -> Option<String> {
        let lines: Vec<&str> = text.lines().collect();
        if position.line as usize >= lines.len() {
            return None;
        }
        
        let line = lines[position.line as usize];
        let char_pos = position.character as usize;
        
        if char_pos >= line.len() {
            return None;
        }
        
        // Find word boundaries
        let start = line[..char_pos]
            .rfind(|c: char| !c.is_alphanumeric() && c != '-' && c != '_' && c != ':' && c != '?')
            .map(|i| i + 1)
            .unwrap_or(0);
            
        let end = line[char_pos..]
            .find(|c: char| !c.is_alphanumeric() && c != '-' && c != '_' && c != ':' && c != '?')
            .map(|i| char_pos + i)
            .unwrap_or(line.len());
        
        if start < end {
            Some(line[start..end].to_string())
        } else {
            None
        }
    }
    
    /// Convert symbol kind to string
    fn symbol_kind_to_string(&self, kind: SymbolKind) -> &'static str {
        match kind {
            SymbolKind::FILE => "file",
            SymbolKind::MODULE => "module",
            SymbolKind::NAMESPACE => "namespace",
            SymbolKind::PACKAGE => "package",
            SymbolKind::CLASS => "class",
            SymbolKind::METHOD => "method",
            SymbolKind::PROPERTY => "property",
            SymbolKind::FIELD => "field",
            SymbolKind::CONSTRUCTOR => "constructor",
            SymbolKind::ENUM => "enum",
            SymbolKind::INTERFACE => "interface",
            SymbolKind::FUNCTION => "function",
            SymbolKind::VARIABLE => "variable",
            SymbolKind::CONSTANT => "constant",
            SymbolKind::STRING => "string",
            SymbolKind::NUMBER => "number",
            SymbolKind::BOOLEAN => "boolean",
            SymbolKind::ARRAY => "array",
            SymbolKind::OBJECT => "object",
            SymbolKind::KEY => "key",
            SymbolKind::NULL => "null",
            SymbolKind::ENUM_MEMBER => "enum member",
            SymbolKind::STRUCT => "struct",
            SymbolKind::EVENT => "event",
            SymbolKind::OPERATOR => "operator",
            SymbolKind::TYPE_PARAMETER => "type parameter",
            _ => "unknown",
        }
    }
    
    /// Format scope information for display
    fn format_scope_info(&self, scope: &SymbolScope) -> String {
        match scope {
            SymbolScope::Global => "global".to_string(),
            SymbolScope::Module(name) => format!("module {}", name),
            SymbolScope::Function(name) => format!("function {}", name),
            SymbolScope::Let(name) => format!("let binding in {}", name),
            SymbolScope::Parameter(name) => format!("parameter of {}", name),
        }
    }
    
    /// Get builtin function information
    fn get_builtin_info(&self, name: &str) -> Option<BuiltinInfo> {
        match name {
            // Arithmetic
            "+" => Some(BuiltinInfo {
                signature: "(+ x y) -> number".to_string(),
                description: "Addition operator for integers and decimals. Type-preserving: integer + integer = integer, otherwise decimal.".to_string(),
                category: "Math".to_string(),
                examples: vec![
                    "(+ 2 3)  ; => 5".to_string(),
                    "(+ 2.5 1.5)  ; => 4.0".to_string(),
                    "(+ 1 2.0)  ; => 3.0".to_string(),
                ],
                see_also: vec!["-".to_string(), "*".to_string(), "/".to_string()],
            }),
            "-" => Some(BuiltinInfo {
                signature: "(- x y) -> number".to_string(),
                description: "Subtraction operator for integers and decimals. Also supports unary negation.".to_string(),
                category: "Math".to_string(),
                examples: vec![
                    "(- 5 3)  ; => 2".to_string(),
                    "(- 10.5 2.5)  ; => 8.0".to_string(),
                    "(- 42)  ; => -42".to_string(),
                ],
                see_also: vec!["+".to_string(), "*".to_string(), "/".to_string()],
            }),
            "*" => Some(BuiltinInfo {
                signature: "(* x y) -> number".to_string(),
                description: "Multiplication operator for integers and decimals.".to_string(),
                category: "Math".to_string(),
                examples: vec![
                    "(* 4 5)  ; => 20".to_string(),
                    "(* 2.5 4)  ; => 10.0".to_string(),
                ],
                see_also: vec!["+".to_string(), "-".to_string(), "/".to_string()],
            }),
            "/" => Some(BuiltinInfo {
                signature: "(/ x y) -> decimal".to_string(),
                description: "Division operator. Always returns a decimal to preserve precision.".to_string(),
                category: "Math".to_string(),
                examples: vec![
                    "(/ 10 2)  ; => 5.0".to_string(),
                    "(/ 7 2)  ; => 3.5".to_string(),
                ],
                see_also: vec!["+".to_string(), "-".to_string(), "*".to_string(), "mod".to_string()],
            }),
            "mod" => Some(BuiltinInfo {
                signature: "(mod x y) -> integer".to_string(),
                description: "Modulo operation. Returns the remainder of integer division.".to_string(),
                category: "Math".to_string(),
                examples: vec![
                    "(mod 10 3)  ; => 1".to_string(),
                    "(mod 15 4)  ; => 3".to_string(),
                ],
                see_also: vec!["/".to_string()],
            }),
            
            // Comparison
            "=" => Some(BuiltinInfo {
                signature: "(= x y) -> bool".to_string(),
                description: "Equality comparison. Works with all types and performs deep equality for objects and lists.".to_string(),
                category: "Comparison".to_string(),
                examples: vec![
                    "(= 1 1)  ; => true".to_string(),
                    "(= \"hello\" \"hello\")  ; => true".to_string(),
                    "(= [1 2] [1 2])  ; => true".to_string(),
                ],
                see_also: vec!["!=".to_string(), "<".to_string(), ">".to_string()],
            }),
            "!=" => Some(BuiltinInfo {
                signature: "(!= x y) -> bool".to_string(),
                description: "Inequality comparison. Opposite of equality.".to_string(),
                category: "Comparison".to_string(),
                examples: vec![
                    "(!= 1 2)  ; => true".to_string(),
                    "(!= \"hello\" \"world\")  ; => true".to_string(),
                ],
                see_also: vec!["=".to_string()],
            }),
            "<" => Some(BuiltinInfo {
                signature: "(< x y) -> bool".to_string(),
                description: "Less than comparison for numbers, strings, and time values.".to_string(),
                category: "Comparison".to_string(),
                examples: vec![
                    "(< 1 2)  ; => true".to_string(),
                    "(< \"apple\" \"banana\")  ; => true".to_string(),
                ],
                see_also: vec![">".to_string(), "<=".to_string(), ">=".to_string()],
            }),
            
            // Control flow
            "if" => Some(BuiltinInfo {
                signature: "(if condition then-expr else-expr) -> any".to_string(),
                description: "Conditional expression. Evaluates condition and returns then-expr if true, else-expr if false.".to_string(),
                category: "Control Flow".to_string(),
                examples: vec![
                    "(if (> x 0) \"positive\" \"non-positive\")".to_string(),
                    "(if authenticated (read accounts user) (enforce false \"Not authenticated\"))".to_string(),
                ],
                see_also: vec!["cond".to_string(), "and".to_string(), "or".to_string()],
            }),
            "let" => Some(BuiltinInfo {
                signature: "(let ((var1 val1) (var2 val2) ...) body) -> any".to_string(),
                description: "Create local variable bindings. Variables are available in the body expression.".to_string(),
                category: "Control Flow".to_string(),
                examples: vec![
                    "(let ((x 10) (y 20)) (+ x y))  ; => 30".to_string(),
                    "(let ((user-balance (read accounts user-id))) (enforce (>= user-balance amount) \"Insufficient funds\"))".to_string(),
                ],
                see_also: vec!["let*".to_string(), "defun".to_string()],
            }),
            
            // Functions
            "defun" => Some(BuiltinInfo {
                signature: "(defun name (param1 param2 ...) body) -> function".to_string(),
                description: "Define a function with parameters and body. Functions are the primary abstraction mechanism in Pact.".to_string(),
                category: "Definition".to_string(),
                examples: vec![
                    "(defun square (x) (* x x))".to_string(),
                    "(defun transfer (from to amount) (with-capability (TRANSFER from to amount) (debit from amount) (credit to amount)))".to_string(),
                ],
                see_also: vec!["defcap".to_string(), "lambda".to_string()],
            }),
            "defcap" => Some(BuiltinInfo {
                signature: "(defcap NAME (param1 param2 ...) body) -> capability".to_string(),
                description: "Define a capability for authorization. Capabilities must be acquired before use and follow specific naming conventions.".to_string(),
                category: "Capability".to_string(),
                examples: vec![
                    "(defcap TRANSFER (from:string to:string amount:decimal) (enforce (!= from to) \"Cannot transfer to self\"))".to_string(),
                ],
                see_also: vec!["with-capability".to_string(), "require-capability".to_string()],
            }),
            "with-capability" => Some(BuiltinInfo {
                signature: "(with-capability (CAP args...) body) -> any".to_string(),
                description: "Acquire a capability for the duration of the body expression. The capability must be defined and all its constraints satisfied.".to_string(),
                category: "Capability".to_string(),
                examples: vec![
                    "(with-capability (TRANSFER sender receiver amount) (update accounts sender {\"balance\": (- old-balance amount)}))".to_string(),
                ],
                see_also: vec!["defcap".to_string(), "require-capability".to_string()],
            }),
            
            // Database operations
            "read" => Some(BuiltinInfo {
                signature: "(read table key) -> object".to_string(),
                description: "Read a row from a table by key. Fails if the key doesn't exist.".to_string(),
                category: "Database".to_string(),
                examples: vec![
                    "(read accounts \"alice\")  ; => {\"balance\": 100.0, \"guard\": ...}".to_string(),
                ],
                see_also: vec!["with-read".to_string(), "write".to_string(), "update".to_string()],
            }),
            "write" => Some(BuiltinInfo {
                signature: "(write table key object) -> string".to_string(),
                description: "Write a row to a table. Creates a new row or overwrites existing data.".to_string(),
                category: "Database".to_string(),
                examples: vec![
                    "(write accounts \"alice\" {\"balance\": 100.0, \"guard\": (read-keyset \"alice-keyset\")})".to_string(),
                ],
                see_also: vec!["read".to_string(), "update".to_string(), "insert".to_string()],
            }),
            "with-read" => Some(BuiltinInfo {
                signature: "(with-read table key binding body) -> any".to_string(),
                description: "Read a row and bind its fields for use in the body. More efficient than separate read + field access.".to_string(),
                category: "Database".to_string(),
                examples: vec![
                    "(with-read accounts user-id { \"balance\" := current-balance } (enforce (>= current-balance amount) \"Insufficient funds\"))".to_string(),
                ],
                see_also: vec!["read".to_string(), "with-default-read".to_string()],
            }),
            
            // List operations
            "map" => Some(BuiltinInfo {
                signature: "(map function list) -> list".to_string(),
                description: "Apply a function to each element of a list, returning a new list with the results.".to_string(),
                category: "List".to_string(),
                examples: vec![
                    "(map (lambda (x) (* x 2)) [1 2 3])  ; => [2 4 6]".to_string(),
                    "(map (at \"balance\") account-list)  ; => [100.0 50.0 25.0]".to_string(),
                ],
                see_also: vec!["filter".to_string(), "fold".to_string()],
            }),
            "filter" => Some(BuiltinInfo {
                signature: "(filter predicate list) -> list".to_string(),
                description: "Filter a list, keeping only elements for which the predicate returns true.".to_string(),
                category: "List".to_string(),
                examples: vec![
                    "(filter (lambda (x) (> x 5)) [1 6 3 8 2])  ; => [6 8]".to_string(),
                ],
                see_also: vec!["map".to_string(), "fold".to_string()],
            }),
            "fold" => Some(BuiltinInfo {
                signature: "(fold function initial list) -> any".to_string(),
                description: "Reduce a list to a single value by applying a function cumulatively.".to_string(),
                category: "List".to_string(),
                examples: vec![
                    "(fold + 0 [1 2 3 4])  ; => 10".to_string(),
                    "(fold max 0 [3 7 2 9 1])  ; => 9".to_string(),
                ],
                see_also: vec!["map".to_string(), "filter".to_string()],
            }),
            
            // String operations
            "length" => Some(BuiltinInfo {
                signature: "(length collection) -> integer".to_string(),
                description: "Get the length of a string, list, or object (number of key-value pairs).".to_string(),
                category: "String".to_string(),
                examples: vec![
                    "(length \"hello\")  ; => 5".to_string(),
                    "(length [1 2 3])  ; => 3".to_string(),
                    "(length {\"a\": 1, \"b\": 2})  ; => 2".to_string(),
                ],
                see_also: vec!["take".to_string(), "drop".to_string()],
            }),
            "format" => Some(BuiltinInfo {
                signature: "(format template args...) -> string".to_string(),
                description: "Format a string template with arguments. Uses {} as placeholders.".to_string(),
                category: "String".to_string(),
                examples: vec![
                    "(format \"Hello, {}!\" \"Alice\")  ; => \"Hello, Alice!\"".to_string(),
                    "(format \"Transfer of {} from {} to {}\" amount from to)".to_string(),
                ],
                see_also: vec!["concat".to_string()],
            }),
            
            // Type checking
            "typeof" => Some(BuiltinInfo {
                signature: "(typeof value) -> string".to_string(),
                description: "Get the type of a value as a string.".to_string(),
                category: "Type".to_string(),
                examples: vec![
                    "(typeof 42)  ; => \"integer\"".to_string(),
                    "(typeof \"hello\")  ; => \"string\"".to_string(),
                    "(typeof [1 2 3])  ; => \"list\"".to_string(),
                ],
                see_also: vec!["is-string".to_string(), "is-integer".to_string()],
            }),
            
            // Guards and enforcement
            "enforce" => Some(BuiltinInfo {
                signature: "(enforce condition message) -> bool".to_string(),
                description: "Assert that a condition is true, or fail with the given message. Returns true if successful.".to_string(),
                category: "Guard".to_string(),
                examples: vec![
                    "(enforce (> amount 0) \"Amount must be positive\")".to_string(),
                    "(enforce (!= from to) \"Cannot transfer to yourself\")".to_string(),
                ],
                see_also: vec!["enforce-one".to_string(), "enforce-guard".to_string()],
            }),
            
            _ => None,
        }
    }
    
    /// Get type information for built-in types
    fn get_type_info(&self, type_name: &str) -> Option<String> {
        match type_name {
            "integer" => Some("64-bit signed integer (-2^63 to 2^63-1)".to_string()),
            "decimal" => Some("Arbitrary precision decimal number".to_string()),
            "string" => Some("UTF-8 encoded string".to_string()),
            "bool" => Some("Boolean value (true or false)".to_string()),
            "time" => Some("UTC timestamp with microsecond precision".to_string()),
            "list" => Some("Homogeneous list of values".to_string()),
            "object" => Some("Key-value map with string keys".to_string()),
            "keyset" => Some("Set of public keys with predicate function".to_string()),
            "guard" => Some("Flexible authorization mechanism".to_string()),
            "module" => Some("Module containing functions and data".to_string()),
            "table" => Some("Database table with typed schema".to_string()),
            _ => None,
        }
    }
}

/// Information about a builtin function
#[derive(Debug, Clone)]
struct BuiltinInfo {
    signature: String,
    description: String,
    category: String,
    examples: Vec<String>,
    see_also: Vec<String>,
}

/// Legacy function for backward compatibility
pub fn get_hover_info(text: &str, position: Position) -> Option<Hover> {
    use crate::semantic::SemanticAnalyzer;
    
    let semantic_analyzer = Arc::new(SemanticAnalyzer::new());
    let provider = HoverProvider::new(semantic_analyzer);
    let dummy_uri = Url::parse("file:///temp.pact").unwrap();
    provider.get_hover(&dummy_uri, text, position)
}