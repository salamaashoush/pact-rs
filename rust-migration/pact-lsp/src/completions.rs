//! Intelligent code completion implementation with context awareness

use crate::semantic::{SemanticAnalyzer, Symbol, SymbolScope};
use tower_lsp::lsp_types::*;
use pact_lexer::{lex, Token};
use pact_parser::parse;
use std::collections::HashSet;
use std::sync::Arc;

/// Intelligent completion provider with context awareness
pub struct CompletionProvider {
    semantic_analyzer: Arc<SemanticAnalyzer>,
}

impl CompletionProvider {
    pub fn new(semantic_analyzer: Arc<SemanticAnalyzer>) -> Self {
        Self { semantic_analyzer }
    }
    
    /// Get intelligent completions at a position
    pub fn get_completions(&self, uri: &Url, text: &str, position: Position) -> Vec<CompletionItem> {
        let context = self.analyze_completion_context(text, position);
        
        match context.completion_type {
            CompletionType::Keywords => self.get_keyword_completions(&context),
            CompletionType::Functions => self.get_function_completions(&context),
            CompletionType::Variables => self.get_variable_completions(uri, &context),
            CompletionType::ModuleMembers => self.get_module_member_completions(&context),
            CompletionType::TypeAnnotations => self.get_type_completions(&context),
            CompletionType::CapabilityNames => self.get_capability_completions(&context),
            CompletionType::TableNames => self.get_table_completions(&context),
            CompletionType::SchemaFields => self.get_schema_field_completions(&context),
            CompletionType::StringLiterals => self.get_string_literal_completions(&context),
            CompletionType::NumberLiterals => self.get_number_literal_completions(&context),
            CompletionType::General => self.get_general_completions(uri, &context),
        }
    }
    
    /// Analyze the context around the cursor to determine what kind of completion is needed
    fn analyze_completion_context(&self, text: &str, position: Position) -> CompletionContext {
        let lines: Vec<&str> = text.lines().collect();
        if position.line as usize >= lines.len() {
            return CompletionContext::default();
        }
        
        let line = lines[position.line as usize];
        let char_pos = position.character as usize;
        
        let prefix = if char_pos <= line.len() {
            &line[..char_pos]
        } else {
            line
        };
        
        // Extract current word
        let word_start = prefix.rfind(|c: char| !c.is_alphanumeric() && c != '-' && c != '_' && c != ':' && c != '.')
            .map(|i| i + 1)
            .unwrap_or(0);
        let current_word = &prefix[word_start..];
        
        // Analyze surrounding context
        let completion_type = self.determine_completion_type(prefix, line, &lines, position.line as usize);
        let in_paren_expr = self.is_in_parentheses_expression(prefix);
        let function_context = self.get_function_context(prefix);
        let module_context = self.get_module_context(&lines, position.line as usize);
        let paren_depth = self.calculate_paren_depth(prefix);
        
        CompletionContext {
            current_word: current_word.to_string(),
            prefix: prefix.to_string(),
            line: line.to_string(),
            position,
            completion_type,
            in_paren_expr,
            function_context,
            module_context,
            paren_depth,
            trigger_character: self.get_trigger_character(prefix),
        }
    }
    
    /// Determine the type of completion needed based on context
    fn determine_completion_type(&self, prefix: &str, line: &str, lines: &[&str], line_idx: usize) -> CompletionType {
        // Check for type annotations (after :)
        if prefix.ends_with(':') {
            return CompletionType::TypeAnnotations;
        }
        
        // Check for module member access (after .)
        if prefix.contains('.') && !prefix.trim().starts_with(';') {
            return CompletionType::ModuleMembers;
        }
        
        // Check if we're in a string literal
        if self.is_in_string_literal(prefix) {
            return CompletionType::StringLiterals;
        }
        
        // Check for defcap context
        if line.trim_start().starts_with("(defcap") || 
           (line_idx > 0 && lines[line_idx - 1].contains("defcap")) {
            return CompletionType::CapabilityNames;
        }
        
        // Check for deftable context
        if line.contains("deftable") {
            return CompletionType::TableNames;
        }
        
        // Check for schema field context
        if line.contains("defschema") || self.is_in_schema_definition(lines, line_idx) {
            return CompletionType::SchemaFields;
        }
        
        // Check for function definition context
        if line.trim_start().starts_with("(defun") {
            return CompletionType::Functions;
        }
        
        // Check if we're at the start of an expression (after opening paren)
        if prefix.trim_end().ends_with('(') {
            return CompletionType::Keywords;
        }
        
        // Check if we're in a function call position
        if self.is_in_function_call_position(prefix) {
            return CompletionType::Functions;
        }
        
        // Check if we're in a variable reference position
        if self.is_in_variable_position(prefix) {
            return CompletionType::Variables;
        }
        
        CompletionType::General
    }
    
    /// Get keyword completions (defun, let, if, etc.)
    fn get_keyword_completions(&self, context: &CompletionContext) -> Vec<CompletionItem> {
        let keywords = vec![
            // Definitions
            ("defun", "Define a function", "(defun ${1:name} (${2:params}) ${3:body})", "Define a new function"),
            ("defcap", "Define a capability", "(defcap ${1:NAME} (${2:params}) ${3:body})", "Define a new capability"),
            ("defconst", "Define a constant", "(defconst ${1:NAME} ${2:value})", "Define a constant value"),
            ("defschema", "Define a schema", "(defschema ${1:name} ${2:fields})", "Define a data schema"),
            ("deftable", "Define a table", "(deftable ${1:name}:{2:schema})", "Define a database table"),
            ("defpact", "Define a pact", "(defpact ${1:name} (${2:params}) ${3:steps})", "Define a multi-step pact"),
            ("module", "Define a module", "(module ${1:name} ${2:keyset} ${3:body})", "Define a new module"),
            ("interface", "Define an interface", "(interface ${1:name} ${2:body})", "Define an interface"),
            
            // Control flow
            ("if", "Conditional expression", "(if ${1:condition} ${2:then} ${3:else})", "Conditional branching"),
            ("let", "Local bindings", "(let ((${1:var} ${2:value})) ${3:body})", "Create local variable bindings"),
            ("let*", "Sequential bindings", "(let* ((${1:var1} ${2:value1}) (${3:var2} ${4:value2})) ${5:body})", "Sequential local bindings"),
            ("lambda", "Anonymous function", "(lambda (${1:params}) ${2:body})", "Create anonymous function"),
            ("cond", "Multi-way conditional", "(cond ((${1:test1} ${2:result1})) (${3:else}))", "Multi-way conditional"),
            
            // Capabilities
            ("with-capability", "Acquire capability", "(with-capability (${1:CAPABILITY} ${2:args}) ${3:body})", "Execute with capability"),
            ("require-capability", "Require capability", "(require-capability ${1:CAPABILITY})", "Require a capability"),
            ("compose-capability", "Compose capabilities", "(compose-capability ${1:CAPABILITY})", "Compose capability"),
            ("install-capability", "Install capability", "(install-capability ${1:CAPABILITY})", "Install capability"),
            
            // Database operations
            ("with-read", "Read and bind", "(with-read ${1:table} ${2:key} { ${3:fields} } ${4:body})", "Read row and bind fields"),
            ("with-default-read", "Read with default", "(with-default-read ${1:table} ${2:key} { ${3:defaults} } { ${4:fields} } ${5:body})", "Read with default values"),
            
            // Module system
            ("use", "Import module", "(use ${1:module-name})", "Import a module"),
            ("implements", "Implement interface", "(implements ${1:interface-name})", "Implement an interface"),
        ];
        
        keywords.iter()
            .filter(|(name, _, _, _)| name.starts_with(&context.current_word))
            .map(|(name, detail, snippet, doc)| {
                CompletionItem {
                    label: name.to_string(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    detail: Some(detail.to_string()),
                    documentation: Some(Documentation::String(doc.to_string())),
                    deprecated: Some(false),
                    preselect: Some(context.current_word.is_empty()),
                    sort_text: Some(format!("0_{}", name)), // Keywords get high priority
                    filter_text: Some(name.to_string()),
                    insert_text: Some(snippet.to_string()),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    insert_text_mode: Some(InsertTextMode::ADJUST_INDENTATION),
                    text_edit: None,
                    additional_text_edits: None,
                    command: None,
                    commit_characters: Some(vec![" ".to_string()]),
                    data: None,
                    tags: None,
                    label_details: Some(CompletionItemLabelDetails {
                        detail: Some(format!(" {}", detail)),
                        description: None,
                    }),
                }
            })
            .collect()
    }
    
    /// Get function completions (built-ins and user-defined)
    fn get_function_completions(&self, context: &CompletionContext) -> Vec<CompletionItem> {
        let mut completions = Vec::new();
        
        // Add builtin functions
        for (name, signature, description, category) in get_builtin_functions() {
            if name.starts_with(&context.current_word) {
                completions.push(CompletionItem {
                    label: name.to_string(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: Some(signature.to_string()),
                    documentation: Some(Documentation::MarkupContent(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!("**{}**\n\n{}\n\n*Category: {}*", signature, description, category),
                    })),
                    deprecated: Some(false),
                    preselect: Some(false),
                    sort_text: Some(format!("1_{}", name)),
                    filter_text: Some(name.to_string()),
                    insert_text: Some(self.get_function_snippet(name, signature)),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    insert_text_mode: Some(InsertTextMode::ADJUST_INDENTATION),
                    text_edit: None,
                    additional_text_edits: None,
                    command: None,
                    commit_characters: Some(vec![" ".to_string()]),
                    data: None,
                    tags: None,
                    label_details: Some(CompletionItemLabelDetails {
                        detail: Some(format!(" {}", signature)),
                        description: Some(category.to_string()),
                    }),
                });
            }
        }
        
        completions
    }
    
    /// Get variable completions from current scope
    fn get_variable_completions(&self, uri: &Url, context: &CompletionContext) -> Vec<CompletionItem> {
        // In a real implementation, this would query the semantic analyzer
        // for variables in scope at the current position
        vec![]
    }
    
    /// Get module member completions
    fn get_module_member_completions(&self, context: &CompletionContext) -> Vec<CompletionItem> {
        // Extract module name from the prefix (before the dot)
        if let Some(dot_pos) = context.prefix.rfind('.') {
            let _module_name = &context.prefix[..dot_pos];
            // In a real implementation, this would look up the module's exported symbols
        }
        vec![]
    }
    
    /// Get type annotation completions
    fn get_type_completions(&self, context: &CompletionContext) -> Vec<CompletionItem> {
        let types = vec![
            ("integer", "64-bit signed integer"),
            ("decimal", "Arbitrary precision decimal"),
            ("string", "UTF-8 string"),
            ("bool", "Boolean value"),
            ("time", "UTC time"),
            ("keyset", "Keyset for authorization"),
            ("guard", "Flexible authorization guard"),
            ("object", "JSON object"),
            ("list", "List of values"),
            ("module", "Module reference"),
            ("table", "Database table"),
        ];
        
        types.iter()
            .filter(|(name, _)| name.starts_with(&context.current_word))
            .map(|(name, desc)| {
                CompletionItem {
                    label: name.to_string(),
                    kind: Some(CompletionItemKind::TYPE_PARAMETER),
                    detail: Some(desc.to_string()),
                    documentation: Some(Documentation::String(desc.to_string())),
                    deprecated: Some(false),
                    preselect: Some(false),
                    sort_text: Some(format!("2_{}", name)),
                    filter_text: Some(name.to_string()),
                    insert_text: Some(name.to_string()),
                    insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
                    insert_text_mode: None,
                    text_edit: None,
                    additional_text_edits: None,
                    command: None,
                    commit_characters: Some(vec![" ".to_string()]),
                    data: None,
                    tags: None,
                    label_details: None,
                }
            })
            .collect()
    }
    
    /// Get capability name completions
    fn get_capability_completions(&self, context: &CompletionContext) -> Vec<CompletionItem> {
        let common_capabilities = vec![
            ("TRANSFER", "Token transfer capability"),
            ("MINT", "Token minting capability"),
            ("BURN", "Token burning capability"),
            ("GOVERNANCE", "Governance capability"),
            ("ADMIN", "Administrative capability"),
            ("UPGRADE", "Contract upgrade capability"),
        ];
        
        common_capabilities.iter()
            .filter(|(name, _)| name.starts_with(&context.current_word.to_uppercase()))
            .map(|(name, desc)| {
                CompletionItem {
                    label: name.to_string(),
                    kind: Some(CompletionItemKind::CONSTANT),
                    detail: Some(desc.to_string()),
                    documentation: Some(Documentation::String(desc.to_string())),
                    deprecated: Some(false),
                    preselect: Some(false),
                    sort_text: Some(format!("3_{}", name)),
                    filter_text: Some(name.to_string()),
                    insert_text: Some(name.to_string()),
                    insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
                    insert_text_mode: None,
                    text_edit: None,
                    additional_text_edits: None,
                    command: None,
                    commit_characters: Some(vec![" ".to_string()]),
                    data: None,
                    tags: None,
                    label_details: None,
                }
            })
            .collect()
    }
    
    /// Get table name completions
    fn get_table_completions(&self, context: &CompletionContext) -> Vec<CompletionItem> {
        // Would query available tables from the workspace
        vec![]
    }
    
    /// Get schema field completions
    fn get_schema_field_completions(&self, context: &CompletionContext) -> Vec<CompletionItem> {
        let common_fields = vec![
            ("balance:decimal", "Account balance"),
            ("guard:guard", "Account guard"),
            ("account:string", "Account identifier"),
            ("amount:decimal", "Transaction amount"),
            ("created:time", "Creation timestamp"),
            ("updated:time", "Last update timestamp"),
        ];
        
        common_fields.iter()
            .filter(|(name, _)| name.starts_with(&context.current_word))
            .map(|(name, desc)| {
                CompletionItem {
                    label: name.to_string(),
                    kind: Some(CompletionItemKind::FIELD),
                    detail: Some(desc.to_string()),
                    documentation: Some(Documentation::String(desc.to_string())),
                    deprecated: Some(false),
                    preselect: Some(false),
                    sort_text: Some(format!("4_{}", name)),
                    filter_text: Some(name.to_string()),
                    insert_text: Some(name.to_string()),
                    insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
                    insert_text_mode: None,
                    text_edit: None,
                    additional_text_edits: None,
                    command: None,
                    commit_characters: Some(vec![" ".to_string()]),
                    data: None,
                    tags: None,
                    label_details: None,
                }
            })
            .collect()
    }
    
    /// Get string literal completions
    fn get_string_literal_completions(&self, context: &CompletionContext) -> Vec<CompletionItem> {
        // Could provide common string patterns, format strings, etc.
        vec![]
    }
    
    /// Get number literal completions  
    fn get_number_literal_completions(&self, context: &CompletionContext) -> Vec<CompletionItem> {
        // Could provide common numeric constants
        vec![]
    }
    
    /// Get general completions (fallback)
    fn get_general_completions(&self, uri: &Url, context: &CompletionContext) -> Vec<CompletionItem> {
        let mut completions = Vec::new();
        
        // Mix of all completion types
        completions.extend(self.get_function_completions(context));
        completions.extend(self.get_keyword_completions(context));
        completions.extend(self.get_variable_completions(uri, context));
        
        completions
    }
    
    // Helper methods for context analysis
    
    fn is_in_parentheses_expression(&self, prefix: &str) -> bool {
        let open_count = prefix.chars().filter(|&c| c == '(').count();
        let close_count = prefix.chars().filter(|&c| c == ')').count();
        open_count > close_count
    }
    
    fn get_function_context(&self, prefix: &str) -> Option<String> {
        // Find the current function being called
        if let Some(last_open) = prefix.rfind('(') {
            let after_paren = &prefix[last_open + 1..];
            if let Some(space_pos) = after_paren.find(' ') {
                Some(after_paren[..space_pos].to_string())
            } else {
                Some(after_paren.to_string())
            }
        } else {
            None
        }
    }
    
    fn get_module_context(&self, lines: &[&str], line_idx: usize) -> Option<String> {
        // Look backwards for module definition
        for i in (0..=line_idx).rev() {
            if lines[i].contains("(module ") {
                // Extract module name
                if let Some(start) = lines[i].find("module ") {
                    let after_module = &lines[i][start + 7..];
                    if let Some(space_pos) = after_module.find(' ') {
                        return Some(after_module[..space_pos].to_string());
                    }
                }
            }
        }
        None
    }
    
    fn calculate_paren_depth(&self, prefix: &str) -> usize {
        let open_count = prefix.chars().filter(|&c| c == '(').count();
        let close_count = prefix.chars().filter(|&c| c == ')').count();
        open_count.saturating_sub(close_count)
    }
    
    fn get_trigger_character(&self, prefix: &str) -> Option<char> {
        prefix.chars().last()
    }
    
    fn is_in_string_literal(&self, prefix: &str) -> bool {
        let mut in_string = false;
        let mut escape_next = false;
        
        for ch in prefix.chars() {
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
        
        in_string
    }
    
    fn is_in_schema_definition(&self, lines: &[&str], line_idx: usize) -> bool {
        // Check if we're inside a defschema block
        for i in (0..=line_idx).rev() {
            let line = lines[i];
            if line.contains("defschema") {
                return true;
            }
            if line.trim().starts_with('(') && !line.contains("defschema") {
                return false;
            }
        }
        false
    }
    
    fn is_in_function_call_position(&self, prefix: &str) -> bool {
        // Check if cursor is in a position where a function call is expected
        prefix.trim_end().ends_with('(') || 
        (prefix.contains('(') && prefix.chars().last().map_or(false, |c| c.is_whitespace()))
    }
    
    fn is_in_variable_position(&self, prefix: &str) -> bool {
        // Check if cursor is in a position where a variable reference is expected
        !prefix.trim_end().ends_with('(') && 
        self.is_in_parentheses_expression(prefix)
    }
    
    fn get_function_snippet(&self, name: &str, signature: &str) -> String {
        // Generate snippet with placeholders for function parameters
        match name {
            "if" => "if ${1:condition} ${2:then} ${3:else}".to_string(),
            "let" => "let ((${1:var} ${2:value})) ${3:body}".to_string(),
            "defun" => "defun ${1:name} (${2:params}) ${3:body}".to_string(),
            _ => {
                // Extract parameter count from signature and generate placeholders
                let param_count = signature.matches(',').count() + 1;
                if param_count <= 1 {
                    name.to_string()
                } else {
                    let placeholders: Vec<String> = (1..=param_count)
                        .map(|i| format!("${{{}:arg{}}}", i, i))
                        .collect();
                    format!("{} {}", name, placeholders.join(" "))
                }
            }
        }
    }
}

/// Completion context information
#[derive(Debug, Clone)]
struct CompletionContext {
    current_word: String,
    prefix: String,
    line: String,
    position: Position,
    completion_type: CompletionType,
    in_paren_expr: bool,
    function_context: Option<String>,
    module_context: Option<String>,
    paren_depth: usize,
    trigger_character: Option<char>,
}

impl Default for CompletionContext {
    fn default() -> Self {
        Self {
            current_word: String::new(),
            prefix: String::new(),
            line: String::new(),
            position: Position::new(0, 0),
            completion_type: CompletionType::General,
            in_paren_expr: false,
            function_context: None,
            module_context: None,
            paren_depth: 0,
            trigger_character: None,
        }
    }
}

/// Types of completions based on context
#[derive(Debug, Clone, PartialEq)]
enum CompletionType {
    Keywords,
    Functions,
    Variables,
    ModuleMembers,
    TypeAnnotations,
    CapabilityNames,
    TableNames,
    SchemaFields,
    StringLiterals,
    NumberLiterals,
    General,
}

/// Legacy function for backward compatibility
pub fn get_completions(text: &str, position: Position) -> Vec<CompletionItem> {
    let semantic_analyzer = Arc::new(SemanticAnalyzer::new());
    let provider = CompletionProvider::new(semantic_analyzer);
    let dummy_uri = Url::parse("file:///temp.pact").unwrap();
    provider.get_completions(&dummy_uri, text, position)
}

/// Get comprehensive builtin function information
fn get_builtin_functions() -> Vec<(&'static str, &'static str, &'static str, &'static str)> {
    vec![
        // Format: (name, signature, description, category)
        
        // Arithmetic
        ("+", "(+ x y)", "Addition operator for numbers", "Math"),
        ("-", "(- x y)", "Subtraction operator for numbers", "Math"),
        ("*", "(* x y)", "Multiplication operator for numbers", "Math"),
        ("/", "(/ x y)", "Division operator for numbers", "Math"),
        ("mod", "(mod x y)", "Modulo operation", "Math"),
        ("abs", "(abs x)", "Absolute value", "Math"),
        ("ceiling", "(ceiling x)", "Round up to nearest integer", "Math"),
        ("floor", "(floor x)", "Round down to nearest integer", "Math"),
        ("round", "(round x precision)", "Round to specified precision", "Math"),
        ("sqrt", "(sqrt x)", "Square root", "Math"),
        ("ln", "(ln x)", "Natural logarithm", "Math"),
        ("exp", "(exp x)", "Exponential function", "Math"),
        ("^", "(^ x y)", "Exponentiation", "Math"),
        
        // Comparison
        ("=", "(= x y)", "Equality comparison", "Comparison"),
        ("!=", "(!= x y)", "Inequality comparison", "Comparison"),
        ("<", "(< x y)", "Less than comparison", "Comparison"),
        (">", "(> x y)", "Greater than comparison", "Comparison"),
        ("<=", "(<= x y)", "Less than or equal comparison", "Comparison"),
        (">=", "(>= x y)", "Greater than or equal comparison", "Comparison"),
        
        // Logic
        ("and", "(and x y)", "Logical AND operation", "Logic"),
        ("or", "(or x y)", "Logical OR operation", "Logic"),
        ("not", "(not x)", "Logical NOT operation", "Logic"),
        
        // String operations
        ("length", "(length s)", "Get string or list length", "String"),
        ("take", "(take n s)", "Take first n characters/elements", "String"),
        ("drop", "(drop n s)", "Drop first n characters/elements", "String"),
        ("str-to-int", "(str-to-int s)", "Parse string to integer", "String"),
        ("str-to-int-base", "(str-to-int-base base s)", "Parse string to integer with base", "String"),
        ("int-to-str", "(int-to-str i base)", "Convert integer to string", "String"),
        ("format", "(format template args)", "Format string with arguments", "String"),
        ("hash", "(hash value)", "Compute hash of value", "String"),
        
        // List operations
        ("map", "(map func list)", "Apply function to each list element", "List"),
        ("filter", "(filter pred list)", "Filter list by predicate", "List"),
        ("fold", "(fold func init list)", "Reduce list with function", "List"),
        ("reverse", "(reverse list)", "Reverse a list", "List"),
        ("sort", "(sort list)", "Sort a list", "List"),
        ("distinct", "(distinct list)", "Remove duplicates from list", "List"),
        ("contains", "(contains item list)", "Check if list contains item", "List"),
        ("enumerate", "(enumerate from list)", "Add indices to list items", "List"),
        ("zip", "(zip list1 list2)", "Zip two lists together", "List"),
        
        // Object operations
        ("at", "(at key object)", "Get value at key from object", "Object"),
        ("has", "(has key object)", "Check if object has key", "Object"),
        ("keys", "(keys object)", "Get all keys from object", "Object"),
        ("bind", "(bind object binding)", "Bind object fields to variables", "Object"),
        
        // Database operations
        ("create-table", "(create-table table)", "Create a new table", "Database"),
        ("describe-table", "(describe-table table)", "Get table schema", "Database"),
        ("insert", "(insert table key object)", "Insert row into table", "Database"),
        ("update", "(update table key object)", "Update row in table", "Database"),
        ("write", "(write table key object)", "Write (insert or update) row", "Database"),
        ("read", "(read table key)", "Read row from table", "Database"),
        ("select", "(select table where)", "Select rows matching condition", "Database"),
        ("keys", "(keys table)", "Get all keys from table", "Database"),
        ("txids", "(txids table key)", "Get transaction IDs for key", "Database"),
        ("keylog", "(keylog table key txid)", "Get key update log", "Database"),
        ("fold-db", "(fold-db table query func)", "Fold over database results", "Database"),
        
        // Capability system
        ("with-capability", "(with-capability cap body)", "Execute with required capability", "Capability"),
        ("require-capability", "(require-capability cap)", "Require capability in scope", "Capability"),
        ("compose-capability", "(compose-capability cap)", "Compose capability with current", "Capability"),
        ("install-capability", "(install-capability cap)", "Install managed capability", "Capability"),
        ("emit-event", "(emit-event cap)", "Emit capability as event", "Capability"),
        
        // Guards and keysets
        ("enforce", "(enforce condition message)", "Assert condition or fail", "Guard"),
        ("enforce-one", "(enforce-one message tests)", "Enforce at least one test passes", "Guard"),
        ("enforce-guard", "(enforce-guard guard)", "Enforce guard condition", "Guard"),
        ("enforce-keyset", "(enforce-keyset keyset)", "Enforce keyset authorization", "Guard"),
        ("keyset-ref-guard", "(keyset-ref-guard keyset)", "Create keyset reference guard", "Guard"),
        ("read-keyset", "(read-keyset key)", "Read keyset from message data", "Guard"),
        ("define-keyset", "(define-keyset name keyset)", "Define a keyset", "Guard"),
        
        // Time operations
        ("time", "(time iso8601)", "Parse ISO8601 time string", "Time"),
        ("parse-time", "(parse-time format timestr)", "Parse time with format", "Time"),
        ("format-time", "(format-time format time)", "Format time as string", "Time"),
        ("add-time", "(add-time time seconds)", "Add seconds to time", "Time"),
        ("diff-time", "(diff-time time1 time2)", "Difference between times", "Time"),
        
        // Crypto operations
        ("validate-keypair", "(validate-keypair public private)", "Validate keypair", "Crypto"),
        ("verify-signature", "(verify-signature message sig public)", "Verify signature", "Crypto"),
        ("public-key", "(public-key scheme)", "Get public key from scheme", "Crypto"),
        ("base64-encode", "(base64-encode bytes)", "Encode bytes as base64", "Crypto"),
        ("base64-decode", "(base64-decode string)", "Decode base64 string", "Crypto"),
        
        // Type operations
        ("typeof", "(typeof value)", "Get type of value", "Type"),
        ("is-string", "(is-string value)", "Check if value is string", "Type"),
        ("is-integer", "(is-integer value)", "Check if value is integer", "Type"),
        ("is-decimal", "(is-decimal value)", "Check if value is decimal", "Type"),
        ("is-bool", "(is-bool value)", "Check if value is boolean", "Type"),
        ("is-list", "(is-list value)", "Check if value is list", "Type"),
        ("is-object", "(is-object value)", "Check if value is object", "Type"),
        ("is-keyset", "(is-keyset value)", "Check if value is keyset", "Type"),
        ("is-guard", "(is-guard value)", "Check if value is guard", "Type"),
        
        // Continuation operations
        ("yield", "(yield object)", "Yield in multi-step pact", "Pact"),
        ("resume", "(resume binding)", "Resume multi-step pact", "Pact"),
        ("pact-id", "(pact-id)", "Get current pact ID", "Pact"),
        ("pact-version", "(pact-version)", "Get pact version", "Pact"),
        
        // Environment operations
        ("env-data", "(env-data key)", "Get environment data", "Environment"),
        ("env-key", "(env-key index)", "Get environment key", "Environment"),
        ("env-keys", "(env-keys)", "Get all environment keys", "Environment"),
        ("env-sigs", "(env-sigs)", "Get environment signatures", "Environment"),
        ("env-hash", "(env-hash)", "Get environment hash", "Environment"),
        ("env-chain-data", "(env-chain-data)", "Get chain metadata", "Environment"),
        ("env-gas", "(env-gas)", "Get gas information", "Environment"),
        ("env-gasmodel", "(env-gasmodel)", "Get gas model", "Environment"),
        ("env-gaslimit", "(env-gaslimit)", "Get gas limit", "Environment"),
        ("env-gasprice", "(env-gasprice)", "Get gas price", "Environment"),
        
        // REPL utilities
        ("load", "(load file)", "Load Pact file", "REPL"),
        ("print", "(print value)", "Print value to output", "REPL"),
        ("expect", "(expect message expected actual)", "Assert equality in tests", "REPL"),
        ("expect-failure", "(expect-failure message expr)", "Expect expression to fail", "REPL"),
        ("bench", "(bench expr)", "Benchmark expression", "REPL"),
        ("typecheck", "(typecheck module)", "Typecheck module", "REPL"),
        
        // Namespace operations
        ("namespace", "(namespace name)", "Set current namespace", "Namespace"),
        ("define-namespace", "(define-namespace name guard)", "Define namespace", "Namespace"),
        ("describe-namespace", "(describe-namespace name)", "Describe namespace", "Namespace"),
    ]
}
