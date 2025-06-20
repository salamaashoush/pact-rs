//! Live validation and enhanced quick fixes for Pact LSP
//!
//! This module provides real-time code validation using the type system,
//! semantic analysis, and enhanced quick fixes for common coding issues.

use crate::semantic::SemanticAnalyzer;
use tower_lsp::lsp_types::*;
use pact_eval::TypeCheckContext;
use pact_parser::{parse, ParsedDef, ParsedExpr, ParsedTopLevel, MArg, Type};
use pact_schema::inference::{TypeInferencer, ConstraintSolver};
use pact_schema::{ConcreteType, TypeScheme as ConcreteTypeScheme};
pub type SchemaType = pact_schema::ConcreteType;
use pact_values::values::PactValue;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

/// Live validation provider with enhanced diagnostics
pub struct LiveValidationProvider {
    semantic_analyzer: Arc<SemanticAnalyzer>,
    type_inferencer: ExprInferencer,
    schema_inferencer: TypeInferencer,
    constraint_solver: ConstraintSolver,
}

/// Enhanced diagnostic information
#[derive(Debug, Clone)]
pub struct EnhancedDiagnostic {
    pub diagnostic: Diagnostic,
    pub fix_suggestions: Vec<QuickFix>,
    pub type_info: Option<TypeInfo>,
    pub related_symbols: Vec<String>,
}

/// Quick fix suggestion
#[derive(Debug, Clone)]
pub struct QuickFix {
    pub title: String,
    pub kind: CodeActionKind,
    pub edit: WorkspaceEdit,
    pub preferred: bool,
}

/// Type information for diagnostics
#[derive(Debug, Clone)]
pub struct TypeInfo {
    pub expected: String,
    pub actual: String,
    pub inferred: Option<String>,
    pub constraints: Vec<String>,
}

/// Validation context
#[derive(Debug, Clone)]
pub struct ValidationContext {
    pub environment: Environment,
    pub module_context: Option<String>,
    pub function_context: Option<String>,
    pub type_context: HashMap<String, SchemaType>,
    pub capability_context: HashSet<String>,
}

impl LiveValidationProvider {
    /// Create a new live validation provider
    pub fn new(semantic_analyzer: Arc<SemanticAnalyzer>) -> Self {
        let type_inferencer = ExprInferencer::new();
        let schema_inferencer = TypeInferencer::new();
        let constraint_solver = ConstraintSolver::new();
        
        Self {
            semantic_analyzer,
            type_inferencer,
            schema_inferencer,
            constraint_solver,
        }
    }
    
    /// Perform live validation on document
    pub fn validate_document(&self, uri: &Url, text: &str) -> Result<Vec<EnhancedDiagnostic>, String> {
        let mut diagnostics = Vec::new();
        
        // Parse the document
        match parse(text) {
            Ok(program) => {
                let lines: Vec<&str> = text.lines().collect();
                let mut context = self.create_validation_context();
                
                // Validate top-level items
                for item in &program.items {
                    self.validate_top_level_item(&mut diagnostics, item, &lines, &mut context)?;
                }
                
                // Perform global validations
                self.perform_global_validations(&mut diagnostics, &program, &lines, &context)?;
                
                Ok(diagnostics)
            },
            Err(parse_error) => {
                // Convert parse error to diagnostic
                let diagnostic = self.create_parse_error_diagnostic(parse_error, text)?;
                Ok(vec![diagnostic])
            }
        }
    }
    
    /// Create validation context
    fn create_validation_context(&self) -> ValidationContext {
        let mut env = Environment::new();
        self.populate_builtin_environment(&mut env);
        
        ValidationContext {
            environment: env,
            module_context: None,
            function_context: None,
            type_context: HashMap::new(),
            capability_context: HashSet::new(),
        }
    }
    
    /// Validate top-level items
    fn validate_top_level_item(
        &self,
        diagnostics: &mut Vec<EnhancedDiagnostic>,
        item: &TopLevel,
        lines: &[&str],
        context: &mut ValidationContext,
    ) -> Result<(), String> {
        match item {
            TopLevel::Module(module) => {
                context.module_context = Some(module.name.clone());
                
                // Validate module governance
                self.validate_governance(diagnostics, &module.governance, &module.span, lines)?;
                
                // Validate module declarations
                for decl in &module.declarations {
                    self.validate_declaration(diagnostics, decl, lines, context)?;
                }
                
                context.module_context = None;
            },
            TopLevel::Interface(interface) => {
                // Validate interface declarations
                for decl in &interface.declarations {
                    self.validate_declaration(diagnostics, decl, lines, context)?;
                }
            },
            TopLevel::Use(use_stmt) => {
                // Validate module reference
                self.validate_module_reference(diagnostics, &use_stmt.module, &use_stmt.span, lines)?;
            },
            TopLevel::Declaration(decl) => {
                self.validate_declaration(diagnostics, decl, lines, context)?;
            },
            TopLevel::Expression(expr) => {
                self.validate_expression(diagnostics, expr, lines, context)?;
            },
        }
        
        Ok(())
    }
    
    /// Validate declarations
    fn validate_declaration(
        &self,
        diagnostics: &mut Vec<EnhancedDiagnostic>,
        decl: &Declaration,
        lines: &[&str],
        context: &mut ValidationContext,
    ) -> Result<(), String> {
        match decl {
            Declaration::Defun(defun) => {
                context.function_context = Some(defun.name.clone());
                
                // Validate function name
                self.validate_identifier(diagnostics, &defun.name, &defun.span, lines, "function")?;
                
                // Validate parameters
                for param in &defun.params {
                    self.validate_parameter(diagnostics, param, &defun.span, lines, context)?;
                }
                
                // Validate return type
                if let Some(return_type) = &defun.return_type {
                    self.validate_type_annotation(diagnostics, return_type, &defun.span, lines, context)?;
                }
                
                // Validate function body
                for expr in &defun.body {
                    self.validate_expression(diagnostics, expr, lines, context)?;
                }
                
                // Type check function consistency
                self.validate_function_type_consistency(diagnostics, defun, lines, context)?;
                
                context.function_context = None;
            },
            Declaration::Defcap(defcap) => {
                // Validate capability name
                self.validate_identifier(diagnostics, &defcap.name, &defcap.span, lines, "capability")?;
                
                // Validate parameters
                for param in &defcap.params {
                    self.validate_parameter(diagnostics, param, &defcap.span, lines, context)?;
                }
                
                // Validate capability body
                for expr in &defcap.body {
                    self.validate_expression(diagnostics, expr, lines, context)?;
                }
                
                // Add capability to context
                context.capability_context.insert(defcap.name.clone());
            },
            Declaration::Defconst(defconst) => {
                // Validate constant name
                self.validate_identifier(diagnostics, &defconst.name, &defconst.span, lines, "constant")?;
                
                // Validate type annotation
                if let Some(type_ann) = &defconst.type_ann {
                    self.validate_type_annotation(diagnostics, type_ann, &defconst.span, lines, context)?;
                }
                
                // Validate constant value
                self.validate_expression(diagnostics, &defconst.value, lines, context)?;
                
                // Type check constant consistency
                self.validate_constant_type_consistency(diagnostics, defconst, lines, context)?;
            },
            Declaration::Defschema(defschema) => {
                // Validate schema name
                self.validate_identifier(diagnostics, &defschema.name, &defschema.span, lines, "schema")?;
                
                // Validate schema fields
                let mut field_names = HashSet::new();
                for field in &defschema.fields {
                    // Check for duplicate field names
                    if !field_names.insert(&field.name) {
                        self.add_duplicate_field_diagnostic(diagnostics, &field.name, &defschema.span, lines)?;
                    }
                    
                    // Validate field type
                    self.validate_type_annotation(diagnostics, &field.ty, &defschema.span, lines, context)?;
                }
                
                // Add schema to type context
                let schema_type = self.convert_schema_to_type(defschema);
                context.type_context.insert(defschema.name.clone(), schema_type);
            },
            Declaration::Deftable(deftable) => {
                // Validate table name
                self.validate_identifier(diagnostics, &deftable.name, &deftable.span, lines, "table")?;
                
                // Validate schema reference
                self.validate_schema_reference(diagnostics, &deftable.schema, &deftable.span, lines, context)?;
            },
            Declaration::Defpact(defpact) => {
                // Validate pact name
                self.validate_identifier(diagnostics, &defpact.name, &defpact.span, lines, "pact")?;
                
                // Validate parameters
                for param in &defpact.params {
                    self.validate_parameter(diagnostics, param, &defpact.span, lines, context)?;
                }
                
                // Validate pact steps
                if defpact.steps.is_empty() {
                    self.add_empty_pact_diagnostic(diagnostics, &defpact.name, &defpact.span, lines)?;
                }
            },
            Declaration::Implements(interface_name) => {
                // Validate interface reference
                self.validate_interface_reference(diagnostics, interface_name, lines)?;
            },
        }
        
        Ok(())
    }
    
    /// Validate expressions with type checking
    fn validate_expression(
        &self,
        diagnostics: &mut Vec<EnhancedDiagnostic>,
        expr: &Expr,
        lines: &[&str],
        context: &ValidationContext,
    ) -> Result<(), String> {
        match expr {
            Expr::Var(name, span) => {
                // Check if variable is defined
                if !self.is_variable_defined(name, context) && !self.semantic_analyzer.is_builtin_function(name) {
                    self.add_undefined_variable_diagnostic(diagnostics, name, span, lines)?;
                }
            },
            Expr::QualifiedVar { module, name, span } => {
                // Validate module reference
                if !self.is_module_available(module, context) {
                    self.add_undefined_module_diagnostic(diagnostics, module, span, lines)?;
                }
            },
            Expr::App { func, args, span } => {
                // Validate function expression
                self.validate_expression(diagnostics, func.as_ref(), lines, context)?;
                
                // Validate arguments
                for arg in args {
                    self.validate_expression(diagnostics, arg, lines, context)?;
                }
                
                // Type check function application
                self.validate_function_application(diagnostics, func.as_ref(), args, span, lines, context)?;
            },
            Expr::Let { bindings, body, span } => {
                // Validate bindings
                let mut binding_names = HashSet::new();
                for binding in bindings {
                    // Check for duplicate bindings
                    if !binding_names.insert(&binding.name) {
                        self.add_duplicate_binding_diagnostic(diagnostics, &binding.name, span, lines)?;
                    }
                    
                    // Validate binding value
                    self.validate_expression(diagnostics, &binding.value, lines, context)?;
                }
                
                // Create new context with bindings
                let mut let_context = context.clone();
                self.add_bindings_to_context(&mut let_context, bindings);
                
                // Validate body expressions
                for expr in body {
                    self.validate_expression(diagnostics, expr, lines, &let_context)?;
                }
            },
            Expr::If { cond, then_expr, else_expr, span } => {
                // Validate condition
                self.validate_expression(diagnostics, cond.as_ref(), lines, context)?;
                
                // Type check condition (should be boolean)
                if let Some(cond_type) = self.infer_expression_type(cond.as_ref(), context) {
                    if cond_type != "bool" {
                        self.add_type_mismatch_diagnostic(
                            diagnostics,
                            "bool",
                            &cond_type,
                            &cond.span(),
                            lines,
                            "Condition must be boolean",
                        )?;
                    }
                }
                
                // Validate branches
                self.validate_expression(diagnostics, then_expr.as_ref(), lines, context)?;
                if let Some(else_e) = else_expr {
                    self.validate_expression(diagnostics, else_e.as_ref(), lines, context)?;
                    
                    // Type check branch consistency
                    self.validate_branch_type_consistency(diagnostics, then_expr.as_ref(), else_e.as_ref(), span, lines, context)?;
                }
            },
            Expr::Lambda { params, body, span } => {
                // Validate parameters
                for param in params {
                    self.validate_parameter(diagnostics, param, span, lines, context)?;
                }
                
                // Create new context with lambda parameters
                let mut lambda_context = context.clone();
                self.add_parameters_to_context(&mut lambda_context, params);
                
                // Validate lambda body
                for expr in body {
                    self.validate_expression(diagnostics, expr, lines, &lambda_context)?;
                }
            },
            Expr::WithCapability { cap, body, span } => {
                // Validate capability expression
                self.validate_expression(diagnostics, cap.as_ref(), lines, context)?;
                
                // Validate capability requirement
                self.validate_capability_requirement(diagnostics, cap.as_ref(), span, lines, context)?;
                
                // Validate body expressions
                for expr in body {
                    self.validate_expression(diagnostics, expr, lines, context)?;
                }
            },
            Expr::List(elements, span) => {
                // Validate list elements
                for element in elements {
                    self.validate_expression(diagnostics, element, lines, context)?;
                }
                
                // Type check list homogeneity
                self.validate_list_type_consistency(diagnostics, elements, span, lines, context)?;
            },
            Expr::Object(fields, span) => {
                // Validate object fields
                let mut field_names = HashSet::new();
                for (key, value) in fields {
                    // Check for duplicate keys
                    if !field_names.insert(key) {
                        self.add_duplicate_object_key_diagnostic(diagnostics, key, span, lines)?;
                    }
                    
                    // Validate field value
                    self.validate_expression(diagnostics, value, lines, context)?;
                }
            },
            // Database operations
            Expr::Read { table, key, span } => {
                self.validate_database_operation(diagnostics, "read", table, Some(key.as_ref()), None, span, lines, context)?;
            },
            Expr::Write { table, key, value, span } => {
                self.validate_database_operation(diagnostics, "write", table, Some(key.as_ref()), Some(value.as_ref()), span, lines, context)?;
            },
            Expr::Insert { table, key, value, span } => {
                self.validate_database_operation(diagnostics, "insert", table, Some(key.as_ref()), Some(value.as_ref()), span, lines, context)?;
            },
            Expr::Update { table, key, value, span } => {
                self.validate_database_operation(diagnostics, "update", table, Some(key.as_ref()), Some(value.as_ref()), span, lines, context)?;
            },
            // Guard operations
            Expr::Enforce { cond, message: _, span } => {
                self.validate_expression(diagnostics, cond.as_ref(), lines, context)?;
                
                // Type check condition
                if let Some(cond_type) = self.infer_expression_type(cond.as_ref(), context) {
                    if cond_type != "bool" {
                        self.add_type_mismatch_diagnostic(
                            diagnostics,
                            "bool",
                            &cond_type,
                            &cond.span(),
                            lines,
                            "Enforce condition must be boolean",
                        )?;
                    }
                }
            },
            _ => {
                // Handle other expression types as needed
            }
        }
        
        Ok(())
    }
    
    /// Perform global validations
    fn perform_global_validations(
        &self,
        diagnostics: &mut Vec<EnhancedDiagnostic>,
        program: &pact_parser::Program,
        lines: &[&str],
        context: &ValidationContext,
    ) -> Result<(), String> {
        // Check for unused imports
        self.check_unused_imports(diagnostics, program, lines)?;
        
        // Check for unreachable code
        self.check_unreachable_code(diagnostics, program, lines)?;
        
        // Check for missing documentation
        self.check_missing_documentation(diagnostics, program, lines)?;
        
        // Validate capability usage
        self.validate_capability_usage(diagnostics, program, lines, context)?;
        
        Ok(())
    }
    
    // Type checking methods
    
    /// Validate function type consistency
    fn validate_function_type_consistency(
        &self,
        diagnostics: &mut Vec<EnhancedDiagnostic>,
        defun: &pact_parser::Defun,
        lines: &[&str],
        context: &ValidationContext,
    ) -> Result<(), String> {
        // Check if declared return type matches inferred return type
        if let Some(declared_type) = &defun.return_type {
            if let Some(inferred_type) = self.infer_function_return_type(&defun.body, context) {
                let declared_str = self.type_to_string(declared_type);
                if declared_str != inferred_type {
                    self.add_return_type_mismatch_diagnostic(
                        diagnostics,
                        &declared_str,
                        &inferred_type,
                        &defun.span,
                        lines,
                    )?;
                }
            }
        }
        
        Ok(())
    }
    
    /// Validate constant type consistency
    fn validate_constant_type_consistency(
        &self,
        diagnostics: &mut Vec<EnhancedDiagnostic>,
        defconst: &pact_parser::Defconst,
        lines: &[&str],
        context: &ValidationContext,
    ) -> Result<(), String> {
        if let Some(declared_type) = &defconst.type_ann {
            if let Some(inferred_type) = self.infer_expression_type(&defconst.value, context) {
                let declared_str = self.type_to_string(declared_type);
                if declared_str != inferred_type {
                    self.add_type_mismatch_diagnostic(
                        diagnostics,
                        &declared_str,
                        &inferred_type,
                        &defconst.value.span(),
                        lines,
                        "Constant value type doesn't match declaration",
                    )?;
                }
            }
        }
        
        Ok(())
    }
    
    /// Validate function application types
    fn validate_function_application(
        &self,
        diagnostics: &mut Vec<EnhancedDiagnostic>,
        func: &Expr,
        args: &[Expr],
        span: &pact_lexer::Span,
        lines: &[&str],
        context: &ValidationContext,
    ) -> Result<(), String> {
        if let Some(func_name) = self.extract_function_name(func) {
            if let Some(signature) = self.get_function_signature(&func_name) {
                // Check argument count
                if args.len() != signature.parameters.len() {
                    self.add_argument_count_mismatch_diagnostic(
                        diagnostics,
                        &func_name,
                        signature.parameters.len(),
                        args.len(),
                        span,
                        lines,
                    )?;
                }
                
                // Check argument types
                for (i, (arg, expected_type)) in args.iter().zip(&signature.parameter_types).enumerate() {
                    if let Some(actual_type) = self.infer_expression_type(arg, context) {
                        if !self.types_compatible(&actual_type, expected_type) {
                            self.add_argument_type_mismatch_diagnostic(
                                diagnostics,
                                &func_name,
                                i,
                                expected_type,
                                &actual_type,
                                &arg.span(),
                                lines,
                            )?;
                        }
                    }
                }
            }
        }
        
        Ok(())
    }
    
    /// Validate branch type consistency
    fn validate_branch_type_consistency(
        &self,
        diagnostics: &mut Vec<EnhancedDiagnostic>,
        then_expr: &Expr,
        else_expr: &Expr,
        span: &pact_lexer::Span,
        lines: &[&str],
        context: &ValidationContext,
    ) -> Result<(), String> {
        if let (Some(then_type), Some(else_type)) = (
            self.infer_expression_type(then_expr, context),
            self.infer_expression_type(else_expr, context)
        ) {
            if !self.types_compatible(&then_type, &else_type) {
                self.add_branch_type_mismatch_diagnostic(
                    diagnostics,
                    &then_type,
                    &else_type,
                    span,
                    lines,
                )?;
            }
        }
        
        Ok(())
    }
    
    /// Validate list type consistency
    fn validate_list_type_consistency(
        &self,
        diagnostics: &mut Vec<EnhancedDiagnostic>,
        elements: &[Expr],
        span: &pact_lexer::Span,
        lines: &[&str],
        context: &ValidationContext,
    ) -> Result<(), String> {
        if elements.len() > 1 {
            if let Some(first_type) = self.infer_expression_type(&elements[0], context) {
                for (i, element) in elements.iter().skip(1).enumerate() {
                    if let Some(element_type) = self.infer_expression_type(element, context) {
                        if !self.types_compatible(&first_type, &element_type) {
                            self.add_list_type_inconsistency_diagnostic(
                                diagnostics,
                                &first_type,
                                &element_type,
                                i + 1,
                                &element.span(),
                                lines,
                            )?;
                        }
                    }
                }
            }
        }
        
        Ok(())
    }
    
    // Helper methods for validation
    
    /// Check if variable is defined in context
    fn is_variable_defined(&self, name: &str, context: &ValidationContext) -> bool {
        // Check in environment
        // This would need to be implemented based on the Environment structure
        // For now, assume all variables starting with uppercase are constants
        name.chars().next().map_or(false, |c| c.is_uppercase()) ||
        context.type_context.contains_key(name)
    }
    
    /// Check if module is available
    fn is_module_available(&self, module: &str, context: &ValidationContext) -> bool {
        // For now, assume some common modules are available
        matches!(module, "coin" | "ns" | "util" | "test")
    }
    
    /// Infer expression type
    fn infer_expression_type(&self, expr: &Expr, context: &ValidationContext) -> Option<String> {
        match expr {
            Expr::Integer(_, _) => Some("integer".to_string()),
            Expr::Decimal(_, _) => Some("decimal".to_string()),
            Expr::String(_, _) => Some("string".to_string()),
            Expr::Bool(_, _) => Some("bool".to_string()),
            Expr::List(elements, _) => {
                if let Some(first) = elements.first() {
                    if let Some(element_type) = self.infer_expression_type(first, context) {
                        Some(format!("[{}]", element_type))
                    } else {
                        Some("[a]".to_string())
                    }
                } else {
                    Some("[]".to_string())
                }
            },
            Expr::Object(_, _) => Some("object".to_string()),
            Expr::Var(name, _) => {
                // Look up in type context or use semantic analyzer
                if let Some(var_type) = context.type_context.get(name) {
                    Some(self.schema_type_to_string(var_type))
                } else {
                    self.semantic_analyzer.get_variable_type(name)
                }
            },
            _ => None,
        }
    }
    
    /// Infer function return type
    fn infer_function_return_type(&self, body: &[Expr], context: &ValidationContext) -> Option<String> {
        if let Some(last_expr) = body.last() {
            self.infer_expression_type(last_expr, context)
        } else {
            Some("unit".to_string())
        }
    }
    
    /// Check if types are compatible
    fn types_compatible(&self, type1: &str, type2: &str) -> bool {
        type1 == type2 || type1 == "a" || type2 == "a"
    }
    
    /// Extract function name from expression
    fn extract_function_name(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::Var(name, _) => Some(name.clone()),
            Expr::QualifiedVar { name, .. } => Some(name.clone()),
            _ => None,
        }
    }
    
    /// Get function signature
    fn get_function_signature(&self, func_name: &str) -> Option<FunctionSignature> {
        // This would be populated with built-in and user-defined function signatures
        match func_name {
            "+" | "-" | "*" => Some(FunctionSignature {
                parameters: vec!["x".to_string(), "y".to_string()],
                parameter_types: vec!["integer".to_string(), "integer".to_string()],
                return_type: "integer".to_string(),
            }),
            "=" | "!=" => Some(FunctionSignature {
                parameters: vec!["x".to_string(), "y".to_string()],
                parameter_types: vec!["a".to_string(), "a".to_string()],
                return_type: "bool".to_string(),
            }),
            _ => None,
        }
    }
    
    // Methods for creating specific diagnostics would continue here...
    // This is a representative sample of the validation framework
    
    /// Create parse error diagnostic
    fn create_parse_error_diagnostic(&self, _error: pact_parser::ParseError, _text: &str) -> Result<EnhancedDiagnostic, String> {
        // Implementation would create diagnostic from parse error
        Ok(EnhancedDiagnostic {
            diagnostic: Diagnostic {
                range: Range {
                    start: Position { line: 0, character: 0 },
                    end: Position { line: 0, character: 0 },
                },
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String("parse-error".to_string())),
                code_description: None,
                source: Some("pact-parser".to_string()),
                message: "Parse error".to_string(),
                related_information: None,
                tags: None,
                data: None,
            },
            fix_suggestions: vec![],
            type_info: None,
            related_symbols: vec![],
        })
    }
    
    // Placeholder implementations for diagnostic creation methods
    fn validate_governance(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _governance: &pact_parser::Governance, _span: &pact_lexer::Span, _lines: &[&str]) -> Result<(), String> { Ok(()) }
    fn validate_module_reference(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _module: &str, _span: &pact_lexer::Span, _lines: &[&str]) -> Result<(), String> { Ok(()) }
    fn validate_identifier(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _name: &str, _span: &pact_lexer::Span, _lines: &[&str], _kind: &str) -> Result<(), String> { Ok(()) }
    fn validate_parameter(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _param: &Parameter, _span: &pact_lexer::Span, _lines: &[&str], _context: &ValidationContext) -> Result<(), String> { Ok(()) }
    fn validate_type_annotation(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _ty: &Type, _span: &pact_lexer::Span, _lines: &[&str], _context: &ValidationContext) -> Result<(), String> { Ok(()) }
    fn validate_schema_reference(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _schema: &str, _span: &pact_lexer::Span, _lines: &[&str], _context: &ValidationContext) -> Result<(), String> { Ok(()) }
    fn validate_interface_reference(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _interface: &str, _lines: &[&str]) -> Result<(), String> { Ok(()) }
    fn validate_database_operation(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _op: &str, _table: &str, _key: Option<&Expr>, _value: Option<&Expr>, _span: &pact_lexer::Span, _lines: &[&str], _context: &ValidationContext) -> Result<(), String> { Ok(()) }
    fn validate_capability_requirement(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _cap: &Expr, _span: &pact_lexer::Span, _lines: &[&str], _context: &ValidationContext) -> Result<(), String> { Ok(()) }
    
    // Additional diagnostic creation methods would be implemented here...
    
    fn type_to_string(&self, ty: &Type) -> String {
        match ty {
            Type::String => "string".to_string(),
            Type::Integer => "integer".to_string(),
            Type::Decimal => "decimal".to_string(),
            Type::Bool => "bool".to_string(),
            _ => "unknown".to_string(),
        }
    }
    
    
    fn populate_builtin_environment(&self, _env: &mut Environment) {
        // Populate with built-in functions and types
    }
    
    fn schema_type_to_string(&self, ty: &SchemaType) -> String {
        format!("{}", ty)
    }
    
    fn convert_schema_to_type(&self, schema: &pact_parser::Defschema) -> SchemaType {
        // Convert Pact schema to schema type
        use pact_schema::{PrimType, Type};
        
        // For now, create an object type
        // In a full implementation, this would properly convert the schema fields
        Type::Object(pact_schema::RowTy::RowConcrete(std::collections::HashMap::new()))
    }
    
    fn add_bindings_to_context(&self, _context: &mut ValidationContext, _bindings: &[pact_parser::Binding]) {
        // Add let bindings to validation context
    }
    
    fn add_parameters_to_context(&self, _context: &mut ValidationContext, _params: &[Parameter]) {
        // Add parameters to validation context
    }
    
    // Global validation methods (placeholder implementations)
    fn check_unused_imports(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _program: &pact_parser::Program, _lines: &[&str]) -> Result<(), String> { Ok(()) }
    fn check_unreachable_code(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _program: &pact_parser::Program, _lines: &[&str]) -> Result<(), String> { Ok(()) }
    fn check_missing_documentation(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _program: &pact_parser::Program, _lines: &[&str]) -> Result<(), String> { Ok(()) }
    fn validate_capability_usage(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _program: &pact_parser::Program, _lines: &[&str], _context: &ValidationContext) -> Result<(), String> { Ok(()) }
    
    // Diagnostic creation helper methods (placeholder implementations)
    fn add_undefined_variable_diagnostic(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _name: &str, _span: &pact_lexer::Span, _lines: &[&str]) -> Result<(), String> { Ok(()) }
    fn add_undefined_module_diagnostic(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _module: &str, _span: &pact_lexer::Span, _lines: &[&str]) -> Result<(), String> { Ok(()) }
    fn add_duplicate_field_diagnostic(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _field: &str, _span: &pact_lexer::Span, _lines: &[&str]) -> Result<(), String> { Ok(()) }
    fn add_duplicate_binding_diagnostic(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _name: &str, _span: &pact_lexer::Span, _lines: &[&str]) -> Result<(), String> { Ok(()) }
    fn add_duplicate_object_key_diagnostic(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _key: &str, _span: &pact_lexer::Span, _lines: &[&str]) -> Result<(), String> { Ok(()) }
    fn add_empty_pact_diagnostic(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _name: &str, _span: &pact_lexer::Span, _lines: &[&str]) -> Result<(), String> { Ok(()) }
    fn add_type_mismatch_diagnostic(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _expected: &str, _actual: &str, _span: &pact_lexer::Span, _lines: &[&str], _message: &str) -> Result<(), String> { Ok(()) }
    fn add_return_type_mismatch_diagnostic(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _declared: &str, _inferred: &str, _span: &pact_lexer::Span, _lines: &[&str]) -> Result<(), String> { Ok(()) }
    fn add_argument_count_mismatch_diagnostic(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _func: &str, _expected: usize, _actual: usize, _span: &pact_lexer::Span, _lines: &[&str]) -> Result<(), String> { Ok(()) }
    fn add_argument_type_mismatch_diagnostic(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _func: &str, _arg_index: usize, _expected: &str, _actual: &str, _span: &pact_lexer::Span, _lines: &[&str]) -> Result<(), String> { Ok(()) }
    fn add_branch_type_mismatch_diagnostic(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _then_type: &str, _else_type: &str, _span: &pact_lexer::Span, _lines: &[&str]) -> Result<(), String> { Ok(()) }
    fn add_list_type_inconsistency_diagnostic(&self, _diagnostics: &mut Vec<EnhancedDiagnostic>, _expected: &str, _actual: &str, _index: usize, _span: &pact_lexer::Span, _lines: &[&str]) -> Result<(), String> { Ok(()) }
}

/// Function signature for validation
#[derive(Debug, Clone)]
struct FunctionSignature {
    parameters: Vec<String>,
    parameter_types: Vec<String>,
    return_type: String,
}