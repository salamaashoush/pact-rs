//! High-performance optimized parser - the one and only parser implementation
//!
//! This parser uses arena allocation and string interning for maximum performance

use super::ast::*;
use super::ast_arena::{ArenaStats, AstArena};
use super::error::{Result, parse_error, unexpected_token};
use compact_str::CompactString;
use crate::lexer::{Lexer, MemoryUsage, Token};
use pact_core::shared::SpanInfo;

/// High-performance parser with arena allocation
pub struct Parser {
    tokens: Vec<(Token, SpanInfo)>,
    position: usize,
    arena: AstArena,
    lexer_stats: Option<MemoryUsage>,
}

impl Parser {
    /// Create a new optimized parser from source code
    pub fn new(source: &str) -> Result<Self> {
        let mut lexer = Lexer::new();
        let tokens = lexer.lex_interned(source)?;
        let lexer_stats = lexer.memory_usage();

        Ok(Self {
            tokens,
            position: 0,
            arena: AstArena::new(),
            lexer_stats: Some(lexer_stats),
        })
    }

    /// Create parser from pre-lexed tokens (for testing)
    pub fn from_tokens(tokens: Vec<(Token, SpanInfo)>) -> Self {
        Self {
            tokens,
            position: 0,
            arena: AstArena::new(),
            lexer_stats: None,
        }
    }
    
    /// Get current span
    fn current_span(&self) -> SpanInfo {
        self.current_token()
            .map(|(_, span)| *span)
            .unwrap_or(SpanInfo::empty())
    }

    /// Parse a complete program
    pub fn parse_program(&mut self) -> Result<Program<SpanInfo>> {
        let mut items = Vec::new();

        while !self.is_at_end() {
            items.push(self.parse_top_level()?);
        }

        Ok(items)
    }

    /// Parse top-level items
    fn parse_top_level(&mut self) -> Result<ParsedTopLevel<SpanInfo>> {
        match self.current_token() {
            Some((Token::OpenParens, _)) => {
                self.advance(); // consume '('

                match self.current_token() {
                    Some((Token::Module, _)) => {
                        self.advance();
                        let module = self.parse_module()?;
                        self.consume(Token::CloseParens)?;
                        Ok(ParsedTopLevel::TLModule(module))
                    }
                    Some((Token::Interface, _)) => {
                        self.advance();
                        let interface = self.parse_interface()?;
                        self.consume(Token::CloseParens)?;
                        Ok(ParsedTopLevel::TLInterface(interface))
                    }
                    Some((Token::Import, _)) => {
                        self.advance();
                        let import = self.parse_import()?;
                        self.consume(Token::CloseParens)?;
                        Ok(ParsedTopLevel::TLUse(import))
                    }
                    _ => {
                        // Must be an expression - backtrack the opening paren
                        self.position -= 1;
                        let expr = self.parse_expression()?;
                        Ok(ParsedTopLevel::TLTerm(expr))
                    }
                }
            }
            _ => {
                // Top-level expression without parens
                let expr = self.parse_expression()?;
                Ok(ParsedTopLevel::TLTerm(expr))
            }
        }
    }

    /// Parse module
    fn parse_module(&mut self) -> Result<ParsedModule<SpanInfo>> {
        // Parse name
        let name: CompactString = match self.current_token() {
            Some((Token::Ident(s), _)) => {
                let result = s.clone().into();
                self.advance();
                result
            }
            _ => return Err(parse_error("Expected module name".to_string(), self.current_span())),
        };

        // Parse governance - handle both single-tick keysets and capability governance
        let governance = match self.current_token() {
            Some((Token::SingleTick(s), _)) => {
                // Single-tick identifier like 'keyset-name
                let result = Governance::KeyGov(s[1..].into());
                self.advance();
                result
            }
            Some((Token::Ident(s), _)) if s.starts_with('\'') => {
                // Legacy: quoted keyset name as identifier
                let result = Governance::KeyGov(s[1..].into());
                self.advance();
                result
            }
            Some((Token::Ident(s), _)) => {
                // Plain identifier - could be capability reference
                let result = Governance::CapGov(s.clone().into());
                self.advance();
                result
            }
            Some((Token::OpenParens, _)) => {
                // Capability governance - parse as expression
                let _cap_expr = self.parse_expression()?;
                // For now, convert to capability governance (simplified)
                Governance::CapGov("capability".into())
            }
            _ => {
                let span = self.current_span();
                return Err(parse_error(
                    "governance (keyset name or capability)".to_string(),
                    span,
                ))
            }
        };

        // Parse definitions
        let mut definitions = Vec::new();
        let mut imports = Vec::new();

        while !self.check(&Token::CloseParens) && !self.is_at_end() {
            if self.check(&Token::OpenParens) {
                self.advance();
                match self.current_token() {
                    Some((Token::Defun, _)) => {
                        self.advance();
                        definitions.push(ParsedDef::Dfun(self.parse_defun()?));
                    }
                    Some((Token::Defcap, _)) => {
                        self.advance();
                        definitions.push(ParsedDef::DCap(self.parse_defcap()?));
                    }
                    Some((Token::Defconst, _)) => {
                        self.advance();
                        definitions.push(ParsedDef::DConst(self.parse_defconst()?));
                    }
                    Some((Token::Defschema, _)) => {
                        self.advance();
                        definitions.push(ParsedDef::DSchema(self.parse_defschema()?));
                    }
                    Some((Token::Deftable, _)) => {
                        self.advance();
                        definitions.push(ParsedDef::DTable(self.parse_deftable()?));
                    }
                    Some((Token::Defpact, _)) => {
                        self.advance();
                        definitions.push(ParsedDef::DPact(self.parse_defpact()?));
                    }
                    Some((Token::Bless, _)) => {
                        self.advance();
                        // Bless takes a string/tick hash
                        match self.current_token() {
                            Some((Token::String(hash), _)) => {
                                imports.push(ExtDecl::ExtBless {
                                    hash: hash.clone().into(),
                                    info: SpanInfo::empty(),
                                });
                                self.advance();
                            }
                            Some((Token::SingleTick(hash), _)) => {
                                imports.push(ExtDecl::ExtBless {
                                    hash: hash.trim_start_matches('\'').into(),
                                    info: SpanInfo::empty(),
                                });
                                self.advance();
                            }
                            _ => return Err(parse_error("Expected hash string".to_string(), self.current_span())),
                        }
                    }
                    Some((Token::Implements, _)) => {
                        self.advance();
                        let interface_name = self.parse_module_name()?;
                        imports.push(ExtDecl::ExtImplements {
                            module: interface_name,
                            info: SpanInfo::empty(),
                        });
                    }
                    Some((Token::Import, _)) => {
                        self.advance();
                        imports.push(ExtDecl::ExtImport(self.parse_import()?));
                    }
                    _ => {
                        // Skip unknown definitions for now
                        self.skip_to_closing_paren();
                    }
                }
                self.consume(Token::CloseParens)?;
            } else {
                self.advance(); // Skip unexpected tokens
            }
        }

        Ok(ParsedModule {
            name,
            governance,
            definitions,
            imports,
            annotations: Vec::new(),
            info: SpanInfo::empty(), // placeholder for span info
        })
    }

    /// Parse interface
    /// Parse interface
    fn parse_interface(&mut self) -> Result<ParsedInterface<SpanInfo>> {
        let name: CompactString = self.expect_symbol("interface name")?;

        let mut imports = Vec::new();
        let mut definitions = Vec::new();
        let annotations = Vec::new();

        // Parse interface body
        while !self.check(&Token::CloseParens) && !self.is_at_end() {
            match self.current_token() {
                Some((Token::OpenParens, _)) => {
                    self.advance();
                    match self.current_token() {
                        Some((Token::Import, _)) => {
                            self.advance();
                            imports.push(self.parse_import()?);
                            self.consume(Token::CloseParens)?;
                        }
                        Some((Token::Defun, _)) => {
                            self.advance();
                            definitions.push(ParsedIfDef::IfDfun(self.parse_ifdefun()?));
                            self.consume(Token::CloseParens)?;
                        }
                        Some((Token::Defcap, _)) => {
                            self.advance();
                            definitions.push(ParsedIfDef::IfDCap(self.parse_ifdefcap()?));
                            self.consume(Token::CloseParens)?;
                        }
                        Some((Token::Defschema, _)) => {
                            self.advance();
                            definitions.push(ParsedIfDef::IfDSchema(self.parse_defschema()?));
                            self.consume(Token::CloseParens)?;
                        }
                        Some((Token::Defconst, _)) => {
                            self.advance();
                            definitions.push(ParsedIfDef::IfDConst(self.parse_defconst()?));
                            self.consume(Token::CloseParens)?;
                        }
                        _ => return Err(parse_error(format!("Expected {}", "interface definition".to_string()), self.current_span())),
                    }
                }
                _ => return Err(parse_error(format!("Expected {}", "interface content".to_string()), self.current_span())),
            }
        }

        Ok(ParsedInterface {
            name,
            imports,
            definitions,
            annotations,
            info: SpanInfo::empty(),
        })
    }

    /// Parse interface function declaration
    fn parse_ifdefun(&mut self) -> Result<IfDefun<SpanInfo>> {
        let func_name = self.expect_symbol("function name")?;

        // Check for return type annotation
        let name = if self.check(&Token::Colon) {
            self.advance(); // consume ':'
            let ret_type = self.parse_type()?;
            MArg {
                name: func_name,
                ty: Some(ret_type),
                info: SpanInfo::empty(),
            }
        } else {
            MArg {
                name: func_name,
                ty: None,
                info: SpanInfo::empty(),
            }
        };

        // Parse arguments
        self.consume(Token::OpenParens)?;
        let mut args = Vec::new();
        while !self.check(&Token::CloseParens) && !self.is_at_end() {
            args.push(self.parse_arg()?);
        }
        self.consume(Token::CloseParens)?;

        Ok(IfDefun {
            name,
            args,
            annotations: Vec::new(),
            info: SpanInfo::empty(),
        })
    }

    /// Parse interface capability declaration
    fn parse_ifdefcap(&mut self) -> Result<IfDefCap<SpanInfo>> {
        let cap_name = self.expect_symbol("capability name")?;

        // Check for return type annotation
        let name = if self.check(&Token::Colon) {
            self.advance(); // consume ':'
            let ret_type = self.parse_type()?;
            MArg {
                name: cap_name,
                ty: Some(ret_type),
                info: SpanInfo::empty(),
            }
        } else {
            MArg {
                name: cap_name,
                ty: None,
                info: SpanInfo::empty(),
            }
        };

        // Parse arguments
        self.consume(Token::OpenParens)?;
        let mut args = Vec::new();
        while !self.check(&Token::CloseParens) && !self.is_at_end() {
            args.push(self.parse_arg()?);
        }
        self.consume(Token::CloseParens)?;

        Ok(IfDefCap {
            name,
            args,
            annotations: Vec::new(),
            meta: None,
            info: SpanInfo::empty(),
        })
    }

    /// Parse import statement
    fn parse_import(&mut self) -> Result<Import<SpanInfo>> {
        // Parse module name (could be qualified)
        let module_name = self.parse_module_name_component()?;

        let module = if self.check(&Token::Dot) {
            // Qualified module name
            self.advance();
            let namespace = NamespaceName(module_name);
            let name = self.parse_module_name_component()?;
            ModuleName {
                name,
                namespace: Some(namespace),
            }
        } else {
            ModuleName {
                name: module_name,
                namespace: None,
            }
        };

        // Optional hash
        let hash = match self.current_token() {
            Some((Token::String(h), _)) => {
                let hash = Some(h.clone().into());
                self.advance();
                hash
            }
            _ => None,
        };

        // Optional import list
        let imports = if self.check(&Token::OpenBracket) {
            self.advance();
            let mut import_list = Vec::new();
            while !self.check(&Token::CloseBracket) && !self.is_at_end() {
                import_list.push(self.expect_symbol("import name")?);
            }
            self.consume(Token::CloseBracket)?;
            Some(import_list)
        } else {
            None
        };

        Ok(Import {
            module,
            hash,
            imports,
            info: SpanInfo::empty(),
        })
    }

    /// Parse function definition
    fn parse_defun(&mut self) -> Result<ParsedDefun<SpanInfo>> {
        let func_name = self.expect_symbol("function name")?;

        // Check for return type annotation on function name
        let name = if self.check(&Token::Colon) {
            self.advance(); // consume ':'
            let ret_type = self.parse_type()?;
            MArg {
                name: func_name,
                ty: Some(ret_type),
                info: SpanInfo::empty(),
            }
        } else {
            MArg {
                name: func_name,
                ty: None,
                info: SpanInfo::empty(),
            }
        };

        // Parse arguments
        self.consume(Token::OpenParens)?;
        let mut args = Vec::new();
        while !self.check(&Token::CloseParens) && !self.is_at_end() {
            let arg_name = self.expect_symbol("argument name")?;

            // Check for type annotation
            let arg_type = if self.check(&Token::Colon) {
                self.advance(); // consume ':'
                Some(self.parse_type()?)
            } else {
                None
            };

            args.push(MArg {
                name: arg_name,
                ty: arg_type,
                info: SpanInfo::empty(),
            });
        }
        self.consume(Token::CloseParens)?;

        // Parse annotations before body
        let mut annotations = Vec::new();
        while let Some((Token::DocAnn | Token::ModelAnn, _)) = self.current_token() {
            annotations.push(self.parse_annotation()?);
        }

        // Parse function body (block of expressions)
        let mut body = Vec::new();
        while !self.check(&Token::CloseParens) && !self.is_at_end() {
            body.push(self.parse_expression()?);
        }

        if body.is_empty() {
            return Err(parse_error(format!("Expected {}", "function body".to_string()), self.current_span()));
        }

        Ok(ParsedDefun {
            name,
            args,
            body,
            annotations,
            info: SpanInfo::empty(),
        })
    }

    /// Parse expression following Haskell grammar: Expr ::= '(' SExpr ')' | Atom
    fn parse_expression(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        match self.current_token() {
            Some((Token::OpenParens, span)) => {
                let _span = *span;
                self.advance(); // consume '('

                // Parse S-expression: LamExpr | LetExpr | GenAppExpr
                let expr = match self.current_token() {
                    Some((Token::Lambda, _)) => self.parse_lambda_expr()?,
                    Some((Token::Let, _)) => self.parse_let_expr()?,
                    Some((Token::LetStar, _)) => self.parse_let_star_expr()?,
                    _ => self.parse_app_expr()?,
                };

                self.consume(Token::CloseParens)?;
                Ok(expr)
            }
            _ => self.parse_atom(),
        }
    }

    /// Parse atomic expressions (variables, literals)
    fn parse_atom(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        match self.current_token() {
            Some((Token::Ident(s), span)) => {
                let name = s.clone();
                let _span = *span;
                self.advance();

                // Check for qualified name (module.function)
                if self.check(&Token::Dot) {
                    self.advance(); // consume '.'
                    let func_name = self.expect_symbol("function name")?;
                    Ok(ParsedExpr::Var(
                        ParsedName::QN(QualifiedName {
                            module: ModuleName {
                                name: name.into(),
                                namespace: None,
                            },
                            name: func_name,
                        }),
                        SpanInfo::empty(),
                    ))
                } else {
                    Ok(ParsedExpr::Var(
                        ParsedName::BN(BareName(name.into())),
                        SpanInfo::empty(),
                    ))
                }
            }
            Some((Token::SingleTick(s), span)) => {
                let name = s.clone();
                let _span = *span;
                self.advance();
                // Single-tick identifiers are typically keyset references
                Ok(ParsedExpr::Var(
                    ParsedName::BN(BareName(name.into())),
                    SpanInfo::empty(),
                ))
            }
            Some((Token::String(s), span)) => {
                let s = s.clone();
                let _span = *span;
                self.advance();
                Ok(ParsedExpr::Constant(
                    Literal::LString(s.into()),
                    SpanInfo::empty(),
                ))
            }
            Some((Token::Number(s), span)) => {
                let s = s.clone();
                let _span = *span;
                self.advance();
                if let Ok(i) = s.parse::<i64>() {
                    Ok(ParsedExpr::Constant(
                        Literal::LInteger(i),
                        SpanInfo::empty(),
                    ))
                } else if let Ok(d) = s.parse::<f64>() {
                    Ok(ParsedExpr::Constant(
                        Literal::LDecimal(Decimal {
                            precision: 18,
                            mantissa: (d * 1000000000000000000.0) as i128,
                        }),
                        SpanInfo::empty(),
                    ))
                } else {
                    let span = self.current_span();
                    Err(parse_error(format!("Invalid number: {}", s), span))
                }
            }
            Some((Token::True, span)) => {
                let _span = *span;
                self.advance();
                Ok(ParsedExpr::Constant(
                    Literal::LBool(true),
                    SpanInfo::empty(),
                ))
            }
            Some((Token::False, _span)) => {
                self.advance();
                Ok(ParsedExpr::Constant(
                    Literal::LBool(false),
                    SpanInfo::empty(),
                ))
            }
            Some((Token::OpenBracket, span)) => {
                let span_info = *span;
                self.parse_list_expr(span_info)
            }
            Some((Token::OpenBrace, span)) => {
                let span_info = *span;
                self.parse_object_expr(span_info)
            }
            _ => {
                let span = self.current_span();
                if let Some((token, _)) = self.current_token() {
                    Err(parse_error(format!(
                        "expression, but found {:?}",
                        token
                    ), span))
                } else {
                    Err(parse_error(
                        "expression, but reached end of input".to_string(),
                        span
                    ))
                }
            }
        }
    }

    /// Parse lambda expressions: lambda '(' [MArg] ')' Block
    fn parse_lambda_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        self.consume(Token::Lambda)?;

        // Parse argument list
        self.consume(Token::OpenParens)?;
        let mut args = Vec::new();
        while !self.check(&Token::CloseParens) && !self.is_at_end() {
            args.push(self.parse_arg()?);
        }
        self.consume(Token::CloseParens)?;

        // Parse body (block of expressions)
        let mut body = Vec::new();
        while !self.check(&Token::CloseParens) && !self.is_at_end() {
            body.push(self.parse_expression()?);
        }

        if body.is_empty() {
            return Err(parse_error(format!("Expected {}", "lambda body".to_string()), self.current_span()));
        }

        Ok(ParsedExpr::Lam {
            args,
            body,
            info: SpanInfo::empty(),
        })
    }

    /// Parse let expressions: let '(' [Binder] ')' Block
    fn parse_let_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        self.consume(Token::Let)?;

        // Parse bindings
        self.consume(Token::OpenParens)?;
        let mut bindings = Vec::new();
        while !self.check(&Token::CloseParens) && !self.is_at_end() {
            bindings.push(self.parse_binder()?);
        }
        self.consume(Token::CloseParens)?;

        // Parse body
        let mut body = Vec::new();
        while !self.check(&Token::CloseParens) && !self.is_at_end() {
            body.push(self.parse_expression()?);
        }

        if body.is_empty() {
            return Err(parse_error(format!("Expected {}", "let body".to_string()), self.current_span()));
        }

        Ok(ParsedExpr::Let {
            form: LetForm::LFLetNormal,
            bindings,
            body,
            info: SpanInfo::empty(),
        })
    }

    /// Parse let* expressions: let* '(' [Binder] ')' Block
    fn parse_let_star_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        self.consume(Token::LetStar)?;

        // Parse bindings (same as let)
        self.consume(Token::OpenParens)?;
        let mut bindings = Vec::new();
        while !self.check(&Token::CloseParens) && !self.is_at_end() {
            bindings.push(self.parse_binder()?);
        }
        self.consume(Token::CloseParens)?;

        // Parse body
        let mut body = Vec::new();
        while !self.check(&Token::CloseParens) && !self.is_at_end() {
            body.push(self.parse_expression()?);
        }

        if body.is_empty() {
            return Err(parse_error(format!("Expected {}", "let* body".to_string()), self.current_span()));
        }

        Ok(ParsedExpr::Let {
            form: LetForm::LFLetStar,
            bindings,
            body,
            info: SpanInfo::empty(),
        })
    }

    /// Parse function application or special forms
    fn parse_app_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        // Check if this is a special form first
        match self.current_token() {
            Some((Token::Ident(name), _)) => {
                match name.as_str() {
                    // Control flow special forms
                    "if" => return self.parse_if_expr(),
                    "and" => return self.parse_and_expr(),
                    "or" => return self.parse_or_expr(),
                    "cond" => return self.parse_cond_expr(),

                    // Enforcement special forms
                    "enforce" => return self.parse_enforce_expr(),
                    "enforce-one" => return self.parse_enforce_one_expr(),

                    // Capability special forms
                    "with-capability" => return self.parse_with_capability_expr(),
                    "require-capability" => return self.parse_require_capability_expr(),
                    "compose-capability" => return self.parse_compose_capability_expr(),
                    "install-capability" => return self.parse_install_capability_expr(),
                    "emit-event" => return self.parse_emit_event_expr(),
                    "create-user-guard" => return self.parse_create_user_guard_expr(),

                    // Try/catch
                    "try" => return self.parse_try_expr(),

                    // Defpact special forms
                    "yield" => return self.parse_yield_expr(),
                    "resume" => return self.parse_resume_expr(),

                    // Binding forms
                    "bind" => return self.parse_bind_expr(),

                    _ => {} // Fall through to normal function application
                }
            }
            // Handle definition forms as top-level expressions
            Some((Token::Defconst, _)) => {
                self.advance();
                return self.parse_defconst_expr();
            }
            Some((Token::Defun, _)) => {
                self.advance();
                return self.parse_defun_expr();
            }
            Some((Token::Defcap, _)) => {
                self.advance();
                return self.parse_defcap_expr();
            }
            Some((Token::Defschema, _)) => {
                self.advance();
                return self.parse_defschema_expr();
            }
            Some((Token::Deftable, _)) => {
                self.advance();
                return self.parse_deftable_expr();
            }
            Some((Token::Defpact, _)) => {
                self.advance();
                return self.parse_defpact_expr();
            }
            _ => {}
        }

        // Parse as normal function application
        let func = Box::new(self.parse_expression()?);

        let mut args = Vec::new();
        while !self.check(&Token::CloseParens) && !self.is_at_end() {
            args.push(self.parse_expression()?);
        }

        Ok(ParsedExpr::App {
            func,
            args,
            info: SpanInfo::empty(),
        })
    }

    /// Parse list literals: '[' [Expr] ']'
    fn parse_list_expr(&mut self, _span: SpanInfo) -> Result<ParsedExpr<SpanInfo>> {
        self.consume(Token::OpenBracket)?;

        let mut elements = Vec::new();
        while !self.check(&Token::CloseBracket) && !self.is_at_end() {
            elements.push(self.parse_expression()?);

            // Optional comma
            if self.check(&Token::Comma) {
                self.advance();
            }
        }

        self.consume(Token::CloseBracket)?;

        Ok(ParsedExpr::List(elements, SpanInfo::empty()))
    }

    /// Parse object literals: '{' [(Field, Expr)] '}'
    /// Also handles binding patterns: {"field" := var}
    fn parse_object_expr(&mut self, _span: SpanInfo) -> Result<ParsedExpr<SpanInfo>> {
        self.consume(Token::OpenBrace)?;

        // Check if this is a binding pattern by looking ahead
        let is_binding_pattern = self.is_binding_pattern();

        if is_binding_pattern {
            // Parse as binding pattern
            self.parse_binding_pattern()
        } else {
            // Parse as regular object
            let mut fields = Vec::new();
            while !self.check(&Token::CloseBrace) && !self.is_at_end() {
                // Parse field name
                let field = match self.current_token() {
                    Some((Token::String(s), _)) => {
                        let field = Field(s.clone().into());
                        self.advance();
                        field
                    }
                    Some((Token::Ident(s), _)) => {
                        let field = Field(s.clone().into());
                        self.advance();
                        field
                    }
                    Some((Token::SingleTick(s), _)) => {
                        // Remove leading tick for field name
                        let field_name = s.trim_start_matches('\'');
                        let field = Field(field_name.into());
                        self.advance();
                        field
                    }
                    _ => return Err(parse_error(format!("Expected {}", "field name".to_string()), self.current_span())),
                };

                self.consume(Token::Colon)?;
                let expr = self.parse_expression()?;

                fields.push((field, expr));

                // Optional comma
                if self.check(&Token::Comma) {
                    self.advance();
                }
            }

            self.consume(Token::CloseBrace)?;

            Ok(ParsedExpr::Object(fields, SpanInfo::empty()))
        }
    }

    /// Parse function argument: IDENT [':' Type]
    fn parse_arg(&mut self) -> Result<MArg<SpanInfo>> {
        let name = self.expect_symbol("argument name")?;

        let ty = if self.check(&Token::Colon) {
            self.advance(); // consume ':'
            Some(self.parse_type()?)
        } else {
            None
        };

        Ok(MArg {
            name,
            ty,
            info: SpanInfo::empty(),
        })
    }

    /// Parse let binding: '(' IDENT Expr ')'
    fn parse_binder(&mut self) -> Result<Binder<SpanInfo>> {
        self.consume(Token::OpenParens)?;

        let name = self.expect_symbol("binding name")?;
        let expr = self.parse_expression()?;

        self.consume(Token::CloseParens)?;

        Ok(Binder {
            arg: MArg {
                name,
                ty: None,
                info: SpanInfo::empty(),
            },
            expr,
        })
    }

    /// Parse if expression: (if cond then [else])
    fn parse_if_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        self.advance(); // consume 'if'

        let cond = Box::new(self.parse_expression()?);
        let then_expr = Box::new(self.parse_expression()?);

        let else_expr = if !self.check(&Token::CloseParens) {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };

        Ok(ParsedExpr::If {
            cond,
            then_expr,
            else_expr,
            info: SpanInfo::empty(),
        })
    }

    /// Parse and expression: (and left right)
    fn parse_and_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        self.advance(); // consume 'and'

        let left = Box::new(self.parse_expression()?);
        let right = Box::new(self.parse_expression()?);

        Ok(ParsedExpr::And {
            left,
            right,
            info: SpanInfo::empty(),
        })
    }

    /// Parse or expression: (or left right)
    fn parse_or_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        self.advance(); // consume 'or'

        let left = Box::new(self.parse_expression()?);
        let right = Box::new(self.parse_expression()?);

        Ok(ParsedExpr::Or {
            left,
            right,
            info: SpanInfo::empty(),
        })
    }

    /// Parse cond expression: (cond (test1 expr1) (test2 expr2) ... [default])
    fn parse_cond_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        self.advance(); // consume 'cond'

        let mut branches = Vec::new();

        while !self.check(&Token::CloseParens) && !self.is_at_end() {
            if self.check(&Token::OpenParens) {
                // Regular conditional branch
                self.advance();
                let test = self.parse_expression()?;
                let expr = self.parse_expression()?;
                self.consume(Token::CloseParens)?;

                branches.push((test, expr));
            } else {
                // Default case - bare expression
                let default_expr = self.parse_expression()?;
                // Add as always-true condition
                branches.push((
                    ParsedExpr::Constant(Literal::LBool(true), SpanInfo::empty()),
                    default_expr,
                ));
                break; // Default must be last
            }
        }

        Ok(ParsedExpr::Cond {
            branches,
            info: SpanInfo::empty(),
        })
    }

    /// Parse enforce expression: (enforce condition error-msg)
    fn parse_enforce_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        self.advance(); // consume 'enforce'

        let cond = Box::new(self.parse_expression()?);
        let msg = Box::new(self.parse_expression()?);

        Ok(ParsedExpr::Enforce {
            cond,
            msg,
            info: SpanInfo::empty(),
        })
    }

    /// Parse enforce-one expression: (enforce-one msg [conditions])
    fn parse_enforce_one_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        self.advance(); // consume 'enforce-one'

        let msg = Box::new(self.parse_expression()?);

        // Parse list of conditions
        self.consume(Token::OpenBracket)?;
        let mut conds = Vec::new();
        while !self.check(&Token::CloseBracket) && !self.is_at_end() {
            conds.push(self.parse_expression()?);
        }
        self.consume(Token::CloseBracket)?;

        Ok(ParsedExpr::EnforceOne {
            msg,
            conds,
            info: SpanInfo::empty(),
        })
    }

    /// Parse with-capability expression: (with-capability (cap) body...)
    fn parse_with_capability_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        self.advance(); // consume 'with-capability'

        let cap = Box::new(self.parse_expression()?);

        let mut body = Vec::new();
        while !self.check(&Token::CloseParens) && !self.is_at_end() {
            body.push(self.parse_expression()?);
        }

        Ok(ParsedExpr::WithCapability {
            cap,
            body,
            info: SpanInfo::empty(),
        })
    }

    /// Parse require-capability expression: (require-capability (cap))
    fn parse_require_capability_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        self.advance(); // consume 'require-capability'

        let cap = Box::new(self.parse_expression()?);

        Ok(ParsedExpr::RequireCapability {
            cap,
            info: SpanInfo::empty(),
        })
    }

    /// Parse compose-capability expression: (compose-capability (cap))
    fn parse_compose_capability_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        self.advance(); // consume 'compose-capability'

        let cap = Box::new(self.parse_expression()?);

        Ok(ParsedExpr::ComposeCapability {
            cap,
            info: SpanInfo::empty(),
        })
    }

    /// Parse install-capability expression: (install-capability (cap))
    fn parse_install_capability_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        self.advance(); // consume 'install-capability'

        let cap = Box::new(self.parse_expression()?);

        Ok(ParsedExpr::InstallCapability {
            cap,
            info: SpanInfo::empty(),
        })
    }

    /// Parse emit-event expression: (emit-event (cap))
    fn parse_emit_event_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        self.advance(); // consume 'emit-event'

        let cap = Box::new(self.parse_expression()?);

        Ok(ParsedExpr::EmitEvent {
            cap,
            info: SpanInfo::empty(),
        })
    }

    /// Parse create-user-guard expression: (create-user-guard name args...)
    fn parse_create_user_guard_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        self.advance(); // consume 'create-user-guard'

        let name = match self.current_token() {
            Some((Token::Ident(s), _)) => {
                let name = ParsedName::BN(BareName(s.clone().into()));
                self.advance();
                name
            }
            _ => return Err(parse_error(format!("Expected {}", "guard name".to_string()), self.current_span())),
        };

        let mut args = Vec::new();
        while !self.check(&Token::CloseParens) && !self.is_at_end() {
            args.push(self.parse_expression()?);
        }

        Ok(ParsedExpr::CreateUserGuard {
            name,
            args,
            info: SpanInfo::empty(),
        })
    }

    /// Parse try expression: (try expr catch-expr)
    fn parse_try_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        self.advance(); // consume 'try'

        let expr = Box::new(self.parse_expression()?);
        let catch = Box::new(self.parse_expression()?);

        Ok(ParsedExpr::Try {
            expr,
            catch,
            info: SpanInfo::empty(),
        })
    }

    /// Parse yield expression: (yield data [target])
    fn parse_yield_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        self.advance(); // consume 'yield'

        let data = Box::new(self.parse_expression()?);

        let target = if !self.check(&Token::CloseParens) {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };

        Ok(ParsedExpr::Yield {
            data,
            target,
            info: SpanInfo::empty(),
        })
    }

    /// Parse resume expression: (resume bindings body)
    fn parse_resume_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        self.advance(); // consume 'resume'

        // Parse bindings
        self.consume(Token::OpenParens)?;
        let mut bindings = Vec::new();
        while !self.check(&Token::CloseParens) && !self.is_at_end() {
            bindings.push(self.parse_binder()?);
        }
        self.consume(Token::CloseParens)?;

        let body = Box::new(self.parse_expression()?);

        Ok(ParsedExpr::Resume {
            bindings,
            body,
            info: SpanInfo::empty(),
        })
    }

    /// Parse bind expression: (bind expr {"field" := var, ...} body)
    fn parse_bind_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        self.advance(); // consume 'bind'

        // Parse the expression to bind
        let _expr = self.parse_expression()?;

        // Parse the binding pattern
        let bindings_expr = self.parse_expression()?;

        // Extract bindings from the pattern
        let bindings = match bindings_expr {
            ParsedExpr::Binding { bindings, .. } => bindings,
            _ => return Err(parse_error(format!("Expected {}", "binding pattern".to_string()), self.current_span())),
        };

        // Parse body expressions
        let mut body = Vec::new();
        while !self.check(&Token::CloseParens) && !self.is_at_end() {
            body.push(self.parse_expression()?);
        }

        Ok(ParsedExpr::Binding {
            bindings,
            body,
            info: SpanInfo::empty(),
        })
    }

    /// Parse type
    fn parse_type(&mut self) -> Result<Type> {
        match self.current_token() {
            Some((Token::OpenBracket, _)) => {
                // List type: [type]
                self.advance();
                let inner_type = self.parse_type()?;
                self.consume(Token::CloseBracket)?;
                Ok(Type::TyList(Box::new(inner_type)))
            }
            Some((Token::Module, _)) => {
                // Handle 'module' keyword as type
                self.advance();
                // Check for module{...} type
                if self.check(&Token::OpenBrace) {
                    self.advance();
                    let mut modules = Vec::new();

                    // Parse module names
                    loop {
                        let mod_name = self.parse_module_name()?;
                        modules.push(mod_name);

                        if self.check(&Token::Comma) {
                            self.advance();
                        } else {
                            break;
                        }
                    }

                    self.consume(Token::CloseBrace)?;
                    Ok(Type::TyModRef(modules))
                } else {
                    Ok(Type::TyAny) // Just 'module' without braces
                }
            }
            Some((Token::Ident(s), _)) => {
                let s_clone = s.clone();
                self.advance();
                match s_clone.as_str() {
                    "integer" => Ok(Type::TyPrim(PrimType::PrimInt)),
                    "string" => Ok(Type::TyPrim(PrimType::PrimString)),
                    "bool" => Ok(Type::TyPrim(PrimType::PrimBool)),
                    "decimal" => Ok(Type::TyPrim(PrimType::PrimDecimal)),
                    "time" => Ok(Type::TyPrim(PrimType::PrimTime)),
                    "keyset" => Ok(Type::TyKeyset),
                    "guard" => Ok(Type::TyPrim(PrimType::PrimGuard)),
                    _ => {
                        // Check for object type: name{schema}
                        if self.check(&Token::OpenBrace) {
                            self.advance();
                            let schema_name = self.expect_symbol("schema name")?;
                            self.consume(Token::CloseBrace)?;
                            Ok(Type::TyObject(ParsedTyName::TBN(BareName(schema_name))))
                        } else {
                            // Default to object type
                            Ok(Type::TyObject(ParsedTyName::TBN(BareName(s_clone.into()))))
                        }
                    }
                }
            }
            _ => Err(parse_error(format!("Expected {}", "type".to_string()), self.current_span())),
        }
    }

    /// Parse module name (could be qualified)
    fn parse_module_name(&mut self) -> Result<ModuleName> {
        let name = self.parse_module_name_component()?;

        if self.check(&Token::Dot) {
            self.advance();
            let namespace = NamespaceName(name);
            let module_name = self.parse_module_name_component()?;
            Ok(ModuleName {
                name: module_name,
                namespace: Some(namespace),
            })
        } else {
            Ok(ModuleName {
                name,
                namespace: None,
            })
        }
    }

    /// Parse capability definition: (defcap NAME (args...) body...)
    fn parse_defcap(&mut self) -> Result<ParsedDefCap<SpanInfo>> {
        let name = MArg {
            name: self.expect_symbol("capability name")?,
            ty: None,
            info: SpanInfo::empty(),
        };

        // Parse arguments
        self.consume(Token::OpenParens)?;
        let mut args = Vec::new();
        while !self.check(&Token::CloseParens) && !self.is_at_end() {
            args.push(self.parse_arg()?);
        }
        self.consume(Token::CloseParens)?;

        // Parse annotations and metadata before body
        let mut annotations = Vec::new();
        let mut meta = None;

        loop {
            match self.current_token() {
                Some((Token::DocAnn | Token::ModelAnn, _)) => {
                    annotations.push(self.parse_annotation()?);
                }
                Some((Token::EventAnn, _)) => {
                    self.advance();
                    meta = Some(DCapMeta::DefEvent);
                }
                Some((Token::ManagedAnn, _)) => {
                    self.advance();
                    // Check for optional managed parameters
                    meta = Some(DCapMeta::DefManaged(None));
                }
                _ => break,
            }
        }

        // Parse body (multiple expressions)
        let mut body = Vec::new();
        while !self.check(&Token::CloseParens) && !self.is_at_end() {
            body.push(self.parse_expression()?);
        }

        if body.is_empty() {
            return Err(parse_error(format!("Expected {}", "capability body".to_string()), self.current_span()));
        }

        Ok(ParsedDefCap {
            name,
            args,
            body,
            annotations,
            meta,
            info: SpanInfo::empty(),
        })
    }

    /// Parse constant definition: (defconst NAME value)
    fn parse_defconst(&mut self) -> Result<ParsedDefConst<SpanInfo>> {
        let name = MArg {
            name: self.expect_symbol("constant name")?,
            ty: None,
            info: SpanInfo::empty(),
        };

        // Parse the value expression
        let value = self.parse_expression()?;

        Ok(ParsedDefConst {
            name,
            value,
            doc: None,
            info: SpanInfo::empty(),
        })
    }

    /// Parse schema definition: (defschema NAME field:type...)
    fn parse_defschema(&mut self) -> Result<DefSchema<SpanInfo>> {
        let name = self.expect_symbol("schema name")?;

        // Parse fields
        let mut fields = Vec::new();
        while !self.check(&Token::CloseParens) && !self.is_at_end() {
            let field_name = self.expect_symbol("field name")?;
            self.consume(Token::Colon)?;
            let field_type = self.parse_type()?;

            fields.push(Arg {
                name: field_name,
                ty: field_type,
                info: SpanInfo::empty(),
            });
        }

        Ok(DefSchema {
            name,
            fields,
            annotations: Vec::new(),
            info: SpanInfo::empty(),
        })
    }

    /// Parse table definition: (deftable NAME:{SCHEMA})
    fn parse_deftable(&mut self) -> Result<DefTable<SpanInfo>> {
        let name = self.expect_symbol("table name")?;

        // Parse schema reference: :{schema-name}
        self.consume(Token::Colon)?;
        self.consume(Token::OpenBrace)?;
        let schema_name = self.expect_symbol("schema name")?;
        self.consume(Token::CloseBrace)?;

        Ok(DefTable {
            name,
            schema: ParsedName::BN(BareName(schema_name)),
            doc: None,
            info: SpanInfo::empty(),
        })
    }

    /// Parse pact definition: (defpact NAME (args...) steps...)
    fn parse_defpact(&mut self) -> Result<DefPact<SpanInfo>> {
        let name = MArg {
            name: self.expect_symbol("pact name")?,
            ty: None,
            info: SpanInfo::empty(),
        };

        // Parse arguments
        self.consume(Token::OpenParens)?;
        let mut args = Vec::new();
        while !self.check(&Token::CloseParens) && !self.is_at_end() {
            args.push(self.parse_arg()?);
        }
        self.consume(Token::CloseParens)?;

        // Parse steps
        let mut steps = Vec::new();
        while !self.check(&Token::CloseParens) && !self.is_at_end() {
            steps.push(self.parse_step()?);
        }

        if steps.is_empty() {
            return Err(parse_error(format!("Expected {}", "pact steps".to_string()), self.current_span()));
        }

        Ok(DefPact {
            name,
            args,
            steps,
            annotations: Vec::new(),
            info: SpanInfo::empty(),
        })
    }

    /// Parse pact step: (step expr [MModel]) or (step entity expr [MModel])
    /// Also: (step-with-rollback expr rollback [MModel]) or (step-with-rollback entity expr rollback [MModel])
    fn parse_step(&mut self) -> Result<PactStep<SpanInfo>> {
        self.consume(Token::OpenParens)?;

        let step_type = match self.current_token() {
            Some((Token::Step, _)) => {
                self.advance();
                
                // Parse the first expression
                let first_expr = self.parse_expression()?;
                
                // Check if there's a second expression
                if !self.check(&Token::CloseParens) && !self.is_at_end() {
                    // If there's a second expression, then first is entity, second is step expr
                    let step_expr = self.parse_expression()?;
                    
                    // Skip optional model for now (we'll implement this later)
                    self.consume(Token::CloseParens)?;
                    
                    Ok(PactStep::Step {
                        entity: Some(first_expr),
                        expr: step_expr,
                        model: None,
                    })
                } else {
                    // Only one expression, so it's the step expression (no entity)
                    self.consume(Token::CloseParens)?;
                    
                    Ok(PactStep::Step {
                        entity: None,
                        expr: first_expr,
                        model: None,
                    })
                }
            }
            Some((Token::StepWithRollback, _)) => {
                self.advance();
                
                // Parse the first expression
                let first_expr = self.parse_expression()?;
                
                // Parse the second expression (rollback)
                let second_expr = self.parse_expression()?;
                
                // Check if there's a third expression
                if !self.check(&Token::CloseParens) && !self.is_at_end() {
                    // Three expressions: entity, step expr, rollback expr
                    let third_expr = self.parse_expression()?;
                    
                    // Skip optional model for now
                    self.consume(Token::CloseParens)?;
                    
                    Ok(PactStep::StepWithRollback {
                        entity: Some(first_expr),
                        expr: second_expr,
                        rollback: third_expr,
                        model: None,
                    })
                } else {
                    // Two expressions: step expr, rollback expr (no entity)
                    self.consume(Token::CloseParens)?;
                    
                    Ok(PactStep::StepWithRollback {
                        entity: None,
                        expr: first_expr,
                        rollback: second_expr,
                        model: None,
                    })
                }
            }
            _ => {
                let span = self.current_span();
                Err(parse_error(
                    "step or step-with-rollback".to_string(),
                    span,
                ))
            }
        };

        step_type
    }

    // Utility methods
    fn current_token(&self) -> Option<&(Token, SpanInfo)> {
        self.tokens.get(self.position)
    }

    fn advance(&mut self) {
        if self.position < self.tokens.len() {
            self.position += 1;
        }
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.tokens.len()
    }

    fn check(&self, expected: &Token) -> bool {
        match self.current_token() {
            Some((token, _)) => std::mem::discriminant(token) == std::mem::discriminant(expected),
            None => false,
        }
    }

    fn consume(&mut self, expected: Token) -> Result<()> {
        if self.check(&expected) {
            self.advance();
            Ok(())
        } else {
            let span = self.current_span();
            let found = self
                .current_token()
                .map(|(t, _)| format!("{:?}", t))
                .unwrap_or_else(|| "end of input".to_string());
            Err(unexpected_token(
                format!("{:?}", expected),
                found,
                span
            ))
        }
    }

    /// Parse annotation: @doc, @model, @event, @managed
    fn parse_annotation(&mut self) -> Result<PactAnn<SpanInfo>> {
        match self.current_token() {
            Some((Token::DocAnn, _)) => {
                self.advance();
                match self.current_token() {
                    Some((Token::String(doc), _)) => {
                        let doc_string = doc.clone().into();
                        self.advance();
                        Ok(PactAnn::PactDoc {
                            ty: PactDocType::PactDocAnn,
                            doc: doc_string,
                        })
                    }
                    _ => Err(parse_error(format!("Expected {}", "doc string".to_string()), self.current_span())),
                }
            }
            Some((Token::ModelAnn, _)) => {
                self.advance();
                // For now, skip model annotations
                Ok(PactAnn::PactModel(Vec::new()))
            }
            _ => Err(parse_error(format!("Expected {}", "annotation".to_string()), self.current_span())),
        }
    }

    fn expect_symbol(&mut self, context: &str) -> Result<CompactString> {
        match self.current_token() {
            Some((Token::Ident(s), _)) => {
                let result = s.clone().into();
                self.advance();
                Ok(result)
            }
            _ => Err(parse_error(format!("Expected {}", context.to_string()), self.current_span())),
        }
    }

    fn parse_module_name_component(&mut self) -> Result<CompactString> {
        match self.current_token() {
            Some((Token::Ident(s), _)) => {
                let result = s.clone().into();
                self.advance();
                Ok(result)
            }
            // Allow keywords as module name components
            Some((Token::Module, _)) => {
                self.advance();
                Ok("module".into())
            }
            Some((Token::Interface, _)) => {
                self.advance();
                Ok("interface".into())
            }
            Some((Token::Import, _)) => {
                self.advance();
                Ok("use".into())
            }
            _ => Err(parse_error(format!("Expected {}", "module name".to_string()), self.current_span())),
        }
    }

    fn skip_to_closing_paren(&mut self) {
        let mut depth = 1;
        while depth > 0 && !self.is_at_end() {
            match self.current_token() {
                Some((Token::OpenParens, _)) => depth += 1,
                Some((Token::CloseParens, _)) => depth -= 1,
                _ => {}
            }
            self.advance();
        }
    }

    /// Check if we're looking at a binding pattern {"field" := var}
    fn is_binding_pattern(&self) -> bool {
        // Look ahead to see if we have := after field name
        let mut pos = self.position;
        let mut found_bind_assign = false;

        while pos < self.tokens.len() {
            match self.tokens.get(pos) {
                Some((Token::BindAssign, _)) => {
                    found_bind_assign = true;
                    break;
                }
                Some((Token::CloseBrace, _)) => break,
                Some((Token::Colon, _)) => break, // Regular object
                _ => pos += 1,
            }
        }

        found_bind_assign
    }

    /// Parse binding pattern: {"field" := var, ...}
    fn parse_binding_pattern(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        let mut bindings = Vec::new();

        while !self.check(&Token::CloseBrace) && !self.is_at_end() {
            // Parse field name
            let field = match self.current_token() {
                Some((Token::String(s), _)) => {
                    let field = Field(s.clone().into());
                    self.advance();
                    field
                }
                Some((Token::SingleTick(s), _)) => {
                    let field_name = s.trim_start_matches('\'');
                    let field = Field(field_name.into());
                    self.advance();
                    field
                }
                _ => return Err(parse_error(format!("Expected {}", "field name".to_string()), self.current_span())),
            };

            // Consume :=
            self.consume(Token::BindAssign)?;

            // Parse binding variable
            let var_name = self.expect_symbol("binding variable")?;

            bindings.push((
                field,
                MArg {
                    name: var_name,
                    ty: None,
                    info: SpanInfo::empty(),
                },
            ));

            // Optional comma
            if self.check(&Token::Comma) {
                self.advance();
            }
        }

        self.consume(Token::CloseBrace)?;

        // This is a binding pattern - for now parse it as a special form
        // In reality, this should only appear as part of a bind expression
        Ok(ParsedExpr::Binding {
            bindings,
            body: vec![], // Empty body for standalone pattern
            info: SpanInfo::empty(),
        })
    }

    /// Parse defconst as expression: defconst NAME value
    fn parse_defconst_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        let defconst = self.parse_defconst()?;
        
        // Convert the defconst to a DefConst expression
        Ok(ParsedExpr::DefConst(Box::new(defconst)))
    }

    /// Parse defun as expression: defun NAME (args...) body...
    fn parse_defun_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        let defun = self.parse_defun()?;
        
        // Convert the defun to a DefFun expression
        Ok(ParsedExpr::DefFun(Box::new(defun)))
    }

    /// Parse defcap as expression: defcap NAME (args...) body...
    fn parse_defcap_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        let defcap = self.parse_defcap()?;
        
        // Convert the defcap to a DefCap expression
        Ok(ParsedExpr::DefCap(Box::new(defcap)))
    }

    /// Parse defschema as expression: defschema NAME field:type...
    fn parse_defschema_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        let defschema = self.parse_defschema()?;
        
        // Convert the defschema to a DefSchema expression
        Ok(ParsedExpr::DefSchema(Box::new(defschema)))
    }

    /// Parse deftable as expression: deftable NAME:{SCHEMA}
    fn parse_deftable_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        let deftable = self.parse_deftable()?;
        
        // Convert the deftable to a DefTable expression
        Ok(ParsedExpr::DefTable(Box::new(deftable)))
    }

    /// Parse defpact as expression: defpact NAME (args...) steps...
    fn parse_defpact_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
        let defpact = self.parse_defpact()?;
        
        // Convert the defpact to a DefPact expression  
        Ok(ParsedExpr::DefPact(Box::new(defpact)))
    }

    /// Get comprehensive performance statistics
    pub fn performance_stats(&self) -> ParserStats {
        ParserStats {
            arena_stats: self.arena.stats(),
            lexer_stats: self.lexer_stats.clone(),
            tokens_parsed: self.position,
            total_tokens: self.tokens.len(),
            parse_errors: 0, // errors are now thrown immediately
        }
    }

    /// Alias for performance_stats for consistency with lexer API
    pub fn memory_stats(&self) -> ParserStats {
        self.performance_stats()
    }
}

/// Comprehensive parser performance statistics
#[derive(Debug, Clone)]
pub struct ParserStats {
    pub arena_stats: ArenaStats,
    pub lexer_stats: Option<MemoryUsage>,
    pub tokens_parsed: usize,
    pub total_tokens: usize,
    pub parse_errors: usize,
}

impl std::fmt::Display for ParserStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Parser Performance Statistics:")?;
        writeln!(f, "Tokens: {}/{}", self.tokens_parsed, self.total_tokens)?;
        writeln!(f, "Parse errors: {}", self.parse_errors)?;
        writeln!(f)?;
        write!(f, "{}", self.arena_stats)?;

        if let Some(ref lexer_stats) = self.lexer_stats {
            writeln!(f)?;
            write!(f, "{}", lexer_stats)?;
        }

        Ok(())
    }
}

/// Convenience function for parsing
pub fn parse(source: &str) -> Result<Program<SpanInfo>> {
    let mut parser = Parser::new(source)?;
    parser.parse_program()
}

/// Parse and return both result and performance statistics
pub fn parse_with_stats(source: &str) -> (Result<Program<SpanInfo>>, ParserStats) {
    match Parser::new(source) {
        Ok(mut parser) => {
            let result = parser.parse_program();
            let stats = parser.performance_stats();
            (result, stats)
        }
        Err(e) => (
            Err(e),
            ParserStats {
                arena_stats: ArenaStats {
                    expressions: 0,
                    arg_lists: 0,
                    binding_lists: 0,
                    types: 0,
                    interned_strings: 0,
                    current_memory: 0,
                    peak_memory: 0,
                    total_allocations: 0,
                },
                lexer_stats: None,
                tokens_parsed: 0,
                total_tokens: 0,
                parse_errors: 1,
            },
        ),
    }
}

/// Parse a single expression from source code
pub fn parse_expression(source: &str) -> Result<ParsedExpr<SpanInfo>> {
    let mut parser = Parser::new(source)?;
    let expr = parser.parse_expression()?;
    
    // Ensure we've consumed all tokens
    if !parser.is_at_end() {
        return Err(parse_error("Expected end of input".to_string(), parser.current_span()));
    }
    
    Ok(expr)
}
