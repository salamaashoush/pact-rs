//! LSP backend implementation

use crate::capabilities;
use crate::code_actions::CodeActionProvider;
use crate::completions::CompletionProvider;
use crate::diagnostics::DiagnosticsAnalyzer;
use crate::document::DocumentStore;
use crate::formatting::PactFormatter;
use crate::hover::HoverProvider;
use crate::inlay_hints::InlayHintProvider;
use crate::live_validation::LiveValidationProvider;
use crate::semantic::SemanticAnalyzer;
use crate::semantic_highlighting::SemanticTokenProvider;
use crate::symbols;
use crate::workspace_symbols::WorkspaceSymbolProvider;
use dashmap::DashMap;
use tower_lsp::lsp_types::*;
use std::sync::Arc;
use tower_lsp::jsonrpc::Result as LspResult;
use tower_lsp::{Client, LanguageServer};

/// The Pact language server backend
pub struct PactLanguageBackend {
    /// LSP client
    pub client: Client,
    
    /// Document store
    pub documents: Arc<DocumentStore>,
    
    /// Diagnostics cache
    pub diagnostics: Arc<DashMap<Url, Vec<Diagnostic>>>,
    
    /// Semantic analyzer for advanced features
    pub semantic_analyzer: Arc<SemanticAnalyzer>,
    
    /// Diagnostics analyzer
    pub diagnostics_analyzer: DiagnosticsAnalyzer,
    
    /// Completion provider
    pub completion_provider: CompletionProvider,
    
    /// Hover provider
    pub hover_provider: HoverProvider,
    
    /// Code formatter
    pub formatter: PactFormatter,
    
    /// Code actions provider
    pub code_action_provider: CodeActionProvider,
    
    /// Semantic token provider
    pub semantic_token_provider: SemanticTokenProvider,
    
    /// Inlay hint provider
    pub inlay_hint_provider: InlayHintProvider,
    
    /// Workspace symbol provider
    pub workspace_symbol_provider: Arc<std::sync::Mutex<WorkspaceSymbolProvider>>,
    
    /// Live validation provider
    pub live_validation_provider: LiveValidationProvider,
}

impl PactLanguageBackend {
    /// Create a new backend
    pub fn new(client: Client) -> Self {
        let semantic_analyzer = Arc::new(SemanticAnalyzer::new());
        let diagnostics_analyzer = DiagnosticsAnalyzer::new();
        let completion_provider = CompletionProvider::new(semantic_analyzer.clone());
        let hover_provider = HoverProvider::new(semantic_analyzer.clone());
        let formatter = PactFormatter::new();
        let code_action_provider = CodeActionProvider::new(semantic_analyzer.clone());
        let semantic_token_provider = SemanticTokenProvider::new(semantic_analyzer.clone());
        let inlay_hint_provider = InlayHintProvider::new(semantic_analyzer.clone());
        let workspace_symbol_provider = Arc::new(std::sync::Mutex::new(WorkspaceSymbolProvider::new(semantic_analyzer.clone())));
        let live_validation_provider = LiveValidationProvider::new(semantic_analyzer.clone());
        
        Self {
            client,
            documents: Arc::new(DocumentStore::new()),
            diagnostics: Arc::new(DashMap::new()),
            semantic_analyzer,
            diagnostics_analyzer,
            completion_provider,
            hover_provider,
            formatter,
            code_action_provider,
            semantic_token_provider,
            inlay_hint_provider,
            workspace_symbol_provider,
            live_validation_provider,
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for PactLanguageBackend {
    async fn initialize(&self, _: InitializeParams) -> LspResult<InitializeResult> {
        Ok(InitializeResult {
            capabilities: capabilities::server_capabilities(),
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Pact language server initialized!")
            .await;
    }

    async fn shutdown(&self) -> LspResult<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let text = params.text_document.text.clone();
        let version = params.text_document.version;
        
        // Store document
        self.documents.open_document(uri.clone(), version, text.clone());
        
        // Index file for workspace symbols
        if let Ok(mut workspace_provider) = self.workspace_symbol_provider.lock() {
            let _ = workspace_provider.index_file(uri.clone(), text.clone());
        }
        
        // Run enhanced diagnostics
        let diagnostics = if let Ok(enhanced_diagnostics) = self.live_validation_provider.validate_document(&uri, &text) {
            enhanced_diagnostics.into_iter().map(|ed| ed.diagnostic).collect()
        } else {
            self.diagnostics_analyzer.analyze(&uri, &text)
        };
        
        self.diagnostics.insert(uri.clone(), diagnostics.clone());
        
        // Publish diagnostics
        self.client
            .publish_diagnostics(uri, diagnostics, Some(version))
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let version = params.text_document.version;
        
        // Update document
        if let Some(change) = params.content_changes.into_iter().last() {
            self.documents.update_document(uri.clone(), version, change.text.clone());
            
            // Re-run diagnostics
            let diagnostics = self.diagnostics_analyzer.analyze(&uri, &change.text);
            self.diagnostics.insert(uri.clone(), diagnostics.clone());
            
            // Publish diagnostics
            self.client
                .publish_diagnostics(uri, diagnostics, Some(version))
                .await;
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.client
            .log_message(
                MessageType::INFO,
                format!("Saved: {}", params.text_document.uri),
            )
            .await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        
        // Remove document and diagnostics
        self.documents.close_document(&uri);
        self.diagnostics.remove(&uri);
        
        // Clear diagnostics
        self.client.publish_diagnostics(uri, vec![], None).await;
    }

    async fn completion(&self, params: CompletionParams) -> LspResult<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        
        if let Some(document) = self.documents.get_document(uri) {
            let completions = self.completion_provider.get_completions(uri, &document.text, position);
            Ok(Some(CompletionResponse::Array(completions)))
        } else {
            Ok(None)
        }
    }

    async fn hover(&self, params: HoverParams) -> LspResult<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        
        if let Some(document) = self.documents.get_document(uri) {
            Ok(self.hover_provider.get_hover(uri, &document.text, position))
        } else {
            Ok(None)
        }
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> LspResult<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        
        if let Some(document) = self.documents.get_document(uri) {
            if let Some(symbol) = self.semantic_analyzer.get_symbol_at_position(uri, position) {
                if let (Some(def_uri), Some(def_range)) = (&symbol.definition_uri, &symbol.definition_range) {
                    let location = Location {
                        uri: def_uri.clone(),
                        range: *def_range,
                    };
                    return Ok(Some(GotoDefinitionResponse::Scalar(location)));
                }
            }
            
            // Fallback: try to find definition by identifier
            if let Some(identifier) = self.extract_identifier_at_position(&document.text, position) {
                // Search for definition in current document
                if let Some(def_location) = self.find_definition_in_document(&document.text, &identifier, uri) {
                    return Ok(Some(GotoDefinitionResponse::Scalar(def_location)));
                }
                
                // TODO: Search in other documents in workspace
            }
        }
        
        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> LspResult<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let include_declaration = params.context.include_declaration;
        
        if let Some(document) = self.documents.get_document(uri) {
            if let Some(symbol) = self.semantic_analyzer.get_symbol_at_position(uri, position) {
                let mut references = self.semantic_analyzer.find_references(&symbol.name);
                
                // If not including declaration, filter it out
                if !include_declaration {
                    if let (Some(def_uri), Some(def_range)) = (&symbol.definition_uri, &symbol.definition_range) {
                        references.retain(|loc| !(loc.uri == *def_uri && loc.range == *def_range));
                    }
                }
                
                return Ok(Some(references));
            }
            
            // Fallback: search by identifier
            if let Some(identifier) = self.extract_identifier_at_position(&document.text, position) {
                let references = self.find_references_in_document(&document.text, &identifier, uri);
                return Ok(Some(references));
            }
        }
        
        Ok(None)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> LspResult<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;
        
        if let Some(document) = self.documents.get_document(uri) {
            let symbols = symbols::get_document_symbols(&document.text);
            Ok(Some(DocumentSymbolResponse::Flat(symbols)))
        } else {
            Ok(None)
        }
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> LspResult<Option<Vec<TextEdit>>> {
        let uri = &params.text_document.uri;
        
        if let Some(document) = self.documents.get_document(uri) {
            match self.formatter.format_document(&document.text) {
                Ok(edits) => Ok(Some(edits)),
                Err(_) => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    async fn range_formatting(&self, params: DocumentRangeFormattingParams) -> LspResult<Option<Vec<TextEdit>>> {
        let uri = &params.text_document.uri;
        let range = params.range;
        
        if let Some(document) = self.documents.get_document(uri) {
            match self.formatter.format_range(&document.text, range) {
                Ok(edits) => Ok(Some(edits)),
                Err(_) => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> LspResult<Option<SignatureHelp>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        
        if let Some(document) = self.documents.get_document(uri) {
            if let Some(signature_help) = self.get_signature_help(&document.text, position) {
                return Ok(Some(signature_help));
            }
        }
        
        Ok(None)
    }

    async fn code_action(&self, params: CodeActionParams) -> LspResult<Option<Vec<CodeActionOrCommand>>> {
        let uri = &params.text_document.uri;
        let range = params.range;
        let context = &params.context;
        
        if let Some(document) = self.documents.get_document(uri) {
            let actions = self.code_action_provider.get_code_actions(uri, &document.text, range, context);
            if actions.is_empty() {
                Ok(None)
            } else {
                Ok(Some(actions))
            }
        } else {
            Ok(None)
        }
    }

    async fn semantic_tokens_full(&self, params: SemanticTokensParams) -> LspResult<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;
        
        if let Some(document) = self.documents.get_document(uri) {
            match self.semantic_token_provider.get_semantic_tokens(uri, &document.text) {
                Ok(tokens) => {
                    // Convert to LSP format
                    let lsp_tokens = crate::semantic_highlighting::tokens_to_lsp_format(&tokens);
                    
                    // Convert to LSP SemanticToken format with relative encoding
                    let mut data = Vec::new();
                    let mut prev_line = 0u32;
                    let mut prev_char = 0u32;
                    
                    for token in lsp_tokens {
                        let delta_line = token.line - prev_line;
                        let delta_char = if delta_line == 0 {
                            token.start - prev_char
                        } else {
                            token.start
                        };
                        
                        data.push(SemanticToken {
                            delta_line,
                            delta_start: delta_char,
                            length: token.length,
                            token_type: token.token_type.to_lsp_type(),
                            token_modifiers_bitset: 0, // no modifiers for now
                        });
                        
                        prev_line = token.line;
                        prev_char = token.start;
                    }
                    
                    Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                        result_id: None,
                        data,
                    })))
                },
                Err(_) => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> LspResult<Option<Vec<InlayHint>>> {
        let uri = &params.text_document.uri;
        let range = params.range;
        
        if let Some(document) = self.documents.get_document(uri) {
            match self.inlay_hint_provider.get_inlay_hints(uri, &document.text, range) {
                Ok(hints) => {
                    if hints.is_empty() {
                        Ok(None)
                    } else {
                        Ok(Some(hints))
                    }
                },
                Err(_) => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    async fn symbol(&self, params: WorkspaceSymbolParams) -> LspResult<Option<Vec<SymbolInformation>>> {
        let query = &params.query;
        
        // Search for symbols across the workspace
        if let Ok(workspace_provider) = self.workspace_symbol_provider.lock() {
            let symbols = workspace_provider.search_symbols(query);
            if symbols.is_empty() {
                Ok(None)
            } else {
                // Convert WorkspaceSymbol to SymbolInformation
                let symbol_infos: Vec<SymbolInformation> = symbols.into_iter().map(|ws| {
                    let location = match ws.location {
                        OneOf::Left(loc) => loc,
                        OneOf::Right(_) => Location {
                            uri: tower_lsp::lsp_types::Url::parse("file:///unknown").unwrap(),
                            range: Range::default(),
                        }
                    };
                    
                    SymbolInformation {
                        name: ws.name,
                        kind: ws.kind,
                        tags: ws.tags,
                        deprecated: None,
                        location,
                        container_name: ws.container_name,
                    }
                }).collect();
                Ok(Some(symbol_infos))
            }
        } else {
            Ok(None)
        }
    }
}

impl PactLanguageBackend {
    /// Extract identifier at position (helper method)
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
            .rfind(|c: char| !c.is_alphanumeric() && c != '-' && c != '_' && c != ':')
            .map(|i| i + 1)
            .unwrap_or(0);
            
        let end = line[char_pos..]
            .find(|c: char| !c.is_alphanumeric() && c != '-' && c != '_' && c != ':')
            .map(|i| char_pos + i)
            .unwrap_or(line.len());
        
        if start < end {
            Some(line[start..end].to_string())
        } else {
            None
        }
    }
    
    /// Find definition of identifier in document
    fn find_definition_in_document(&self, text: &str, identifier: &str, uri: &Url) -> Option<Location> {
        let lines: Vec<&str> = text.lines().collect();
        
        for (line_idx, line) in lines.iter().enumerate() {
            // Look for function definitions
            if line.contains("defun") && line.contains(identifier) {
                if let Some(def_start) = line.find(identifier) {
                    return Some(Location {
                        uri: uri.clone(),
                        range: Range {
                            start: Position {
                                line: line_idx as u32,
                                character: def_start as u32,
                            },
                            end: Position {
                                line: line_idx as u32,
                                character: (def_start + identifier.len()) as u32,
                            },
                        },
                    });
                }
            }
            
            // Look for capability definitions
            if line.contains("defcap") && line.contains(identifier) {
                if let Some(def_start) = line.find(identifier) {
                    return Some(Location {
                        uri: uri.clone(),
                        range: Range {
                            start: Position {
                                line: line_idx as u32,
                                character: def_start as u32,
                            },
                            end: Position {
                                line: line_idx as u32,
                                character: (def_start + identifier.len()) as u32,
                            },
                        },
                    });
                }
            }
            
            // Look for constant definitions
            if line.contains("defconst") && line.contains(identifier) {
                if let Some(def_start) = line.find(identifier) {
                    return Some(Location {
                        uri: uri.clone(),
                        range: Range {
                            start: Position {
                                line: line_idx as u32,
                                character: def_start as u32,
                            },
                            end: Position {
                                line: line_idx as u32,
                                character: (def_start + identifier.len()) as u32,
                            },
                        },
                    });
                }
            }
            
            // Look for let bindings
            if line.contains("let") && line.contains(identifier) {
                // Simple heuristic: look for (identifier value) pattern
                if let Some(let_pos) = line.find("let") {
                    let after_let = &line[let_pos..];
                    if let Some(binding_start) = after_let.find(&format!("({}", identifier)) {
                        let abs_start = let_pos + binding_start + 1;
                        return Some(Location {
                            uri: uri.clone(),
                            range: Range {
                                start: Position {
                                    line: line_idx as u32,
                                    character: abs_start as u32,
                                },
                                end: Position {
                                    line: line_idx as u32,
                                    character: (abs_start + identifier.len()) as u32,
                                },
                            },
                        });
                    }
                }
            }
        }
        
        None
    }
    
    /// Find all references to identifier in document
    fn find_references_in_document(&self, text: &str, identifier: &str, uri: &Url) -> Vec<Location> {
        let mut references = Vec::new();
        let lines: Vec<&str> = text.lines().collect();
        
        for (line_idx, line) in lines.iter().enumerate() {
            let mut start_pos = 0;
            
            // Find all occurrences of the identifier in this line
            while let Some(pos) = line[start_pos..].find(identifier) {
                let abs_pos = start_pos + pos;
                
                // Check if this is a whole word (not part of another identifier)
                let is_start_boundary = abs_pos == 0 || 
                    !line.chars().nth(abs_pos - 1).unwrap_or(' ').is_alphanumeric();
                let is_end_boundary = abs_pos + identifier.len() >= line.len() || 
                    !line.chars().nth(abs_pos + identifier.len()).unwrap_or(' ').is_alphanumeric();
                
                if is_start_boundary && is_end_boundary {
                    references.push(Location {
                        uri: uri.clone(),
                        range: Range {
                            start: Position {
                                line: line_idx as u32,
                                character: abs_pos as u32,
                            },
                            end: Position {
                                line: line_idx as u32,
                                character: (abs_pos + identifier.len()) as u32,
                            },
                        },
                    });
                }
                
                start_pos = abs_pos + 1;
            }
        }
        
        references
    }
    
    /// Get signature help for function calls
    fn get_signature_help(&self, text: &str, position: Position) -> Option<SignatureHelp> {
        let lines: Vec<&str> = text.lines().collect();
        if position.line as usize >= lines.len() {
            return None;
        }
        
        let line = lines[position.line as usize];
        let char_pos = position.character as usize;
        
        // Find the current function call context
        let prefix = if char_pos <= line.len() {
            &line[..char_pos]
        } else {
            line
        };
        
        // Find the opening parenthesis of the current function call
        let mut paren_depth = 0;
        let mut current_call_start = None;
        
        for (i, ch) in prefix.char_indices().rev() {
            match ch {
                ')' => paren_depth += 1,
                '(' => {
                    if paren_depth == 0 {
                        current_call_start = Some(i + 1);
                        break;
                    } else {
                        paren_depth -= 1;
                    }
                },
                _ => {}
            }
        }
        
        if let Some(call_start) = current_call_start {
            // Extract function name
            let after_paren = &prefix[call_start..];
            let func_name = after_paren.split_whitespace().next()?;
            
            // Get signature information for the function
            if let Some(signature_info) = self.get_function_signature(func_name) {
                // Calculate which parameter we're currently on
                let param_text = &prefix[call_start..];
                let active_parameter = param_text.matches(' ').count().saturating_sub(1);
                
                return Some(SignatureHelp {
                    signatures: vec![signature_info],
                    active_signature: Some(0),
                    active_parameter: Some(active_parameter as u32),
                });
            }
        }
        
        None
    }
    
    /// Get signature information for a function
    fn get_function_signature(&self, func_name: &str) -> Option<SignatureInformation> {
        match func_name {
            "+" => Some(SignatureInformation {
                label: "(+ x y)".to_string(),
                documentation: Some(Documentation::String("Addition operator. Returns the sum of two numbers.".to_string())),
                parameters: Some(vec![
                    ParameterInformation {
                        label: ParameterLabel::Simple("x".to_string()),
                        documentation: Some(Documentation::String("First number".to_string())),
                    },
                    ParameterInformation {
                        label: ParameterLabel::Simple("y".to_string()),
                        documentation: Some(Documentation::String("Second number".to_string())),
                    },
                ]),
                active_parameter: None,
            }),
            "-" => Some(SignatureInformation {
                label: "(- x y)".to_string(),
                documentation: Some(Documentation::String("Subtraction operator. Returns the difference of two numbers.".to_string())),
                parameters: Some(vec![
                    ParameterInformation {
                        label: ParameterLabel::Simple("x".to_string()),
                        documentation: Some(Documentation::String("Minuend".to_string())),
                    },
                    ParameterInformation {
                        label: ParameterLabel::Simple("y".to_string()),
                        documentation: Some(Documentation::String("Subtrahend".to_string())),
                    },
                ]),
                active_parameter: None,
            }),
            "if" => Some(SignatureInformation {
                label: "(if condition then-expr else-expr)".to_string(),
                documentation: Some(Documentation::String("Conditional expression. Evaluates condition and returns then-expr if true, else-expr if false.".to_string())),
                parameters: Some(vec![
                    ParameterInformation {
                        label: ParameterLabel::Simple("condition".to_string()),
                        documentation: Some(Documentation::String("Boolean expression to evaluate".to_string())),
                    },
                    ParameterInformation {
                        label: ParameterLabel::Simple("then-expr".to_string()),
                        documentation: Some(Documentation::String("Expression to evaluate if condition is true".to_string())),
                    },
                    ParameterInformation {
                        label: ParameterLabel::Simple("else-expr".to_string()),
                        documentation: Some(Documentation::String("Expression to evaluate if condition is false".to_string())),
                    },
                ]),
                active_parameter: None,
            }),
            "let" => Some(SignatureInformation {
                label: "(let ((var1 val1) (var2 val2) ...) body)".to_string(),
                documentation: Some(Documentation::String("Create local variable bindings for use in body expression.".to_string())),
                parameters: Some(vec![
                    ParameterInformation {
                        label: ParameterLabel::Simple("bindings".to_string()),
                        documentation: Some(Documentation::String("List of (variable value) pairs".to_string())),
                    },
                    ParameterInformation {
                        label: ParameterLabel::Simple("body".to_string()),
                        documentation: Some(Documentation::String("Expression where variables are available".to_string())),
                    },
                ]),
                active_parameter: None,
            }),
            "defun" => Some(SignatureInformation {
                label: "(defun name (params...) body)".to_string(),
                documentation: Some(Documentation::String("Define a function with name, parameters, and body.".to_string())),
                parameters: Some(vec![
                    ParameterInformation {
                        label: ParameterLabel::Simple("name".to_string()),
                        documentation: Some(Documentation::String("Function name".to_string())),
                    },
                    ParameterInformation {
                        label: ParameterLabel::Simple("params".to_string()),
                        documentation: Some(Documentation::String("Parameter list".to_string())),
                    },
                    ParameterInformation {
                        label: ParameterLabel::Simple("body".to_string()),
                        documentation: Some(Documentation::String("Function body expression".to_string())),
                    },
                ]),
                active_parameter: None,
            }),
            "with-capability" => Some(SignatureInformation {
                label: "(with-capability (CAPABILITY args...) body)".to_string(),
                documentation: Some(Documentation::String("Acquire capability and execute body with it in scope.".to_string())),
                parameters: Some(vec![
                    ParameterInformation {
                        label: ParameterLabel::Simple("capability".to_string()),
                        documentation: Some(Documentation::String("Capability to acquire with arguments".to_string())),
                    },
                    ParameterInformation {
                        label: ParameterLabel::Simple("body".to_string()),
                        documentation: Some(Documentation::String("Expression to execute with capability".to_string())),
                    },
                ]),
                active_parameter: None,
            }),
            "read" => Some(SignatureInformation {
                label: "(read table key)".to_string(),
                documentation: Some(Documentation::String("Read a row from a database table by key.".to_string())),
                parameters: Some(vec![
                    ParameterInformation {
                        label: ParameterLabel::Simple("table".to_string()),
                        documentation: Some(Documentation::String("Table to read from".to_string())),
                    },
                    ParameterInformation {
                        label: ParameterLabel::Simple("key".to_string()),
                        documentation: Some(Documentation::String("Row key to read".to_string())),
                    },
                ]),
                active_parameter: None,
            }),
            "write" => Some(SignatureInformation {
                label: "(write table key object)".to_string(),
                documentation: Some(Documentation::String("Write a row to a database table.".to_string())),
                parameters: Some(vec![
                    ParameterInformation {
                        label: ParameterLabel::Simple("table".to_string()),
                        documentation: Some(Documentation::String("Table to write to".to_string())),
                    },
                    ParameterInformation {
                        label: ParameterLabel::Simple("key".to_string()),
                        documentation: Some(Documentation::String("Row key".to_string())),
                    },
                    ParameterInformation {
                        label: ParameterLabel::Simple("object".to_string()),
                        documentation: Some(Documentation::String("Data object to write".to_string())),
                    },
                ]),
                active_parameter: None,
            }),
            "map" => Some(SignatureInformation {
                label: "(map function list)".to_string(),
                documentation: Some(Documentation::String("Apply function to each element of a list.".to_string())),
                parameters: Some(vec![
                    ParameterInformation {
                        label: ParameterLabel::Simple("function".to_string()),
                        documentation: Some(Documentation::String("Function to apply to each element".to_string())),
                    },
                    ParameterInformation {
                        label: ParameterLabel::Simple("list".to_string()),
                        documentation: Some(Documentation::String("List to process".to_string())),
                    },
                ]),
                active_parameter: None,
            }),
            "filter" => Some(SignatureInformation {
                label: "(filter predicate list)".to_string(),
                documentation: Some(Documentation::String("Filter list by predicate function.".to_string())),
                parameters: Some(vec![
                    ParameterInformation {
                        label: ParameterLabel::Simple("predicate".to_string()),
                        documentation: Some(Documentation::String("Boolean function to test each element".to_string())),
                    },
                    ParameterInformation {
                        label: ParameterLabel::Simple("list".to_string()),
                        documentation: Some(Documentation::String("List to filter".to_string())),
                    },
                ]),
                active_parameter: None,
            }),
            "fold" => Some(SignatureInformation {
                label: "(fold function initial list)".to_string(),
                documentation: Some(Documentation::String("Reduce list to single value using function.".to_string())),
                parameters: Some(vec![
                    ParameterInformation {
                        label: ParameterLabel::Simple("function".to_string()),
                        documentation: Some(Documentation::String("Reduction function taking (accumulator element)".to_string())),
                    },
                    ParameterInformation {
                        label: ParameterLabel::Simple("initial".to_string()),
                        documentation: Some(Documentation::String("Initial accumulator value".to_string())),
                    },
                    ParameterInformation {
                        label: ParameterLabel::Simple("list".to_string()),
                        documentation: Some(Documentation::String("List to reduce".to_string())),
                    },
                ]),
                active_parameter: None,
            }),
            _ => None,
        }
    }
}