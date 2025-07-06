//! Document symbols implementation

use tower_lsp::lsp_types::*;
use pact_syntax::{lex, Token, Span};

/// Get document symbols
pub fn get_document_symbols(text: &str) -> Vec<SymbolInformation> {
    let mut symbols = Vec::new();
    let tokens = match lex(text) {
        Ok(tokens) => tokens,
        Err(_) => return symbols,
    };
    
    let mut i = 0;
    while i < tokens.len() {
        let (token, span) = &tokens[i];
        
        // Look for definition patterns: (defun name ...)
        if matches!(token, Token::OpenParens) {
            if let Some((next_token, _)) = tokens.get(i + 1) {
                match next_token {
                    Token::Defun => {
                        if let Some((name_token, name_span)) = tokens.get(i + 2) {
                            if let Token::Ident(name) = name_token {
                                add_symbol(&mut symbols, name, SymbolKind::FUNCTION, name_span, text);
                            }
                        }
                    }
                    Token::Defcap => {
                        if let Some((name_token, name_span)) = tokens.get(i + 2) {
                            if let Token::Ident(name) = name_token {
                                add_symbol(&mut symbols, name, SymbolKind::FUNCTION, name_span, text);
                            }
                        }
                    }
                    Token::Module => {
                        if let Some((name_token, name_span)) = tokens.get(i + 2) {
                            if let Token::Ident(name) = name_token {
                                add_symbol(&mut symbols, name, SymbolKind::MODULE, name_span, text);
                            }
                        }
                    }
                    Token::Defconst => {
                        if let Some((name_token, name_span)) = tokens.get(i + 2) {
                            if let Token::Ident(name) = name_token {
                                add_symbol(&mut symbols, name, SymbolKind::CONSTANT, name_span, text);
                            }
                        }
                    }
                    Token::Defschema => {
                        if let Some((name_token, name_span)) = tokens.get(i + 2) {
                            if let Token::Ident(name) = name_token {
                                add_symbol(&mut symbols, name, SymbolKind::STRUCT, name_span, text);
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        
        i += 1;
    }
    
    symbols
}

/// Helper function to add a symbol
fn add_symbol(
    symbols: &mut Vec<SymbolInformation>,
    name: &str,
    kind: SymbolKind,
    span: &Span,
    text: &str,
) {
    let ((start_line, start_char), (end_line, end_char)) = to_line_col(span, text);
    
    symbols.push(SymbolInformation {
        name: name.to_string(),
        kind,
        tags: None,
        #[allow(deprecated)]
        deprecated: Some(false),
        location: Location {
            uri: Url::parse("file:///tmp/dummy.pact").unwrap(), // Placeholder - caller should update
            range: Range {
                start: Position {
                    line: start_line,
                    character: start_char,
                },
                end: Position {
                    line: end_line,
                    character: end_char,
                },
            },
        },
        container_name: None,
    });
}

/// Convert span to line/column positions
fn to_line_col(span: &Span, text: &str) -> ((u32, u32), (u32, u32)) {
    let mut line = 0;
    let mut col = 0;
    let mut start_line = 0;
    let mut start_col = 0;
    let mut found_start = false;
    
    for (i, ch) in text.char_indices() {
        if i == span.start {
            start_line = line;
            start_col = col;
            found_start = true;
        }
        
        if i == span.end {
            return ((start_line, start_col), (line, col));
        }
        
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    
    // If we haven't found the end, use the current position
    if found_start {
        ((start_line, start_col), (line, col))
    } else {
        ((0, 0), (0, 0))
    }
}