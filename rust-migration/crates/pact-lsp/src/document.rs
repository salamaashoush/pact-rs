//! Document management

use dashmap::DashMap;
use tower_lsp::lsp_types::Url;
use ropey::Rope;

/// A document in the workspace
#[derive(Debug, Clone)]
pub struct Document {
    /// Document URI
    pub uri: Url,
    
    /// Document version
    pub version: i32,
    
    /// Document text
    pub text: String,
    
    /// Rope for efficient text operations
    pub rope: Rope,
}

impl Document {
    /// Create a new document
    pub fn new(uri: Url, version: i32, text: String) -> Self {
        let rope = Rope::from(text.as_str());
        Self {
            uri,
            version,
            text,
            rope,
        }
    }
    
    /// Update document text
    pub fn update(&mut self, version: i32, text: String) {
        self.version = version;
        self.text = text;
        self.rope = Rope::from(self.text.as_str());
    }
}

/// Document store
pub struct DocumentStore {
    /// Documents indexed by URI
    documents: DashMap<Url, Document>,
}

impl DocumentStore {
    /// Create a new document store
    pub fn new() -> Self {
        Self {
            documents: DashMap::new(),
        }
    }
    
    /// Open a document
    pub fn open_document(&self, uri: Url, version: i32, text: String) {
        let document = Document::new(uri.clone(), version, text);
        self.documents.insert(uri, document);
    }
    
    /// Update a document
    pub fn update_document(&self, uri: Url, version: i32, text: String) {
        if let Some(mut doc) = self.documents.get_mut(&uri) {
            doc.update(version, text);
        }
    }
    
    /// Close a document
    pub fn close_document(&self, uri: &Url) {
        self.documents.remove(uri);
    }
    
    /// Get a document
    pub fn get_document(&self, uri: &Url) -> Option<Document> {
        self.documents.get(uri).map(|doc| doc.clone())
    }
}