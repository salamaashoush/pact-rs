//! Main LSP server implementation

use crate::backend::PactLanguageBackend;
use anyhow::Result;
use tower_lsp::{LspService, Server};

/// The main Pact language server
pub struct PactLanguageServer {
    /// The LSP service
    service: LspService<PactLanguageBackend>,
    
    /// The socket for communication
    socket: tower_lsp::ClientSocket,
}

impl PactLanguageServer {
    /// Create a new language server
    pub fn new() -> (Self, tower_lsp::Client) {
        let (service, socket) = LspService::new(PactLanguageBackend::new);
        let client = service.inner().client.clone();
        
        (
            Self { service, socket },
            client,
        )
    }
    
    /// Run the server on stdin/stdout
    pub async fn run(self) -> Result<()> {
        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();
        
        Server::new(stdin, stdout, self.socket)
            .serve(self.service)
            .await;
        
        Ok(())
    }
}

/// Run the LSP server
pub async fn run_lsp_server() -> Result<()> {
    let (server, _client) = PactLanguageServer::new();
    server.run().await
}