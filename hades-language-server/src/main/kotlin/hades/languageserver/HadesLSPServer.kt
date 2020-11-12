package hades.languageserver

import hades.languageserver.logging.logger
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.messages.Either
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.services.LanguageServer
import org.eclipse.lsp4j.services.TextDocumentService
import org.eclipse.lsp4j.services.WorkspaceService
import java.util.concurrent.CompletableFuture

class HadesLSPServer : LanguageServer, TextDocumentService, WorkspaceService {
    private lateinit var client: LanguageClient
    private val log = logger<HadesLSPServer>()

    override fun initialize(params: InitializeParams?): CompletableFuture<InitializeResult> {
        return CompletableFuture.completedFuture(InitializeResult().apply {
            capabilities = ServerCapabilities().apply {
                textDocumentSync = Either.forRight(TextDocumentSyncOptions().apply {
                    openClose = true
                    change = TextDocumentSyncKind.Incremental
                    save  = SaveOptions().apply {
                        includeText = false
                    }
                })
                hoverProvider = true
            }
        })
    }

    override fun hover(position: TextDocumentPositionParams): CompletableFuture<Hover> {
        return CompletableFuture.completedFuture(
            Hover().apply {
                contents = Either.forRight(MarkupContent().apply {
                    kind = MarkupKind.PLAINTEXT
                    value = "Hover not implemented"
                })
            }
        )
    }

    override fun shutdown(): CompletableFuture<Any> {
        log.info("Shutting down");
        return CompletableFuture.completedFuture(null)
    }

    override fun exit() {
        log.info("Exiting")
    }

    override fun getTextDocumentService(): TextDocumentService = this

    override fun getWorkspaceService(): WorkspaceService = this

    override fun didOpen(params: DidOpenTextDocumentParams) {
        log.debug("didOpen(${params.textDocument.uri}, ${params.textDocument.version})")
    }

    override fun didChange(params: DidChangeTextDocumentParams) {
        log.debug("didChange(${params.textDocument})")
    }

    override fun didClose(params: DidCloseTextDocumentParams) {
        log.debug("didClose(${params.textDocument.uri})")
    }

    override fun didSave(params: DidSaveTextDocumentParams) {
        log.debug("didSave(${params.textDocument.uri})")
    }

    override fun didChangeConfiguration(params: DidChangeConfigurationParams) {
        log.debug("didChangeConfiguration(${params.settings})")
    }

    override fun didChangeWatchedFiles(params: DidChangeWatchedFilesParams) {
        log.debug("didChangeWatchedFiles(${params.changes})")
    }

    fun connect(client: LanguageClient) {
        this.client = client
    }

}