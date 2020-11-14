package hades.languageserver

import hades.asURI
import hades.diagnostics.Diagnostic
import hades.file
import hades.languageserver.logging.logger
import kotlinx.coroutines.*
import kotlinx.coroutines.future.future
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.messages.Either
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.services.LanguageServer
import org.eclipse.lsp4j.services.TextDocumentService
import org.eclipse.lsp4j.services.WorkspaceService
import java.util.concurrent.CompletableFuture
import java.util.concurrent.Executors
import kotlin.system.exitProcess

class HadesLSPServer : LanguageServer, TextDocumentService, WorkspaceService {
    private val log = logger()
    private val ioScope = CoroutineScope(Dispatchers.IO)
    private val computeContext = Executors
        .newFixedThreadPool(Runtime.getRuntime().availableProcessors()) {
            Thread(it, "hades-compute")
        }.asCoroutineDispatcher()
    private val computeScope = CoroutineScope(computeContext)
    private lateinit var client: LanguageClient
    private val astService = ASTService(
        ioCtx = ioScope.coroutineContext,
        computeCtx = computeContext,
    )

    override fun initialize(params: InitializeParams): CompletableFuture<InitializeResult> = computeScope.future {
        astService.initialize(params.rootUri.asURI)
        InitializeResult().apply {
            capabilities = ServerCapabilities().apply {
                textDocumentSync = Either.forRight(TextDocumentSyncOptions().apply {
                    openClose = true
                    change = TextDocumentSyncKind.Incremental
                    save = SaveOptions().apply {
                        includeText = false
                    }
                })
                hoverProvider = true
            }
        }
    }

    override fun hover(position: TextDocumentPositionParams): CompletableFuture<Hover?> = computeScope.future {
        null
    }

    override fun shutdown(): CompletableFuture<Any> = ioScope.future {
        // temporary hack to fix leaking of the server process
        // on editor close
        launch(ioScope.coroutineContext) {
            delay(1000)
            exitProcess(0)
        }
    }

    override fun exit() {
        exitProcess(0)
    }

    override fun getTextDocumentService(): TextDocumentService = this

    override fun getWorkspaceService(): WorkspaceService = this

    override fun didOpen(params: DidOpenTextDocumentParams) {
        log.debug("didOpen(${params.textDocument.uri}, ${params.textDocument.version})")
        computeScope.launch {
            val diagnostics = astService.didOpen(params.textDocument.uri.asURI, params.textDocument.text)
            ioScope.launch {
                client.publishDiagnostics(PublishDiagnosticsParams().apply {
                    uri = params.textDocument.uri
                    this.diagnostics =  diagnostics.map {
                        it.toLSPDiagnostic()
                    }
                })
            }
        }
    }

    private fun Diagnostic.toLSPDiagnostic(): org.eclipse.lsp4j.Diagnostic {
        val self = this
        return Diagnostic().apply {
            message = self.kind.message
            range = Range(
                Position(
                    self.range.span.start.line - 1,
                    self.range.span.start.column - 1,
                ),
                Position(
                    self.range.span.stop.line - 1,
                    self.range.span.stop.column - 1,
                )
            )
        }
    }

    override fun didChange(params: DidChangeTextDocumentParams) {
        // TODO
    }

    override fun didClose(params: DidCloseTextDocumentParams) {
        log.debug("didClose(${params.textDocument.uri})")
    }

    override fun didSave(params: DidSaveTextDocumentParams) {
        computeScope.launch {
            val text = withContext(ioScope.coroutineContext) {
                params.textDocument.uri.asURI.file.readText()
            }
            val diagnostics = astService.didSave(params.textDocument.uri.asURI, text)
            ioScope.launch {
                client.publishDiagnostics(PublishDiagnosticsParams().apply {
                    uri = params.textDocument.uri
                    this.diagnostics = diagnostics.map {
                        it.toLSPDiagnostic()
                    }
                })
            }
        }
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