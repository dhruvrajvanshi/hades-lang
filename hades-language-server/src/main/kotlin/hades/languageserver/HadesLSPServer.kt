package hades.languageserver

import hades.asURI
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
            val diagnostics = astService.didOpen(params.textDocument.uri.asURI, params.textDocument.text, params.textDocument.version)
        }
    }

    override fun didChange(params: DidChangeTextDocumentParams) {
        ioScope.launch {
            astService.didChange(params.textDocument.uri.asURI, params.textDocument.version, params.contentChanges)
        }
    }

    override fun didClose(params: DidCloseTextDocumentParams) {
        log.debug("didClose(${params.textDocument.uri})")
    }

    override fun didSave(params: DidSaveTextDocumentParams) {
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