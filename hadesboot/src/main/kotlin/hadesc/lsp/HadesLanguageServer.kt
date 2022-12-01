package hadesc.lsp

import hadesc.context.Context
import hadesc.unit
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.messages.Either
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.services.LanguageClientAware
import org.eclipse.lsp4j.services.LanguageServer
import org.eclipse.lsp4j.services.TextDocumentService
import org.eclipse.lsp4j.services.WorkspaceService
import java.util.concurrent.CompletableFuture

class HadesLanguageServer(private val ctx: Context): LanguageServer, LanguageClientAware {
    private val textDocumentService = HadesTextDocumentService(ctx)
    private val workspaceService = HadesWorkspaceService()
    override fun initialize(params: InitializeParams?): CompletableFuture<InitializeResult> {
        return CompletableFuture.completedFuture(InitializeResult().apply {
            serverInfo = ServerInfo("hades")
            capabilities = ServerCapabilities().apply {
                diagnosticProvider = DiagnosticRegistrationOptions("hades-lsp")
                textDocumentSync = Either.forRight(TextDocumentSyncOptions().apply {
                    openClose = true
                    change = TextDocumentSyncKind.Incremental
                })
            }
        })
    }

    override fun shutdown(): CompletableFuture<Any> =
        CompletableFuture.completedFuture(null)

    override fun exit() = unit

    override fun getTextDocumentService(): TextDocumentService = textDocumentService

    override fun getWorkspaceService(): WorkspaceService = workspaceService

    override fun connect(client: LanguageClient) {
        textDocumentService.connect(client)
    }
}
