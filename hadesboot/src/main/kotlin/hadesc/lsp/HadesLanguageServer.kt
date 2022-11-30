package hadesc.lsp

import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.services.LanguageClientAware
import org.eclipse.lsp4j.services.LanguageServer
import org.eclipse.lsp4j.services.TextDocumentService
import org.eclipse.lsp4j.services.WorkspaceService
import java.util.concurrent.CompletableFuture

class HadesLanguageServer: LanguageServer, LanguageClientAware {
    private val textDocumentService = HadesTextDocumentService()
    private val workspaceService = HadesWorkspaceService()
    override fun initialize(params: InitializeParams?): CompletableFuture<InitializeResult> {
        TODO("Not yet implemented")
    }

    override fun shutdown(): CompletableFuture<Any> {
        TODO("Not yet implemented")
    }

    override fun exit() {
        TODO("Not yet implemented")
    }

    override fun getTextDocumentService(): TextDocumentService = textDocumentService

    override fun getWorkspaceService(): WorkspaceService = workspaceService

    override fun connect(client: LanguageClient) {
        textDocumentService.connect(client)
    }
}
