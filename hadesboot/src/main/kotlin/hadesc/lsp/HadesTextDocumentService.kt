package hadesc.lsp

import hadesc.context.Context
import hadesc.unit
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.services.TextDocumentService

class HadesTextDocumentService(private val ctx: Context): TextDocumentService {
    private var client: LanguageClient? = null
    fun connect(client: LanguageClient) {
        this.client = client
    }

    override fun didOpen(params: DidOpenTextDocumentParams) {
        client?.logMessage(
            MessageParams(
                MessageType.Info,
                "didOpen($params)"
            )
        )
        publishDiagnostics(params.textDocument.uri)
    }

    override fun didChange(params: DidChangeTextDocumentParams) {
        publishDiagnostics(params.textDocument.uri)
    }

    override fun didClose(params: DidCloseTextDocumentParams) = unit

    override fun didSave(params: DidSaveTextDocumentParams) = unit

    private fun publishDiagnostics(documentURI: String) {
        ctx.build()
        ctx.diagnosticReporter.errors.groupBy { it.sourceLocation.file.path }.forEach { (path, errors) ->
            client?.publishDiagnostics(PublishDiagnosticsParams().apply {
                diagnostics = errors.map {
                    Diagnostic().apply {
                        range = Range(
                            Position(it.sourceLocation.start.line, it.sourceLocation.start.column),
                            Position(it.sourceLocation.stop.line, it.sourceLocation.stop.column),
                        )
                        message = it.kind.prettyPrint()
                    }
                }
            })
        }
    }
}