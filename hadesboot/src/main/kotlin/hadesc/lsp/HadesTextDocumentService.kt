package hadesc.lsp

import hadesc.context.Context
import hadesc.unit
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.services.TextDocumentService
import java.net.URI
import java.nio.file.Path

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
        publishDiagnostics()
    }

    override fun didSave(params: DidSaveTextDocumentParams) {
        ctx.onFileChange(Path.of(URI.create(params.textDocument.uri)))
        publishDiagnostics()
    }

    override fun didChange(params: DidChangeTextDocumentParams) {
        publishDiagnostics()
    }

    override fun didClose(params: DidCloseTextDocumentParams) = unit

    private fun publishDiagnostics() {
        ctx.checkProgram()
        ctx.diagnosticReporter.errors.forEach { (path, errors) ->
            client?.publishDiagnostics(PublishDiagnosticsParams().apply {
                uri = path.toUri().toString()
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