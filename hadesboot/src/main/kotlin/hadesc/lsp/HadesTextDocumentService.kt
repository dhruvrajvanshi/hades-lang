package hadesc.lsp

import org.eclipse.lsp4j.DidChangeTextDocumentParams
import org.eclipse.lsp4j.DidCloseTextDocumentParams
import org.eclipse.lsp4j.DidOpenTextDocumentParams
import org.eclipse.lsp4j.DidSaveTextDocumentParams
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.services.TextDocumentService

class HadesTextDocumentService: TextDocumentService {
    private lateinit var client: LanguageClient
    fun connect(client: LanguageClient) {
        this.client = client
    }

    override fun didOpen(params: DidOpenTextDocumentParams?) {
        TODO()
    }

    override fun didChange(params: DidChangeTextDocumentParams?) {
        TODO("Not yet implemented")
    }

    override fun didClose(params: DidCloseTextDocumentParams?) {
        TODO("Not yet implemented")
    }

    override fun didSave(params: DidSaveTextDocumentParams?) {
        TODO("Not yet implemented")
    }
}