package hadesc.lsp

import hadesc.unit
import org.eclipse.lsp4j.DidChangeConfigurationParams
import org.eclipse.lsp4j.DidChangeWatchedFilesParams
import org.eclipse.lsp4j.services.WorkspaceService

class HadesWorkspaceService: WorkspaceService {
    override fun didChangeConfiguration(params: DidChangeConfigurationParams) = unit

    override fun didChangeWatchedFiles(params: DidChangeWatchedFilesParams) = unit
}