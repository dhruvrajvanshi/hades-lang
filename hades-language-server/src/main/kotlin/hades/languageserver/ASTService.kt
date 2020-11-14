package hades.languageserver

import hades.URI
import hades.ast.SourceFile
import hades.ast.parsing.Parser
import hades.diagnostics.Diagnostic
import hades.languageserver.logging.logger
import kotlinx.coroutines.withContext
import java.util.concurrent.ConcurrentHashMap
import kotlin.coroutines.CoroutineContext

class ASTService(
    private val computeCtx: CoroutineContext,
    private val ioCtx: CoroutineContext
) {
    private val log = logger()
    private val sourceFiles = ConcurrentHashMap<URI, SourceFile>()
    private val sourceText = ConcurrentHashMap<URI, SourceText>()
    private val syntaxErrors = ConcurrentHashMap<URI, List<Diagnostic>>()

    fun initialize(rootDirectory: URI) {}

    suspend fun didOpen(file: URI, text: String): List<Diagnostic> = withContext(computeCtx) {
        sourceText[file] = SourceText(text)
        val (sourceFile, diagnostics) = Parser(file, text).parseSourceFile()
        syntaxErrors[file] = diagnostics
        sourceFiles[file] = sourceFile
        diagnostics
    }
}

private class SourceText(
    val text: String
)
