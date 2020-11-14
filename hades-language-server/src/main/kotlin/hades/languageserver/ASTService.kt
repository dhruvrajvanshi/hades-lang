package hades.languageserver

import hades.URI
import hades.ast.SourceFile
import hades.ast.parsing.Parser
import hades.diagnostics.Diagnostic
import hades.languageserver.logging.logger
import kotlinx.coroutines.withContext
import kotlinx.coroutines.withTimeout
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
    private val newlineOffsets = ConcurrentHashMap<URI, List<Int>>()

    fun initialize(rootDirectory: URI) {}

    suspend fun didOpen(file: URI, text: String): List<Diagnostic> = withContext(computeCtx) {
        sourceText[file] = SourceText(text)

        val result = withTimeout(4000) {
            Parser(file, text).parseSourceFile()
        }
        syntaxErrors[file] = result.diagnostics
        sourceFiles[file] = result.sourceFile
        newlineOffsets[file] = result.newlineOffsets
        result.diagnostics
    }

    fun getHoverInfo(file: URI, line: Int, column: Int): String? {
        val sourceFile = sourceFiles[file] ?: return null
        val sourceText = sourceText[file] ?: return null

        val offset = positionToOffset(file, line, column)
        for (definition in sourceFile.definitions) {
            log.info("Def: ${definition.meta.startOffset}; ${definition.meta.stopOffset}; Offset: $offset")
            if (definition.containsOffset(offset)) {
                return "definition: $definition"
            }
        }
        return "offset: $offset; Char: ${sourceText.text[offset]}"
    }

    private fun positionToOffset(file: URI, line: Int, column: Int): Int {
        val newlineOffsets = listOf(0) + (this.newlineOffsets[file] ?: emptyList())
        val lineStartOffset = newlineOffsets[line - 1]
        if (line == 1) {
            return lineStartOffset + column - 1
        }
        return lineStartOffset + column
    }
}

private class SourceText(
    val text: String
)
