package hades.languageserver

import hades.URI
import hades.ast.SourceFile
import hades.ast.parsing.Parser
import hades.ast.parsing.ParserInput
import hades.diagnostics.Diagnostic
import hades.languageserver.logging.logger
import kotlinx.coroutines.sync.Mutex
import kotlinx.coroutines.sync.withLock
import kotlinx.coroutines.withContext
import kotlinx.coroutines.withTimeout
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j.TextDocumentContentChangeEvent
import java.util.concurrent.ConcurrentHashMap
import kotlin.coroutines.CoroutineContext

class ASTService(
    private val computeCtx: CoroutineContext,
    private val ioCtx: CoroutineContext,
) {
    private val log = logger()
    private val sourceFiles = ConcurrentHashMap<URI, SourceFile>()
    private val sourceText = ConcurrentHashMap<URI, SourceText>()
    private val syntaxErrors = ConcurrentHashMap<URI, List<Diagnostic>>()
    private val lineLengths = ConcurrentHashMap<URI, List<Int>>()
    private val edits = ConcurrentHashMap<URI, MutableList<List<Edit>>>()
    private val didChangeMutex = ConcurrentHashMap<URI, Mutex>()

    fun initialize(rootDirectory: URI) {}

    suspend fun didOpen(file: URI, text: String, documentVersion: Int): List<Diagnostic> {
        edits[file] = mutableListOf()
        return getSyntaxDiagnosticsOnFullEdit(file, text, documentVersion)
    }

    suspend fun didSave(file: URI, text: String, documentVersion: Int): List<Diagnostic> {
        return getSyntaxDiagnosticsOnFullEdit(file, text, documentVersion)
    }

    private suspend fun getSyntaxDiagnosticsOnFullEdit(file: URI, text: String, documentVersion: Int): List<Diagnostic> =
        withContext(computeCtx) {
            sourceText[file] = SourceText(text)

            val result = withTimeout(4000) {
                Parser(file, StringParserInput(text), documentVersion).parseSourceFile()
            }
            syntaxErrors[file] = result.diagnostics
            sourceFiles[file] = result.sourceFile
            lineLengths[file] = result.lineLengths
            log.info("Text changed to \n$text")
            log.info("Republishing ${result.diagnostics.size} diagnostic(s)")
            result.diagnostics
        }

    suspend fun didChange(uri: URI, version: Int, contentChanges: List<TextDocumentContentChangeEvent>) = withContext(computeCtx) {
        didChangeMutex.getOrPut(uri) { Mutex() }.withLock {
            processChange(DidChangeNotification(uri, version, contentChanges))
        }
    }

    private suspend fun processChange(notification: DidChangeNotification) {
        val uri = notification.uri
        val contentChanges = notification.contentChanges
        val version = notification.version
        val fileEdits = requireNotNull(edits[uri])
        val thisVersionChanges = contentChanges.map {
            val (startOffset, endOffset) = rangeToOffsets(uri, it.range)
            Edit(startOffset, endOffset, it.text)
        }

        fileEdits.add(thisVersionChanges)
        var text = requireNotNull(sourceText[uri])
        for (change in thisVersionChanges) {
            text = applyEdit(text, change)
        }

        lineLengths[uri] = text.text.split('\n').map { it.length + 1 }
        sourceText[uri] = text
        val parseResult = Parser(
            uri,
            StringParserInput(text.text),
            version,
        ).parseSourceFile()
        sourceFiles[uri] = parseResult.sourceFile
    }

    private fun applyEdit(source: SourceText, change: Edit): SourceText {
        log.debug("${change.startOffset}, ${change.endOffset}")
        val prefix = source.text.substring(0 until change.startOffset)
        val postfix = source.text.substring(change.endOffset until source.text.length)
        val middle = change.text
        val newText = prefix + middle + postfix

        log.debug("\n$newText")
        return SourceText(newText)
    }

    private fun rangeToOffsets(uri: URI, range: Range): Pair<Int, Int> {
        val startOffset = positionToOffset(uri, range.start)
        val stopOffset = positionToOffset(uri, range.end)
        return startOffset to stopOffset
    }

    private fun positionToOffset(uri: URI, position: Position): Int {
        val lineLengths = requireNotNull(lineLengths[uri])
        if (position.line == 0) {
            return position.character
        }
        val lineStartOffset = lineLengths.take(position.line).fold(0) { acc, i -> acc + i }
        return lineStartOffset + position.character
    }
}

class StringParserInput(private val text: String) : ParserInput {
    override fun get(index: Int): Char {
        return text[index]
    }

    override fun substring(range: IntRange): String {
        return text.substring(range)
    }

    override val length: Int get() = text.length

}

private data class Edit(
        val startOffset: Int,
        val endOffset: Int,
        val text: String,
)

private data class SourceText(
    val text: String
)

private data class DidChangeNotification(
    val uri: URI,
    val version: Int,
    val contentChanges: List<TextDocumentContentChangeEvent>,
)
