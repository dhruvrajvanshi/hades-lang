package hades.ast.parsing

import hades.URI
import hades.ast.*
import hades.diagnostics.Diagnostic
import hades.diagnostics.DiagnosticKind
import hades.languageserver.logging.logger

typealias t = Token.Kind
val DEFINITION_START_TOKENS = setOf(
    t.DEF,
    t.IMPORT,
)

class Parser(
    private val file: URI,
    input: String,
) {
    private val log = logger()
    private val diagnostics = mutableListOf<Diagnostic>()
    private val lexer = Lexer(file, input) { diagnostics.add(it) }
    private var currentToken = lexer.nextToken()
    fun parseSourceFile(): Pair<SourceFile, List<Diagnostic>> {
        val definitions = mutableListOf<Definition>()
        while (currentToken.kind != Token.Kind.EOF) {
            definitions.add(parseDefinition())
        }
        return SourceFile(
            meta = ASTMeta(file, length = 0),
            definitions = listOf(),
        ) to diagnostics
    }

    private fun parseDefinition(): Definition = when(currentToken.kind) {
        else -> {
            val startToken = currentToken
            val meta = skipUntilDefinitionExpected()
            val stopToken = currentToken
            diagnostics.add(
                Diagnostic(
                    kind = DiagnosticKind.DeclarationExpected,
                    range = SourceRange(
                        file,
                        Span(
                            startToken.span.start,
                            stopToken.span.stop,
                        )
                    )
                )
            )
            Definition.Error(meta)
        }
    }

    private fun skipUntilDefinitionExpected(): ASTMeta {
        var length = advance().length
        while (currentToken.kind !in listOf(Token.Kind.EOF) && currentToken.kind !in DEFINITION_START_TOKENS) {
            length += advance().length
        }
        return ASTMeta(file, length)
    }


    private fun advance(): Token {
        val result = currentToken
        currentToken = lexer.nextToken()
        return result
    }
}