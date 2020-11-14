package hades.ast.parsing

import hades.URI
import hades.ast.*
import hades.diagnostics.Diagnostic
import hades.diagnostics.DiagnosticKind
import hades.languageserver.logging.logger
import kotlinx.coroutines.yield

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
    private var lastToken = lexer.nextToken()
    private var currentToken = lastToken
    suspend fun parseSourceFile(): ParseResult {
        val startToken = currentToken
        val definitions = mutableListOf<Definition>()
        var previousToken = currentToken
        while (currentToken.kind != Token.Kind.EOF) {
            definitions.add(parseDefinition())
            if (previousToken === currentToken) {
                advance()
            }
            previousToken = currentToken
            yield()
        }
        val sourceFile = SourceFile(
            meta = makeMeta(startToken, currentToken),
            definitions = definitions,
        )
        return ParseResult(
            sourceFile,
            diagnostics,
            lexer.newlineOffsets
        )
    }

    private fun parseDefinition(): Definition = when(currentToken.kind) {
        t.IMPORT -> parseImportDefinition()
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

    private fun parseImportDefinition(): Definition {
        val start = expect(t.IMPORT)
        val path = parseModulePath()
        expect(t.AS)
        val name = parseBindingIdentifier()
        terminateStatement()
        return Definition.ImportAs(
            makeMeta(start, name),
            importToken = start,
            path,
            name
        )
    }

    private fun terminateStatement() {
        if (currentToken.kind != t.SEMICOLON) {
            diagnostics.add(
                Diagnostic(
                    SourceRange(file, lastToken.span),
                    DiagnosticKind.MissingSemicolon
                )
            )
        } else {
            advance()
        }
    }

    private fun parseModulePath(): ModulePath {
        val startToken = currentToken
        val head = parseIdentifier()
        val tail = mutableListOf<Identifier>()
        while (at(t.DOT)) {
            advance()
            tail.add(parseIdentifier())
        }

        return ModulePath(
            makeMeta(startToken, tail.lastOrNull() ?: head),
            head,
            tail
        )
    }

    private fun makeMeta(start: HasOffsetSpan, stop: HasOffsetSpan): ASTMeta {
        return ASTMeta(
            file,
            startOffset = start.offsetSpan.start,
            stopOffset = stop.offsetSpan.stop + 1,
        )
    }

    private fun parseIdentifier(): Identifier {
        val token = expect(t.IDENTIFIER)
        return Identifier(
            makeMeta(token, token),
            name = Name(token.text)
        )
    }

    private fun parseBindingIdentifier(): BindingIdentifier {
        val token = expect(t.IDENTIFIER)
        return BindingIdentifier(
            makeMeta(token, token),
            name = Name(token.text)
        )
    }


    private fun at(kind: Token.Kind) = currentToken.kind == kind

    private fun expect(kind: Token.Kind): Token {
        return if (at(kind)) {
            advance()
        } else {
            diagnostics.add(
                Diagnostic(
                    range = SourceRange(file, currentToken.span),
                    kind = DiagnosticKind.TokenExpected(kind),
                )
            )
            log.debug("Expected $kind; Found $currentToken")
            return currentToken
        }
    }

    private fun skipUntilDefinitionExpected(): ASTMeta {
        val startToken = advance()
        while (currentToken.kind != Token.Kind.EOF && currentToken.kind !in DEFINITION_START_TOKENS) {
            advance()
        }
        return makeMeta(startToken, lastToken)
    }


    private fun advance(): Token {
        lastToken = currentToken
        currentToken = lexer.nextToken()
        return lastToken
    }
}

data class ParseResult(
    val sourceFile: SourceFile,
    val diagnostics: List<Diagnostic>,
    val newlineOffsets: List<Int>,
)