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

val TYPE_ANNOTATION_FOLLOW_SET = setOf(
    t.COMMA, t.RPAREN, t.EQUAL, t.SEMICOLON, t.RSQB,
    t.LBRACE,
)
val STATEMENT_FOLLOW_SET = setOf(
    t.RBRACE, t.VAL, t.DEFER, t.IDENTIFIER,
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
        t.DEF -> parseDef()
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

    private fun parseDef(): Definition {
        val start = expect(t.DEF)
        val name = parseBindingIdentifier()
        val typeParams: List<TypeParam>? = null
        val params = parseParams()
        val returnType = parseOptionalTypeAnnotation()
        val body = parseBlock()
        return Definition.Def(
            makeMeta(start, body),
            name,
            typeParams = typeParams,
            params,
            returnType,
            body
        )
    }

    private fun parseBlock(): Block {
        val start = expect(t.LBRACE)
        val statements = mutableListOf<Statement>()
        while (!at(t.EOF) && !at(t.RBRACE)) {
            statements.add(parseStatement())
        }
        val stop = expect(t.RBRACE)
        return Block(
            makeMeta(start, stop),
            statements
        )
    }

    private fun parseStatement(): Statement = when (currentToken.kind) {
        else -> {
            val startToken = currentToken
            val meta = skipUntilTokenReached(STATEMENT_FOLLOW_SET)
            val stopToken = currentToken
            diagnostics.add(
                Diagnostic(
                    kind = DiagnosticKind.StatementExpected,
                    range = SourceRange(
                        file,
                        Span(
                            startToken.span.start,
                            stopToken.span.stop,
                        )
                    )
                )
            )
            Statement.Error(meta)
        }
    }

    private fun parseOptionalTypeAnnotation(): TypeAnnotation? {
        if (!at(t.COLON)) {
            return null
        }
        advance()
        return parseTypeAnnotation()
    }

    private fun parseTypeAnnotation(): TypeAnnotation = when (currentToken.kind) {
        else -> {
            val startToken = currentToken
            val meta = skipUntilTokenReached(TYPE_ANNOTATION_FOLLOW_SET)
            val stopToken = currentToken
            diagnostics.add(
                Diagnostic(
                    kind = DiagnosticKind.TypeAnnotationExpected,
                    range = SourceRange(
                        file,
                        Span(
                            startToken.span.start,
                            stopToken.span.stop,
                        )
                    )
                )
            )
            TypeAnnotation.Error(meta)
        }
    }

    private fun parseParams(): List<Param> {
        expect(t.LPAREN)
        val params = mutableListOf<Param>()
        while (!at(t.EOF) && !at(t.RPAREN)) {
            params.add(parseParam())
            if (!at(t.RPAREN)) {
                expect(t.COMMA)
            }
        }
        expect(t.RPAREN)
        return params
    }

    private fun parseParam(): Param {
        val name = parseBindingIdentifier()
        val annotation = parseOptionalTypeAnnotation()

        return Param(
            makeMeta(name, annotation ?: name),
            name,
            annotation,
        )

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
            advance()
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
        return skipUntilTokenReached(DEFINITION_START_TOKENS)
    }

    private fun skipUntilTokenReached(s: Set<Token.Kind>): ASTMeta {
        val startToken = advance()
        while (currentToken.kind != Token.Kind.EOF && currentToken.kind !in s) {
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