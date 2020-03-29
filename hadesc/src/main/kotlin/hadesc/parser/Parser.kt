package hadesc.parser

import hadesc.ast.*
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.qualifiedpath.QualifiedName

internal typealias tt = Token.Kind

private val declarationRecoveryTokens = setOf(tt.EOF, tt.IMPORT, tt.DEF, tt.EXTERN)
private val byteStringEscapes = mapOf(
    'n' to '\n',
    '0' to '\u0000'
)

@OptIn(ExperimentalStdlibApi::class)
class Parser(val ctx: Context, val moduleName: QualifiedName, file: SourcePath) {
    private val lexer = Lexer(file)
    private var currentToken = lexer.nextToken()

    fun parseSourceFile(): SourceFile {
        return SourceFile(moduleName, parseDeclarations())
    }

    private fun parseDeclarations(): List<Declaration> = buildList {
        while (currentToken.kind != tt.EOF) {
            add(parseDeclaration())
        }
    }.toList()

    private fun parseDeclaration(): Declaration {
        return when (currentToken.kind) {
            tt.IMPORT -> parseDeclarationImportAs()
            tt.DEF -> parseDeclarationFunctionDef()
            tt.EXTERN -> parseExternFunctionDef()
            else -> {
                val location = recoverFromError(Diagnostic.Kind.DeclarationExpected)
                Declaration(
                    location,
                    Declaration.Kind.Error
                )
            }
        }
    }

    private fun parseExternFunctionDef(): Declaration {
        val start = expect(tt.EXTERN)
        expect(tt.DEF)
        val name = parseBinder()
        val params = buildList {
            expect(tt.LPAREN)
            var isFirst = true
            while (!(at(tt.RPAREN) || at(tt.EOF))) {
                if (!isFirst && !at(tt.RPAREN)) {
                    expect(tt.COMMA)
                } else {
                    isFirst = false
                }
                add(parseTypeAnnotation())
            }
            expect(tt.RPAREN)
        }
        expect(tt.COLON)
        val returnType = parseTypeAnnotation()
        expect(tt.EQ)

        val externName = parseIdentifier()

        expect(tt.SEMICOLON)
        return Declaration(
            makeLocation(start, returnType),
            Declaration.Kind.ExternFunctionDef(
                binder = name,
                paramTypes = params,
                returnType = returnType,
                externName = externName
            )
        )
    }

    private fun makeLocation(start: HasLocation, stop: HasLocation): SourceLocation {
        return SourceLocation.between(start, stop)
    }

    private fun parseDeclarationImportAs(): Declaration {
        val start = expect(tt.IMPORT)
        val modulePath = parseQualifiedPath()
        expect(tt.AS)
        val asName = parseBinder()
        expect(Token.Kind.SEMICOLON)

        return Declaration(
            makeLocation(start, asName),
            Declaration.Kind.ImportAs(modulePath, asName)
        )
    }

    private fun parseQualifiedPath(): QualifiedPath = QualifiedPath(buildList {
        add(parseIdentifier())
        while (at(tt.DOT)) {
            advance()
            add(parseIdentifier())
        }
    })

    private fun parseIdentifier(): Identifier {
        val tok = expect(tt.ID)
        return Identifier(tok.location, ctx.makeName(tok.text))
    }

    private fun parseDeclarationFunctionDef(): Declaration {
        val start = expect(tt.DEF)
        val name = parseBinder()
        val params = parseParams()
        expect(tt.COLON)
        val annotation = parseTypeAnnotation()
        val block = parseBlock()
        return Declaration(
            location = makeLocation(start, block),
            kind = Declaration.Kind.FunctionDef(
                name = name,
                params = params,
                returnType = annotation,
                body = block
            )
        )
    }

    private fun parseBlock(): Block {
        val start = expect(tt.LBRACE)
        val members = parseBlockMembers()
        val stop = expect(tt.RBRACE)
        return Block(makeLocation(start, stop), members)
    }

    private fun parseBlockMembers(): List<Block.Member> = buildList {
        while (!(at(tt.RBRACE) || at(tt.EOF))) {
            add(parseBlockMember())
        }
    }

    private fun parseBlockMember(): Block.Member {
        return when (currentToken.kind) {
            else -> {
                val expr = parseExpression()
                if (expr.kind is Expression.Kind.Error) {
                    recoverFromError()
                } else {
                    expect(tt.SEMICOLON)
                }
                Block.Member.Expression(expr)
            }
        }
    }

    private fun parseExpression(): Expression {
        val head = when (currentToken.kind) {
            tt.ID -> parseExpressionVar()
            tt.BYTE_STRING -> parseExpressionByteString()
            else -> {
                val location = advance().location
                ctx.diagnosticReporter.report(location, Diagnostic.Kind.ExpressionExpected)
                Expression(
                    location = location,
                    kind = Expression.Kind.Error
                )
            }
        }
        return parseExpressionTail(head)
    }

    private fun parseExpressionByteString(): Expression {
        val token = expect(tt.BYTE_STRING)
        val bytes = buildList {
            val firstCharIndexInQuote = 2
            var i = firstCharIndexInQuote
            while (i < token.text.length - 1) {
                val char = token.text[i]
                if (char == '\\') {
                    i++
                    assert(i < token.text.length - 1) { TODO("Byte string ended abruptly") }
                    val escapeChar = byteStringEscapes[token.text[i]]
                    if (escapeChar != null) {
                        add(escapeChar.toByte())
                    } else {
                        TODO("Invalid byte string escape $escapeChar in ${token.location}")
                    }

                } else {
                    addAll("$char".encodeToByteArray().toList())
                }

                i++
            }
        }
        return Expression(token.location, Expression.Kind.ByteString(bytes.toByteArray()))
    }

    private fun parseExpressionTail(head: Expression): Expression {
        return when (currentToken.kind) {
            tt.LPAREN -> {
                advance()
                val args = buildList {
                    while (!(at(tt.RPAREN) || at(tt.EOF))) {
                        add(parseArg())
                    }
                }
                val stop = expect(tt.RPAREN)
                parseExpressionTail(
                    Expression(
                        location = makeLocation(head, stop),
                        kind = Expression.Kind.Call(
                            head,
                            args
                        )
                    )
                )
            }
            tt.DOT -> {
                advance()
                val ident = parseIdentifier()
                parseExpressionTail(
                    Expression(
                        makeLocation(head, ident),
                        Expression.Kind.Property(head, ident)
                    )
                )
            }
            else -> head
        }
    }

    private fun parseArg(): Arg = Arg(parseExpression())

    private fun parseExpressionVar(): Expression {
        val identifier = parseIdentifier()
        return Expression(
            location = identifier.location,
            kind = Expression.Kind.Var(identifier)
        )
    }

    private fun parseParams(): List<Param> = buildList {
        expect(tt.LPAREN)
        var first = true
        while (!(at(tt.RPAREN) || at(tt.EOF))) {
            if (!first) {
                expect(tt.COMMA)
            } else {
                first = false
            }
            add(parseParam())
        }
        expect(tt.RPAREN)
    }

    private fun parseParam(): Param {
        val binder = parseBinder()
        val annotation = parseOptionalAnnotation()
        return Param(
            location = makeLocation(binder, annotation ?: binder),
            binder = binder,
            annotation = annotation
        )
    }

    private fun parseBinder(): Binder {
        return Binder(parseIdentifier())
    }

    private fun parseOptionalAnnotation(): TypeAnnotation? = if (at(tt.COLON)) {
        expect(tt.COLON)
        parseTypeAnnotation()
    } else {
        null
    }

    private fun parseTypeAnnotation(): TypeAnnotation {
        return when (currentToken.kind) {
            tt.ID -> {
                val id = parseIdentifier()
                TypeAnnotation(
                    id.location,
                    TypeAnnotation.Kind.Var(id)
                )
            }
            tt.STAR -> {
                val start = advance()
                val to = parseTypeAnnotation()
                TypeAnnotation(
                    makeLocation(start, to),
                    TypeAnnotation.Kind.Ptr(to)
                )
            }
            else -> {
                val location = advance().location
                ctx.diagnosticReporter.report(location, Diagnostic.Kind.TypeAnnotationExpected)
                TypeAnnotation(
                    location,
                    TypeAnnotation.Kind.Error
                )

            }
        }
    }

    private fun expect(kind: Token.Kind): Token {
        return if (currentToken.kind == kind) {
            advance()
        } else {
            val result = advance()
            ctx.diagnosticReporter.report(result.location, Diagnostic.Kind.UnexpectedToken(kind, result))
            result
        }
    }

    private fun at(kind: tt): Boolean = currentToken.kind == kind

    private fun recoverFromError(
        diagnostic: Diagnostic.Kind? = null,
        recoverySet: Set<tt> = declarationRecoveryTokens
    ): SourceLocation {
        val startToken = currentToken
        var lastToken = advance()
        while (true) {
            if (isEOF()) {
                break
            } else if (currentToken.kind == Token.Kind.SEMICOLON) {
                advance()
                break
            } else if (recoverySet.contains(currentToken.kind)) {
                break
            } else {
                lastToken = currentToken
                advance()
            }
        }

        val location = SourceLocation(
            file = startToken.location.file,
            start = startToken.location.start,
            stop = lastToken.location.stop
        )
        if (diagnostic != null) {
            ctx.diagnosticReporter.report(location, diagnostic)
        }
        return location

    }

    private fun advance(): Token {
        val result = currentToken
        currentToken = lexer.nextToken()
        return result
    }

    private fun isEOF() =
        currentToken.kind == tt.EOF

}

