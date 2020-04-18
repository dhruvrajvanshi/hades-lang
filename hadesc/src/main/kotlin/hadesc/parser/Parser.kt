package hadesc.parser

import hadesc.ast.*
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.location.HasLocation
import hadesc.location.Position
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.qualifiedname.QualifiedName

internal typealias tt = Token.Kind

private val declarationRecoveryTokens = setOf(tt.EOF, tt.IMPORT, tt.DEF, tt.EXTERN, tt.STRUCT)
private val structMemberRecoveryTokens = setOf(tt.EOF, tt.VAL, tt.DEF)
private val statementRecoveryTokens = setOf(tt.EOF, tt.RETURN, tt.VAL)
private val byteStringEscapes = mapOf(
    'n' to '\n',
    '0' to '\u0000'
)

@OptIn(ExperimentalStdlibApi::class)
class Parser(val ctx: Context, val moduleName: QualifiedName, val file: SourcePath) {
    private val lexer = Lexer(file)
    private var currentToken = lexer.nextToken()

    fun parseSourceFile(): SourceFile {
        val declarations = parseDeclarations()
        val start = Position(1, 1)
        val stop = declarations.lastOrNull()?.location?.stop ?: start
        val location = SourceLocation(file, start, stop)
        val sourceFile = SourceFile(location, moduleName, declarations)
        ctx.resolver.onParseSourceFile(sourceFile)
        return sourceFile
    }

    private fun parseDeclarations(): List<Declaration> = buildList {
        while (currentToken.kind != tt.EOF) {
            add(parseDeclaration())
        }
    }.toList()

    private fun parseDeclaration(): Declaration {
        val decl = when (currentToken.kind) {
            tt.IMPORT -> parseDeclarationImportAs()
            tt.DEF -> parseDeclarationFunctionDef()
            tt.STRUCT -> parseStructDeclaration()
            tt.EXTERN -> parseExternFunctionDef()
            else -> {
                val location = recoverFromError(Diagnostic.Kind.DeclarationExpected)
                Declaration.Error(location)
            }
        }
        ctx.resolver.onParseDeclaration(decl)
        return decl
    }

    private fun parseStructDeclaration(): Declaration {
        val start = expect(tt.STRUCT)
        val binder = parseBinder()

        expect(tt.LBRACE)
        val members = buildList {
            while (!isEOF() && !at(tt.RBRACE)) {
                add(parseStructMember())
            }
        }
        val stop = expect(tt.RBRACE)

        return Declaration.Struct(
            makeLocation(start, stop),
            binder,
            members
        )
    }

    private fun parseStructMember(): Declaration.Struct.Member = when (currentToken.kind) {
        tt.VAL -> parseValStructMember()
        else -> {
            recoverFromError()
            Declaration.Struct.Member.Error
        }
    }

    private fun parseValStructMember(): Declaration.Struct.Member {
        expect(tt.VAL)
        val binder = parseBinder()
        expect(tt.COLON)
        val annotation = parseTypeAnnotation()
        expect(tt.SEMICOLON)
        return Declaration.Struct.Member.Field(
            binder,
            annotation
        )
    }

    private fun parseExternFunctionDef(): Declaration {
        val start = expect(tt.EXTERN)
        expect(tt.DEF)
        val name = parseBinder()
        val params = buildList {
            expect(tt.LPAREN)
            var isFirst = true
            while (!(at(tt.RPAREN) || at(tt.EOF))) {
                if (!isFirst) {
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
        return Declaration.ExternFunctionDef(
            makeLocation(start, returnType),
            binder = name,
            paramTypes = params,
            returnType = returnType,
            externName = externName
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

        return Declaration.ImportAs(modulePath, asName)
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
        val typeParams = parseOptionalTypeParams()
        val scopeStartToken = expect(tt.LPAREN)
        val params = parseParams(scopeStartToken)
        expect(tt.COLON)
        val annotation = parseTypeAnnotation()
        val block = parseBlock()
        return Declaration.FunctionDef(
            location = makeLocation(start, block),
            name = name,
            scopeStartToken = scopeStartToken,
            typeParams = typeParams,
            params = params,
            returnType = annotation,
            body = block
        )
    }

    private fun parseOptionalTypeParams(): List<TypeParam> = buildList {

        if (currentToken.kind == tt.LSQB) {
            advance()
            var isFirst = true
            while (!isEOF() && !at(tt.RSQB)) {
                if (!isFirst) {
                    expect(tt.COMMA)
                } else {
                    isFirst = false
                }
                val binder = parseBinder()
                add(TypeParam(binder = binder))
            }

            expect(tt.RSQB)
        }
    }

    private fun parseBlock(): Block {
        val start = expect(tt.LBRACE)
        val members = parseBlockMembers()
        val stop = expect(tt.RBRACE)
        val result = Block(makeLocation(start, stop), members)
        ctx.resolver.onParseBlock(result)
        return result
    }

    private fun parseBlockMembers(): List<Block.Member> = buildList {
        while (!(at(tt.RBRACE) || at(tt.EOF))) {
            add(parseBlockMember())
        }
    }

    private fun parseBlockMember(): Block.Member {
        return when (currentToken.kind) {
            tt.RETURN, tt.VAL -> Block.Member.Statement(parseStatement())
            else -> {
                val expr = parseExpression()
                expect(tt.SEMICOLON)
                Block.Member.Expression(expr)
            }
        }
    }

    private fun parseStatement(): Statement {
        return when (currentToken.kind) {
            tt.RETURN -> parseReturnStatement()
            tt.VAL -> parseValStatement()
            else -> {
                val location = recoverFromError(Diagnostic.Kind.StatementExpected, statementRecoveryTokens)
                Statement.Error(location)
            }
        }
    }

    private fun parseReturnStatement(): Statement {
        val start = expect(tt.RETURN)
        val value = parseExpression()
        expect(tt.SEMICOLON)
        return Statement.Return(
            makeLocation(start, value),
            value
        )
    }

    private fun parseValStatement(): Statement {
        val start = expect(tt.VAL)
        val binder = parseBinder()
        val typeAnnotation = parseOptionalAnnotation()
        expect(tt.EQ)
        val rhs = parseExpression()
        expect(tt.SEMICOLON)
        return Statement.Val(
            makeLocation(start, rhs),
            binder,
            typeAnnotation,
            rhs
        )
    }

    private fun parseExpression(): Expression {
        val head = when (currentToken.kind) {
            tt.ID -> parseExpressionVar()
            tt.BYTE_STRING -> parseExpressionByteString()
            tt.TRUE -> {
                Expression.BoolLiteral(advance().location, true)
            }
            tt.FALSE -> {
                Expression.BoolLiteral(advance().location, false)
            }
            else -> {
                val location = advance().location
                ctx.diagnosticReporter.report(location, Diagnostic.Kind.ExpressionExpected)
                Expression.Error(location)
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
        return Expression.ByteString(token.location, bytes.toByteArray())
    }

    private fun parseExpressionTail(head: Expression): Expression {
        return when (currentToken.kind) {
            tt.LPAREN -> {
                advance()
                val args = buildList {
                    var first = true
                    while (!(at(tt.RPAREN) || at(tt.EOF))) {
                        if (!first) {
                            expect(tt.COMMA)
                        } else {
                            first = false
                        }
                        add(parseArg())
                    }
                }
                val stop = expect(tt.RPAREN)
                parseExpressionTail(
                    Expression.Call(
                        makeLocation(head, stop),
                        head,
                        args
                    )
                )
            }
            tt.DOT -> {
                advance()
                val ident = parseIdentifier()
                parseExpressionTail(
                    Expression.Property(
                        makeLocation(head, ident),
                        head,
                        ident
                    )
                )
            }
            else -> head
        }
    }

    private fun parseArg(): Arg = Arg(parseExpression())

    private fun parseExpressionVar(): Expression {
        val identifier = parseIdentifier()
        return Expression.Var(identifier)
    }

    private fun parseParams(lparen: Token? = null): List<Param> = buildList {
        lparen ?: expect(tt.LPAREN)
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
                TypeAnnotation.Var(id)
            }
            tt.STAR -> {
                val start = advance()
                val to = parseTypeAnnotation()
                TypeAnnotation.Ptr(
                    makeLocation(start, to),
                    to
                )
            }
            else -> {
                val location = advance().location
                ctx.diagnosticReporter.report(location, Diagnostic.Kind.TypeAnnotationExpected)
                TypeAnnotation.Error(location)

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

