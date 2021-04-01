package hadesc.parser

import hadesc.ast.*
import hadesc.context.Context
import hadesc.diagnostics.Diagnostic
import hadesc.ir.BinaryOperator
import hadesc.location.HasLocation
import hadesc.location.Position
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.qualifiedname.QualifiedName
import kotlin.math.exp

internal typealias tt = Token.Kind

private val declarationRecoveryTokens = setOf(
        tt.EOF, tt.IMPORT, tt.DEF, tt.EXTERN, tt.STRUCT, tt.CONST,
        tt.TRAIT, tt.IMPLEMENTATION, tt.AT_SYMBOL,
        tt.TYPE, tt.EXTENSION
)
private val statementPredictors = setOf(
    tt.RETURN, tt.VAL, tt.WHILE, tt.IF,
    tt.DEFER
)
private val statementRecoveryTokens: Set<TokenKind> = setOf(tt.EOF, tt.WHILE) + statementPredictors
private val byteStringEscapes = mapOf(
        'n' to '\n',
        '0' to '\u0000',
        'r' to '\r'
)

private val OPERATORS = listOf(
        listOf(tt.PIPELINE),
        listOf(tt.AND, tt.OR),
        listOf(
                tt.LESS_THAN,
                tt.LESS_THAN_EQUAL,
                tt.GREATER_THAN_EQUAL,
                tt.GREATER_THAN),
        listOf(tt.EQEQ, tt.BANG_EQ),
        listOf(tt.PLUS, tt.MINUS),
        listOf(tt.STAR)
)

typealias op = BinaryOperator

private val BINARY_OPERATORS = mapOf(
        tt.LESS_THAN to op.LESS_THAN,
        tt.LESS_THAN_EQUAL to op.LESS_THAN_EQUAL,
        tt.GREATER_THAN to op.GREATER_THAN,
        tt.GREATER_THAN_EQUAL to op.GREATER_THAN_EQUAL,
        tt.PLUS to op.PLUS,
        tt.MINUS to op.MINUS,
        tt.STAR to op.TIMES,
        tt.AND to op.AND,
        tt.OR to op.OR,
        tt.EQEQ to op.EQUALS,
        tt.BANG_EQ to op.NOT_EQUALS
).apply {
    for (tokenType in keys) {
        require(OPERATORS.any { it.contains(tokenType) } || tokenType == tt.PIPELINE) {
            "operator token type $tokenType missing in precedence table"
        }
    }
}

object SyntaxError : Error()

@OptIn(ExperimentalStdlibApi::class)
class Parser(
        private val ctx: Context,
        private val moduleName: QualifiedName,
        private val file: SourcePath
) {
    private val tokenBuffer = TokenBuffer(maxLookahead = 4, lexer = Lexer(file))
    private val currentToken get() = tokenBuffer.currentToken

    fun parseSourceFile(): SourceFile {
        val declarations = parseDeclarations()
        val start = Position(1, 1)
        val location = SourceLocation(file, start, currentToken.location.stop)
        val sourceFile = SourceFile(location, moduleName, declarations)
        ctx.resolver.onParseSourceFile(sourceFile)
        return sourceFile
    }

    private fun parseDeclarations(): List<Declaration> = buildList<Declaration> {
        while (currentToken.kind != tt.EOF) {
            try {
                add(parseDeclaration())
            } catch (e: SyntaxError) {
                recoverFromError(stopBefore = declarationRecoveryTokens)
            }
        }
    }.toList()

    private fun parseDeclaration(): Declaration {
        val decorators = parseDecorators()
        val decl = when (currentToken.kind) {
            tt.IMPORT -> parseImportDeclaration()
            tt.DEF,
            tt.AT_SYMBOL -> parseDeclarationFunctionDef()
            tt.STRUCT -> parseStructDeclaration(decorators = decorators)
            tt.EXTERN -> when(tokenBuffer.peek(1).kind) {
                tt.CONST -> parseExternConstDef()
                else -> parseExternFunctionDef()
            }
            tt.CONST -> parseConstDef()
            tt.TYPE -> parseTypeAliasDeclaration()
            tt.EXTENSION -> parseExtensionDef()
            tt.TRAIT -> parseTraitDef()
            tt.IMPLEMENTATION -> parseImplementationDef()
            tt.SEALED -> parseSealedDef(decorators)
            else -> {
                syntaxError(currentToken.location, Diagnostic.Kind.DeclarationExpected)
            }
        }
        ctx.resolver.onParseDeclaration(decl)
        return decl
    }

    private fun parseExternConstDef(): Declaration {
        val start = expect(tt.EXTERN)
        expect(tt.CONST)
        val name = parseBinder()
        expect(tt.COLON)
        val typeAnnotation = parseTypeAnnotation()
        expect(tt.EQ)
        val externName = parseIdentifier()
        expect(tt.SEMICOLON)
        return Declaration.ExternConst(
            makeLocation(start, externName),
            name,
            typeAnnotation,
            externName,
        )
    }

    private fun parseSealedDef(decorators: List<Decorator>): Declaration {
        val start = expect(tt.SEALED)
        expect(tt.TYPE)
        val name = parseBinder()
        val typeParams = parseOptionalTypeParams()
        expect(tt.LBRACE)
        val cases = buildList {
            while (!at(tt.RBRACE) && !at(tt.EOF)) {
                add(parseSealedTypeCase())
                expect(tt.SEMICOLON)
            }
        }
        val stop = expect(tt.RBRACE)
        return Declaration.SealedType(
            makeLocation(start, stop),
            decorators,
            name,
            typeParams,
            cases
        )
    }

    private fun parseSealedTypeCase(): Declaration.SealedType.Case {
        val name = parseBinder()
        val params = if (at(tt.LPAREN)) {
            advance()
            val list = buildList {
                var first = true
                while (!at(tt.RPAREN) && !at(tt.EOF)) {
                    if (!first) {
                        expect(tt.COMMA)
                    }
                    first = false
                    add(parseParam())
                }
            }
            expect(tt.RPAREN)
            list
        } else null
        return Declaration.SealedType.Case(
            name,
            params
        )
    }

    private fun parseImplementationDef(): Declaration {
        val start = expect(tt.IMPLEMENTATION)
        val typeParams = parseOptionalTypeParams()
        val traitRef = parseQualifiedPath()
        expect(tt.LSQB)
        val traitArguments = parseSeperatedList(tt.COMMA, tt.RSQB) {
            parseTypeAnnotation()
        }
        expect(tt.RSQB)
        val whereClause = parseOptionalWhereClause()
        expect(tt.LBRACE)
        val defs = buildList {
            while (!at(tt.EOF) && !at(tt.RBRACE)) {
                add(parseDeclaration())
            }
        }
        val stop = expect(tt.RBRACE)
        return Declaration.ImplementationDef(
                makeLocation(start, stop),
                typeParams,
                traitRef,
                traitArguments,
                whereClause,
                defs
        )
    }

    private fun parseTraitDef(): Declaration {
        val start = expect(tt.TRAIT)
        val binder = parseBinder()
        val typeParams = parseOptionalTypeParams() ?: emptyList()
        expect(tt.LBRACE)
        val signatures = buildList {
            while (!at(tt.RBRACE) && !at(tt.EOF)) {
                add(parseTraitMember())
                expect(tt.SEMICOLON)
            }
        }
        val stop = expect(tt.RBRACE)
        return Declaration.TraitDef(
                makeLocation(start, stop),
                binder,
                typeParams,
                signatures
        )
    }

    private fun parseTraitMember() = when (currentToken.kind) {
        tt.DEF -> Declaration.TraitMember.Function(parseFunctionSignature())
        else -> {
            expect(tt.TYPE)
            val result = Declaration.TraitMember.AssociatedType(parseBinder())
            expect(tt.SEMICOLON)
            result
        }
    }

    private fun parseExtensionDef(): Declaration.ExtensionDef {
        val start = expect(tt.EXTENSION)
        val binder = parseBinder()
        val typeParams = parseOptionalTypeParams()
        expect(tt.FOR)
        val forType = parseTypeAnnotation()
        val whereClause = parseOptionalWhereClause()
        expect(tt.LBRACE)
        val functions = mutableListOf<Declaration>()
        while (!at(tt.EOF) && !at(tt.RBRACE)) {
            functions.add(parseDeclaration())
        }
        val end = expect(tt.RBRACE)
        return Declaration.ExtensionDef(
                makeLocation(start, end),
                binder,
                typeParams,
                forType,
                whereClause,
                functions
        )
    }

    private fun parseDecorators(): List<Decorator> = buildList {
        while (at(tt.AT_SYMBOL)) {
            val start = advance()
            val name = parseIdentifier()
            val args = if (at(tt.LPAREN)) {
                advance()
                val args = parseSeperatedList(tt.COMMA, tt.RPAREN) { parseIdentifier() }
                expect(tt.RPAREN)
                args
            } else emptyList()
            add(Decorator(makeLocation(start, name), name, args))
        }
    }

    private fun parseOptionalWhereClause(): WhereClause? {
        return if (at(tt.WHERE)) {
            val start = advance()
            val refs = parseSeperatedList(seperator = tt.COMMA, terminator = tt.LBRACE) {
                parseTraitRef()
            }
            val stop: HasLocation = refs.lastOrNull() ?: start
            WhereClause(makeLocation(start, stop), refs)
        } else {
            null
        }
    }

    private fun parseTraitRef(): TraitRequirementAnnotation {
        val path = parseQualifiedPath()
        expect(tt.LSQB)
        val args = parseSeperatedList(tt.COMMA, tt.RSQB) {
            parseTypeAnnotation()
        }
        expect(tt.RSQB)
        return TraitRequirementAnnotation(path, args)
    }

    private fun parseFunctionSignature(): FunctionSignature {
        val start = expect(tt.DEF)
        val binder = parseBinder()
        val typeParams = parseOptionalTypeParams()
        val (thisParamFlags, params) = parseParams()
        expect(tt.COLON)
        val returnType = parseTypeAnnotation()
        val whereClause = parseOptionalWhereClause()
        return FunctionSignature(
            makeLocation(start, returnType),
            binder,
            typeParams,
            thisParamFlags,
            params,
            returnType,
            whereClause
        )
    }

    private fun parseConstDef(): Declaration.ConstDefinition {
        val start = expect(tt.CONST)
        val name = parseBinder()
        val annotation = parseOptionalAnnotation()
        expect(tt.EQ)
        val rhs = parseExpression()
        expect(tt.SEMICOLON)
        return Declaration.ConstDefinition(
                makeLocation(start, rhs),
                name,
                annotation,
                rhs
        )
    }

    private fun parseTypeAliasDeclaration(): Declaration {
        val start = expect(tt.TYPE)
        val binder = parseBinder()
        val params = parseOptionalTypeParams()
        expect(tt.EQ)
        val body = parseTypeAnnotation()
        expect(tt.SEMICOLON)

        return Declaration.TypeAlias(
                makeLocation(start, body),
                binder,
                params,
                body
        )

    }

    private fun parseStructDeclaration(decorators: List<Decorator>): Declaration {
        val start = expect(tt.STRUCT)
        val binder = parseBinder()
        val typeParams = parseOptionalTypeParams()

        expect(tt.LBRACE)
        val members = buildList {
            while (!isEOF() && !at(tt.RBRACE)) {
                add(parseStructMember())
            }
        }
        val stop = expect(tt.RBRACE)

        return Declaration.Struct(
                makeLocation(start, stop),
                decorators,
                binder,
                typeParams,
                members
        )
    }

    private fun parseStructMember(): Declaration.Struct.Member = when (currentToken.kind) {
        tt.VAL -> parseValStructMember()
        else -> {
            syntaxError(currentToken.location, Diagnostic.Kind.DeclarationExpected)
        }
    }

    private fun parseValStructMember(): Declaration.Struct.Member {
        expect(tt.VAL)
        val isMutable = if (at(tt.MUT)) {
            advance()
            true
        } else {
            false
        }
        val binder = parseBinder()
        expect(tt.COLON)
        val annotation = parseTypeAnnotation()
        expect(tt.SEMICOLON)
        return Declaration.Struct.Member.Field(
                binder,
                isMutable,
                annotation
        )
    }

    private fun parseExternFunctionDef(): Declaration {
        val start = expect(tt.EXTERN)
        expect(tt.DEF)
        val name = parseBinder()
        expect(tt.LPAREN)
        val params = parseSeperatedList(seperator = tt.COMMA, terminator = tt.RPAREN) {
            parseTypeAnnotation()
        }
        expect(tt.RPAREN)
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

    private fun parseImportDeclaration(): Declaration {
        val start = expect(tt.IMPORT)
        val prefix = mutableListOf(parseIdentifier())

        while (at(tt.DOT)) {
            advance()
            if (at(tt.LBRACE)) {
                break
            }
            prefix.add(parseIdentifier())
        }

        val modulePath = QualifiedPath(prefix)
        if (at(tt.LBRACE)) {
            advance()
            var isFirst = true
            val names = mutableListOf<Binder>()
            while (!at(tt.RBRACE) && !at(tt.EOF)) {
                if (!isFirst) {
                    expect(tt.COMMA)
                }
                names.add(parseBinder())
                isFirst = false
            }
            val end = expect(tt.RBRACE)
            expect(tt.SEMICOLON)
            return Declaration.ImportMembers(makeLocation(start, end), modulePath, names)

        } else {
            expect(tt.AS)
            val asName = parseBinder()
            expect(Token.Kind.SEMICOLON)

            return Declaration.ImportAs(modulePath, asName)
        }

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

    private fun parseDeclarationFunctionDef(): Declaration.FunctionDef {
        val externName = if (at(tt.AT_SYMBOL)) {
            advance()
            expect(tt.EXTERN)
            expect(tt.LPAREN)
            val name = parseIdentifier()
            expect(tt.RPAREN)
            name
        } else null
        val signature = parseFunctionSignature()
        val body = parseBlock()
        return Declaration.FunctionDef(
                location = makeLocation(signature, body),
                externName = externName,
                signature = signature,
                body = body
        )
    }

    private fun parseOptionalTypeParams(): List<TypeParam>? = if (currentToken.kind == tt.LSQB) {
        advance()
        val list: List<TypeParam> = parseSeperatedList(tt.COMMA, tt.RSQB) {
            parseTypeParam()
        }
        expect(tt.RSQB)
        list
    } else {
        null
    }

    private fun parseTypeParam(): TypeParam {
        val binder = parseBinder()
        return TypeParam(binder)
    }

    private fun parseBlock(): Block {
        val start = expect(tt.LBRACE)
        val members = parseBlockMembers()
        val stop = expect(tt.RBRACE)
        val result = Block(makeLocation(start, stop), startToken = start, members)
        ctx.resolver.onParseBlock(result)
        return result
    }

    private fun parseBlockMembers(): List<Block.Member> = buildList {
        while (!(at(tt.RBRACE) || at(tt.EOF))) {
            try {
                add(parseBlockMember())
            } catch (e: SyntaxError) {
                recoverFromError(statementRecoveryTokens)
            }
        }
    }

    private fun parseBlockMember(): Block.Member {
        return when {
            isStatementPredicted() -> Block.Member.Statement(parseStatement())
            else -> {
                val expr = parseExpression()
                if (expr is Expression.Deref && at(Token.Kind.EQ)) {
                    advance()
                    val rhs = parseExpression()
                    expect(tt.SEMICOLON)
                    val statement = Statement.PointerAssignment(
                        makeLocation(expr, rhs),
                        expr,
                        rhs
                    )
                    Block.Member.Statement(
                        statement
                    )
                } else {
                    expect(tt.SEMICOLON)
                    Block.Member.Expression(expr)
                }
            }
        }
    }

    private fun isStatementPredicted(): Boolean {
        return currentToken.kind in statementPredictors
                // LocalAssignment
                || (currentToken.kind == TokenKind.ID && tokenBuffer.peek(1).kind == TokenKind.EQ)
                || isPropertyAssignmentPredicted()
    }

    private fun isPropertyAssignmentPredicted(): Boolean {
        return currentToken.kind == TokenKind.ID &&
                tokenBuffer.peek(1).kind == TokenKind.DOT &&
                tokenBuffer.peek(2).kind == TokenKind.ID &&
                tokenBuffer.peek(3).kind == TokenKind.EQ
    }

    private fun parseStatement(): Statement {
        return when (currentToken.kind) {
            tt.RETURN -> parseReturnStatement()
            tt.VAL -> parseValStatement()
            tt.WHILE -> parseWhileStatement()
            tt.IF -> parseIfStatement()
            tt.ID -> {
                if (tokenBuffer.peek(1).kind == tt.DOT) {
                    parseMemberAssignment()
                } else {
                    parseLocalAssignment()
                }
            }
            tt.DEFER -> parseDeferStatement()
            else -> {
                syntaxError(currentToken.location, Diagnostic.Kind.StatementExpected)
            }
        }
    }

    private fun parseDeferStatement(): Statement {
        val start = expect(tt.DEFER)
        val member = parseBlockMember()
        return Statement.Defer(
            makeLocation(start, member),
            member
        )
    }

    private fun parseMemberAssignment(): Statement {
        val lhs = parseExpression()
        expect(tt.EQ)
        val rhs = parseExpression()
        expect(tt.SEMICOLON)

        if (lhs !is Expression.Property) {
            return syntaxError(lhs.location, Diagnostic.Kind.NotAStructField)
        }
        return Statement.MemberAssignment(
            makeLocation(lhs, rhs),
            lhs,
            rhs
        )

    }

    private fun parseLocalAssignment(): Statement {
        val name = parseIdentifier()
        expect(tt.EQ)
        val value = parseExpression()
        expect(tt.SEMICOLON)
        return Statement.LocalAssignment(
                makeLocation(name, value),
                name,
                value
        )
    }

    private fun parseIfStatement(): Statement {
        val start = expect(tt.IF)
        val condition = parseExpression()
        val ifTrue = parseBlock()
        val ifFalse = if (at(tt.ELSE)) {
            advance()
            parseBlock()
        } else {
            null
        }
        return Statement.If(
                location = makeLocation(start, ifFalse ?: ifTrue),
                condition = condition,
                ifTrue = ifTrue,
                ifFalse = ifFalse
        )
    }

    private fun parseWhileStatement(): Statement {
        val start = expect(tt.WHILE)
        val condition = parseExpression()
        val block = parseBlock()
        return Statement.While(
                makeLocation(start, block),
                condition,
                block
        )
    }

    private fun parseReturnStatement(): Statement {
        val start = expect(tt.RETURN)
        val value = if (at(tt.SEMICOLON))
            null
        else parseExpression()
        expect(tt.SEMICOLON)
        return Statement.Return(
                makeLocation(start, value ?: start),
                value
        )
    }

    private fun parseValStatement(): Statement {
        val start = expect(tt.VAL)
        val isMutable = if (at(tt.MUT)) {
            advance()
            true
        } else {
            false
        }
        val binder = parseBinder()
        val typeAnnotation = parseOptionalAnnotation()
        expect(tt.EQ)
        val rhs = parseExpression()
        expect(tt.SEMICOLON)
        return Statement.Val(
                makeLocation(start, rhs),
                isMutable,
                binder,
                typeAnnotation,
                rhs
        )
    }

    private fun parseExpression(): Expression {
        return parseExpressionMinPrecedence(0)
    }

    private fun parseExpressionMinPrecedence(minPrecedence: Int): Expression {
        return if (minPrecedence == OPERATORS.size) {
            parsePrimaryExpression()
        } else {
            var currentExpression = parseExpressionMinPrecedence(minPrecedence + 1)
            while (currentToken.kind in OPERATORS[minPrecedence]
                // operators are only allowed on the same line as the left hand side expression
                && currentExpression.location.stop.line == currentToken.location.start.line
            ) {
                val opToken = advance()
                currentExpression = makeBinOp(
                        currentExpression,
                        opToken.kind,
                        parseExpressionMinPrecedence(minPrecedence + 1)
                )
            }

            currentExpression
        }
    }

    private fun makeBinOp(lhs: Expression, operatorToken: TokenKind, rhs: Expression): Expression {
        if (operatorToken == tt.PIPELINE) {
            return Expression.PipelineOperator(
                makeLocation(lhs, rhs),
                lhs,
                rhs
            )
        }
        val operator = requireNotNull(BINARY_OPERATORS[operatorToken]) {
            "Bug: Token type not found in binary operators table $operatorToken"
        }
        return Expression.BinaryOperation(
                makeLocation(lhs, rhs),
                lhs,
                operator,
                rhs
        )
    }

    private fun parsePrimaryExpression(withTail: Boolean = true, allowCalls: Boolean = true): Expression {
        val head = when (currentToken.kind) {
            tt.LPAREN -> {
                advance()
                val result = parseExpression()
                expect(tt.RPAREN)
                result
            }
            tt.ID -> parseExpressionVar()
            tt.BYTE_STRING -> parseExpressionByteString()
            tt.NULLPTR -> {
                Expression.NullPtr(advance().location)
            }
            tt.INT_LITERAL -> {
                val token = advance()
                Expression.IntLiteral(token.location, token.text.toInt())
            }
            tt.HEX_INT_LITERAL -> {
                val token = advance()
                require(token.text.startsWith("0x"))
                Expression.IntLiteral(token.location, token.text.drop(2).toInt(16))
            }
            tt.TRUE -> {
                Expression.BoolLiteral(advance().location, true)
            }
            tt.FALSE -> {
                Expression.BoolLiteral(advance().location, false)
            }
            tt.NOT -> {
                val start = advance()
                val expression = parsePrimaryExpression()
                Expression.Not(makeLocation(start, expression), expression)
            }
            tt.SIZE_OF -> {
                val start = advance()
                expect(tt.LSQB)
                val type = parseTypeAnnotation()
                val stop = expect(tt.RSQB)
                Expression.SizeOf(makeLocation(start, stop), type)
            }
            tt.AMPERSAND -> {
                val start = advance()
                val isMut = if (at(tt.MUT)) {
                    advance()
                    true
                } else false
                val expression = parsePrimaryExpression(withTail = false)
                if (isMut) {
                    Expression.AddressOfMut(makeLocation(start, expression), expression)
                } else {
                    Expression.AddressOf(makeLocation(start, expression), expression)
                }
            }
            tt.STAR -> {
                val start = advance()
                // *x.y + z
                // should be parsed as (*(x.y)) + z
                // that's why this is parsePrimaryExpression() not parseExpression()
                val expression = parsePrimaryExpression()
                Expression.Deref(makeLocation(start, expression), expression)
            }
            tt.POINTER_CAST -> {
                val start = advance()
                expect(tt.LSQB)
                val toType = parseTypeAnnotation()
                expect(tt.RSQB)
                expect(tt.LPAREN)
                val arg = parseExpression()
                val stop = expect(tt.RPAREN)
                Expression.PointerCast(
                        makeLocation(start, stop),
                        toType,
                        arg
                )

            }
            tt.UNSAFE_CAST -> {
                val start = advance()
                expect(tt.LSQB)
                val toType = parseTypeAnnotation()
                expect(tt.RSQB)
                expect(tt.LPAREN)
                val arg = parseExpression()
                val stop = expect(tt.RPAREN)
                Expression.UnsafeCast(
                    makeLocation(start, stop),
                    toType,
                    arg
                )
            }
            tt.IF -> {
                val start = advance()
                expect(tt.LPAREN)
                val condition = parseExpression()
                expect(tt.RPAREN)
                val trueBranch = parseExpression()
                expect(tt.ELSE)
                val falseBranch = parseExpression()
                Expression.If(
                        makeLocation(start, falseBranch),
                        condition,
                        trueBranch,
                        falseBranch
                )
            }
            tt.NEW -> {
                val start = advance()
                val path = parseQualifiedPath()
                val typeArgs = if (at(tt.LSQB)) {
                    advance()
                    val args = parseSeperatedList(tt.COMMA, tt.RSQB) {
                        parseTypeAnnotation()
                    }
                    expect(tt.RSQB)
                    args
                } else null
                expect(tt.LPAREN)
                val args = parseSeperatedList(tt.COMMA, tt.RPAREN) { parseArg() }
                val stop = expect(tt.RPAREN)
                Expression.New(
                        makeLocation(start, stop),
                        path,
                        typeArgs,
                        args
                )
            }
            tt.THIS -> {
                Expression.This(advance().location)
            }
            tt.VBAR -> parseClosureExpression()
            tt.WHEN -> parseWhenExpression()
            tt.REF -> parseRefExpression()
            tt.MOVE -> {
                val start = advance()
                val expression = parseExpression()
                Expression.Move(
                    makeLocation(start, expression),
                    expression
                )
            }
            else -> {
                val location = advance().location
                syntaxError(location, Diagnostic.Kind.ExpressionExpected)
            }
        }
        if (!withTail) return head
        return parseExpressionTail(head, allowCalls)
    }

    private fun parseRefExpression(): Expression {
        val start = expect(tt.REF)
        val isMutable =
            if (at(tt.MUT)) {
                advance()
                true
            } else false
        val value = parsePrimaryExpression(allowCalls = false)
        return Expression.Ref(
            makeLocation(start, value),
            isMutable,
            value
        )
    }

    private fun parseWhenExpression(): Expression {
        val start = expect(tt.WHEN)
        val value = parseExpression()
        expect(tt.LBRACE)
        val arms = buildList {
            var first = true
            while (!at(tt.RBRACE) && !at(tt.EOF)) {
                if (!first) {
                    expect(tt.COMMA)
                }
                first = false
                add(parseWhenArm())
            }
        }
        val stop = expect(tt.RBRACE)
        val result = Expression.When(
            makeLocation(start, stop),
            value,
            arms
        )
        ctx.resolver.onParseWhenExpression(result)
        return result
    }

    private fun parseWhenArm(): Expression.WhenArm {
        val result = if (currentToken.kind == tt.IS) {
            advance()
            val name = if (tokenBuffer.peek(1).kind == tt.COLON) {
                val binder = parseBinder()
                expect(tt.COLON)
                binder
            } else {
                null
            }
            val caseName = parseIdentifier()
            expect(tt.ARROW)
            val value = parseExpression()
            Expression.WhenArm.Is(
                name,
                caseName,
                value
            )
        } else {
            expect(tt.ELSE)
            expect(tt.ARROW)
            val value = parseExpression()
            Expression.WhenArm.Else(value)
        }
        ctx.resolver.onParseWhenArm(result)
        return result
    }

    private fun parseClosureExpression(): Expression {
        val start = expect(tt.VBAR)
        val params = parseSeperatedList(seperator = tt.COMMA, terminator = tt.VBAR) { parseParam() }
        expect(tt.VBAR)
        val returnType = parseOptionalAnnotation()
        val body = if (at(tt.LBRACE)) {
            ClosureBody.Block(parseBlock())
        } else {
            if (at(tt.ARROW)) advance()
            ClosureBody.Expression(parseExpression())
        }

        val closure = Expression.Closure(
            makeLocation(start, body),
            params,
            returnType,
            body
        )
        ctx.resolver.onParseClosure(closure)
        return closure
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

    private fun parseExpressionTail(head: Expression, allowCalls: Boolean = true): Expression {
        if (at(tt.LPAREN) && !allowCalls) return head
        if (at(tt.LSQB) && !allowCalls) return head
        return when (currentToken.kind) {
            tt.LSQB -> {
                advance()
                val typeArgs = parseSeperatedList(tt.COMMA, tt.RSQB) {
                    parseTypeAnnotation()
                }
                val rsqb = expect(tt.RSQB)
                if (at(tt.LPAREN)) {
                    expect(tt.LPAREN)
                    val args = parseSeperatedList(tt.COMMA, tt.RPAREN) {
                        parseArg()
                    }
                    val stop = expect(tt.RPAREN)
                    parseExpressionTail(
                            Expression.Call(
                                    makeLocation(head, stop),
                                    typeArgs,
                                    head,
                                    args
                            )
                    )
                } else {
                    parseExpressionTail(Expression.TypeApplication(
                            makeLocation(head, rsqb),
                            head,
                            typeArgs
                    ))
                }
            }
            tt.VBAR -> {
                val arg = parseArg()
                val call = Expression.Call(
                    location = makeLocation(head, arg),
                    typeArgs = null,
                    callee = head,
                    args = listOf(arg)
                )
                if (at(tt.DOT)) {
                    parseExpressionTail(call)
                } else {
                    call
                }
            }
            tt.LPAREN -> {
                advance()
                val args = parseSeperatedList(tt.COMMA, tt.RPAREN) {
                    parseArg()
                }
                val stop = expect(tt.RPAREN)
                if (at(tt.VBAR)) {
                    val lambda = parseExpression()
                    Expression.Call(
                        makeLocation(head, lambda),
                        null,
                        head,
                        args + Arg(lambda)
                    )
                } else {
                    parseExpressionTail(
                        Expression.Call(
                            makeLocation(head, stop),
                            null,
                            head,
                            args
                        )
                    )
                }
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
            tt.AS -> {
                advance()
                val type = parseTypeAnnotation()
                Expression.As(
                    makeLocation(head, type),
                    head,
                    type,
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

    private fun parseParams(lparen: Token? = null): Pair<FunctionSignature.ThisParamFlags?, List<Param>> {
        var hasThis = false
        var isThisPtr = false
        var isThisRef = false
        var isThisMut = false
        val params = buildList {
            lparen ?: expect(tt.LPAREN)
            var first = true
            while (!(at(tt.RPAREN) || at(tt.EOF))) {
                if (!first) {
                    expect(tt.COMMA)
                } else {
                    first = false
                    if (at(tt.STAR)) {
                        advance()
                        isThisPtr = true
                        hasThis = true
                        if (at(tt.MUT)) {
                            isThisMut = true
                            advance()
                        }
                        expect(tt.THIS)
                        continue
                    } else if (at(tt.REF)) {
                        advance()
                        isThisRef = true
                        hasThis = true
                        if (at(tt.MINUS)) {
                            isThisMut = true
                            advance()
                        }
                        expect(tt.THIS)
                        continue
                    }
                    if (at(tt.THIS)) {
                        hasThis = true
                        advance()
                        continue
                    }
                }
                add(parseParam())
            }
            expect(tt.RPAREN)
        }
        val thisParamFlags = if (hasThis) {
            FunctionSignature.ThisParamFlags(
                    isPointer = isThisPtr,
                    isMutable = isThisMut,
                    isRef = isThisRef,
            )
        } else null
        return thisParamFlags to params
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
        val head = when (currentToken.kind) {
            tt.ID -> {
                val id = parseIdentifier()
                if (at(tt.DOT)) {
                    advance()
                    val second = parseIdentifier()
                    val path = QualifiedPath(listOf(id, second))
                    TypeAnnotation.Qualified(makeLocation(id, second), path)
                } else {
                    TypeAnnotation.Var(id)
                }
            }
            tt.STAR -> {
                val start = advance()
                val isMutable = if (at(tt.MUT)) {
                    advance()
                    true
                } else false
                val to = parseTypeAnnotation()
                if (isMutable) {
                    TypeAnnotation.MutPtr(
                        makeLocation(start, to),
                        to
                    )
                } else {
                    TypeAnnotation.Ptr(
                        makeLocation(start, to),
                        to
                    )
                }
            }
            tt.VBAR -> {
                val start = advance()
                val from = parseSeperatedList(tt.COMMA, terminator = tt.VBAR) {
                    parseTypeAnnotation()
                }
                expect(tt.VBAR)
                expect(tt.ARROW)
                val to = parseTypeAnnotation()
                TypeAnnotation.Function(
                        makeLocation(start, to),
                        from,
                        to
                )
            }
            tt.LPAREN -> {
                advance()
                val annotation = parseTypeAnnotation()
                expect(tt.RPAREN)
                annotation
            }
            tt.UNION -> {
                val start = advance()
                expect(tt.LSQB)
                val args = parseSeperatedList(tt.COMMA, terminator = tt.RSQB) {
                    parseTypeAnnotation()
                }
                val stop = expect(tt.RSQB)
                TypeAnnotation.Union(
                        makeLocation(start, stop),
                        args
                )
            }
            tt.REF -> {
                val start = advance()
                val isMutable =
                    if (at(tt.MUT)) {
                        advance()
                        true
                    } else false
                val inner = parseTypeAnnotation()
                TypeAnnotation.Ref(
                    makeLocation(start, inner),
                    isMutable,
                    inner
                )
            }
            else -> {
                val location = advance().location
                syntaxError(location, Diagnostic.Kind.TypeAnnotationExpected)
            }
        }
        return parseTypeAnnotationTail(head)
    }

    private fun <T> syntaxError(location: SourceLocation, kind: Diagnostic.Kind): T {
        ctx.diagnosticReporter.report(location, kind)
        throw SyntaxError
    }

    private fun parseTypeAnnotationTail(head: TypeAnnotation): TypeAnnotation {
        return when (currentToken.kind) {
            tt.LSQB -> {
                advance()
                val args = parseSeperatedList(seperator = tt.COMMA, terminator = tt.RSQB) {
                    parseTypeAnnotation()
                }
                val end = expect(tt.RSQB)
                parseTypeAnnotationTail(TypeAnnotation.Application(
                        makeLocation(head, end),
                        head,
                        args
                ))
            }
            tt.DOT -> {
                advance()
                val identifier = parseIdentifier()
                parseTypeAnnotationTail(TypeAnnotation.Select(
                    makeLocation(head, identifier),
                    head,
                    identifier
                ))
            }
            else -> head
        }
    }

    private fun <T> parseSeperatedList(
            seperator: Token.Kind,
            terminator: Token.Kind,
            parseItem: () -> T
    ): List<T> = buildList {
        var isFirst = true
        while (currentToken.kind != terminator && currentToken.kind != tt.EOF) {
            if (!isFirst) {
                expect(seperator)
            }
            isFirst = false
            add(parseItem())
        }
    }

    private fun expect(kind: Token.Kind): Token {
        return if (currentToken.kind == kind) {
            advance()
        } else {
            val lastToken = tokenBuffer.lastToken
            if (lastToken != null && kind == tt.SEMICOLON && (
                        currentToken.kind == tt.EOF ||
                        currentToken.location.start.line > lastToken.location.start.line)) {
                currentToken
            } else {
                syntaxError(currentToken.location, Diagnostic.Kind.UnexpectedToken(kind, currentToken))
            }
        }
    }

    private fun at(kind: tt): Boolean = currentToken.kind == kind

    private fun recoverFromError(
            stopBefore: Set<tt> = declarationRecoveryTokens
    ) {
        advance()
        while (true) {
            if (isEOF()) {
                break
            } else if (currentToken.kind == Token.Kind.SEMICOLON) {
                advance()
                break
            } else if (stopBefore.contains(currentToken.kind)) {
                break
            } else {
                advance()
            }
        }
    }

    private fun advance(): Token {
        return tokenBuffer.advance()
    }

    private fun isEOF() =
            currentToken.kind == tt.EOF
}

class TokenBuffer(private val maxLookahead: Int, private val lexer: Lexer) {
    private val buffer: Array<Token> = Array(maxLookahead) { lexer.nextToken() }

    private var current = 0

    private var _lastToken: Token? = null

    val lastToken get() = _lastToken

    val currentToken: Token get() {
        return buffer[current]
    }


    fun advance(): Token {
        val result = currentToken
        buffer[current] = lexer.nextToken()
        current = (current + 1) % maxLookahead
        _lastToken = result
        return result
    }

    fun peek(offset: Int): Token {
        require(offset < maxLookahead) {"Tried to peek past max lookahead $maxLookahead"}
        return buffer[(current + offset) % maxLookahead]
    }

}

