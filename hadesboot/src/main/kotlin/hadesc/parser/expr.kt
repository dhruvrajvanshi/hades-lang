package hadesc.parser

import hadesc.ast.TokenKind
import hadesc.location.SourceLocation

sealed interface Expression {
    val location: SourceLocation
}
private fun Parser.parseExpression(): Expression {
    return parseExpressionMinPrecedence(0)
}

private fun Parser.parseExpressionMinPrecedence(minPrecedence: Int): Expression {
    return if (minPrecedence == OPERATORS.size) {
        parsePrimaryExpression()
    } else {
        var currentExpression = parseExpressionMinPrecedence(minPrecedence + 1)
        while (currentToken.kind in OPERATORS[minPrecedence] &&
            // operators are only allowed on the same line as the left hand side expression
            currentExpression.location.stop.line == currentToken.location.start.line
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

private fun makeBinOp(lhs: Expression, operatorToken: TokenKind, rhs: Expression): Expression = TODO()
private fun parsePrimaryExpression(withTail: Boolean = true, allowCalls: Boolean = true): Expression = TODO()


private val OPERATORS = listOf(
    listOf(tt.AND, tt.OR),
    listOf(
        tt.LESS_THAN,
        tt.LESS_THAN_EQUAL,
        tt.GREATER_THAN_EQUAL,
        tt.GREATER_THAN
    ),
    listOf(tt.EQEQ, tt.BANG_EQ),
    listOf(tt.PLUS, tt.MINUS),
    listOf(tt.STAR, tt.SLASH, tt.PERCENT)
)

enum class BinaryOperator {
    PLUS,
    MINUS,
    TIMES,
    DIV,
    AND,
    OR,
    EQUALS,
    NOT_EQUALS,
    REM,
    LESS_THAN,
    LESS_THAN_EQUAL,
    GREATER_THAN,
    GREATER_THAN_EQUAL,
}
typealias op = BinaryOperator

private val BINARY_OPERATORS = mapOf(
    tt.LESS_THAN to op.LESS_THAN,
    tt.LESS_THAN_EQUAL to op.LESS_THAN_EQUAL,
    tt.GREATER_THAN to op.GREATER_THAN,
    tt.GREATER_THAN_EQUAL to op.GREATER_THAN_EQUAL,
    tt.PLUS to op.PLUS,
    tt.MINUS to op.MINUS,
    tt.STAR to op.TIMES,
    tt.SLASH to op.DIV,
    tt.AND to op.AND,
    tt.OR to op.OR,
    tt.EQEQ to op.EQUALS,
    tt.BANG_EQ to op.NOT_EQUALS,
    tt.PERCENT to op.REM
).apply {
    for (tokenType in keys) {
        require(OPERATORS.any { it.contains(tokenType) }) {
            "operator token type $tokenType missing in precedence table"
        }
    }
}