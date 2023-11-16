package hadesc.parser

import hadesc.ast.*
import hadesc.text.Text
import llvm.makeList
import java.nio.file.Path

internal typealias tt = Token.Kind

private val byteStringEscapes = mapOf(
    'n' to '\n',
    '0' to '\u0000',
    'r' to '\r',
    '\\' to '\\'
)

class Parser(
    file: Path,
    text: Text
) {
    private val tokenBuffer = TokenBuffer(maxLookahead = 4, lexer = Lexer(file, text))
    val currentToken get() = tokenBuffer.currentToken


    inline fun <reified T> parseSeparatedList(
        separator: Token.Kind,
        terminator: Token.Kind,
        parseItem: () -> T
    ): List<T> = makeList {
        var isFirst = true
        while (currentToken.kind != terminator && currentToken.kind != tt.EOF) {
            if (!isFirst) {
                expect(separator)
            }
            isFirst = false
            add(parseItem())
        }
    }

    fun expect(kind: Token.Kind): Token {
        return if (currentToken.kind == kind) {
            advance()
        } else {
            val lastToken = tokenBuffer.lastToken
            if (lastToken != null && kind == tt.SEMICOLON && (
                currentToken.kind == tt.EOF ||
                    currentToken.location.start.line > lastToken.location.start.line
                )
            ) {
                currentToken
            } else {
                TODO("Unexpected token: ${currentToken.kind}; Expected $kind")
            }
        }
    }

    fun advance(): Token {
        return tokenBuffer.advance()
    }

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
        require(offset < maxLookahead) { "Tried to peek past max lookahead $maxLookahead" }
        return buffer[(current + offset) % maxLookahead]
    }
}
