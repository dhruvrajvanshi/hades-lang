package hadesc.parser

import hadesc.ast.Token
import hadesc.location.Position
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import java.io.File

private const val EOF_CHAR = Char.MIN_VALUE

val KEYWORDS = mapOf(
    "import" to tt.IMPORT,
    "def" to tt.DEF,
    "as" to tt.AS,
    "extern" to tt.EXTERN,
    "return" to tt.RETURN,
    "val" to tt.VAL,
    "struct" to tt.STRUCT,
    "true" to tt.TRUE,
    "false" to tt.FALSE,
    "this" to tt.THIS
)

val SINGLE_CHAR_TOKENS = mapOf(
    ';' to tt.SEMICOLON,
    '.' to tt.DOT,
    '(' to tt.LPAREN,
    ')' to tt.RPAREN,
    '{' to tt.LBRACE,
    '}' to tt.RBRACE,
    ',' to tt.COMMA,
    ':' to tt.COLON,
    '=' to tt.EQ,
    '*' to tt.STAR,
    '[' to tt.LSQB,
    ']' to tt.RSQB
)

class Lexer(private val file: SourcePath) {
    // TODO: Handle this during lexing instead of string replace
    private val text: String = File(file.path.toUri()).readText().replace("\r", "")
    private val state = State()

    data class State(
        var startOffset: Int = 0,
        var currentOffset: Int = 0,
        var startLine: Int = 1,
        var lastLine: Int = 1,
        var currentLine: Int = 1,
        var startColumn: Int = 1,
        var lastColumn: Int = 1,
        var currentColumn: Int = 1
    ) {
        fun startPosition(): Position = Position(startLine, startColumn)
        fun stopPosition(): Position = Position(lastLine, lastColumn)
    }

    fun nextToken(): Token {
        skipWhitespace()
        startToken()
        val c = currentChar
        return when {
            c == EOF_CHAR -> makeToken(tt.EOF)
            c.isIdentifierStarter() -> {
                identifierOrKeyword()
            }
            SINGLE_CHAR_TOKENS.containsKey(c) -> {
                advance()
                makeToken(SINGLE_CHAR_TOKENS.getValue(c))
            }
            else -> {
                advance()
                makeToken(Token.Kind.ERROR)
            }
        }
    }

    private fun identifierOrKeyword(): Token {
        val first = advance()
        if (first == 'b' && currentChar == '"') {
            advance()
            while (true) {
                if (currentChar == '"' || currentChar == EOF_CHAR) {
                    advance()
                    break
                } else {
                    advance()
                }
            }
            return makeToken(tt.BYTE_STRING)
        }
        while (currentChar.isIdentifierChar()) {
            advance()
        }
        return makeToken(KEYWORDS[lexeme()] ?: Token.Kind.ID)
    }

    private fun Char.isIdentifierStarter(): Boolean {
        return isLetter() || '_' == this
    }

    private fun Char.isIdentifierChar(): Boolean {
        return isIdentifierStarter() || isDigit()
    }


    private fun skipWhitespace() {
        while (currentChar.isWhitespace()) {
            advance()
        }
    }

    private fun startToken() {
        state.startLine = state.currentLine
        state.startColumn = state.currentColumn
        state.startOffset = state.currentOffset
    }

    private fun advance(): Char {
        val lastChar = currentChar
        if (currentChar == EOF_CHAR) {
            return lastChar
        }
        state.lastLine = state.currentLine
        state.lastColumn = state.currentColumn

        state.currentOffset++

        if (lastChar == '\n') {
            if (currentChar != EOF_CHAR) {
                state.currentLine++
                state.currentColumn = 1
            }
        } else {
            state.currentColumn++
        }
        return lastChar
    }

    private fun makeToken(kind: Token.Kind): Token {
        return Token(
            kind,
            location = SourceLocation(
                file,
                start = state.startPosition(),
                stop = state.stopPosition()
            ),
            text = lexeme()
        )
    }

    private fun lexeme(): String {
        return text.slice(state.startOffset until state.currentOffset)
    }

    private val currentChar
        get() = if (state.currentOffset < text.length) {
            text[state.currentOffset]
        } else {
            EOF_CHAR
        }
}