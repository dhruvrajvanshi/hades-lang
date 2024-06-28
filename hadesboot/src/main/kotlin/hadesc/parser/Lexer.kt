package hadesc.parser

import hadesc.ast.Token
import hadesc.location.Position
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.text.Text

private const val EOF_CHAR = Char.MIN_VALUE

val KEYWORDS = mapOf(
    "import" to tt.IMPORT,
    "fn" to tt.FN,
    "as" to tt.AS,
    "extern" to tt.EXTERN,
    "return" to tt.RETURN,
    "val" to tt.VAL,
    "let" to tt.VAL,
    "struct" to tt.STRUCT,
    "true" to tt.TRUE,
    "false" to tt.FALSE,
    "nullptr" to tt.NULLPTR,
    "while" to tt.WHILE,
    "not" to tt.NOT,
    "and" to tt.AND,
    "or" to tt.OR,
    "if" to tt.IF,
    "else" to tt.ELSE,
    "const" to tt.CONST,
    "size_of" to tt.SIZE_OF,
    "align_of" to tt.ALIGN_OF,
    "pointer_cast" to tt.POINTER_CAST,
    "mut" to tt.MUT,
    "interface" to tt.INTERFACE,
    "implementation" to tt.IMPLEMENTATION,
    "for" to tt.FOR,
    "is" to tt.IS,
    "enum" to tt.ENUM,
    "union" to tt.UNION,
    "defer" to tt.DEFER,
    "type" to tt.TYPE,
    "where" to tt.WHERE,
    "this" to tt.THIS,
    "extension" to tt.EXTENSION,
    "match" to tt.MATCH,
    "@" to tt.AT_SYMBOL,
    "@intrinsic" to tt.AT_INTRINSIC,
    "move" to tt.MOVE,
    "ref" to tt.REF,
    "array" to tt.ARRAY
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
    '*' to tt.STAR,
    '[' to tt.LSQB,
    ']' to tt.RSQB,
    '&' to tt.AMPERSAND,
    '/' to tt.SLASH,
    '%' to tt.PERCENT
)

class Lexer(private val file: SourcePath, text: Text) {
    private val state = State(text)

    class State(input: Text) {
        private val iter = input.iterator()
        var currentChar = iter.nextOrEOFChar()
        // Parsing // comments needs one token of lookahead
        var nextChar = iter.nextOrEOFChar()
        var lexeme = ""
        private var startLine: Int = 1
        private var startColumn: Int = 1
        private var currentLine: Int = 1
        private var currentColumn: Int = 1

        fun startPosition(): Position = Position(startLine, startColumn)
        fun stopPosition(): Position = Position(currentLine, currentColumn)

        fun startToken() {
            startLine = currentLine
            startColumn = currentColumn
            lexeme = ""
        }

        fun advance(): Char {
            val result = currentChar
            currentChar = nextChar
            nextChar = iter.nextOrEOFChar()
            if (result == '\n') {
                currentLine++
                currentColumn = 1
            } else {
                currentColumn++
            }
            lexeme += result
            return result
        }
    }

    fun nextToken(): Token {
        skipWhitespace()
        if (currentChar == '/' && state.nextChar == '/') {
            advance()
            advance()
            while (currentChar != '\n' && currentChar != EOF_CHAR) {
                advance()
            }
            advance()
            return nextToken()
        }
        startToken()
        val c = currentChar
        return when {
            c == EOF_CHAR -> makeToken(tt.EOF)
            c.isIdentifierStarter() -> {
                identifierOrKeyword()
            }
            c == '#' -> {
                advance()
                while (currentChar.isIdentifierChar()) {
                    advance()
                }
                makeToken(tt.PRAGMA)
            }
            c == '=' -> {
                advance()
                if (currentChar == '=') {
                    advance()
                    makeToken(tt.EQEQ)
                } else {
                    makeToken(tt.EQ)
                }
            }
            c == '>' -> {
                advance()
                if (currentChar == '=') {
                    advance()
                    makeToken(tt.GREATER_THAN_EQUAL)
                } else {
                    makeToken(tt.GREATER_THAN)
                }
            }
            c == '<' -> {
                advance()
                if (currentChar == '=') {
                    advance()
                    makeToken(tt.LESS_THAN_EQUAL)
                } else {
                    makeToken(tt.LESS_THAN)
                }
            }
            c == '+' -> {
                advance()
                makeToken(tt.PLUS)
            }
            c == '-' -> {
                advance()
                when {
                    currentChar == '>' -> {
                        advance()
                        makeToken(tt.ARROW)
                    }
                    currentChar.isDigit() -> {
                        intLiteral()
                    }
                    else -> {
                        makeToken(tt.MINUS)
                    }
                }
            }
            c == '!' -> {
                advance()
                if (currentChar == '=') {
                    advance()
                    makeToken(tt.BANG_EQ)
                } else {
                    makeToken(tt.ERROR)
                }
            }
            c == '|' -> {
                advance()
                if (currentChar == '>') {
                    advance()
                    makeToken(tt.PIPELINE)
                } else {
                    makeToken(tt.VBAR)
                }
            }
            SINGLE_CHAR_TOKENS.containsKey(c) -> {
                advance()
                makeToken(SINGLE_CHAR_TOKENS.getValue(c))
            }
            c.isDigit() -> {
                intLiteral()
            }
            else -> {
                advance()
                makeToken(Token.Kind.ERROR)
            }
        }
    }

    private fun intLiteral(): Token {
        val first = advance()
        if (first == '0' && currentChar == 'x') {
            advance()
            advance()
            while (currentChar.isDigit() || currentChar in CharRange('A', 'F') || currentChar in CharRange('a', 'f')) {
                advance()
            }
            return makeToken(tt.HEX_INT_LITERAL)
        }
        while (currentChar.isDigit()) {
            advance()
        }
        if (currentChar == '.') {
            advance()
            while (currentChar.isDigit()) {
                advance()
            }
            return makeToken(tt.FLOAT_LITERAL)
        } else {
            return makeToken(tt.INT_LITERAL)
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
        } else if (first == 'b' && currentChar == '\'') {
            advance()
            while (currentChar != '\'' && currentChar != EOF_CHAR) {
                advance()
            }
            if (currentChar == '\'') advance()
            return makeToken(tt.BYTE_CHAR_LITERAL)
        }
        while (currentChar.isIdentifierChar()) {
            advance()
        }
        return makeToken(KEYWORDS[lexeme()] ?: Token.Kind.ID)
    }

    private fun Char.isIdentifierStarter(): Boolean {
        return isLetter() || '_' == this || '@' == this
    }

    private fun Char.isIdentifierChar(): Boolean {
        return isIdentifierStarter() || isDigit()
    }

    private fun skipWhitespace() {
        while (currentChar.isWhitespace()) {
            advance()
        }
    }

    private fun startToken() = state.startToken()

    private fun advance(): Char = state.advance()

    private fun makeToken(kind: Token.Kind): Token {
        return Token(
            kind,
            location = SourceLocation(
                file,
                state.startPosition(),
                state.stopPosition()
            ),
            text = lexeme()
        )
    }

    private fun lexeme(): String = state.lexeme

    private val currentChar
        get() = state.currentChar
}

private fun Iterator<Char>.nextOrEOFChar(): Char = if (hasNext()) next() else EOF_CHAR
