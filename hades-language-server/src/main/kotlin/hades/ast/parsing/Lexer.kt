package hades.ast.parsing

import hades.URI
import hades.ast.Position
import hades.ast.SourceRange
import hades.ast.Span
import hades.diagnostics.Diagnostic
import hades.diagnostics.DiagnosticKind
import hades.languageserver.logging.logger

private const val EOF_CHAR = Char.MIN_VALUE
private typealias T = Token.Kind

private val KEYWORDS: Map<String, T> = mutableMapOf(
    "import" to T.IMPORT,
    "as" to T.AS,
    "def" to T.DEF,
    "val" to T.VAL,
    "defer" to T.DEFER,
)

private val SINGLE_CHAR_TOKENS: Map<Char, T> = mutableMapOf(
    '.' to T.DOT,
    ';' to T.SEMICOLON,
    '*' to T.STAR,
    '(' to T.LPAREN,
    ')' to T.RPAREN,
    ':' to T.COLON,
    '{' to T.LBRACE,
    '}' to T.RBRACE,
    ',' to T.COMMA,
)

class Lexer(
    private val file: URI,
    private val input: String,
    private val diagnostic: (Diagnostic) -> Unit,
) {
    private val log = logger()
    private var startIndex = 0
    private var currentIndex = 0
    private var startLine = 1
    private var startColumn = 1
    private var lastLine = 1
    private var lastColumn = 1
    private var currentLine = 1
    private var currentColumn = 1
    val newlineOffsets = mutableListOf<Int>()


    fun nextToken(): Token {
        skipWhiteSpace()
        startToken()
        return when (val startChar = currentChar) {
            EOF_CHAR -> makeToken(Token.Kind.EOF)
            '=' -> {
                advance()
                if (currentChar == '=') {
                    advance()
                    makeToken(T.EQUALEQUAL)
                } else {
                    makeToken(T.EQUAL)
                }
            }
            else -> when {
                startChar.isIdentifierChar -> identifierOrKeyword()
                SINGLE_CHAR_TOKENS.containsKey(startChar) -> {
                    advance()
                    makeToken(requireNotNull(SINGLE_CHAR_TOKENS[startChar]))
                }
                else -> {
                    while (!currentChar.isWhitespace() && currentChar != EOF_CHAR) {
                        advance()
                    }
                    diagnostic(
                        Diagnostic(
                            range = makeRange(),
                            kind = DiagnosticKind.UnexpectedCharacter(startChar),
                        )
                    )
                    makeToken(T.ERROR)
                }
            }
        }
    }

    private val Char.isIdentifierStarter get() = isLetter() || this == '_'
    private val Char.isIdentifierChar get() = isIdentifierStarter || isDigit()

    private fun identifierOrKeyword(): Token {
        advance()
        while (currentChar.isIdentifierChar) {
            advance()
        }
        val token = makeToken(T.IDENTIFIER)
        val keywordKind = KEYWORDS[token.text]
        return if (keywordKind != null) {
            token.copy(kind = keywordKind)
        } else {
            token
        }
    }

    private fun makeRange(): SourceRange {
        return SourceRange(
            file,
            span = Span(
                start = Position(currentLine, currentColumn),
                stop = Position(lastLine, lastColumn),
            )

        )
    }

    private fun skipWhiteSpace() {
        while (currentChar.isWhitespace()) {
            advance()
        }
    }

    private fun startToken() {
        startLine = currentLine
        startColumn = currentColumn
        startIndex = currentIndex
    }

    private fun advance() {
        val lastChar = currentChar
        lastLine = currentLine
        lastColumn = currentColumn

        currentIndex++

        if (lastChar == '\n') {
            newlineOffsets.add(currentIndex - 1)
            currentLine++
            currentColumn = 1
        } else {
            currentColumn++
        }
    }

    private val currentChar
        get(): Char = if (currentIndex >= input.length) {
            EOF_CHAR
        } else {
            input[currentIndex]
        }


    private fun makeToken(kind: Token.Kind): Token {
        return Token(
            kind = kind,
            text = input.substring(startIndex until currentIndex),
            span = Span(
                Position(startLine, startColumn),
                Position(lastLine, lastColumn),
            ),
            startOffset = startIndex,
            stopOffset = currentIndex - 1
        )
    }
}