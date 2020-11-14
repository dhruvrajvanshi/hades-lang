package hades.ast.parsing

import hades.URI
import hades.ast.Position
import hades.ast.SourceRange
import hades.ast.Span
import hades.diagnostics.Diagnostic
import hades.diagnostics.DiagnosticKind

private const val EOF_CHAR = Char.MIN_VALUE
private typealias T = Token.Kind

class Lexer(
    private val file: URI,
    private val input: String,
    private val diagnostic: (Diagnostic) -> Unit
) {
    private var startIndex = 0
    private var tokenLength = 0
    private var startLine = 1
    private var startColumn = 1
    private var lastLine = 1
    private var lastColumn = 1
    private var currentLine = 1
    private var currentColumn = 1

    fun nextToken(): Token {
        skipWhiteSpace()
        startToken()
        return when (val startChar = currentChar) {
            EOF_CHAR -> makeToken(Token.Kind.EOF)
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
    }

    private fun advance() {
        val lastChar = currentChar
        lastLine = currentLine
        lastColumn = currentColumn
        tokenLength++

        if (lastChar == '\n') {
            currentLine++
            currentColumn = 1
        } else {
            currentColumn++
        }
    }

    private val currentChar get(): Char = if (currentIndex >= input.length) {
        EOF_CHAR
    } else {
        input[currentIndex]
    }

    private val currentIndex get() = startIndex + tokenLength

    private fun makeToken(kind: Token.Kind): Token {
        return Token(
            kind = kind,
            length = tokenLength,
            text = input.substring(startIndex until startIndex + tokenLength),
            span = Span(
                Position(startLine, startColumn),
                Position(lastLine, lastColumn),
            )
        )
    }
}