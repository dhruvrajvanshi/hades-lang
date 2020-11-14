package hades.diagnostics

import hades.ast.SourceRange
import hades.ast.parsing.Token

data class Diagnostic(
    val range: SourceRange,
    val kind: DiagnosticKind,
    val hints: List<String> = emptyList(),
)

enum class DiagnosticSeverity {
    ERROR,
    WARNING,
}

private typealias S = DiagnosticSeverity
sealed class DiagnosticKind(
    val severity: DiagnosticSeverity = S.ERROR
) {
    object DeclarationExpected : DiagnosticKind()

    data class UnexpectedCharacter(
        val character: Char
    ) : DiagnosticKind()

    data class UnexpectedToken(
        val token: Token
    ) : DiagnosticKind(S.ERROR)

    data class TokenExpected(val kind: Token.Kind) : DiagnosticKind(S.ERROR)

    object MissingSemicolon : DiagnosticKind(S.ERROR)


    val message
        get(): String = when (this) {
            DeclarationExpected -> "Declaration expected"
            is UnexpectedCharacter -> "Unexpected character '$character'"
            is UnexpectedToken -> "Unexpected token '${token.text}'"
            MissingSemicolon -> "Missing semicolon"
            is TokenExpected -> "Expected $kind"
        }
}

