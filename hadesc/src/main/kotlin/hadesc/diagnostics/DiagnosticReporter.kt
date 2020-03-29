package hadesc.diagnostics

import hadesc.ast.Token
import hadesc.location.SourceLocation
import hadesc.location.SourcePath

data class Diagnostic(
    val sourceLocation: SourceLocation, val kind: Kind
) {
    enum class Severity {
        WARNING,
        ERROR,
    }

    sealed class Kind(val severity: Severity) {
        data class UnexpectedToken(
            val expected: Token.Kind,
            val found: Token
        ) : Diagnostic.Kind(Severity.ERROR)

        object DeclarationExpected : Kind(Severity.ERROR)
        object TypeAnnotationExpected : Kind(Severity.ERROR)
        object StatementExpected : Kind(Severity.ERROR)
        object ExpressionExpected : Kind(Severity.ERROR)

        override fun toString(): String = when (this) {
            DeclarationExpected -> "Declaration expected"
            TypeAnnotationExpected -> "Type expected"
            ExpressionExpected -> "Expression expected"
            StatementExpected -> "Statement expected"
            is UnexpectedToken -> "Unexpected token ${found.text}; Expected $expected"
        }

    }
}

class DiagnosticReporter {
    var hasErrors = false
    private val fileLines = mutableMapOf<SourcePath, List<String>>()
    fun report(location: SourceLocation, kind: Diagnostic.Kind) {
        if (kind.severity == Diagnostic.Severity.ERROR) {
            hasErrors = true
        }
        printErrLn("${kind.severity}: ${location}: $kind")
        printLocationLine(location)
        for (i in 0..location.start.column + location.start.line.toString().length) {
            System.err.print(' ')
        }
        printErrLn("^")
    }

    private fun printLocationLine(location: SourceLocation) {
        val lines = fileLines.computeIfAbsent(location.file) {
            it.path.toFile().readLines()
        }
        val startLine = lines[location.start.line - 1]
        printErrLn("${location.start.line}| $startLine")
    }

    private fun printErrLn(string: String) {
        System.err.println(string)
    }


}