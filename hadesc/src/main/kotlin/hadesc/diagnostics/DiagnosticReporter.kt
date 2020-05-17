package hadesc.diagnostics

import hadesc.Name
import hadesc.ast.Binder
import hadesc.ast.Token
import hadesc.ir.BinaryOperator
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.types.Type

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
        object UnboundVariable : Kind(Severity.ERROR)
        object UnboundThis : Diagnostic.Kind(Severity.ERROR)
        object AmbiguousExpression : Diagnostic.Kind(Severity.ERROR)
        object NotAConst : Diagnostic.Kind(Severity.ERROR)

        data class TypeNotCallable(val type: Type) : Kind(Severity.ERROR)
        data class MissingArgs(val required: Int) : Diagnostic.Kind(Severity.ERROR)
        data class TooManyArgs(val required: Int) : Diagnostic.Kind(Severity.ERROR)
        data class TypeNotAssignable(val source: Type, val destination: Type) : Diagnostic.Kind(Severity.ERROR)
        data class NoSuchProperty(val type: Type, val property: Name) : Diagnostic.Kind(Severity.ERROR)
        data class UnboundType(val name: Name) : Diagnostic.Kind(Severity.ERROR)

        data class UninferrableTypeParam(val binder: Binder) : Diagnostic.Kind(Severity.ERROR)
        data class IncompleteType(val requiredArgs: Int) : Diagnostic.Kind(Severity.ERROR)
        data class TypeNotEqualityComparable(val type: Type) : Diagnostic.Kind(Severity.ERROR)
        data class OperatorNotApplicable(val operator: BinaryOperator) : Diagnostic.Kind(Severity.ERROR)
        data class NotAPointerType(val type: Type) : Diagnostic.Kind(Severity.ERROR)

        object NotAnAddressableValue : Diagnostic.Kind(Severity.ERROR)
        object AssignmentToImmutableVariable : Diagnostic.Kind(Severity.ERROR)
        object TooManyTypeArgs : Diagnostic.Kind(Severity.ERROR)
        object InterfaceMemberExpected : Diagnostic.Kind(Severity.ERROR)
        object NotAnInterface : Diagnostic.Kind(Severity.ERROR)

        fun prettyPrint(): String = when (this) {
            DeclarationExpected -> "Declaration expected"
            TypeAnnotationExpected -> "Type expected"
            ExpressionExpected -> "Expression expected"
            StatementExpected -> "Statement expected"
            is UnexpectedToken -> "Unexpected token ${found.text}; Expected $expected"
            UnboundVariable -> "Unbound variable"
            is TypeNotCallable -> "Type ${type.prettyPrint()} is not callable"
            is MissingArgs -> "Missing args; $required required"
            is TooManyArgs -> "Too many args; $required required"
            is TypeNotAssignable -> "Type ${source.prettyPrint()} is not assignable to ${destination.prettyPrint()}"
            is NoSuchProperty -> "Type ${type.prettyPrint()} has no property named ${property.text}"
            is UnboundType -> "Unbound type variable ${name.text}"
            is UninferrableTypeParam -> "Uninferrable type parameter ${binder.identifier.name.text}; Explicit type annotation required. (Defined at ${binder.identifier.location})"
            is IncompleteType -> "Incomplete type; Required $requiredArgs arg(s)"
            UnboundThis -> "'this' is not bound. Try adding a receiver parameter to the enclosing function"
            AmbiguousExpression -> "Expression cannot be inferred; A type annotation is required"
            NotAConst -> "Not a const. Only CInt and Bool values are allowed as global constants"
            is TypeNotEqualityComparable -> "Type ${type.prettyPrint()} is not equatable"
            is OperatorNotApplicable -> "Operator not applicable type"
            NotAnAddressableValue -> "Not an addressable value"
            is NotAPointerType -> "${type.prettyPrint()} is not a pointer type"
            AssignmentToImmutableVariable -> "Variable is not mutable. Try declaring it using 'val mut' instead of 'val'"
            is TooManyTypeArgs -> "Too many type args"
            InterfaceMemberExpected -> "interface member expected"
            NotAnInterface -> "Not an interface"
        }

    }
}

class DiagnosticReporter {
    var hasErrors = false
    private val fileLines = mutableMapOf<SourcePath, List<String>>()
    public val errors = mutableListOf<Diagnostic>()
    fun report(location: SourceLocation, kind: Diagnostic.Kind) {
        if (kind.severity == Diagnostic.Severity.ERROR) {
            hasErrors = true
        }
        errors.add(Diagnostic(location, kind))
        printErrLn("${kind.severity}: ${location.file.path}:(${location.start.line}:${location.start.column}): ${kind.prettyPrint()}")
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