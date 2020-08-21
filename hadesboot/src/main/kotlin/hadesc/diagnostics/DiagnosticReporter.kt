package hadesc.diagnostics

import com.diogonunes.jcolor.Ansi.colorize
import com.diogonunes.jcolor.Attribute
import hadesc.Name
import hadesc.ast.Binder
import hadesc.ast.QualifiedPath
import hadesc.ast.Token
import hadesc.ir.BinaryOperator
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.types.Type
import kotlin.streams.toList

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
        object FieldExpected : Kind(Severity.ERROR)
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
        data class UnboundTypePath(val path: QualifiedPath) : Diagnostic.Kind(Severity.ERROR)

        object NotAnAddressableValue : Diagnostic.Kind(Severity.ERROR)
        object AssignmentToImmutableVariable : Diagnostic.Kind(Severity.ERROR)
        object TooManyTypeArgs : Diagnostic.Kind(Severity.ERROR)
        object TooFewTypeArgs : Diagnostic.Kind(Severity.ERROR)
        object InterfaceMemberExpected : Diagnostic.Kind(Severity.ERROR)
        object NotAnInterface : Diagnostic.Kind(Severity.ERROR)
        object UnboundThisType : Diagnostic.Kind(Severity.ERROR)
        object NoImplementationFound : Diagnostic.Kind(Severity.ERROR)
        object PatternExpected : Diagnostic.Kind(Severity.ERROR)
        object ExpectedEnumType : Diagnostic.Kind(Severity.ERROR)
        object NestedPatternsNotAllowed : Diagnostic.Kind(Severity.ERROR)
        object UnreachablePattern : Diagnostic.Kind(Severity.WARNING)
        object UnboundPattern : Diagnostic.Kind(Severity.ERROR)
        object NonExhaustivePatterns : Diagnostic.Kind(Severity.ERROR)
        object PatternParamMismatch : Diagnostic.Kind(Severity.ERROR)
        object DuplicateVariantName : Diagnostic.Kind(Severity.ERROR)
        object InvalidNewExpression : Diagnostic.Kind(Severity.ERROR)
        object NotAStructField : Diagnostic.Kind(Severity.ERROR)
        object StructFieldNotMutable : Diagnostic.Kind(Severity.ERROR)
        object ValNotMutable : Diagnostic.Kind(Severity.ERROR)
        object NoSuchModule : Diagnostic.Kind(Severity.ERROR)
        object StatementNotAllowedInDefer : Diagnostic.Kind(Severity.ERROR)
        object MissingReturnValue : Diagnostic.Kind(Severity.ERROR)
        object InvalidTypeApplication : Diagnostic.Kind(Severity.ERROR)
        object MissingTypeAnnotation : Diagnostic.Kind(Severity.ERROR)
        object TypeParametersNotAllowedInInterfaceMethods : Diagnostic.Kind(Severity.ERROR)
        object InterfaceMemberMustBeAnExtensionMethod : Diagnostic.Kind(Severity.ERROR)
        object InterfaceMethodReceiverMustBeThisOrThisPtr : Diagnostic.Kind(Severity.ERROR)
        object WhereClauseMustReferToATypeParam : Diagnostic.Kind(Severity.ERROR)
        object UnboundInterface : Diagnostic.Kind(Severity.ERROR)
        object NotAConstructor : Diagnostic.Kind(Severity.ERROR)
        object UnknownAnnotation : Diagnostic.Kind(Severity.ERROR)
        object InvalidPipelineExpression : Diagnostic.Kind(Severity.ERROR)

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
            is OperatorNotApplicable -> "Operator not applicable to type"
            NotAnAddressableValue -> "Not an addressable value"
            is NotAPointerType -> "${type.prettyPrint()} is not a pointer type"
            AssignmentToImmutableVariable -> "Variable is not mutable. Try declaring it using 'val mut' instead of 'val'"
            is TooManyTypeArgs -> "Too many type args"
            InterfaceMemberExpected -> "interface member expected"
            NotAnInterface -> "Not an interface"
            UnboundThisType -> "This type not allowed here"
            NoImplementationFound -> "No implementation found"
            PatternExpected -> "Pattern expected"
            TooFewTypeArgs -> "Too few type arguments"
            ExpectedEnumType -> "Expected an enum type"
            NestedPatternsNotAllowed -> "Nested patterns not allowed"
            UnreachablePattern -> "Unreachable pattern"
            UnboundPattern -> "Unbound pattern"
            NonExhaustivePatterns -> "Non exhaustive patterns"
            PatternParamMismatch -> "Pattern param length mismatch"
            DuplicateVariantName -> "Duplicate variant name"
            InvalidNewExpression -> "Invalid new target; Expected a struct declaration"
            NotAStructField -> "Not a struct field"
            StructFieldNotMutable -> "Struct field not mutable"
            FieldExpected -> "Struct field expected"
            ValNotMutable -> "Not a mutable value"
            NoSuchModule -> "No such module"
            StatementNotAllowedInDefer -> "Illegal defer statement"
            MissingReturnValue -> "Missing return value"
            InvalidTypeApplication -> "Invalid type application"
            MissingTypeAnnotation -> "Missing type annotation"
            is UnboundTypePath -> "Unbound type ${path.identifiers.joinToString(".") {it.name.text}}"
            TypeParametersNotAllowedInInterfaceMethods -> "Type parameters are not allowed in interface methods"
            InterfaceMemberMustBeAnExtensionMethod -> "Interface member must be an extension method (try adding a receiver/this parameter)"
            InterfaceMethodReceiverMustBeThisOrThisPtr -> "Interface method receiver must be either This or *This"
            WhereClauseMustReferToATypeParam -> "Where clause must refer to a type parameter"
            UnboundInterface -> "Unbound interface"
            NotAConstructor -> "Not a constructor"
            UnknownAnnotation -> "Unknown annotation"
            InvalidPipelineExpression -> "This expression type is not a valid pipeline expression"
        }

    }
}

class DiagnosticReporter {
    var hasErrors = false
    private val fileLines = mutableMapOf<SourcePath, List<String>>()
    val errors = mutableListOf<Diagnostic>()
    private val hasErrorAtLocation = mutableSetOf<SourceLocation>()
    fun report(location: SourceLocation, kind: Diagnostic.Kind) {
        if (location in hasErrorAtLocation) {
            return
        }
        hasErrorAtLocation.add(location)
        if (kind.severity == Diagnostic.Severity.ERROR) {
            hasErrors = true
        }
        errors.add(Diagnostic(location, kind))
        val severityColor = if (kind.severity == Diagnostic.Severity.ERROR) {
            Attribute.RED_TEXT()
        } else {
            Attribute.YELLOW_TEXT()
        }
        val severity = colorize(kind.severity.toString(), severityColor, Attribute.BOLD())
        val path = colorize(location.file.path.toString(), Attribute.BOLD())
        val lineInfo = colorize("(${location.start.line}:${location.start.column})", Attribute.BOLD());
        val coloredKind = colorize(kind.prettyPrint(), Attribute.BOLD())
        printErrLn("${path}:${lineInfo}: ${severity}: $coloredKind")
        printLocationLine(location)
        for (i in 0 until location.start.column) {
            System.err.print(' ')
        }
        System.err.print(colorize("^", Attribute.RED_TEXT(), Attribute.BOLD()))
        if (location.start.line == location.stop.line) {

            for (i in 0 until location.stop.column - location.start.column) {
                System.err.print(colorize("~", Attribute.RED_TEXT(), Attribute.BOLD()))
            }

        }
        System.err.println()
        System.err.println()
    }

    private fun printLocationLine(location: SourceLocation) {
        val lines = fileLines.computeIfAbsent(location.file) {
            it.path.toFile().readLines()
        }
        val startLine = lines[location.start.line - 1]
        val lineno = colorize(location.start.line.toString(), Attribute.DIM())
        printErrLn(" $lineno\t| $startLine")
        System.err.print(" ${location.start.line.toString().chars().toList().joinToString(" ") { "" }  }\t|")
    }

    private fun printErrLn(string: String) {
        System.err.println(string)
    }

}