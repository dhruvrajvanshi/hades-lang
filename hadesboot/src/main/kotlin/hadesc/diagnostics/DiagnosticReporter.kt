package hadesc.diagnostics

import com.diogonunes.jcolor.Ansi
import com.diogonunes.jcolor.Attribute
import hadesc.Name
import hadesc.analysis.TraitRequirement
import hadesc.ast.Binder
import hadesc.ast.Declaration
import hadesc.ast.QualifiedPath
import hadesc.ast.Token
import hadesc.hir.BinaryOperator
import hadesc.location.SourceLocation
import hadesc.location.SourcePath
import hadesc.types.Type
import org.apache.commons.lang3.SystemUtils
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
        ) : Kind(Severity.ERROR)

        object DeclarationExpected : Kind(Severity.ERROR)
        object FieldExpected : Kind(Severity.ERROR)
        object TypeAnnotationExpected : Kind(Severity.ERROR)
        object StatementExpected : Kind(Severity.ERROR)
        object ExpressionExpected : Kind(Severity.ERROR)
        data class UnboundVariable(val name: Name): Kind(Severity.ERROR)
        object UnboundThis : Kind(Severity.ERROR)
        object AmbiguousExpression : Kind(Severity.ERROR)
        object NotAConst : Kind(Severity.ERROR)

        data class TypeNotCallable(val type: Type) : Kind(Severity.ERROR)
        data class MissingArgs(val required: Int) : Kind(Severity.ERROR)
        data class TooManyArgs(val required: Int) : Kind(Severity.ERROR)
        data class TypeNotAssignable(val source: Type, val destination: Type) : Kind(Severity.ERROR)
        data class NoSuchProperty(val type: Type, val property: Name) : Kind(Severity.ERROR)
        data class UnboundType(val name: Name) : Kind(Severity.ERROR)

        data class UninferrableTypeParam(val name: Name, val location: SourceLocation) : Kind(Severity.ERROR)
        data class IncompleteType(val requiredArgs: Int) : Kind(Severity.ERROR)
        data class TypeNotEqualityComparable(val type: Type) : Kind(Severity.ERROR)
        data class OperatorNotApplicable(val operator: BinaryOperator) : Kind(Severity.ERROR)
        data class NotAPointerType(val type: Type) : Kind(Severity.ERROR)
        data class UnboundTypePath(val path: QualifiedPath) : Kind(Severity.ERROR)
        data class DuplicateDeclaration(val existingBindingLocation: SourceLocation) : Kind(Severity.ERROR)

        object NotAnAddressableValue : Kind(Severity.ERROR)
        object AssignmentToImmutableVariable : Kind(Severity.ERROR)
        object TooManyTypeArgs : Kind(Severity.ERROR)
        object TooFewTypeArgs : Kind(Severity.ERROR)
        object NotATrait : Kind(Severity.ERROR)
        object UnboundThisType : Kind(Severity.ERROR)
        object PatternExpected : Kind(Severity.ERROR)
        object ExpectedEnumType : Kind(Severity.ERROR)
        object NestedPatternsNotAllowed : Kind(Severity.ERROR)
        object UnreachablePattern : Kind(Severity.WARNING)
        object UnboundPattern : Kind(Severity.ERROR)
        data class NonExhaustivePatterns(val name: Name) : Kind(Severity.ERROR)
        object NonExhaustivePrimitivePatterns : Kind(Severity.ERROR)
        object PatternParamMismatch : Kind(Severity.ERROR)
        object DuplicateVariantName : Kind(Severity.ERROR)
        object InvalidNewExpression : Kind(Severity.ERROR)
        object NotAStructField : Kind(Severity.ERROR)
        object StructFieldNotMutable : Kind(Severity.ERROR)
        object ValNotMutable : Kind(Severity.ERROR)
        object NoSuchModule : Kind(Severity.ERROR)
        object StatementNotAllowedInDefer : Kind(Severity.ERROR)
        object MissingReturnValue : Kind(Severity.ERROR)
        object InvalidTypeApplication : Kind(Severity.ERROR)
        object MissingTypeAnnotation : Kind(Severity.ERROR)
        object TypeParametersNotAllowedInTraitMethods : Kind(Severity.ERROR)
        object WhereClauseMustReferToATypeParam : Kind(Severity.ERROR)
        object NotAConstructor : Kind(Severity.ERROR)
        object UnknownAnnotation : Kind(Severity.ERROR)
        object InvalidPipelineExpression : Kind(Severity.ERROR)
        object OnlyFunctionDefsAllowedInsideExtensionDefs : Kind(Severity.ERROR)
        object MissingTraitThisParam : Kind(Severity.ERROR)
        object ReceiverParamsNotAllowedInTraitFunctions : Kind(Severity.ERROR)
        object TypeParamsNotAllowedInTraitFunctions : Kind(Severity.ERROR)
        object OnlyFunctionDefsAndTypeAliasesAllowedInImplementationDefs : Kind(Severity.ERROR)
        object ReturnTypeNotInferred : Kind(Severity.ERROR)
        object NoSuchMember : Kind(Severity.ERROR)
        object ReturningFromVoidFunction : Kind(Severity.ERROR)
        object MissingThisParam : Kind(Severity.ERROR)
        data class TraitRequirementNotSatisfied(val requirement: TraitRequirement) : Kind(Severity.ERROR)

        data class MissingImplMethod(val name: Name) : Kind(Severity.ERROR)
        data class TraitMethodTypeMismatch(val expected: Type, val found: Type) : Kind(Severity.ERROR)
        data class DuplicateTypeBinding(val existing: Binder) : Kind(Severity.ERROR)
        data class DuplicateValueBinding(val existing: Binder) : Kind(Severity.ERROR)
        object TakingAddressOfClosureDisallowed: Kind(Severity.ERROR)
        object ReturnTypeMustNotContainClosuresOrRefs : Kind(Severity.ERROR)
        object UseAfterMove : Kind(Severity.ERROR)
        object NotAnIntegralValue : Kind(Severity.ERROR)
        object BlockExpressionMustEndWithExpression : Kind(Severity.ERROR)
        object InvalidIntrinsic : Kind(Severity.ERROR)
        data class TypeDoesNotSupportArithmetic(val type: Type): Kind(Severity.ERROR)

        data class NoSuchAssociatedType(val name: Name) : Kind(Severity.ERROR)
        data class NotAnArrayType(val type: Type): Kind(Severity.ERROR)

        data class MissingAssociatedType(val name: Name) : Kind(Severity.ERROR)
        data class InvalidEscape(val c: Char) : Kind(Severity.ERROR)
        data class NotAMatchableType(val type: Type) : Kind(Severity.ERROR)
        data class NotAnEnumType(val type: Type) : Kind(Severity.ERROR)
        data class NoSuchCase(val declaration: Declaration.Enum, val name: Name) : Kind(Severity.ERROR)

        fun prettyPrint(): String = when (this) {
            DeclarationExpected -> "Declaration expected"
            TypeAnnotationExpected -> "Type expected"
            ExpressionExpected -> "Expression expected"
            StatementExpected -> "Statement expected"
            is UnexpectedToken -> "Unexpected token ${found.text}; Expected $expected"
            is UnboundVariable -> "Unbound variable: ${name.text}"
            is TypeNotCallable -> "Type ${type.prettyPrint()} is not callable"
            is MissingArgs -> "Missing args; $required required"
            is TooManyArgs -> "Too many args; $required required"
            is TypeNotAssignable -> "Type ${source.prettyPrint()} is not assignable to ${destination.prettyPrint()}"
            is NoSuchProperty -> "Type ${type.prettyPrint()} has no property named ${property.text}"
            is UnboundType -> "Unbound type variable ${name.text}"
            is UninferrableTypeParam -> "Uninferrable type parameter ${name.text}; Explicit type annotation required. (Defined at ${location})"
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
            NotATrait -> "Not a trait"
            UnboundThisType -> "This type not allowed here"
            PatternExpected -> "Pattern expected"
            TooFewTypeArgs -> "Too few type arguments"
            ExpectedEnumType -> "Expected an enum type"
            NestedPatternsNotAllowed -> "Nested patterns not allowed"
            UnreachablePattern -> "Unreachable pattern"
            UnboundPattern -> "Unbound pattern"
            is NonExhaustivePatterns -> "Non exhaustive patterns; Missing case: ${name.text}"
            is NonExhaustivePrimitivePatterns -> "Non exhaustive patterns"
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
            TypeParametersNotAllowedInTraitMethods -> "Type parameters are not allowed in trait methods"
            WhereClauseMustReferToATypeParam -> "Where clause must refer to a type parameter"
            NotAConstructor -> "Not a constructor"
            UnknownAnnotation -> "Unknown annotation"
            InvalidPipelineExpression -> "This expression type is not a valid pipeline expression."
            OnlyFunctionDefsAllowedInsideExtensionDefs -> "Only function definitions are allowed inside extensions."
            ReceiverParamsNotAllowedInTraitFunctions -> "Receiver params not are allowed in trait functions."
            TypeParamsNotAllowedInTraitFunctions -> "Type params are not allowed in trait functions."
            is DuplicateDeclaration -> "Duplicate binding. Previously defined at $existingBindingLocation."
            OnlyFunctionDefsAndTypeAliasesAllowedInImplementationDefs -> "Only functions are allowed inside implementation definitions."
            MissingTraitThisParam -> "Trait definition must have at least 1 type parameter."
            ReturnTypeNotInferred -> "Return type of this function cannot be inferred. Add an explicit annotation."
            NoSuchMember -> "No such member."
            is MissingImplMethod -> "Missing implementation for method '${name.text}'."
            is TraitMethodTypeMismatch -> "Trait method type mismatch: expected: ${expected.prettyPrint()}, found ${found.prettyPrint()}"
            is DuplicateTypeBinding -> "Duplicate type binding."
            is DuplicateValueBinding -> "Duplicate value binding."
            ReturningFromVoidFunction -> "Void functions can't return a value."
            MissingThisParam -> "Missing this param."
            is TraitRequirementNotSatisfied -> "Trait requirement (${requirement.prettyPrint()}) not satisfied."
            TakingAddressOfClosureDisallowed -> "Taking the address of a closure is disallowed."
            ReturnTypeMustNotContainClosuresOrRefs -> "Return types cannot contain closures or refs"
//            is TypeNotCopyable -> "Type ${type.prettyPrint()} is not copyable"
            UseAfterMove -> "Use after move"
            NotAnIntegralValue -> "Not an integral value"
            is NoSuchAssociatedType -> "No such associated type ${name.text}"
            is MissingAssociatedType -> "Missing associated type: ${name.text}"
            is NotAnArrayType -> "${type.prettyPrint()} is not an array type"
            BlockExpressionMustEndWithExpression -> "Block expressions must end with an expression, not a statement"
            InvalidIntrinsic -> "Invalid intrinsic"
            is TypeDoesNotSupportArithmetic -> "Type '${type.prettyPrint()}' does not support arithmetic."
            is InvalidEscape -> "Invalid escape character '${c}'"
            is NotAMatchableType -> "Not a matchable type: ${type.prettyPrint()}"
            is NotAnEnumType -> "Not an enum type: ${type.prettyPrint()}"
            is NoSuchCase -> "No such case. Allowed cases: ${declaration.cases.map { it.name.name }}"
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
        val lineInfo = colorize("(${location.start.line}:${location.start.column})", Attribute.BOLD())
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

    private fun colorize(text: String, vararg attribute: Attribute): String {
        if (SystemUtils.IS_OS_WINDOWS) {
            return text
        }
        return Ansi.colorize(text, *attribute)
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