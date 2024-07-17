package hadesc.hir

import hadesc.Name
import hadesc.ast.Binder
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type

/**
 * Subset of HIRExpressions that don't nest.
 */
typealias HIROperand = HIRExpression
sealed interface HIRExpression : HIRNode {
    val type: Type
    sealed interface LocalName {
        val name: Name
    }

    data class GlobalRef(
        override val location: SourceLocation,
        override val type: Type,
        val name: QualifiedName
    ) : HIRExpression

    data class ParamRef(
        override val location: SourceLocation,
        override val type: Type,
        override val name: Name,
        val binder: Binder
    ) : HIRExpression, LocalName

    data class LocalRef(
        override val location: SourceLocation,
        override val type: Type,
        override val name: Name
    ) : HIRExpression, LocalName

    data class TraitMethodRef(
        override val location: SourceLocation,
        override val type: Type,
        val traitName: QualifiedName,
        val traitArgs: List<Type>,
        val methodName: Name
    ) : HIRExpression

    override fun prettyPrint(): String = when (this) {
        is GlobalRef -> name.mangle()
        is ParamRef -> name.text
        is TraitMethodRef -> "${traitName.mangle()}[${traitArgs.joinToString(", ") {it.prettyPrint()} }]." +
            methodName.text
        is HIRConstant.CString -> "c\"" + text
            .replace("\"", "\"\"")
            .replace("\\", "\\\\") + "\""
        is HIRConstant.BoolValue -> value.toString()
        is HIRConstant.IntValue -> value.toString()
        is HIRConstant.FloatValue -> value.toString()
        is HIRConstant.Void -> "void"
        is HIRConstant.NullPtr -> "null : ${type.prettyPrint()}"
        is LocalRef -> "%${name.text}"
        is HIRConstant.SizeOf -> "size_of[${type.prettyPrint()}]"
        is HIRConstant.AlignOf -> "align_of[${type.prettyPrint()}]"
        is HIRConstant.Error -> "(#error $message)"
        is HIRConstant.StructValue ->
            type.prettyPrint() + " " + values.joinToString(", ", "{ ", " }") { it.prettyPrint() }

        is HIRConstant.GlobalFunctionRef -> name.mangle()
    }
}
