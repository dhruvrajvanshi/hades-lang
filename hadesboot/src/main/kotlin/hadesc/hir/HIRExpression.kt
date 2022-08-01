package hadesc.hir

import hadesc.Name
import hadesc.analysis.ClosureCaptures
import hadesc.assertions.requireUnreachable
import hadesc.ast.Binder
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type
import hadesc.types.ptr

/**
 * Subset of HIRExpressions that don't nest.
 */
sealed interface HIROperand: HIRExpression
sealed interface HIRExpression: HIRNode {
    val type: Type
    sealed interface LocalName {
        val name: Name
    }

    data class GlobalRef(
            override val location: SourceLocation,
            override val type: Type,
            val name: QualifiedName
    ) : HIRExpression, HIROperand

    data class ParamRef(
            override val location: SourceLocation,
            override val type: Type,
            override val name: Name,
            val binder: Binder,
    ) : HIRExpression, HIROperand, LocalName

    data class LocalRef(
        override val location: SourceLocation,
        override val type: Type,
        override val name: Name
    ) : HIRExpression, HIROperand, LocalName


    data class TraitMethodRef(
            override val location: SourceLocation,
            override val type: Type,
            val traitName: QualifiedName,
            val traitArgs: List<Type>,
            val methodName: Name,
    ) : HIRExpression, HIROperand

    data class Closure(
        override val location: SourceLocation,
        override val type: Type,
        val captures: ClosureCaptures,
        val params: List<HIRParam>,
        val returnType: Type,
        val body: HIRBlock
    ) : HIRExpression

    data class InvokeClosure(
        override val location: SourceLocation,
        override val type: Type,
        val closure: HIROperand,
        val args: List<HIRExpression>,
    ) : HIRExpression

    override fun prettyPrint(): String = when(this) {
        is GlobalRef -> name.mangle()
        is ParamRef -> name.text
        is TraitMethodRef -> "${traitName.mangle()}[${traitArgs.joinToString(", ") {it.prettyPrint()} }]." +
                methodName.text
        is Closure -> "|${params.joinToString { it.name.text + ": " + it.type.prettyPrint() }}|: ${returnType.prettyPrint()} ${body.prettyPrint().prependIndent("  ")}"
        is InvokeClosure -> "invoke_closure ${closure.prettyPrint()}(${args.joinToString { it.prettyPrint() }})"
        is HIRConstant.ByteString -> "b\"" + String(bytes)
            .replace("\"", "\"\"")
            .replace("\\", "\\\\") + "\""
        is HIRConstant.BoolValue -> value.toString()
        is HIRConstant.IntValue -> value.toString()
        is HIRConstant.FloatValue -> value.toString()
        is HIRConstant.Void -> "void"
        is HIRConstant.NullPtr -> "null : ${type.prettyPrint()}"
        is LocalRef -> "%${name.text}"
        is HIRConstant.SizeOf -> "size_of[${type.prettyPrint()}]"
    }
}
fun HIRExpression.withType(type: Type): HIRExpression = when (this) {
    is HIRExpression.Closure -> copy(type = type)
    is HIRExpression.GlobalRef -> copy(type = type)
    is HIRConstant.BoolValue -> copy(type = type)
    is HIRConstant.ByteString -> copy(type = type)
    is HIRConstant.SizeOf -> copy(type = type)
    is HIRExpression.LocalRef -> copy(type = type)
    is HIRExpression.ParamRef -> copy(type = type)
    is HIRExpression.TraitMethodRef -> copy(type = type)
    is HIRExpression.InvokeClosure -> copy(type = type)
    is HIRConstant.NullPtr -> {
        require(type is Type.Ptr)
        copy(type = type)
    }
    is HIRConstant.Void,
    is HIRConstant.FloatValue,
    is HIRConstant.IntValue -> requireUnreachable()
}
