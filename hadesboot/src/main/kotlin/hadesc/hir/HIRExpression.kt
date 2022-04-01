package hadesc.hir

import hadesc.Name
import hadesc.analysis.ClosureCaptures
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
    @Deprecated("Use HIRStatement.Call")
    data class Call(
            override val location: SourceLocation,
            override val type: Type,
            val callee: HIROperand,
            val args: List<HIRExpression>
    ) : HIRExpression

    data class TypeApplication(
            override val location: SourceLocation,
            override val type: Type,
            val expression: HIROperand,
            val args: List<Type>
    ) : HIRExpression, HIROperand

    data class GlobalRef(
            override val location: SourceLocation,
            override val type: Type,
            val name: QualifiedName
    ) : HIRExpression, HIROperand

    data class ParamRef(
            override val location: SourceLocation,
            override val type: Type,
            val name: Name,
            val binder: Binder,
    ) : HIRExpression, HIROperand

    data class ValRef(
            override val location: SourceLocation,
            override val type: Type,
            val name: Name
    ) : HIRExpression, HIROperand {
        @Deprecated("Temporary method added for refactoring")
        fun asPtr(): LocalRef {
            return HIRExpression.LocalRef(location, type.ptr(), name)
        }
    }

    data class LocalRef(
        override val location: SourceLocation,
        override val type: Type,
        val name: Name
    ) : HIRExpression, HIROperand


    data class BinOp(
        override val location: SourceLocation,
        override val type: Type,
        val lhs: HIRExpression,
        val operator: BinaryOperator,
        val rhs: HIRExpression
    ) : HIRExpression

    data class NullPtr(
            override val location: SourceLocation,
            override val type: Type.Ptr
    ) : HIRExpression, HIROperand

    data class SizeOf(
            override val location: SourceLocation,
            override val type: Type,
            val ofType: Type
    ) : HIRExpression, HIROperand

    data class PointerCast(
        override val location: SourceLocation,
        val toPointerOfType: Type,
        val value: HIRExpression
    ) : HIRExpression {
        override val type: Type
            get() = Type.Ptr(toPointerOfType, isMutable = true)
    }

    data class TraitMethodRef(
            override val location: SourceLocation,
            override val type: Type,
            val traitName: QualifiedName,
            val traitArgs: List<Type>,
            val methodName: Name,
    ) : HIRExpression, HIROperand

    data class UnsafeCast(
        override val location: SourceLocation,
        override val type: Type,
        val value: HIRExpression,
    ) : HIRExpression

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
        val closure: HIRExpression,
        val args: List<HIRExpression>,
    ) : HIRExpression

    data class IntegerConvert(
        override val location: SourceLocation,
        override val type: Type,
        val value: HIRExpression,
    ) : HIRExpression

    data class BlockExpression(override val type: Type, val block: HIRBlock): HIRExpression {
        override val location get() = block.location
    }

    override fun prettyPrint(): String = when(this) {
        is Call -> {
            "${callee.prettyPrint()}(${args.joinToString(", ") { it.prettyPrint() } })"
        }
        is GlobalRef -> name.mangle()
        is ParamRef -> name.text
        is ValRef -> "%${name.text}"
        is BinOp -> "(${lhs.prettyPrint()} ${operator.prettyPrint()} ${rhs.prettyPrint()})"
        is NullPtr -> "(nullptr : ${type.prettyPrint()})"
        is SizeOf -> "size_of[${type.prettyPrint()}]"
        is TypeApplication -> {
            val typeArgsStr = "[${args.joinToString(", ") { it.prettyPrint() } }]"
            "${expression.prettyPrint()}$typeArgsStr"
        }
        is PointerCast -> "(pointer-cast ${value.prettyPrint()} to ${type.prettyPrint()})"
        is TraitMethodRef -> "${traitName.mangle()}[${traitArgs.joinToString(", ") {it.prettyPrint()} }]." +
                methodName.text
        is UnsafeCast -> "unsafe_cast[${type.prettyPrint()}](${value.prettyPrint()})"
        is Closure -> "|${params.joinToString { it.name.text + ": " + it.type.prettyPrint() }}|: ${returnType.prettyPrint()} ${body.prettyPrint()}"
        is InvokeClosure -> "invoke_closure ${closure.prettyPrint()}(${args.joinToString { it.prettyPrint() }})"
        is IntegerConvert -> "(${value.prettyPrint()} as ${type.prettyPrint()})"
        is BlockExpression -> block.prettyPrint()
        is HIRConstant.ByteString -> "b\"" + String(bytes)
            .replace("\"", "\"\"")
            .replace("\\", "\\\\") + "\""
        is HIRConstant.BoolValue -> value.toString()
        is HIRConstant.IntValue -> value.toString()
        is HIRConstant.FloatValue -> value.toString()
        is HIRConstant.Void -> "void"
        is LocalRef -> "%${name.text}"
    }
}