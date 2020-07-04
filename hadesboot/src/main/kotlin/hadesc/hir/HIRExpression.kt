package hadesc.hir

import hadesc.Name
import hadesc.ir.BinaryOperator
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type

sealed class HIRExpression: HasLocation {
    abstract val type: Type
    data class Call(
            override val location: SourceLocation,
            override val type: Type,
            val callee: HIRExpression,
            val typeArgs: List<Type>?,
            val args: List<HIRExpression>
    ) : HIRExpression()

    data class GlobalRef(
            override val location: SourceLocation,
            override val type: Type,
            val name: QualifiedName
    ) : HIRExpression()

    data class Constant(
            val constant: HIRConstant
    ) : HIRExpression() {
        override val location: SourceLocation
            get() = constant.location

        override val type: Type
            get() = constant.type
    }

    data class ParamRef(
            override val location: SourceLocation,
            override val type: Type,
            val name: Name
    ) : HIRExpression()

    data class ValRef(
            override val location: SourceLocation,
            override val type: Type,
            val name: Name
    ) : HIRExpression()

    data class GetStructField(
            override val location: SourceLocation,
            override val type: Type,
            val lhs: HIRExpression,
            val name: Name,
            val index: Int
    ) : HIRExpression()

    data class ThisRef(
            override val location: SourceLocation,
            override val type: Type
    ) : HIRExpression()

    data class MethodRef(
            override val location: SourceLocation,
            override val type: Type,
            val thisValue: HIRExpression,
            val method: HIRExpression
    ) : HIRExpression()

    data class Not(
            val expression: HIRExpression
    ) : HIRExpression() {
        override val location get() = expression.location
        override val type get() = expression.type
    }

    data class BinOp(
            override val location: SourceLocation,
            override val type: Type,
            val lhs: HIRExpression,
            val operator: BinaryOperator,
            val rhs: HIRExpression
    ) : HIRExpression()

    data class NullPtr(
            override val location: SourceLocation,
            override val type: Type.Ptr
    ) : HIRExpression()

    data class SizeOf(
            override val location: SourceLocation,
            override val type: Type,
            val ofType: Type
    ) : HIRExpression()

    data class AddressOf(
            override val location: SourceLocation,
            override val type: Type.Ptr,
            val name: Name
    ) : HIRExpression()

    data class BoundRef(
            override val location: SourceLocation,
            val param: HIRConstraintParam
    ) : HIRExpression() {
        override val type: Type
            get() = param.type
    }

    fun prettyPrint(): String = when(this) {
        is Call -> {
            val typeArgsStr = if (typeArgs == null)
                ""
            else "[${typeArgs.joinToString(", ") { it.prettyPrint() } }]"
            "${callee.prettyPrint()}${typeArgsStr}(${args.joinToString(", ") { it.prettyPrint() } })"
        }
        is GlobalRef -> name.mangle()
        is Constant -> constant.prettyPrint()
        is ParamRef -> name.text
        is ValRef -> name.text
        is GetStructField -> "${lhs.prettyPrint()}.${name.text}"
        is ThisRef -> "this"
        is MethodRef -> "(${thisValue.prettyPrint()} :: ${method.prettyPrint()})"
        is Not -> "not ${expression.prettyPrint()}"
        is BinOp -> "(${lhs.prettyPrint()} ${operator.prettyPrint()} ${rhs.prettyPrint()})"
        is NullPtr -> "(nullptr : ${type.prettyPrint()})"
        is SizeOf -> "size_of[${type.prettyPrint()}]"
        is AddressOf -> "&${name.text}"
        is BoundRef -> TODO()
    }
}