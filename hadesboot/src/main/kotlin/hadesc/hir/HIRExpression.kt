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
            val args: List<HIRExpression>
    ) : HIRExpression()

    data class TypeApplication(
            override val location: SourceLocation,
            override val type: Type,
            val expression: HIRExpression,
            val args: List<Type>
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

    data class GetStructFieldPointer(
            override val location: SourceLocation,
            override val type: Type,
            val lhs: HIRExpression,
            val memberName: Name,
            val memberIndex: Int
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

    data class Load(
        override val location: SourceLocation,
        override val type: Type,
        val ptr: HIRExpression
    ) : HIRExpression()

    data class PointerCast(
        override val location: SourceLocation,
        val toPointerOfType: Type,
        val value: HIRExpression
    ) : HIRExpression() {
        init {
            require(value.type is Type.Ptr)
        }
        override val type: Type
            get() = Type.Ptr(toPointerOfType, isMutable = true)
    }

    data class TraitMethodCall(
            override val location: SourceLocation,
            override val type: Type,
            val traitName: QualifiedName,
            val traitArgs: List<Type>,
            val methodName: Name,
            val args: List<HIRExpression>
    ) : HIRExpression()

    data class UnsafeCast(
        override val location: SourceLocation,
        override val type: Type,
        val value: HIRExpression,
    ) : HIRExpression()

    data class When(
        override val location: SourceLocation,
        override val type: Type,
        val discriminant: HIRExpression,
        val cases: List<Case>
    ) : HIRExpression() {
        data class Case(
            val casePayloadType: Type,
            val caseName: Name,
            val valueBinder: Name,
            val expression: HIRExpression
        ) {
            fun prettyPrint() = "is ${valueBinder.text}: ${caseName.text} -> ${expression.prettyPrint()}"
        }
    }

    fun prettyPrint(): String = when(this) {
        is Call -> {
            "${callee.prettyPrint()}(${args.joinToString(", ") { it.prettyPrint() } })"
        }
        is GlobalRef -> name.mangle()
        is Constant -> constant.prettyPrint()
        is ParamRef -> name.text
        is ValRef -> name.text
        is GetStructField -> "${lhs.prettyPrint()}.${name.text} : ${type.prettyPrint()}"
        is Not -> "not ${expression.prettyPrint()}"
        is BinOp -> "(${lhs.prettyPrint()} ${operator.prettyPrint()} ${rhs.prettyPrint()})"
        is NullPtr -> "(nullptr : ${type.prettyPrint()})"
        is SizeOf -> "size_of[${type.prettyPrint()}]"
        is AddressOf -> "&${name.text}"
        is TypeApplication -> {
            val typeArgsStr = "[${args.joinToString(", ") { it.prettyPrint() } }]"
            "${expression.prettyPrint()}$typeArgsStr"
        }
        is Load -> "*${ptr.prettyPrint()}"
        is PointerCast -> "(pointer-cast ${value.prettyPrint()} to ${type.prettyPrint()})"
        is GetStructFieldPointer -> "(${lhs.prettyPrint()}.${memberName.text})"
        is TraitMethodCall -> "${traitName.mangle()}[${traitArgs.joinToString(", ") {it.prettyPrint()} }]." +
                "${methodName.text}(${args.joinToString(", ") { it.prettyPrint() } })"
        is UnsafeCast -> "unsafe_cast[${type.prettyPrint()}](${value.prettyPrint()})"
        is When -> "when (${discriminant.prettyPrint()}) {\n" +
                cases.joinToString("\n") { "  " + it.prettyPrint() } +
                "\n} : ${type.prettyPrint()}"
    }
}