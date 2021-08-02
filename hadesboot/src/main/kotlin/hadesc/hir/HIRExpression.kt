package hadesc.hir

import hadesc.Name
import hadesc.analysis.ClosureCaptures
import hadesc.ast.Binder
import hadesc.ir.BinaryOperator
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type

sealed class HIRExpression: HIRNode {
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
            val name: Name,
            val binder: Binder,
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
    ) : HIRExpression() {
        init {
            require(lhs.type !is Type.Ptr) {
                TODO()
            }
        }
    }

    data class GetStructFieldPointer(
            override val location: SourceLocation,
            override val type: Type.Ptr,
            val lhs: HIRExpression,
            val memberName: Name,
            val memberIndex: Int
    ) : HIRExpression() {
        init {
            require(lhs.type is Type.Ptr)
        }
    }

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
        override val type: Type
            get() = Type.Ptr(toPointerOfType, isMutable = true)
    }

    data class TraitMethodRef(
            override val location: SourceLocation,
            override val type: Type,
            val traitName: QualifiedName,
            val traitArgs: List<Type>,
            val methodName: Name,
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

    data class Closure(
        override val location: SourceLocation,
        override val type: Type,
        val captures: ClosureCaptures,
        val params: List<HIRParam>,
        val returnType: Type,
        val body: HIRBlock
    ) : HIRExpression()

    data class InvokeClosure(
        override val location: SourceLocation,
        override val type: Type,
        val closure: HIRExpression,
        val args: List<HIRExpression>,
    ) : HIRExpression()

    data class IntegerConvert(
        override val location: SourceLocation,
        override val type: Type,
        val value: HIRExpression,
    ) : HIRExpression()

    data class ArrayIndex(
        override val location: SourceLocation,
        override val type: Type,
        val array: HIRExpression,
        val index: HIRExpression
    ) : HIRExpression()

    override fun prettyPrint(): String = when(this) {
        is Call -> {
            "${callee.prettyPrint()}(${args.joinToString(", ") { it.prettyPrint() } })"
        }
        is GlobalRef -> name.mangle()
        is Constant -> constant.prettyPrint()
        is ParamRef -> name.text
        is ValRef -> name.text
        is GetStructField -> "(${lhs.prettyPrint()}.${name.text} : ${type.prettyPrint()})"
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
        is GetStructFieldPointer -> "(gep (${lhs.prettyPrint()} ${memberName.text}) : ${type.prettyPrint()})"
        is TraitMethodRef -> "${traitName.mangle()}[${traitArgs.joinToString(", ") {it.prettyPrint()} }]." +
                methodName.text
        is UnsafeCast -> "unsafe_cast[${type.prettyPrint()}](${value.prettyPrint()})"
        is When -> "when (${discriminant.prettyPrint()}) {\n" +
                cases.joinToString("\n") { "  " + it.prettyPrint() } +
                "\n} : ${type.prettyPrint()}"
        is Closure -> "|${params.joinToString { it.name.text + ": " + it.type.prettyPrint() }}|: ${returnType.prettyPrint()} ${body.prettyPrint()}"
        is InvokeClosure -> "invoke_closure ${closure.prettyPrint()}(${args.joinToString { it.prettyPrint() }})"
        is IntegerConvert -> "(${value.prettyPrint()} as ${type.prettyPrint()})"
        is ArrayIndex -> "${array.prettyPrint()}.[${index.prettyPrint()}]"
    }
}