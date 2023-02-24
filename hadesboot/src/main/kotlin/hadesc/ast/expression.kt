package hadesc.ast

import hadesc.hir.BinaryOperator
import hadesc.location.HasLocation
import hadesc.location.SourceLocation

sealed interface Expression : HasLocation {
    data class Error(override val location: SourceLocation) : Expression
    data class Var(
        val name: Identifier
    ) : Expression {
        override val location: SourceLocation
            get() = name.location
    }

    data class Call(
        override val location: SourceLocation,
        val callee: Expression,
        val args: List<Arg>
    ) : Expression

    data class Property(
        override val location: SourceLocation,
        val lhs: Expression,
        val property: Identifier
    ) : Expression

    data class ByteString(
        override val location: SourceLocation,
        val bytes: ByteArray
    ) : Expression

    data class BoolLiteral(
        override val location: SourceLocation,
        val value: Boolean
    ) : Expression

    data class NullPtr(
        override val location: SourceLocation
    ) : Expression

    data class IntLiteral(
        override val location: SourceLocation,
        val value: Int
    ) : Expression

    data class ByteCharLiteral(
        override val location: SourceLocation,
        val value: Char
    ) : Expression

    data class Not(
        override val location: SourceLocation,
        val expression: Expression
    ) : Expression

    data class BinaryOperation(
        override val location: SourceLocation,
        val lhs: Expression,
        val operator: BinaryOperator,
        val rhs: Expression
    ) : Expression

    data class SizeOf(
        override val location: SourceLocation,
        val type: TypeAnnotation
    ) : Expression

    data class AlignOf(
        override val location: SourceLocation,
        val type: TypeAnnotation
    ) : Expression

    data class AddressOf(
        override val location: SourceLocation,
        val expression: Expression
    ) : Expression

    data class AddressOfMut(
        override val location: SourceLocation,
        val expression: Expression
    ) : Expression

    data class Deref(
        override val location: SourceLocation,
        val expression: Expression
    ) : Expression

    data class PointerCast(
        override val location: SourceLocation,
        val toType: TypeAnnotation,
        val arg: Expression
    ) : Expression

    data class If(
        override val location: SourceLocation,
        val condition: Expression,
        val trueBranch: Expression,
        val falseBranch: Expression
    ) : Expression

    data class TypeApplication(
        override val location: SourceLocation,
        val lhs: Expression,
        val args: List<TypeAnnotation>
    ) : Expression

    data class This(
        override val location: SourceLocation
    ) : Expression

    data class Closure(
        override val location: SourceLocation,
        val params: List<Param>,
        val returnType: TypeAnnotation?,
        val body: ClosureBody
    ) : Expression, ScopeTree

    data class As(
        override val location: SourceLocation,
        val lhs: Expression,
        val rhs: TypeAnnotation
    ) : Expression

    data class BlockExpression(val block: Block) : Expression {
        override val location: SourceLocation
            get() = block.location
    }

    data class Intrinsic(
        override val location: SourceLocation,
        val intrinsicType: IntrinsicType
    ) : Expression

    data class UnaryMinus(
        override val location: SourceLocation,
        val expression: Expression
    ) : Expression

    data class Match(
        override val location: SourceLocation,
        val value: Expression,
        val arms: List<Arm>
    ) : Expression, ScopeTree {
        data class Arm(
            val pattern: Pattern,
            val value: Expression
        ) : ScopeTree {
            override val location: SourceLocation
                get() = SourceLocation.between(pattern, value)
        }
    }

    data class Uninitialized(
        override val location: SourceLocation
    ) : Expression

    data class FloatLiteral(override val location: SourceLocation, val value: Double) : Expression

    data class Move(override val location: SourceLocation, val name: Identifier) : Expression

    data class ArrayLiteral(
        override val location: SourceLocation,
        val type: TypeAnnotation?,
        val length: UInt?,
        val items: List<Expression>
    ) : Expression
}

enum class IntrinsicType {
    ADD,
    SUB,
    MUL,

    PTR_TO_INT,
    INT_TO_PTR,

    MEMCPY,

    ERROR
}

sealed class ClosureBody : HasLocation {
    data class Block(val block: hadesc.ast.Block) : ClosureBody() {
        override val location: SourceLocation
            get() = block.location
    }
    data class Expression(val expression: hadesc.ast.Expression) : ClosureBody() {
        override val location: SourceLocation
            get() = expression.location
    }
}

data class Arg(
    val expression: Expression
) : HasLocation {
    override val location: SourceLocation
        get() = expression.location
}

fun Expression.withoutTypeArgs() = when (this) {
    is Expression.TypeApplication -> this.lhs
    else -> this
}
