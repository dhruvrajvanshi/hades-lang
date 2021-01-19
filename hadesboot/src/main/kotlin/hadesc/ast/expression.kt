package hadesc.ast

import hadesc.ir.BinaryOperator
import hadesc.location.HasLocation
import hadesc.location.SourceLocation

sealed class Expression : HasLocation {
    data class Error(override val location: SourceLocation) : Expression()
    data class Var(
        val name: Identifier
    ) : Expression() {
        override val location: SourceLocation
            get() = name.location
    }

    data class Call(
        override val location: SourceLocation,
        val typeArgs: List<TypeAnnotation>?,
        val callee: Expression,
        val args: List<Arg>
    ) : Expression()

    data class Property(
        override val location: SourceLocation,
        val lhs: Expression,
        val property: Identifier
    ) : Expression()

    data class ByteString(
        override val location: SourceLocation,
        val bytes: ByteArray
    ) : Expression()

    data class BoolLiteral(
        override val location: SourceLocation,
        val value: Boolean
    ) : Expression()

    data class NullPtr(
        override val location: SourceLocation
    ) : Expression()

    data class IntLiteral(
        override val location: SourceLocation,
        val value: Int
    ) : Expression()

    data class Not(
        override val location: SourceLocation,
        val expression: Expression
    ) : Expression()


    data class BinaryOperation(
        override val location: SourceLocation,
        val lhs: Expression,
        val operator: BinaryOperator,
        val rhs: Expression
    ) : Expression()

    data class PipelineOperator(
        override val location: SourceLocation,
        val lhs: Expression,
        val rhs: Expression
    ) : Expression()

    data class SizeOf(
        override val location: SourceLocation,
        val type: TypeAnnotation
    ) : Expression()

    data class AddressOf(
        override val location: SourceLocation,
        val expression: Expression
    ) : Expression()

    data class AddressOfMut(
        override val location: SourceLocation,
        val expression: Expression
    ) : Expression()

    data class Deref(
        override val location: SourceLocation,
        val expression: Expression
    ) : Expression()

    data class PointerCast(
        override val location: SourceLocation,
        val toType: TypeAnnotation,
        val arg: Expression
    ) : Expression()

    data class If(
        override val location: SourceLocation,
        val condition: Expression,
        val trueBranch: Expression,
        val falseBranch: Expression
    ) : Expression()

    data class TypeApplication(
        override val location: SourceLocation,
        val lhs: Expression,
        val args: List<TypeAnnotation>
    ) : Expression()

    data class New(
        override val location: SourceLocation,
        val qualifiedPath: QualifiedPath,
        val typeArgs: List<TypeAnnotation>?,
        val args: List<Arg>
    ) : Expression()

    data class This(
            override val location: SourceLocation
    ) : Expression()

    data class Closure(
        override val location: SourceLocation,
        val params: List<Param>,
        val returnType: TypeAnnotation?,
        val body: ClosureBody
    ) : Expression()

    data class TraitMethodCall(
            override val location: SourceLocation,
            val traitName: QualifiedPath,
            val traitArgs: List<TypeAnnotation>,
            val methodName: Identifier,
            val args: List<Arg>
    ) : Expression()

    data class UnsafeCast(
        override val location: SourceLocation,
        val toType: TypeAnnotation,
        val value: Expression
    ) : Expression()
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
