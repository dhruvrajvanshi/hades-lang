package hadesc.ast

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
}

data class Arg(
    val expression: Expression
) : HasLocation {
    override val location: SourceLocation
        get() = expression.location
}