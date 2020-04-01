package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class Expression(
    override val location: SourceLocation,
    val kind: Kind
) : HasLocation {
    sealed class Kind {
        object Error : Kind()
        data class Var(val name: Identifier) : Kind()
        data class Call(val callee: Expression, val args: List<Arg>) : Kind()
        data class Property(val lhs: Expression, val property: Identifier) : Kind()
        data class ByteString(val bytes: ByteArray) : Kind()
        data class BoolLiteral(val value: Boolean) : Kind()
    }
}

data class Arg(
    val expression: Expression
) : HasLocation {
    override val location: SourceLocation
        get() = expression.location
}