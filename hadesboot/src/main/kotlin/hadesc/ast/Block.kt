package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class Block(
    override val location: SourceLocation,
    val members: List<Member>
) : HasLocation {
    sealed class Member: HasLocation {
        data class Expression(val expression: hadesc.ast.Expression) : Member()
        data class Statement(val statement: hadesc.ast.Statement) : Member()


        override val location: SourceLocation
            get() = when (this) {
                is Expression -> expression.location
                is Statement -> statement.location
            }
    }
}