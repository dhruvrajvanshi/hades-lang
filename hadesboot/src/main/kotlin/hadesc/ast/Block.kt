package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class Block(
    override val location: SourceLocation,
    val members: List<Member>
) : HasLocation {
    sealed class Member {
        data class Expression(val expression: hadesc.ast.Expression) : Member()
        data class Statement(val statement: hadesc.ast.Statement) : Member()
    }
}