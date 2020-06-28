package hadesc.hir

import hadesc.Name
import hadesc.location.HasLocation
import hadesc.location.SourceLocation

sealed class HIRStatement: HasLocation {
    data class Expression(
            val expression: HIRExpression
    ) : HIRStatement() {
        override val location: SourceLocation get() = expression.location
    }

    data class Return(
            override val location: SourceLocation,
            val expression: HIRExpression
    ): HIRStatement()

    data class ReturnVoid(override val location: SourceLocation) : HIRStatement()
    data class Val(
            override val location: SourceLocation,
            val name: Name,
            val isMutable: Boolean,
            val rhs: HIRExpression
    ) : HIRStatement() {
        val type get() = rhs.type
    }
}