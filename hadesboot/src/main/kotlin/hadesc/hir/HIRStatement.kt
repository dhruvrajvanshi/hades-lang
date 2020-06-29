package hadesc.hir

import hadesc.Name
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.types.Type

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
    data class ValDeclaration(
            override val location: SourceLocation,
            val name: Name,
            val isMutable: Boolean,
            val type: Type
    ) : HIRStatement()


    data class Assignment(
            override val location: SourceLocation,
            val name: Name,
            val value: HIRExpression
    ) : HIRStatement()

    data class If(
            override val location: SourceLocation,
            val condition: HIRExpression,
            val trueBranch: HIRBlock,
            val falseBranch: HIRBlock
    ) : HIRStatement()

    data class While(
            override val location: SourceLocation,
            val condition: HIRExpression,
            val body: HIRBlock
    ) : HIRStatement()
}