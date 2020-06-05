package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

sealed class Statement : HasLocation {
    data class Return(
        override val location: SourceLocation,
        val value: Expression
    ) : Statement()

    data class Val(
        override val location: SourceLocation,
        val isMutable: Boolean,
        val binder: Binder,
        val typeAnnotation: TypeAnnotation?,
        val rhs: Expression
    ) : Statement()

    data class While(
        override val location: SourceLocation,
        val condition: Expression,
        val body: Block
    ) : Statement()

    data class If(
        override val location: SourceLocation,
        val condition: Expression,
        val ifTrue: Block,
        val ifFalse: Block?
    ) : Statement()

    data class LocalAssignment(
        override val location: SourceLocation,
        val name: Identifier,
        val value: Expression
    ) : Statement()

    data class Error(override val location: SourceLocation) : Statement()
}
