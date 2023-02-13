package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

sealed interface Pattern : HasLocation {
    data class IntLiteral(
        override val location: SourceLocation,
        val value: Long
    ) : Pattern

    data class EnumCase(
        val identifier: Identifier,
        val args: List<Pattern>?
    ) : Pattern {
        override val location: SourceLocation
            get() = identifier.location
    }

    data class Val(val binder: Binder) : Pattern {
        override val location: SourceLocation
            get() = binder.location
    }

    data class Wildcard(override val location: SourceLocation) : Pattern
}
