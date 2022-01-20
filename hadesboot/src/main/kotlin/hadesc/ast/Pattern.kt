package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

sealed interface Pattern: HasLocation {
    data class IntLiteral(
        override val location: SourceLocation,
        val value: Long
    ): Pattern

    data class EnumVariant(
        val identifier: Identifier
    ): Pattern {
        override val location: SourceLocation
            get() = identifier.location
    }

    data class Wildcard(override val location: SourceLocation): Pattern
}