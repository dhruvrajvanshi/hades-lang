package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

sealed interface Pattern: HasLocation {
    data class IntLiteral(
        override val location: SourceLocation,
        val value: Long
    ): Pattern

    data class Wildcard(override val location: SourceLocation): Pattern
}