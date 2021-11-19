package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

sealed interface Pattern: HasLocation {
    data class IntLiteral(
        override val location: SourceLocation,
        val value: Long
    ): Pattern

    data class EnumCase(
        val name: Identifier,
        val params: List<Pattern>?
    ): Pattern {
        override val location get() = name.location
    }

    data class Val(val name: Binder): Pattern, HasLocation by name

    data class Wildcard(override val location: SourceLocation): Pattern
}