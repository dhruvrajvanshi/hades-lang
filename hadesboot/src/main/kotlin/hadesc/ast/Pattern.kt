package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation


sealed class Pattern: HasLocation {
    data class DotName(val identifier: Identifier, val params: List<Pattern>) : Pattern() {
        override val location get() =
            SourceLocation.between(identifier, params.lastOrNull() ?: identifier)
    }
    data class Name(val binder: Binder): Pattern() {
        override val location: SourceLocation
            get() = binder.location
    }

    data class Else(override val location: SourceLocation): Pattern()
}