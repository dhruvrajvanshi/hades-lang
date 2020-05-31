package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation


sealed class Pattern: HasLocation {
    data class DotName(val identifier: Identifier, val params: List<Pattern>) : Pattern() {
        override val location get() =
            SourceLocation.between(identifier, params.lastOrNull() ?: identifier)
    }
    data class Name(val identifier: Identifier): Pattern() {
        override val location: SourceLocation
            get() = identifier.location
    }
}