package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class QualifiedPath(
    val identifiers: List<Identifier>
) : HasLocation {
    init {
        assert(identifiers.isNotEmpty()) { "Created empty QualifiedPath" }
    }

    override val location: SourceLocation
        get() {
            val start = identifiers.first()
            val last = identifiers.last()
            return SourceLocation.between(start, last)
        }
}