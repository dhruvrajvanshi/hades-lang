package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class Decorator(
        override val location: SourceLocation,
        val name: Identifier,
        val args: List<Identifier>,
) : HasLocation
