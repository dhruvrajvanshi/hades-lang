package hadesc.ast

import hadesc.Name
import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class Identifier(
    override val location: SourceLocation,
    val name: Name
) : HasLocation
