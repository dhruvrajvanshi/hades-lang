package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class WhereClause(
    override val location: SourceLocation,
    val params: List<Param>
) : HasLocation
