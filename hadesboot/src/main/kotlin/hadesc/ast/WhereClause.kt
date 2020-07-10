package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class WhereClause(
    override val location: SourceLocation,
    val constraints: List<WhereClauseConstraint>
) : HasLocation

data class WhereClauseConstraint(
    val param: Identifier,
    val interfaceRef: InterfaceRef
): HasLocation {
    override val location get() = SourceLocation.between(param, interfaceRef)
}
