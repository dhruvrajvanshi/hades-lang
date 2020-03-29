package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class Param(
    override val location: SourceLocation,
    val binder: Binder,
    val annotation: TypeAnnotation?
) : HasLocation