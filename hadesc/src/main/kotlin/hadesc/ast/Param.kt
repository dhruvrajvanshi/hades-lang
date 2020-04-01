package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class Param(
    val binder: Binder,
    val annotation: TypeAnnotation?
) : HasLocation {
    override val location: SourceLocation
        get() = SourceLocation.between(binder, annotation ?: binder)
}

data class TypeParam(
    val binder: Binder
) : HasLocation {
    override val location: SourceLocation
        get() = binder.location
}
