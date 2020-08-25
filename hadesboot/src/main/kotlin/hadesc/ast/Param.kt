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

@Deprecated("")
data class ThisParam(
    override val location: SourceLocation,
    val annotation: TypeAnnotation
) : HasLocation

data class TypeParam(
    val binder: Binder
) : HasLocation {
    fun prettyPrint(): String = binder.identifier.name.text
    override val location: SourceLocation
        get() = binder.location

    override fun hashCode(): Int {
        return location.hashCode()
    }

    override fun equals(other: Any?): Boolean {
        return other is Binder && location == other.location
    }
}
