package hadesc.hir

import hadesc.Name
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.types.Type

data class HIRParam(
        override val location: SourceLocation,
        val name: Name,
        val type: Type
) : HasLocation {
    fun prettyPrint() = "${name.text}: ${type.prettyPrint()}"
}