package hadesc.hir

import hadesc.ast.Binder
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.types.Type

data class HIRParam(
    override val location: SourceLocation,
    val binder: Binder,
    val type: Type
) : HasLocation {
    fun prettyPrint() = "${name.text}: ${type.prettyPrint()}"
    val name get() = binder.name
}
