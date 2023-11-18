package hadesc.hir

import hadesc.BinderId
import hadesc.Name
import hadesc.ast.Binder
import hadesc.ast.Identifier
import hadesc.location.SourceLocation

data class HIRTypeParam(
    val location: SourceLocation,
    val name: Name,
    val id: BinderId
) {
    fun prettyPrint(): String = name.text

    fun toBinder(): Binder = Binder(Identifier(location, name), id)
}
