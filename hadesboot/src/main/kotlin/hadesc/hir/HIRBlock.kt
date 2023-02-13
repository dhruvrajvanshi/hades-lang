package hadesc.hir

import hadesc.Name
import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class HIRBlock(
    override val location: SourceLocation,
    val name: Name,
    val statements: MutableList<HIRStatement> = mutableListOf()
) : HasLocation {
    fun prettyPrint(): String = "{\n" + statements.joinToString("\n") {
        it.prettyPrint().prependIndent("  ")
    } + "\n}"
}
