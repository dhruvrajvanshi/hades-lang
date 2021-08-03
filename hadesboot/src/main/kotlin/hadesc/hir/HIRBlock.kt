package hadesc.hir

import hadesc.Name
import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class HIRBlock(
        override val location: SourceLocation,
        val statements: MutableList<HIRStatement> = mutableListOf(),
        val name: Name? = null
): HasLocation {
    fun prettyPrint(): String = "{\n" + statements.joinToString("\n") {
        it.prettyPrint().prependIndent("  ")
    } + "\n}"
}