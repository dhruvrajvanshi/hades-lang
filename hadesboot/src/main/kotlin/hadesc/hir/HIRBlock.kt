package hadesc.hir

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class HIRBlock(
        override val location: SourceLocation,
        val statements: List<HIRStatement>
): HasLocation {
    fun prettyPrint(): String = "{\n" + statements.joinToString("\n") {
        it.prettyPrint().prependIndent("  ")
    } + "\n}"
}