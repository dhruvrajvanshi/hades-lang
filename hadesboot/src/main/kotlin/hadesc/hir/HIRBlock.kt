package hadesc.hir

import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.printIndent

data class HIRBlock(
        override val location: SourceLocation,
        val statements: List<HIRStatement>
): HasLocation {
    fun prettyPrint(indent: Int = 0): String = "{\n" + statements.joinToString("\n") {
        printIndent(indent + 1) + it.prettyPrint()
    } + "\n}"
}