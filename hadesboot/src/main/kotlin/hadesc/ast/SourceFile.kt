package hadesc.ast

import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName

data class SourceFile(
    override val location: SourceLocation,
    val id: Id,
    val moduleName: QualifiedName,
    val declarations: List<Declaration>,
) : ScopeTree {
    @JvmInline
    value class Id(val value: Int)
}
