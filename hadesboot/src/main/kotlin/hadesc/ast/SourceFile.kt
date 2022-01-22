package hadesc.ast

import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName

data class SourceFile(
    override val location: SourceLocation,
    val moduleName: QualifiedName,
    val declarations: List<Declaration>
): ScopeTree