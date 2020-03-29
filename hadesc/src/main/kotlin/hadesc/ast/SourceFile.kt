package hadesc.ast

import hadesc.qualifiedpath.QualifiedName

data class SourceFile(
    val moduleName: QualifiedName,
    val declarations: List<Declaration>
)