package hadesc.hir

import hadesc.Name
import hadesc.location.SourceLocation

data class HIRTypeParam(
        val location: SourceLocation,
        val name: Name
)