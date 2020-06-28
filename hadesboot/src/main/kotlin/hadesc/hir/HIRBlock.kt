package hadesc.hir

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class HIRBlock(
        override val location: SourceLocation,
        val statements: List<HIRStatement>
): HasLocation