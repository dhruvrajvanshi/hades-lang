package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class Statement(
    override val location: SourceLocation,
    val kind: Kind
) : HasLocation {
    sealed class Kind {
        object Error : Kind()
    }
}
