package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class TypeAnnotation(
    override val location: SourceLocation,
    val kind: Kind
) : HasLocation {
    sealed class Kind {
        object Error: Kind()
        data class Var(val name: Identifier) : Kind()
        data class Ptr(val to: TypeAnnotation) : Kind()
    }
}