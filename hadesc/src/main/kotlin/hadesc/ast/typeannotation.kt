package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

sealed class TypeAnnotation : HasLocation {
    data class Error(override val location: SourceLocation) : TypeAnnotation()
    data class Var(val name: Identifier) : TypeAnnotation() {
        override val location: SourceLocation
            get() = name.location
    }

    data class Ptr(
        override val location: SourceLocation,
        val to: TypeAnnotation
    ) : TypeAnnotation()

    data class Application(
        override val location: SourceLocation,
        val callee: TypeAnnotation,
        val args: List<TypeAnnotation>
    ) : TypeAnnotation()
}