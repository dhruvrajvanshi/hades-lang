package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class TraitRef(
        val path: QualifiedPath,
        val typeArgs: List<TypeAnnotation>?
): HasLocation {
        override val location get() = SourceLocation.between(path, typeArgs?.lastOrNull() ?: path)
}