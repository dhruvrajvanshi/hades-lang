package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class TraitRequirementAnnotation(
    val path: QualifiedPath,
    val typeArgs: List<TypeAnnotation>?,
    val negated: Boolean
) : HasLocation {
    override val location get() = SourceLocation.between(path, typeArgs?.lastOrNull() ?: path)
}
