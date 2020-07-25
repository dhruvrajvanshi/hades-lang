package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

sealed class ModulePath: HasLocation {
    data class Name(val identifier: Identifier): ModulePath() {
        override val location: SourceLocation
            get() = identifier.location
    }

    data class TypeApplication(
            override val location: SourceLocation,
            val base: ModulePath,
            val args: List<TypeAnnotation>
    ): ModulePath()

    data class Property(
            val base: ModulePath,
            val property: Identifier
    ) : ModulePath() {
        override val location: SourceLocation
            get() = SourceLocation.between(base, property)
    }

}