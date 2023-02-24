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

    data class MutPtr(
        override val location: SourceLocation,
        val to: TypeAnnotation
    ) : TypeAnnotation()

    data class Application(
        override val location: SourceLocation,
        val callee: TypeAnnotation,
        val args: List<TypeAnnotation>
    ) : TypeAnnotation()

    data class Qualified(
        override val location: SourceLocation,
        val qualifiedPath: QualifiedPath
    ) : TypeAnnotation()

    data class FunctionPtr(
        override val location: SourceLocation,
        val from: List<TypeAnnotation>,
        val to: TypeAnnotation
    ) : TypeAnnotation()

    data class Closure(
        override val location: SourceLocation,
        val from: List<TypeAnnotation>,
        val to: TypeAnnotation
    ) : TypeAnnotation()

    data class Union(
        override val location: SourceLocation,
        val args: List<TypeAnnotation>
    ) : TypeAnnotation()

    data class Select(
        override val location: SourceLocation,
        val lhs: TypeAnnotation,
        val rhs: Identifier
    ) : TypeAnnotation()

    data class Array(
        override val location: SourceLocation,
        val itemType: TypeAnnotation,
        val length: UInt
    ) : TypeAnnotation()
}
