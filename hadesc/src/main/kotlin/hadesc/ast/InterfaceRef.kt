package hadesc.ast

data class InterfaceRef(
        val path: QualifiedPath,
        val typeArgs: List<TypeAnnotation>?
)