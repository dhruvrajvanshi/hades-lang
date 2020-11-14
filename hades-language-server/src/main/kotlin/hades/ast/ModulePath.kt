package hades.ast

data class ModulePath(
    val head: Name,
    val tail: List<Name>
)