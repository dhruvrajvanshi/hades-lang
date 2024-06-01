package hadesboot.middle

sealed interface Type {
    data object USize : Type
    data object ISize : Type
    data object I32: Type
    data object U32: Type
    data class Tuple(val members: List<Type>) : Type
}