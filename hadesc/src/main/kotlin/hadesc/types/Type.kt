package hadesc.types

sealed class Type {
    object Byte : Type()
    object Void : Type()
    data class RawPtr(val to: Type) : Type()
    data class Function(val from: List<Type>, val to: Type) : Type()
}
