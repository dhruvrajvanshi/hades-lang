package hadesc.types

import hadesc.Name
import hadesc.qualifiedname.QualifiedName

sealed class Type {
    object Error : Type()
    object Byte : Type()
    object Void : Type()
    data class RawPtr(val to: Type) : Type()
    data class Function(val from: List<Type>, val to: Type) : Type()
    data class Struct(val name: QualifiedName, val memberTypes: Map<Name, Type>) : Type()
}
