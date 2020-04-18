package hadesc.types

import hadesc.Name
import hadesc.ast.Binder
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName

sealed class Type {
    object Error : Type()
    object Byte : Type()
    object Void : Type()
    object Bool : Type()
    data class RawPtr(val to: Type) : Type()
    data class Param(val binder: Binder) {
        fun prettyPrint(): String {
            return binder.identifier.name.text
        }
    }

    data class Function(val from: List<Type>, val typeParams: List<Param>?, val to: Type) : Type()

    data class Struct(val name: QualifiedName, val memberTypes: Map<Name, Type>) : Type() {
        private val indices = memberTypes.keys.map { it.text }.toList()
        fun indexOf(key: String) = indices.indexOf(key)

    }

    data class ParamRef(
        val binder: Binder,
        val typeParamIndex: Int
    ) : Type()

    data class Deferred(
        val paramRef: ParamRef,
        val callLocation: SourceLocation
    ) : Type()

    fun prettyPrint(): String = when (this) {
        Error -> "<ErrorType>"
        Byte -> "Byte"
        Void -> "Void"
        Bool -> "Bool"
        is RawPtr -> "*${to.prettyPrint()}"
        is Function -> {
            val typeParams = if (this.typeParams != null) {
                "[${this.typeParams.joinToString(", ") { it.prettyPrint() }}]"
            } else ""
            "$typeParams(${from.joinToString(", ") { it.prettyPrint() }}) -> ${to.prettyPrint()}"
        }
        is Struct -> "%${name.names.joinToString(".") { it.text }}"
        is ParamRef -> this.binder.identifier.name.text
        is Deferred -> TODO()
    }
}
