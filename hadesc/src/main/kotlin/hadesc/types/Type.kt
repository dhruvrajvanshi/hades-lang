package hadesc.types

import hadesc.Name
import hadesc.ast.Binder
import hadesc.ast.TypeParam
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName

sealed class Type {
    object Error : Type()
    object Byte : Type()
    object Void : Type()
    object Bool : Type()
    data class RawPtr(val to: Type) : Type()
    data class Function(val from: List<Type>, val to: Type) : Type()
    data class GenericFunction(
        val typeParams: List<TypeParam>,
        val from: List<Type>,
        val to: Type
    ) : Type()

    data class Struct(val name: QualifiedName, val memberTypes: Map<Name, Type>) : Type() {
        private val indices = memberTypes.keys.map { it.text }.toList()
        fun indexOf(key: String) = indices.indexOf(key)

    }

    // this isn't a real runtime type
    // any property accesses on this should
    // be resolved to a fully qualified global
    // name
    data class ModuleAlias(
        val qualifiedName: QualifiedName
    ) : Type()

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
        is Function -> "(${from.joinToString(", ") { it.prettyPrint() }}) -> ${to.prettyPrint()}"
        is GenericFunction ->
            "[${typeParams.joinToString(", ") { it.prettyPrint() }}]" +
                    "(${from.joinToString(", ") { it.prettyPrint() }}) -> ${to.prettyPrint()}"
        is Struct -> "%${name.names.joinToString(".") { it.text }}"
        is ModuleAlias -> TODO()
        is ParamRef -> TODO()
        is Deferred -> TODO()
    }
}
