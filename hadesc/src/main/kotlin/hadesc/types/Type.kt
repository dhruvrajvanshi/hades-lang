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
    object CInt : Type()
    data class RawPtr(val to: Type) : Type()
    data class Param(val binder: Binder) {
        fun prettyPrint(): String {
            return binder.identifier.name.text
        }
    }

    data class Function(val from: List<Type>, val typeParams: List<Param>?, val to: Type) : Type()

    data class Struct(
        val constructor: Constructor,
        val memberTypes: Map<Name, Type>
    ) : Type() {
        private val indices = memberTypes.keys.map { it.text }.toList()
        fun indexOf(key: String) = indices.indexOf(key)

    }

    data class Constructor(val binder: Binder?, val name: QualifiedName, val params: List<Param>?) : Type()

    data class ParamRef(val name: Binder) : Type()

    data class GenericInstance(val name: Binder, val id: Long) : Type()

    data class Application(val callee: Type, val args: List<Type>) : Type()


    fun prettyPrint(): String = when (this) {
        Error -> "<ErrorType>"
        Byte -> "Byte"
        Void -> "Void"
        Bool -> "Bool"
        CInt -> "CInt"
        is RawPtr -> "*${to.prettyPrint()}"
        is Function -> {
            val typeParams = if (this.typeParams != null) {
                "[${this.typeParams.joinToString(", ") { it.prettyPrint() }}]"
            } else ""
            "$typeParams(${from.joinToString(", ") { it.prettyPrint() }}) -> ${to.prettyPrint()}"
        }
        is Struct -> constructor.name.names.joinToString(".") { it.text }
        is ParamRef -> this.name.identifier.name.text
        is GenericInstance -> name.identifier.name.text
        is Application -> "${callee.prettyPrint()}[${args.joinToString(", ") { it.prettyPrint() }}]"
        is Constructor -> name.mangle()
    }

    fun applySubstitution(substitution: Map<SourceLocation, Type>): Type = when (this) {
        is GenericInstance,
        Error,
        Byte,
        Void,
        CInt,
        Bool -> this
        is RawPtr -> RawPtr(this.to.applySubstitution(substitution))
        is Function -> Function(
            typeParams = this.typeParams,
            from = this.from.map { it.applySubstitution(substitution) },
            to = this.to.applySubstitution(substitution)
        )
        is Struct -> {
            Struct(
                constructor = this.constructor,
                memberTypes = this.memberTypes.mapValues { it.value.applySubstitution(substitution) }
            )
        }
        is ParamRef -> {
            substitution[this.name.location] ?: this
        }
        is Application -> {
            Application(callee.applySubstitution(substitution), args.map { it.applySubstitution(substitution) })
        }
        is Constructor -> this
    }
}

