package hadesc.types

import hadesc.ast.Binder
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName

sealed class Type {
    object Error : Type()
    object Byte : Type()
    object Void : Type()
    object Bool : Type()
    object CInt : Type()
    object Size : Type()
    data class RawPtr(val to: Type) : Type()
    data class Param(val binder: Binder) {
        fun prettyPrint(): String {
            return binder.identifier.name.text
        }
    }

    data class Function(
        val receiver: Type?,
        val from: List<Type>,
        val typeParams: List<Param>?,
        val to: Type,
        val constraints: List<Constraint> = listOf()
    ) : Type()

    data class Constraint(
        val interfaceName: QualifiedName,
        val args: List<Type>,
        val param: Param
    ) {
        fun prettyPrint(): String {
            val argsStr = if (args.isEmpty())
                ""
            else "[${args.joinToString(", ") {it.prettyPrint()} }]"
            return "${param.binder.identifier.name.text}: ${interfaceName.mangle()}$argsStr"
        }
    }

    data class Constructor(val binder: Binder?, val name: QualifiedName, val params: List<Param>?) : Type()

    data class ParamRef(val name: Binder) : Type()

    data class GenericInstance(val name: Binder, val id: Long) : Type()

    data class Application(val callee: Constructor, val args: List<Type>) : Type()
    data class ThisRef(val location: SourceLocation) : Type()


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
            val receiver = if (this.receiver == null) "" else "this: ${this.receiver.prettyPrint()}, "
            val whereClause = if (constraints.isEmpty()) "" else " where ${ this.constraints.joinToString(", ") {it.prettyPrint()} }"
            "$typeParams($receiver${from.joinToString(", ") { it.prettyPrint() }}) -> ${to.prettyPrint()}$whereClause"
        }
        is ParamRef -> this.name.identifier.name.text
        is GenericInstance -> name.identifier.name.text
        is Application -> "${callee.prettyPrint()}[${args.joinToString(", ") { it.prettyPrint() }}]"
        is Constructor -> name.mangle()
        Size -> "Size"
        is ThisRef -> "This"
    }

    fun applySubstitution(substitution: Map<SourceLocation, Type>, thisType: Type? = null): Type {
        fun Type.recurse(): Type {
            return applySubstitution(substitution, thisType)
        }
        return when (this) {
            is GenericInstance,
            Error,
            Byte,
            Void,
            CInt,
            Size,
            Bool -> this
            is RawPtr -> RawPtr(to.recurse())
            is Function -> Function(
                receiver = receiver?.recurse(),
                typeParams = this.typeParams,
                from = this.from.map { it.recurse() },
                to = this.to.recurse(),
                constraints = this.constraints.map {
                    Constraint(
                            it.interfaceName,
                            param = it.param,
                            args = it.args.map { arg -> arg.recurse() }
                    )
                }
            )
            is ParamRef -> {
                substitution[this.name.location] ?: this
            }
            is Application -> {
                Application(callee.recurse() as Constructor, args.map { it.recurse() })
            }
            is Constructor -> this
            is ThisRef -> thisType?.recurse() ?: this
        }
    }

    override fun toString(): String {
        return prettyPrint()
    }
}

