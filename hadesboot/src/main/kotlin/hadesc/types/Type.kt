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
    object Double : Type()
    object Size : Type()
    data class Ptr(val to: Type, val isMutable: Boolean) : Type()
    data class Param(val binder: Binder) {
        fun prettyPrint(): String {
            return binder.identifier.name.text
        }
    }

    data class Function(
        val receiver: Type?,
        val from: List<Type>,
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

    data class Constructor(val binder: Binder?, val name: QualifiedName) : Type()

    data class ParamRef(val name: Binder) : Type()

    data class TypeFunction(val params: List<Param>, val body: Type): Type()

    data class GenericInstance(
        val name: Binder,
        val id: Long,
        val constraints: List<Constraint>,
        val intantiationLocation: SourceLocation
    ) : Type()

    data class Application(val callee: Type, val args: List<Type>) : Type()
    data class ThisRef(val location: SourceLocation) : Type()
    data class UntaggedUnion(val members: List<Type>) : Type()


    fun prettyPrint(): String = when (this) {
        Error -> "<ErrorType>"
        Byte -> "Byte"
        Void -> "Void"
        Bool -> "Bool"
        CInt -> "Int"
        Double -> "Double"
        is Ptr -> {
            if (isMutable)
                "*mut ${to.prettyPrint()}"
            else "*${to.prettyPrint()}"
        }
        is Function -> {
            val receiver = if (this.receiver == null) "" else "this: ${this.receiver.prettyPrint()}, "
            val whereClause = if (constraints.isEmpty()) "" else " where ${ this.constraints.joinToString(", ") {it.prettyPrint()} }"
            "($receiver${from.joinToString(", ") { it.prettyPrint() }}) -> ${to.prettyPrint()}$whereClause"
        }
        is ParamRef -> this.name.identifier.name.text
        is GenericInstance -> "${name.identifier.name.text} : ${constraints.joinToString(", ") {
            val argsStr = if (it.args.isEmpty()) ""
            else "[${it.args.joinToString(", ")}]"
            it.interfaceName.mangle() + argsStr
        } }"
        is Application -> "${callee.prettyPrint()}[${args.joinToString(", ") { it.prettyPrint() }}]"
        is Constructor -> name.mangle()
        Size -> "Size"
        is ThisRef -> "This"
        is UntaggedUnion -> "union[" + members.joinToString(", ") { it.prettyPrint() } + "]"
        is TypeFunction -> "type[${params.joinToString(", ") { it.prettyPrint() }}] => ${body.prettyPrint()}"
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
            Double,
            Bool -> this
            is Ptr -> Ptr(to.recurse(), isMutable = isMutable)
            is Function -> Function(
                receiver = receiver?.recurse(),
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
                Application(callee.recurse(), args.map { it.recurse() })
            }
            is Constructor -> this
            is ThisRef -> thisType?.recurse() ?: this
            is UntaggedUnion -> UntaggedUnion(
                    members.map { it.recurse() }
            )
            is TypeFunction -> TypeFunction(
                    params,
                    body.recurse()
            )
        }
    }

    override fun toString(): String {
        return prettyPrint()
    }
}

typealias Substitution = Map<SourceLocation, Type>

