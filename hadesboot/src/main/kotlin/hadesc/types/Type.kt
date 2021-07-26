package hadesc.types

import hadesc.Name
import hadesc.analysis.TraitRequirement
import hadesc.ast.Binder
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName

sealed class Type {
    data class Error(val location: SourceLocation) : Type()
    object Void : Type()
    object Bool : Type()
    data class Integral(val size: Int, val isSigned: Boolean) : Type()
    data class FloatingPoint(val size: Int) : Type() {
        init {
            require(size == 16 || size == 32 || size == 64) {
                "Floating type of $size is not allowed"
            }
        }
    }
    object Double : Type()
    object Size : Type()
    data class Ptr(val to: Type, val isMutable: Boolean) : Type()
    data class Param(val binder: Binder) {
        fun prettyPrint(): String {
            return binder.identifier.name.text
        }

        val ref get() = ParamRef(binder)
    }

    data class Function(
            val from: List<Type>,
            val traitRequirements: List<TraitRequirement>?,
            val to: Type
    ) : Type()

    data class Constructor(val binder: Binder?, val name: QualifiedName) : Type()

    data class ParamRef(val name: Binder) : Type()

    data class TypeFunction(val params: List<Param>, val body: Type): Type()

    data class GenericInstance(
        val name: Binder,
        val id: Long
    ) : Type()

    data class Uninferrable(val name: Binder) : Type()

    data class Application(val callee: Type, val args: List<Type>) : Type()
    data class UntaggedUnion(val members: List<Type>) : Type()

    data class AssociatedTypeRef(val binder: Binder) : Type()

    data class Select(val traitName: QualifiedName, val traitArgs: List<Type>, val associatedTypeName: Name) : Type()


    fun prettyPrint(): String = when (this) {
        is Error -> "Error<$location>"
        Void -> "Void"
        Bool -> "Bool"
        Double -> "Double"
        is Ptr -> {
            if (isMutable)
                "*mut ${to.prettyPrint()}"
            else "*${to.prettyPrint()}"
        }
        is Function -> {
            "(${from.joinToString(", ") { it.prettyPrint() }}) -> ${to.prettyPrint()}"
        }
        is ParamRef -> this.name.identifier.name.text
        is GenericInstance -> name.identifier.name.text
        is Application -> "${callee.prettyPrint()}[${args.joinToString(", ") { it.prettyPrint() }}]"
        is Constructor -> name.mangle()
        Size -> "Size"
        is UntaggedUnion -> "union[" + members.joinToString(", ") { it.prettyPrint() } + "]"
        is TypeFunction -> "type[${params.joinToString(", ") { it.prettyPrint() }}] => ${body.prettyPrint()}"
        is Integral -> "${if(isSigned) "i" else "u" }${size}"
        is FloatingPoint -> "f${size}"
        is Uninferrable -> name.name.text
        is AssociatedTypeRef -> binder.identifier.name.text
        is Select -> "${traitName.mangle()}[${traitArgs.joinToString(", ") { it.prettyPrint() }}].${associatedTypeName.text}"
    }

    fun isIntegral() = when(this) {
        is Integral -> true
        is Size -> true
        else -> false
    }

    fun applySubstitution(substitution: Map<SourceLocation, Type>, thisType: Type? = null): Type {
        fun Type.recurse(): Type {
            return applySubstitution(substitution, thisType)
        }
        return when (this) {
            is GenericInstance,
            is Error,
            Void,
            Size,
            Double,
            is Integral,
            is FloatingPoint,
            Bool -> this
            is Ptr -> Ptr(to.recurse(), isMutable = isMutable)
            is Function -> Function(
                from = this.from.map { it.recurse() },
                traitRequirements = this.traitRequirements?.map {
                    TraitRequirement(it.traitRef, it.arguments.map { t -> t.recurse() })
                },
                to = this.to.recurse()
            )
            is ParamRef -> {
                substitution[this.name.location] ?: this
            }
            is Application -> {
                Application(callee.recurse(), args.map { it.recurse() })
            }
            is Constructor -> this
            is UntaggedUnion -> UntaggedUnion(
                    members.map { it.recurse() }
            )
            is TypeFunction -> TypeFunction(
                    params,
                    body.recurse()
            )
            is Uninferrable -> this
            is AssociatedTypeRef -> {
                substitution[this.binder.location] ?: this
            }
            is Select -> copy(
                traitArgs = traitArgs.map { it.recurse() }
            )
        }
    }

    override fun toString(): String {
        return prettyPrint()
    }

    fun typeArgs(): List<Type> = when(this) {
        is Application -> args
        else -> emptyList()
    }
}

typealias Substitution = Map<SourceLocation, Type>

