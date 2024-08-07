package hadesc.types

import hadesc.BinderId
import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.ast.Binder
import hadesc.ast.TypeParam
import hadesc.hir.HIRTypeParam
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName

typealias ParamRef = Type.Param
sealed interface Type {
    data class Error(val location: SourceLocation, val message: String? = null) : Type
    object Void : Type
    object Bool : Type
    data object CChar : Type
    data class Integral(val size: Int, val isSigned: Boolean) : Type
    data class FloatingPoint(val size: Int) : Type {
        init {
            require(size == 16 || size == 32 || size == 64) {
                "Floating type of $size is not allowed"
            }
        }
    }
    data class Size(val isSigned: Boolean) : Type
    data class Ptr(val to: Type, val isMutable: Boolean) : Type
    data class Param(val binder: Binder): Type {
        val name get() = binder
    }

    data class FunctionPtr(
        val from: List<Type>,
        val to: Type,
    ) : Type

    data class Closure(
        val from: List<Type>,
        val to: Type
    ) : Type

    data class Constructor(val name: QualifiedName) : Type

    data class ForAll(
        val params: List<Param>,
        val body: Type,
    ) : Type

    data class GenericInstance(
        val originalName: Name,
        val location: SourceLocation,
        val id: Long
    ) : Type

    data class Application(val callee: Type, val args: List<Type>) : Type
    data class UntaggedUnion(val members: List<Type>) : Type


    /**
     * A GC managed pointer to [inner] type.
     * This type never exists in source code.
     * If Foo is a reference struct, then
     * it will be lowered to a ref Foo, where
     * Foo becomes a normal struct.
     *
     * The runtime representation of ref types
     * is unspecified, but you can think of it
     * as a pointer to the inner type.
     */
    data class Ref(val inner: Type) : Type
    data class Array(val itemType: Type, val length: Int) : Type

    fun prettyPrint(): String = when (this) {
        is Error -> "Error<$location>"
        Void -> "void"
        Bool -> "bool"
        CChar -> "cchar"
        is Ptr -> {
            if (isMutable) {
                "*mut ${to.prettyPrint()}"
            } else {
                "*${to.prettyPrint()}"
            }
        }
        is FunctionPtr -> {
            "fn(${from.joinToString(", ") { it.prettyPrint() }}) -> ${to.prettyPrint()}"
        }
        is Param -> this.name.identifier.name.text
        is GenericInstance -> originalName.text
        is Application -> "${callee.prettyPrint()}<${args.joinToString(", ") { it.prettyPrint() }}>"
        is Constructor -> name.mangle()
        is Size -> if (isSigned) "isize" else "usize"
        is UntaggedUnion -> "union<" + members.joinToString(", ") { it.prettyPrint() } + ">"
        is ForAll -> "for<${params.joinToString(", ") { it.prettyPrint() }}> => ${body.prettyPrint()}"
        is Integral -> "${if (isSigned) "i" else "u" }$size"
        is FloatingPoint -> "f$size"
        is Closure -> {
            val returnTy = to.prettyPrint()
            val params = from.joinToString(",") { it.prettyPrint() }
            "|$params| -> $returnTy"
        }

        is Ref -> "ref ${inner.prettyPrint()}"
        is Array -> "type[${itemType.prettyPrint()}, $length]"
    }

    fun isIntegral() = when (this) {
        is Integral -> true
        is Size -> true
        else -> false
    }

    fun applySubstitution(substitution: Substitution, thisType: Type? = null): Type {
        fun Type.recurse(): Type {
            return applySubstitution(substitution, thisType)
        }
        return when (this) {
            is GenericInstance,
            is Error,
            Void,
            is Size,
            is Integral,
            is FloatingPoint,
            Bool -> this
            is Ptr -> Ptr(to.recurse(), isMutable = isMutable)
            is FunctionPtr -> FunctionPtr(
                from = this.from.map { it.recurse() },
                to = this.to.recurse()
            )
            is ParamRef -> {
                substitution[this.name.id] ?: this
            }
            is Application -> {
                Application(callee.recurse(), args.map { it.recurse() })
            }
            is Constructor -> this
            is UntaggedUnion -> UntaggedUnion(
                members.map { it.recurse() }
            )
            is ForAll -> ForAll(
                params,
                body.recurse()
            )
            is Closure -> copy(
                from = from.map { it.recurse() },
                to = to.recurse()
            )

            is Ref -> copy(inner.recurse())
            is Array -> Array(itemType.recurse(), length)
            CChar -> this
        }
    }

    fun typeArgs(): List<Type> = when (this) {
        is Application -> args
        else -> emptyList()
    }

    fun nominalName(): QualifiedName = when (this) {
        is Constructor -> name
        is Application -> callee.nominalName()
        is Ref -> inner.nominalName()
        else -> requireUnreachable { prettyPrint() }
    }

    fun applyTypeArgs(typeParams: List<HIRTypeParam>?, typeArgs: List<Type>): Type {
        return if (typeParams == null) {
            check(typeArgs.isEmpty())
            this
        } else {
            check(typeArgs.size == typeParams.size)
            val subst = typeParams.zip(typeArgs).map { (param, arg) ->
                Param(param.toBinder()) to arg
            }.toSubstitution()
            this.applySubstitution(subst)
        }
    }
    fun applyTypeParams(typeParams: List<TypeParam>?, args: List<Type>): Type {
        val subst = Substitution(typeParams?.zip(args)?.associate { it.first.binder.id to it.second } ?: emptyMap())
        return applySubstitution(subst)
    }

    companion object {
        val usize = Size(isSigned = false)
        val isize = Size(isSigned = true)
        val u8 = Integral(8, isSigned = false)
        val f32 = FloatingPoint(32)
        val f64 = FloatingPoint(64)
        val i64 = Integral(isSigned = true, size = 64)
        val i32 = Integral(isSigned = true, size = 32)
    }
}

private val emptySubstitutionValue = Substitution(ofMap = emptyMap())
fun emptySubstitution() = emptySubstitutionValue
class Substitution(ofMap: Map<BinderId, Type>) {
    private val map: Map<BinderId, Type> = ofMap

    operator fun get(binderId: BinderId): Type? = map[binderId]

    fun mapValues(transform: (Map.Entry<BinderId, Type>) -> Type) = Substitution(ofMap = map.mapValues(transform))

    companion object {
        fun of(params: List<HIRTypeParam>?, args: List<Type>?): Substitution {
            if (params == null) {
                return emptySubstitution()
            }
            check(args != null)
            check(params.size == args.size)

            return Substitution(
                ofMap = params.zip(args).associate {
                    it.first.id to it.second
                }
            )
        }
    }
}
fun Map<BinderId, Type>.toSubstitution() = Substitution(ofMap = this)
fun Iterable<Pair<Type.Param, Type>>.toSubstitution() = Substitution(ofMap = associate { it.first.binder.id to it.second })

fun Type.ptr(isMutable: Boolean = false) = Type.Ptr(this, isMutable = isMutable)
fun Type.mutPtr() = Type.Ptr(this, isMutable = true)
