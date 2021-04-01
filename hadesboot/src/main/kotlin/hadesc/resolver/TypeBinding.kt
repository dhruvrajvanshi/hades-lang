package hadesc.resolver

import hadesc.ast.Binder
import hadesc.ast.Declaration
import hadesc.location.HasLocation
import hadesc.types.Type

sealed class TypeBinding {
    data class Struct(
            val declaration: Declaration.Struct
    ) : TypeBinding()

    data class TypeParam(val binder: Binder) : TypeBinding()

    data class TypeAlias(
            val declaration: Declaration.TypeAlias
    ) : TypeBinding()

    data class Trait(
            val declaration: Declaration.TraitDef
    ) : TypeBinding()

    data class SealedType(
        val declaration: Declaration.SealedType) : TypeBinding() {}

    data class Builtin(val type: Type) : TypeBinding()

    data class AssociatedType(val binder: Binder) : TypeBinding()

    fun isGlobal() = when(this) {
        is Builtin -> true
        is SealedType -> true
        is Struct -> true
        is Trait -> true
        is TypeAlias -> true
        is TypeParam -> false
        is AssociatedType -> false
    }

    fun isLocalTo(scope: HasLocation) = when(this) {
        is TypeParam -> binder.location.isWithin(scope.location)
        else -> false
    }
}
