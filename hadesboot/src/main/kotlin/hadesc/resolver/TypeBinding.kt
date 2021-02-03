package hadesc.resolver

import hadesc.ast.Binder
import hadesc.ast.Declaration
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
}
