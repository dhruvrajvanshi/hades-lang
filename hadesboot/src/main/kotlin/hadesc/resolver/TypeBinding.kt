package hadesc.resolver

import hadesc.ast.Binder
import hadesc.ast.Declaration
import hadesc.ast.InterfaceRef

sealed class TypeBinding {
    data class Struct(
            val declaration: Declaration.Struct
    ) : TypeBinding()

    data class TypeParam(val binder: Binder) : TypeBinding()

    data class Enum(
            val declaration: Declaration.Enum
    ) : TypeBinding()

    data class TypeAlias(
            val declaration: Declaration.TypeAlias
    ) : TypeBinding()

    data class Interface(
            val declaration: Declaration.InterfaceDef
    ) : TypeBinding()
}
