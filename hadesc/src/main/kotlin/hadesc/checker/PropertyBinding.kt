package hadesc.checker

import hadesc.ast.Declaration
import hadesc.resolver.ValueBinding
import hadesc.types.Type

sealed class PropertyBinding {
    abstract val type: Type
    data class Global(
            override val type: Type,
            val binding: ValueBinding
    ): PropertyBinding()
    data class StructField(
            override val type: Type,
            val structDecl: Declaration.Struct,
            val member: Declaration.Struct.Member
    ): PropertyBinding()

    /**
     * extension function defined at top level
     */
    data class GlobalExtensionFunction(
            override val type: Type,
            val def: Declaration.FunctionDef
    ): PropertyBinding()

    /**
     * extension function defined inside an interface
     */
    data class InterfaceExtensionFunction(
            override val type: Type
    ): PropertyBinding()
}