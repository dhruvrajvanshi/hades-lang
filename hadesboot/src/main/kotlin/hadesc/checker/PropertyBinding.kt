package hadesc.checker

import hadesc.ast.Declaration
import hadesc.resolver.Binding
import hadesc.types.Type

sealed class PropertyBinding {
    abstract val type: Type
    data class Global(
            override val type: Type,
            val binding: Binding
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
        override val type: Type,
        val implementationBinding: ImplementationBinding,
        val memberIndex: Int
    ): PropertyBinding()
}