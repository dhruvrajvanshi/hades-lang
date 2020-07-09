package hadesc.typer

import hadesc.ast.Declaration
import hadesc.resolver.Binding
import hadesc.types.Type

sealed class PropertyBinding {
    data class Global(
            val binding: Binding
    ): PropertyBinding()
    data class StructField(
            val structDecl: Declaration.Struct,
            val memberIndex: Int
    ): PropertyBinding() {
        val member get(): Declaration.Struct.Member.Field = structDecl.members[memberIndex] as Declaration.Struct.Member.Field
    }
    data class StructFieldPointer(
        val structDecl: Declaration.Struct,
        val memberIndex: Int
    ): PropertyBinding() {
        val member get(): Declaration.Struct.Member.Field = structDecl.members[memberIndex] as Declaration.Struct.Member.Field
    }

    /**
     * extension function defined at top level
     */
    data class GlobalExtensionFunction(
            val def: Declaration.FunctionDef
    ): PropertyBinding()

    /**
     * extension function defined inside an interface
     */
    data class InterfaceExtensionFunction(
        val implementationBinding: ImplementationBinding,
        val memberIndex: Int
    ): PropertyBinding()
}