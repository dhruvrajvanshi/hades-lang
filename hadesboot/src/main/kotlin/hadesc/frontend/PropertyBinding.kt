package hadesc.frontend

import hadesc.ast.Declaration
import hadesc.resolver.Binding

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
}