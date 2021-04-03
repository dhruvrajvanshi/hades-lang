package hadesc.frontend

import hadesc.Name
import hadesc.ast.Declaration
import hadesc.ast.Identifier
import hadesc.ast.QualifiedPath
import hadesc.qualifiedname.QualifiedName
import hadesc.resolver.Binding
import hadesc.types.Type

sealed class PropertyBinding {
    data class Global(
            val binding: Binding
    ): PropertyBinding()
    data class StructField(
            val structDecl: Declaration.Struct,
            val memberIndex: Int,
            val type: Type
    ): PropertyBinding() {
        val member get(): Declaration.Struct.Member.Field = structDecl.members[memberIndex] as Declaration.Struct.Member.Field
    }
    data class StructFieldPointer(
        val structDecl: Declaration.Struct,
        val memberIndex: Int,
        val type: Type
    ): PropertyBinding() {
        val member get(): Declaration.Struct.Member.Field = structDecl.members[memberIndex] as Declaration.Struct.Member.Field
    }
    data class ExtensionDef(
        val extensionDef: Declaration.ExtensionDef,
        val functionIndex: Int,
        val type: Type
    ) : PropertyBinding() {
        val functionDef get() = extensionDef.functionDefs[functionIndex]
    }

    data class WhereParamRef(
            val traitDef: Declaration.TraitDef,
            val memberIndex: Int,
            val type: Type
    ) : PropertyBinding() {
        val member = traitDef.signatures[memberIndex]
    }

    data class SealedTypeCaseConstructor(
        val declaration: Declaration.SealedType,
        val case: Declaration.SealedType.Case,
        val type: Type
    ) : PropertyBinding()

    data class WhenCaseFieldRef(
        val declaration: Declaration.SealedType,
        val caseName: Name,
        val typeArgs: List<Type>,
        val name: Identifier,
        val propertyIndex: Int,
        val propertyName: Identifier,
        val type: Type
    ) : PropertyBinding()

    data class TraitFunctionRef(
        val traitName: QualifiedName,
        val args: List<Type>,
        val type: Type
    ) : PropertyBinding()
}