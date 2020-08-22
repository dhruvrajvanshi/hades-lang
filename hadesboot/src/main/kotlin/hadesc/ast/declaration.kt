package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

sealed class Declaration : HasLocation {
    data class Error(override val location: SourceLocation) : Declaration()
    data class ImportAs(
        val modulePath: QualifiedPath,
        val asName: Binder
    ) : Declaration() {
        override val location: SourceLocation
            get() = SourceLocation.between(modulePath, asName)
    }

    data class FunctionDef(
        override val location: SourceLocation,
        val externName: Identifier?,
        val signature: FunctionSignature,
        val body: Block
    ) : Declaration() {
        val name get() = signature.name
        val typeParams get() = signature.typeParams
        val params get() = signature.params
    }

    data class ConstDefinition(
        override val location: SourceLocation,
        val name: Binder,
        val annotation: TypeAnnotation?,
        val initializer: Expression
    ) : Declaration()

    data class ExternFunctionDef(
        override val location: SourceLocation,
        val binder: Binder,
        val paramTypes: List<TypeAnnotation>,
        val returnType: TypeAnnotation,
        val externName: Identifier
    ) : Declaration()

    data class Struct(
        override val location: SourceLocation,
        val decorators: List<Decorator>,
        val binder: Binder,
        val typeParams: List<TypeParam>? = null,
        val members: List<Member>
    ) : Declaration() {

        sealed class Member {
            data class Field(
                val binder: Binder,
                val isMutable: Boolean,
                val typeAnnotation: TypeAnnotation
            ) : Member()
        }
    }

    data class Enum(
        override val location: SourceLocation,
        val name: Binder,
        val typeParams: List<TypeParam>?,
        val cases: List<Case>
    ) : Declaration() {
        data class Case(
            val name: Binder,
            val params: List<TypeAnnotation>
        )
    }

    data class TypeAlias(
            override val location: SourceLocation,
            val name: Binder,
            val typeParams: List<TypeParam>?,
            val rhs: TypeAnnotation
    ) : Declaration()

    data class ExtensionDef(
            override val location: SourceLocation,
            val name: Binder,
            val typeParams: List<TypeParam>?,
            val forType: TypeAnnotation,
            val functionDefs: List<FunctionDef>
    ) : Declaration()
}


