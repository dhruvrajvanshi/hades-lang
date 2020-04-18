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
        val scopeStartToken: Token,
        val name: Binder,
        val typeParams: List<TypeParam>?,
        val params: List<Param>,
        val returnType: TypeAnnotation,
        val body: Block
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
        val binder: Binder,
        val members: List<Member>,
        val typeParams: List<TypeParam>? = null
    ) : Declaration() {

        sealed class Member {
            object Error : Member()
            data class Field(
                val binder: Binder,
                val typeAnnotation: TypeAnnotation
            ) : Member()
        }
    }

}


