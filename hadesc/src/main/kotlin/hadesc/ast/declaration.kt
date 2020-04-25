package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

inline class DeclarationFlags constructor(val flags: Int) {
    companion object {
        val EMPTY = DeclarationFlags(0)
        val METHOD = DeclarationFlags(1 shl 0)
        val STATIC = DeclarationFlags(1 shl 1)
        val PUBLIC = DeclarationFlags(1 shl 2)
    }

    infix fun and(other: DeclarationFlags): DeclarationFlags {
        return DeclarationFlags(flags or other.flags)
    }

    operator fun contains(expected: DeclarationFlags): Boolean {
        return (flags and expected.flags) > 0
    }
}

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
        val flags: DeclarationFlags,
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
        val typeParams: List<TypeParam>? = null,
        val members: List<Member>
    ) : Declaration() {

        sealed class Member {
            object Error : Member()
            data class Field(
                val binder: Binder,
                val typeAnnotation: TypeAnnotation
            ) : Member()

            data class Method(
                val function: FunctionDef
            ) : Member()
        }
    }

}


