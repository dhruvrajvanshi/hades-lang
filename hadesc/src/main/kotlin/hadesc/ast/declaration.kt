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
        val thisParam: ThisParam?,
        val params: List<Param>,
        val returnType: TypeAnnotation,
        val body: Block
    ) : Declaration()

    data class ConstDefinition(
        override val location: SourceLocation,
        val name: Binder,
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
        }
    }

    data class Interface(
        override val location: SourceLocation,
        val name: Binder,
        val typeParams: List<TypeParam>?,
        val members: List<Member>
    ) : Declaration() {
        sealed class Member {
            data class FunctionSignature(
                val binder: Binder,
                val typeParams: List<TypeParam>?,
                val thisParam: ThisParam?,
                val params: List<Param>,
                val returnType: TypeAnnotation
            ) : Member()
        }
    }

    data class Implementation(
        override val location: SourceLocation,
        val interfaceRef: InterfaceRef,
        val forType: TypeAnnotation,
        val members: List<Member>
    ) : Declaration() {
        sealed class Member {
            data class FunctionDef(val functionDef: Declaration.FunctionDef): Member()
        }
    }
}


