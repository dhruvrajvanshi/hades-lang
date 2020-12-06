package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import kotlin.math.sign

sealed class Declaration : HasLocation {
    /**
     * Location of the first few tokens for error reporting
     * Useful for larger declarations where we wan't to report
     * an error for the entire declaration but the full declaration
     * is just to noisy of an error span. For example, an error like,
     * "Only function defs are allowed inside extensions" should ideally
     * only highlight the first token or two for a struct declaration.
     */
    open val startLoc get() = location

    data class Error(override val location: SourceLocation) : Declaration()
    data class ImportAs(
        val modulePath: QualifiedPath,
        val asName: Binder
    ) : Declaration() {
        override val location: SourceLocation
            get() = SourceLocation.between(modulePath, asName)

        override val startLoc: SourceLocation
            get() = modulePath.location
    }
    data class ImportMembers(
        override val location: SourceLocation,
        val modulePath: QualifiedPath,
        val names: List<Identifier>,
    ) : Declaration()

    data class FunctionDef(
        override val location: SourceLocation,
        val externName: Identifier?,
        val signature: FunctionSignature,
        val body: Block
    ) : Declaration() {
        val name get() = signature.name
        val typeParams get() = signature.typeParams
        val params get() = signature.params
        override val startLoc: SourceLocation
            get() = signature.name.location
    }

    data class ConstDefinition(
        override val location: SourceLocation,
        val name: Binder,
        val annotation: TypeAnnotation?,
        val initializer: Expression
    ) : Declaration() {
        override val startLoc: SourceLocation
            get() = name.location
    }

    data class ExternFunctionDef(
        override val location: SourceLocation,
        val binder: Binder,
        val paramTypes: List<TypeAnnotation>,
        val returnType: TypeAnnotation,
        val externName: Identifier
    ) : Declaration() {
        override val startLoc: SourceLocation
            get() = binder.location
    }

    data class Struct(
        override val location: SourceLocation,
        val decorators: List<Decorator>,
        val binder: Binder,
        val typeParams: List<TypeParam>? = null,
        val members: List<Member>
    ) : Declaration() {
        override val startLoc: SourceLocation
            get() = binder.location

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
        override val startLoc: SourceLocation
            get() = name.location

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
            val declarations: List<Declaration>
    ) : Declaration() {
        val functionDefs get(): List<FunctionDef> = declarations.filterIsInstance<FunctionDef>()
    }

    data class TraitDef(
            override val location: SourceLocation,
            val name: Binder,
            val params: List<TypeParam>,
            val signatures: List<FunctionSignature>
    ) : Declaration() {
        fun findMethodSignature(methodName: Identifier): FunctionSignature? =
            signatures.find { it.name.identifier.name == methodName.name }
    }

    data class ImplementationDef(
            override val location: SourceLocation,
            val typeParams: List<TypeParam>?,
            val traitRef: QualifiedPath,
            val traitArguments: List<TypeAnnotation>,
            val whereClause: WhereClause?,
            val body: List<Declaration>,
    ) : Declaration()
}


