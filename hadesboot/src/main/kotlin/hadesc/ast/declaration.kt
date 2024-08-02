package hadesc.ast

import hadesc.Name
import hadesc.location.HasLocation
import hadesc.location.SourceLocation

sealed interface Declaration : HasLocation {
    /**
     * Location of the first few tokens for error reporting
     * Useful for larger declarations where we wan't to report
     * an error for the entire declaration but the full declaration
     * is just to noisy of an error span. For example, an error like,
     * "Only function defs are allowed inside extensions" should ideally
     * only highlight the first token or two for a struct declaration.
     */
    val startLoc get() = location

    data class Error(override val location: SourceLocation) : Declaration
    data class ImportAs(
        val modulePath: QualifiedPath,
        val asName: Binder
    ) : Declaration {
        override val location: SourceLocation
            get() = SourceLocation.between(modulePath, asName)

        override val startLoc: SourceLocation
            get() = modulePath.location
    }
    data class ImportMembers(
        override val location: SourceLocation,
        val modulePath: QualifiedPath,
        val names: List<Binder>
    ) : Declaration

    data class FunctionDef(
        override val location: SourceLocation,
        val externName: Identifier?,
        val signature: FunctionSignature,
        val body: Block
    ) : Declaration, ScopeTree, HasDefId by signature, HasTypeParams by signature {
        val name get() = signature.name
        val params get() = signature.params
        override val startLoc: SourceLocation
            get() = signature.name.location
    }

    data class ConstDefinition(
        override val location: SourceLocation,
        val name: Binder,
        val annotation: TypeAnnotation?,
        val initializer: Expression
    ) : Declaration {
        override val startLoc: SourceLocation
            get() = name.location
    }

    data class ExternFunctionDef(
        override val location: SourceLocation,
        val binder: Binder,
        val paramTypes: List<TypeAnnotation>,
        val returnType: TypeAnnotation,
        val externName: Identifier
    ) : Declaration {
        override val startLoc: SourceLocation
            get() = binder.location
    }

    data class ExternConst(
        override val location: SourceLocation,
        val name: Binder,
        val type: TypeAnnotation,
        val externName: Identifier
    ) : Declaration

    data class Struct(
        override val location: SourceLocation,
        override val defId: DefId,
        val decorators: List<Decorator>,
        val binder: Binder,
        override val typeParams: List<TypeParam>? = null,
        val members: List<Member>,
        val isRef: Boolean
    ) : Declaration, ScopeTree, HasDefId, HasTypeParams {
        override val startLoc: SourceLocation
            get() = binder.location

        sealed class Member {
            data class Field(
                val binder: Binder,
                val isMutable: Boolean,
                val typeAnnotation: TypeAnnotation
            ) : Member()
        }

        val fields get() = members.filterIsInstance<Member.Field>()
    }

    data class TypeAlias(
        override val location: SourceLocation,
        override val defId: DefId,
        val name: Binder,
        override val typeParams: List<TypeParam>?,
        val rhs: TypeAnnotation
    ) : Declaration, ScopeTree, HasTypeParams, HasDefId

    data class ExtensionDef(
        override val location: SourceLocation,
        override val defId: DefId,
        val name: Binder,
        override val typeParams: List<TypeParam>?,
        val forType: TypeAnnotation,
        val declarations: List<Declaration>
    ) : Declaration, ScopeTree, HasTypeParams, HasDefId {
        val functionDefs get(): List<FunctionDef> = declarations.filterIsInstance<FunctionDef>()
    }

    data class Enum(
        override val location: SourceLocation,
        override val defId: DefId,
        val decorators: List<Decorator>,
        val name: Binder,
        override val typeParams: List<TypeParam>?,
        val cases: List<Case>
    ) : Declaration, ScopeTree, HasDefId, HasTypeParams {
        fun getCase(name: Name): Pair<Case, Int>? {
            var result: Pair<Case, Int>? = null
            cases.forEachIndexed { index, case ->
                if (case.name.name == name) {
                    result = case to index
                    return@forEachIndexed
                }
            }
            return result
        }

        data class Case(
            val name: Binder,
            val params: List<EnumCaseParam>?
        )
    }
}

data class EnumCaseParam(val binder: Binder?, val annotation: TypeAnnotation)
