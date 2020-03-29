package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class Declaration(
    override val location: SourceLocation,
    val kind: Kind
): HasLocation {
    sealed class Kind {
        object Error: Kind()
        data class ImportAs(
            val modulePath: QualifiedPath,
            val asName: Binder
        ) : Kind()

        data class FunctionDef(
            val name: Binder,
            val params: List<Param>,
            val returnType: TypeAnnotation,
            val body: Block
        ) : Kind()

        data class ExternFunctionDef(
            val binder: Binder,
            val paramTypes: List<TypeAnnotation>,
            val returnType: TypeAnnotation,
            val externName: Identifier
        ) : Kind()
    }

}

typealias DeclarationKind = Declaration.Kind


