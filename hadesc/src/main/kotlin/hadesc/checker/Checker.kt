package hadesc.checker

import hadesc.ast.Identifier
import hadesc.ast.TypeAnnotation
import hadesc.context.Context
import hadesc.types.Type

class Checker(val ctx: Context) {
    fun annotationToType(annotation: TypeAnnotation): Type {
        return when (annotation.kind) {
            TypeAnnotation.Kind.Error -> Type.Void
            is TypeAnnotation.Kind.Var -> when (identToString(annotation.kind.name)) {
                // TODO: Use scoped lookup here
                "Byte" -> Type.Byte
                "Void" -> Type.Void
                else -> TODO("Unbound type variable ${annotation.kind.name.name.text}")
            }
            is TypeAnnotation.Kind.Ptr -> Type.RawPtr(annotationToType(annotation.kind.to))
        }
    }

    private fun identToString(identifier: Identifier): String {
        return identifier.name.text
    }
}