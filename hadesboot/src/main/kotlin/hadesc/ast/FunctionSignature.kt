package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class FunctionSignature(
    override val location: SourceLocation,
    val name: Binder,
    val typeParams: List<TypeParam>?,
    val thisParamFlags: ThisParamFlags?,
    val params: List<Param>,
    val returnType: TypeAnnotation,
    val whereClause: WhereClause?
) : HasLocation {
    data class ThisParamFlags(
            val isPointer: Boolean,
            val isRef: Boolean,
            val isMutable: Boolean
    ) {
        init {
            require(!(isPointer && isRef))
        }
    }
}