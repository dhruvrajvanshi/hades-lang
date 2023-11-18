package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class FunctionSignature(
    override val location: SourceLocation,
    override val defId: DefId,
    val name: Binder,
    override val typeParams: List<TypeParam>?,
    val thisParamBinder: Binder?,
    val thisParamFlags: ThisParamFlags?,
    val params: List<Param>,
    val returnType: TypeAnnotation,
    val whereClause: WhereClause?
) : HasLocation, HasTypeParams, HasDefId {
    init {
        check(
            thisParamBinder == null && thisParamFlags == null ||
                thisParamBinder != null && thisParamFlags != null
        )
    }
    data class ThisParamFlags(
        val isPointer: Boolean,
        val isMutable: Boolean
    )
}
