package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class FunctionSignature(
    override val location: SourceLocation,
    val name: Binder,
    val typeParams: List<TypeParam>?,
    val thisParam: ThisParam?,
    val params: List<Param>,
    val returnType: TypeAnnotation,
    val whereClause: WhereClause?
) : HasLocation