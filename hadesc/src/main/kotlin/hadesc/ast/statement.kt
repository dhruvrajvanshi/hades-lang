package hadesc.ast

import hadesc.location.HasLocation
import hadesc.location.SourceLocation

data class Statement(
    override val location: SourceLocation,
    val kind: Kind
) : HasLocation {
    sealed class Kind {
        data class Return(val value: Expression) : Kind()
        data class Val(val binder: Binder, val typeAnnotation: TypeAnnotation?, val rhs: Expression) : Kind()
        object Error : Kind()
    }
}
