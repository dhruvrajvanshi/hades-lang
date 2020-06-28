package hadesc.hir

import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.types.Type

sealed class HIRConstant: HasLocation {
    abstract val type: Type

    data class ByteString(
            override val location: SourceLocation,
            override val type: Type,
            val bytes: ByteArray
    ) : HIRConstant()

    data class BoolValue(
            override val location: SourceLocation,
            override val type: Type,
            val value: Boolean
    ) : HIRConstant()
}