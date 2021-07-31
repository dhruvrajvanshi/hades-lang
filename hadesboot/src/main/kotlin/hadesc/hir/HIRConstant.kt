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

    data class IntValue(
            override val location: SourceLocation,
            override val type: Type,
            val value: Int
    ) : HIRConstant()

    data class FloatValue(
        override val location: SourceLocation,
        override val type: Type.FloatingPoint,
        val value: Double
    ) : HIRConstant()

    fun prettyPrint(): String = when(this) {
        is ByteString -> "b\"" + String(bytes)
                .replace("\"", "\"\"")
                .replace("\\", "\\\\") + "\""
        is BoolValue -> value.toString()
        is IntValue -> value.toString()
        is FloatValue -> value.toString()
    }
}