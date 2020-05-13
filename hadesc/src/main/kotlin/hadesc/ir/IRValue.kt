package hadesc.ir

import hadesc.Name
import hadesc.location.HasLocation
import hadesc.location.SourceLocation
import hadesc.types.Type

sealed class IRValue : HasLocation {

    abstract val type: Type

    override fun toString(): String {
        return prettyPrint()
    }

    @OptIn(ExperimentalStdlibApi::class)
    fun prettyPrint(): String = when (this) {
        is IRBool -> value.toString()
        is IRByteString -> "b\"${value.decodeToString()}\""
        is IRVariable -> name.prettyPrint()
        is IRGetStructField -> "${lhs.prettyPrint()}.${rhs.text}"
        is IRCIntConstant -> value.toString()
        is IRNullPtr -> "nullptr"
        is IRMethodRef -> "${thisArg.prettyPrint()}.${method.prettyPrint()}"
        is IRSizeOf -> "size_of[${ofType.prettyPrint()}]"
    }
}

class IRBool(
        override val type: Type,
        override val location: SourceLocation,
        val value: Boolean
) : IRValue()

class IRByteString(
        override val type: Type,
        override val location: SourceLocation,
        val value: ByteArray
) : IRValue()

data class IRVariable(
        override val type: Type,
        override val location: SourceLocation,
        val name: IRName
) : IRValue()

class IRGetStructField(
        override val type: Type,
        override val location: SourceLocation,
        val lhs: IRValue,
        val rhs: Name,
        val index: Int
) : IRValue()

class IRCIntConstant(
        override val type: Type,
        override val location: SourceLocation,
        val value: Int
) : IRValue()

class IRNullPtr(
        override val type: Type,
        override val location: SourceLocation
) : IRValue()

class IRMethodRef(
        override val type: Type,
        override val location: SourceLocation,
        val thisArg: IRValue,
        val method: IRGlobalName
) : IRValue()

class IRSizeOf(
    override val type: Type,
    override val location: SourceLocation,
    val ofType: Type
) : IRValue()

