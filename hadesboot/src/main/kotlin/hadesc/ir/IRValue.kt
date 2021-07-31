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

    fun prettyPrint(): String = when (this) {
        is IRBool -> value.toString()
        is IRByteString -> "b\"${value.decodeToString()}\""
        is IRVariable -> name.prettyPrint()
        is IRGetStructField -> "${lhs.prettyPrint()}.${rhs?.text ?: index}"
        is IRCIntConstant -> "${type.prettyPrint()} $value"
        is IRNullPtr -> "nullptr"
        is IRMethodRef -> "${thisArg.prettyPrint()}::${method.prettyPrint()}"
        is IRSizeOf -> "size_of[${ofType.prettyPrint()}]"
        is IRPointerCast -> "pointer_cast[${toPointerOfType.prettyPrint()}](${arg.prettyPrint()})"
        is IRAggregate -> "${type.prettyPrint()} { ${values.joinToString(", ") {it.prettyPrint()}} }"
        is IRGetElementPointer -> "gep(${ptr.prettyPrint()}, $offset)"
        is IRUnsafeCast -> "unsafe_cast[${type.prettyPrint()}](${value.prettyPrint()})"
        is IRTruncate -> "truncate ${value.prettyPrint()} to ${type.prettyPrint()}"
        is IRZExt -> "zext ${value.prettyPrint()} to ${type.prettyPrint()}"
        is IRFloatConstant -> "${type.prettyPrint()} $value"
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
        val rhs: Name?,
        val index: Int
) : IRValue()

class IRCIntConstant(
        override val type: Type,
        override val location: SourceLocation,
        val value: Int
) : IRValue()

class IRFloatConstant(
    override val type: Type,
    override val location: SourceLocation,
    val value: Double
) : IRValue()

class IRNullPtr(
        override val type: Type,
        override val location: SourceLocation
) : IRValue()

class IRMethodRef(
        override val type: Type,
        override val location: SourceLocation,
        val thisArg: IRValue,
        val method: IRValue
) : IRValue()

class IRSizeOf(
    override val type: Type,
    override val location: SourceLocation,
    val ofType: Type
) : IRValue()

class IRPointerCast(
    override val type: Type,
    override val location: SourceLocation,
    val toPointerOfType: Type,
    val arg: IRValue
) : IRValue()

class IRAggregate(
    override val type: Type,
    override val location: SourceLocation,
    val values: List<IRValue>
) : IRValue()

class IRGetElementPointer(
    override val type: Type,
    override val location: SourceLocation,
    val ptr: IRValue,
    val offset: Int
) : IRValue()

class IRUnsafeCast(
    override val type: Type,
    override val location: SourceLocation,
    val value: IRValue
) : IRValue()

class IRZExt(
    override val type: Type,
    override val location: SourceLocation,
    val value: IRValue
) : IRValue()

class IRTruncate(
    override val type: Type,
    override val location: SourceLocation,
    val value: IRValue,
) : IRValue()

