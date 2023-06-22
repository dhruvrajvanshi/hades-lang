package mir

sealed interface MIRValue {
    val type: MIRType

    data class I32(
        val value: Int
    ): MIRValue {
        override val type get(): MIRType.I32 = MIRType.I32
    }
    data class U8(val value: Byte): MIRValue {
        override val type get(): MIRType.U8 = MIRType.U8
    }
    data class LocalRef(
        override val type: MIRType,
        val name: String,
    ): MIRValue

    data class ParamRef(
        override val type: MIRType,
        val name: String,
    ): MIRValue

    data class StaticRef(
        override val type: MIRType,
        val name: String,
    ): MIRValue

    data class CStrLiteral(val text: String): MIRValue {
        override val type get(): MIRType.Pointer = MIRType.U8.ptr()
    }
}
