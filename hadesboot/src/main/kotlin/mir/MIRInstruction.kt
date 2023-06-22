package mir

sealed interface MIRInstruction {
    val location: MIRLocation
    data class Return(
        override val location: MIRLocation,
        val value: MIRValue
    ): MIRInstruction

    data class IAdd(
        override val location: MIRLocation,
        override val name: String,
        override val type: MIRType,
        val lhs: MIRValue,
        val rhs: MIRValue,
    ): MIRNameBinder, MIRInstruction

    data class IWidenCast(
        override val location: MIRLocation,
        override val name: String,
        val toType: MIRType,
        val value: MIRValue,
    ): MIRNameBinder, MIRInstruction {
        override val type: MIRType
            get() = toType
    }

    sealed interface MIRNameBinder {
        val type: MIRType
        val name: String
    }
}