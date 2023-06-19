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

    sealed interface MIRNameBinder {
        val type: MIRType
        val name: String
    }
}