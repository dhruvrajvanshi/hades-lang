package mir

sealed interface MIRInstruction {
    val location: MIRLocation
    data class Return(
        override val location: MIRLocation,
        val value: MIRValue
    ): MIRInstruction
}