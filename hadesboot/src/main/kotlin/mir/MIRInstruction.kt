package mir

sealed interface MIRInstruction {
    data class Return(val value: MIRValue): MIRInstruction
}