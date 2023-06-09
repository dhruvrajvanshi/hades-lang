package mir

class MIRBasicBlock(val name: String, val instructions: List<MIRInstruction>)

class MIRBasicBlockBuilder(val name: String) {
    private val instructions = mutableListOf<MIRInstruction>()

    fun emit(instruction: MIRInstruction) {
        instructions.add(instruction)
    }

    fun emitReturn(value: MIRValue) {
        emit(MIRInstruction.Return(value))
    }

    internal fun build(): MIRBasicBlock = MIRBasicBlock(name, instructions)

}

fun buildBasicBlock(name: String, run: MIRBasicBlockBuilder.() -> Unit): MIRBasicBlock {
    val builder = MIRBasicBlockBuilder(name)
    builder.run()
    return builder.build()
}