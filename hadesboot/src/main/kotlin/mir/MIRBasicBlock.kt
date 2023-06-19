package mir

import java.nio.file.Path

class MIRBasicBlock(val name: String, val instructions: List<MIRInstruction>)

class MIRBasicBlockBuilder(
    val name: String,
    var location: MIRLocation,
) {
    private val instructions = mutableListOf<MIRInstruction>()

    fun emit(instruction: MIRInstruction) {
        instructions.add(instruction)
    }

    fun emitReturn(value: MIRValue) {
        emit(MIRInstruction.Return(location, value))
    }

    internal fun build(): MIRBasicBlock = MIRBasicBlock(name, instructions)

}

fun buildBasicBlock(
    path: Path,
    name: String, run: MIRBasicBlockBuilder.() -> Unit
): MIRBasicBlock {
    val builder = MIRBasicBlockBuilder(name, MIRLocation(1, 1, path))
    builder.run()
    return builder.build()
}