package mir


class MIRBasicBlock(val name: String, val instructions: List<MIRInstruction>)

class MIRBasicBlockBuilder(
    val name: String,
    var location: MIRLocation,
    private val functionLocals: MutableMap<String, MIRType>,
) {
    private val instructions = mutableListOf<MIRInstruction>()

    fun emit(instruction: MIRInstruction) {
        when (instruction) {
            is MIRInstruction.MIRNameBinder -> {
                check(instruction.name !in functionLocals)
                functionLocals[instruction.name] = instruction.type
            }
            else -> Unit
        }
        instructions.add(instruction)
    }

    fun emitReturn(value: MIRValue) {
        emit(MIRInstruction.Return(location, value))
    }

    fun emitIAdd(name: String, lhs: MIRValue, rhs: MIRValue) {
        emit(
            MIRInstruction.IAdd(
                location,
                name,
                lhs.type,
                lhs,
                rhs
            )
        )
    }

    fun localRef(name: String): MIRValue {
        val localType = functionLocals[name] ?: error("Undeclared local: $name")

        return MIRValue.LocalRef(localType, name)
    }

    internal fun build(): MIRBasicBlock = MIRBasicBlock(name, instructions)

}
