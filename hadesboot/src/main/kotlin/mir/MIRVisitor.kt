package mir

interface MIRVisitor {
    fun visitObject(module: MIRValue.Object) {
        for ((_, value) in module.values.entries) {
            visitValue(value)
        }
    }
    fun visitValue(value: MIRValue) = when(value) {
        is MIRValue.Function -> {
            for (basicBlock in value.basicBlocks) {
                visitBasicBlock(basicBlock)
            }
        }
        is MIRValue.I32 -> Unit
        is MIRValue.Object -> visitObject(value)
    }

    fun visitBasicBlock(basicBlock: MIRBasicBlock) {
        for (instruction in basicBlock.instructions) {
            visitInstruction(instruction)
        }
    }

    fun visitInstruction(instruction: MIRInstruction): Unit = when(instruction) {
        is MIRInstruction.Return -> visitValue(instruction.value)
        is MIRInstruction.IAdd -> {
            visitValue(instruction.lhs)
            visitValue(instruction.rhs)
        }
    }
}
