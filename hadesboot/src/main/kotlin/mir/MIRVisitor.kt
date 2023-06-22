package mir

interface MIRVisitor {
    fun visitModule(module: MIRModule) {
        for (declaration in module.declarations) {
            visitDeclaration(declaration)
        }
    }

    fun visitDeclaration(declaration: MIRDeclaration) = when(declaration) {
        is MIRDeclaration.Function -> {
            for (basicBlock in declaration.basicBlocks) {
                visitBasicBlock(basicBlock)
            }
        }

        is MIRDeclaration.StaticDefinition -> {
            visitValue(declaration.initializer)
        }
    }

    fun visitValue(value: MIRValue) = when(value) {
        is MIRValue.I32 -> Unit
        is MIRValue.LocalRef -> Unit
        is MIRValue.StaticRef -> Unit
        is MIRValue.U8 -> Unit
        is MIRValue.CStrLiteral -> Unit
        is MIRValue.ParamRef -> Unit
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
        is MIRInstruction.IWidenCast -> visitValue(instruction.value)
        is MIRInstruction.Call -> {
            visitValue(instruction.function)
            instruction.args.forEach { visitValue(it) }
        }
    }
}
