package hadesc.ir.passes

import hadesc.assertions.requireUnreachable
import hadesc.ir.*
import hadesc.types.Type

interface TransformationPass: TypeTransformer {
    val builder: IRBuilder
    val module: IRModule
    val inputModule: IRModule

    fun run(): IRModule {
        for (definition in inputModule) {
            lowerDefinition(definition)
        }
        return module
    }

    fun lowerDefinition(definition: IRDefinition): Unit = when (definition) {
        is IRFunctionDef -> lowerFunctionDef(definition)
        is IRInterfaceDef -> lowerInterfaceDef(definition)
        is IRImplementationDef -> lowerImplementationDef(definition)
        is IRConstDef -> lowerIRConstDef(definition)
        is IRStructDef -> lowerIRStructDef(definition)
        is IRExternFunctionDef -> lowerExternFunctionDef(definition)
    }

    fun lowerIRStructDef(definition: IRStructDef) {
        module.addStructDef(
                constructorType = lowerType(definition.constructorType) as Type.Function,
                typeParams = definition.typeParams?.map { lowerTypeParam(it) },
                name = lowerGlobalName(definition.globalName),
                instanceType = lowerType(definition.instanceType),
                fields = definition.fields.mapValues { lowerType(it.value) }
        )
    }

    fun lowerGlobalName(name: IRGlobalName): IRGlobalName {
        return name
    }
    fun lowerLocalName(name: IRLocalName): IRLocalName {
        return name
    }

    fun lowerName(name: IRName): IRName = when(name) {
        is IRLocalName -> lowerLocalName(name)
        is IRGlobalName -> lowerGlobalName(name)
    }

    fun lowerTypeParam(typeParam: IRTypeParam): IRTypeParam {
        return IRTypeParam(
                lowerLocalName(typeParam.name),
                binder = typeParam.binder
        )
    }

    fun lowerExternFunctionDef(definition: IRExternFunctionDef) {
        module.addExternFunctionDef(
                name = lowerGlobalName(definition.name),
                type = lowerType(definition.type) as Type.Function,
                externName = definition.externName
        )
    }

    fun lowerIRConstDef(definition: IRConstDef) {
        module.addConstDef(
                type = lowerType(definition.type),
                name = lowerGlobalName(definition.name),
                initializer = lowerValue(definition.initializer)
        )
    }

    fun lowerValue(value: IRValue): IRValue = when(value) {
        is IRBool -> value
        is IRByteString -> value
        is IRCIntConstant -> value
        is IRVariable -> lowerVariable(value)
        is IRGetStructField -> lowerGetStructField(value)
        is IRNullPtr -> lowerNullPtr(value)
        is IRSizeOf -> lowerSizeOf(value)
        is IRPointerCast -> lowerPointerCast(value)
        is IRMethodRef -> lowerMethodRef(value)
    }

    fun lowerMethodRef(value: IRMethodRef): IRValue {
        return builder.buildMethodRef(
                lowerType(value.type),
                location = value.location,
                method = lowerValue(value.method),
                thisArg = lowerValue(value.thisArg)
        )
    }

    fun lowerPointerCast(value: IRPointerCast): IRValue {
        return IRPointerCast(
                type = lowerType(value.type),
                location = value.location,
                arg = lowerValue(value.arg),
                toPointerOfType = lowerType(value.toPointerOfType)
        )
    }

    fun lowerSizeOf(value: IRSizeOf): IRValue {
        return IRSizeOf(
                type = lowerType(value.type),
                location = value.location,
                ofType = lowerType(value.ofType)
        )
    }

    fun lowerNullPtr(value: IRNullPtr): IRValue {
        return IRNullPtr(
                location = value.location,
                type = lowerType(value.type)
        )
    }

    fun lowerGetStructField(value: IRGetStructField): IRValue {
        return IRGetStructField(
                type = lowerType(value.type),
                location = value.location,
                index = value.index,
                rhs = value.rhs,
                lhs = lowerValue(value.lhs)
        )
    }

    fun lowerVariable(variable: IRVariable): IRValue {
        return IRVariable(
                type = lowerType(variable.type),
                location = variable.location,
                name = lowerName(variable.name)
        )
    }

    fun lowerImplementationDef(definition: IRImplementationDef) {
        requireUnreachable()
    }

    fun lowerInterfaceDef(definition: IRInterfaceDef) {
        requireUnreachable()
    }

    fun lowerFunctionDef(definition: IRFunctionDef) {
        val newEntryBlock = IRBlock()
        lowerBlock(definition.entryBlock, newEntryBlock)
        val name = lowerGlobalName(definition.name)
        val params = definition.params.map { lowerParam(it) }
        val fn = module.addGlobalFunctionDef(
                location = definition.signature.location,
                name = name,
                type = lowerType(definition.type) as Type.Function,
                typeParams = definition.typeParams?.map { lowerTypeParam(it) },
                receiverType = definition.signature.receiverType?.let { lowerType(it) },
                entryBlock = newEntryBlock,
                params = params,
                constraints = definition.signature.constraints.map {
                    IRConstraint(
                            name = it.name
                            ,
                            typeParam = it.typeParam,
                            interfaceRef = it.interfaceRef.copy(
                                    typeArgs = it.interfaceRef.typeArgs.map { arg -> lowerType(arg) }
                            )
                    )
                }
        )
        for (block in definition.blocks) {
            val newBlock = IRBlock(block.name)
            fn.appendBlock(newBlock)
            lowerBlock(oldBlock = block, newBlock = newBlock)
        }

    }

    fun lowerParam(it: IRParam, offsetBy: Int = 0): IRParam {
        return IRParam(
                lowerLocalName(it.name),
                type = lowerType(it.type),
                location = it.location,
                functionName = lowerGlobalName(it.functionName),
                index = it.index + offsetBy
        )
    }

    fun lowerBlock(oldBlock: IRBlock, newBlock: IRBlock) {
        builder.position = newBlock
        for (instruction in oldBlock) {
            lowerInstruction(instruction)
        }
    }

    fun lowerInstruction(instruction: IRInstruction): Unit = when(instruction) {
        is IRReturnInstruction -> lowerReturnInstruction(instruction)
        is IRAlloca -> lowerAllocaInstruction(instruction)
        is IRStore -> lowerStoreInstruction(instruction)
        is IRLoad -> lowerLoadInstruction(instruction)
        IRReturnVoidInstruction -> lowerRetVoidInstruction(instruction)
        is IRBinOp -> lowerBinOpInstruction(instruction)
        is IRCall -> lowerCallInstruction(instruction)
        is IRNot -> lowerNotInstruction(instruction)
        is IRBr -> lowerBrInstruction(instruction)
        is IRJump -> lowerJumpInstruction(instruction)
    }

    fun lowerRetVoidInstruction(instruction: IRInstruction) {
        builder.buildRetVoid()
    }

    fun lowerJumpInstruction(instruction: IRJump) {
        builder.buildJump(
                instruction.location,
                instruction.label
        )
    }

    fun lowerBrInstruction(instruction: IRBr) {
        builder.buildBranch(
                instruction.location,
                condition = lowerValue(instruction.condition),
                ifTrue = instruction.ifTrue,
                ifFalse = instruction.ifFalse
        )
    }

    fun lowerNotInstruction(instruction: IRNot) {
        builder.buildNot(
                type = lowerType(instruction.type),
                location = instruction.location,
                name = lowerLocalName(instruction.name),
                value = lowerValue(instruction.arg)
        )
    }

    fun lowerCallInstruction(instruction: IRCall) {
        builder.buildCall(
                name = lowerLocalName(instruction.name),
                location = instruction.location,
                type = lowerType(instruction.type),
                typeArgs = instruction.typeArgs?.map { lowerType(it) },
                callee = lowerValue(instruction.callee),
                args = instruction.args.map { lowerValue(it) }
        )
    }

    fun lowerBinOpInstruction(instruction: IRBinOp) {
        builder.buildBinOp(
                type = lowerType(instruction.type),
                name = lowerLocalName(instruction.name),
                lhs = lowerValue(instruction.lhs),
                operator = instruction.operator,
                rhs = lowerValue(instruction.rhs)
        )
    }

    fun lowerLoadInstruction(instruction: IRLoad) {
        builder.buildLoad(
                type = lowerType(instruction.type),
                name = lowerLocalName(instruction.name),
                ptr = lowerValue(instruction.ptr)
        )
    }

    fun lowerStoreInstruction(instruction: IRStore) {
        builder.buildStore(
                ptr = lowerValue(instruction.ptr),
                value = lowerValue(instruction.value)
        )
    }

    fun lowerAllocaInstruction(instruction: IRAlloca) {
        builder.buildAlloca(
                type = lowerType(instruction.type),
                name = lowerLocalName(instruction.name)
        )
    }

    fun lowerReturnInstruction(instruction: IRReturnInstruction) {
        builder.buildReturn(
                lowerValue(instruction.value)
        )
    }
}

