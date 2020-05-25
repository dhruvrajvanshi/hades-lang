package hadesc.ir.passes

import hadesc.assertions.requireUnreachable
import hadesc.context.Context
import hadesc.ir.*
import hadesc.types.Type

@OptIn(ExperimentalStdlibApi::class)
class ExplicitThis(
    private val ctx: Context,
    private val oldModule: IRModule
) {
    private val builder = IRBuilder()
    private val module = IRModule()
    fun run(): IRModule {
        for (definition in oldModule) {
            lowerDefinition(definition)
        }
        return module
    }

    private fun lowerDefinition(definition: IRDefinition): Unit = when (definition) {
        is IRFunctionDef -> lowerFunctionDef(definition)
        is IRInterfaceDef -> lowerInterfaceDef(definition)
        is IRImplementationDef -> lowerImplementationDef(definition)
        is IRConstDef -> lowerIRConstDef(definition)
        is IRStructDef -> lowerIRStructDef(definition)
        is IRExternFunctionDef -> lowerExternFunctionDef(definition)
    }

    private fun lowerType(type: Type): Type = when (type) {
        Type.Error -> requireUnreachable()
        Type.Byte,
        Type.Void,
        Type.Bool,
        Type.CInt,
        Type.Size -> type
        is Type.RawPtr -> Type.RawPtr(lowerType(type.to))
        is Type.Function -> {
            val nonReceiverParams = type.from.map { lowerType(it) }
            val from =  if (type.receiver != null)
                listOf(lowerType(type.receiver)) + nonReceiverParams
            else nonReceiverParams
            Type.Function(
                    receiver = null,
                    typeParams = type.typeParams,
                    from = from,
                    to = lowerType(type.to)
            )
        }
        is Type.Struct -> {
            Type.Struct(
                    constructor = lowerType(type.constructor) as Type.Constructor,
                    memberTypes = type.memberTypes.mapValues { lowerType(it.value) }
            )
        }
        is Type.Constructor -> Type.Constructor(
                type.binder,
                name = type.name,
                params = type.params
        )
        is Type.ParamRef -> type
        is Type.GenericInstance -> requireUnreachable()
        is Type.Application -> Type.Application(
                callee = lowerType(type.callee),
                args = type.args.map { lowerType(it) }
        )
        is Type.ThisRef -> requireUnreachable()
    }

    private fun lowerIRStructDef(definition: IRStructDef) {
        module.addStructDef(
                constructorType = lowerType(definition.constructorType) as Type.Function,
                typeParams = definition.typeParams?.map { lowerTypeParam(it) },
                name = lowerGlobalName(definition.globalName),
                instanceType = lowerType(definition.instanceType),
                fields = definition.fields.mapValues { lowerType(it.value) }
        )
    }

    private fun lowerGlobalName(name: IRGlobalName): IRGlobalName {
        return name
    }
    private fun lowerLocalName(name: IRLocalName): IRLocalName {
        return name
    }

    private fun lowerName(name: IRName): IRName = when(name) {
        is IRLocalName -> lowerLocalName(name)
        is IRGlobalName -> lowerGlobalName(name)
    }

    private fun lowerTypeParam(typeParam: IRTypeParam): IRTypeParam {
        return IRTypeParam(
                lowerLocalName(typeParam.name),
                binderLocation = typeParam.binderLocation
        )
    }

    private fun lowerExternFunctionDef(definition: IRExternFunctionDef) {
        module.addExternFunctionDef(
                name = lowerGlobalName(definition.name),
                type = lowerType(definition.type) as Type.Function,
                externName = definition.externName
        )
    }

    private fun lowerIRConstDef(definition: IRConstDef) {
        module.addConstDef(
                type = lowerType(definition.type),
                name = lowerGlobalName(definition.name),
                initializer = lowerValue(definition.initializer)
        )
    }

    private fun lowerValue(value: IRValue): IRValue = when(value) {
        is IRBool -> value
        is IRByteString -> value
        is IRCIntConstant -> value
        is IRVariable -> lowerVariable(value)
        is IRGetStructField -> lowerGetStructField(value)
        is IRNullPtr -> lowerNullPtr(value)
        is IRSizeOf -> lowerSizeOf(value)
        is IRPointerCast -> lowerPointerCast(value)
        is IRMethodRef -> requireUnreachable()
    }

    private fun lowerPointerCast(value: IRPointerCast): IRValue {
        return IRPointerCast(
                type = lowerType(value.type),
                location = value.location,
                arg = lowerValue(value.arg),
                toPointerOfType = lowerType(value.toPointerOfType)
        )
    }

    private fun lowerSizeOf(value: IRSizeOf): IRValue {
        return IRSizeOf(
                type = lowerType(value.type),
                location = value.location,
                ofType = lowerType(value.ofType)
        )
    }

    private fun lowerNullPtr(value: IRNullPtr): IRValue {
        return IRNullPtr(
                location = value.location,
                type = lowerType(value.type)
        )
    }

    private fun lowerGetStructField(value: IRGetStructField): IRValue {
        return IRGetStructField(
                type = lowerType(value.type),
                location = value.location,
                index = value.index,
                rhs = value.rhs,
                lhs = lowerValue(value.lhs)
        )
    }

    private fun lowerVariable(variable: IRVariable): IRValue {
        return IRVariable(
                type = lowerType(variable.type),
                location = variable.location,
                name = lowerName(variable.name)
        )
    }

    private fun lowerImplementationDef(definition: IRImplementationDef) {
        TODO()
    }

    private fun lowerInterfaceDef(definition: IRInterfaceDef) {
        TODO()
    }

    private fun lowerFunctionDef(definition: IRFunctionDef) {
        val newEntryBlock = IRBlock()
        lowerBlock(definition.entryBlock, newEntryBlock)
        val name = lowerGlobalName(definition.name)
        val params = if (definition.signature.receiverType != null) buildList {
            add(IRParam(
                    IRLocalName(ctx.makeName("this")),
                    type = lowerType(definition.signature.receiverType),
                    index = 0,
                    location = definition.signature.location,
                    functionName = name
            ))
            addAll(definition.params.map { lowerParam(it, offsetBy = 1) })
        } else definition.params.map { lowerParam(it) }
        val fn = module.addGlobalFunctionDef(
                location = definition.signature.location,
                name = name,
                type = lowerType(definition.type) as Type.Function,
                typeParams = definition.typeParams?.map { lowerTypeParam(it) },
                receiverType = null,
                entryBlock = newEntryBlock,
                params = params
        )
        for (block in definition.blocks) {
            val newBlock = IRBlock(block.name)
            fn.appendBlock(newBlock)
            lowerBlock(oldBlock = block, newBlock = newBlock)
        }

    }

    private fun lowerParam(it: IRParam, offsetBy: Int = 0): IRParam {
        return IRParam(
                lowerLocalName(it.name),
                type = lowerType(it.type),
                location = it.location,
                functionName = lowerGlobalName(it.functionName),
                index = it.index + offsetBy
        )
    }

    private fun lowerBlock(oldBlock: IRBlock, newBlock: IRBlock) {
        builder.position = newBlock
        for (instruction in oldBlock) {
            lowerInstruction(instruction)
        }
    }

    private fun lowerInstruction(instruction: IRInstruction): Unit = when(instruction) {
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

    private fun lowerRetVoidInstruction(instruction: IRInstruction) {
        builder.buildRetVoid()
    }

    private fun lowerJumpInstruction(instruction: IRJump) {
        builder.buildJump(
                instruction.location,
                instruction.label
        )
    }

    private fun lowerBrInstruction(instruction: IRBr) {
        builder.buildBranch(
                instruction.location,
                condition = lowerValue(instruction.condition),
                ifTrue = instruction.ifTrue,
                ifFalse = instruction.ifFalse
        )
    }

    private fun lowerNotInstruction(instruction: IRNot) {
        builder.buildNot(
                type = lowerType(instruction.type),
                location = instruction.location,
                name = lowerLocalName(instruction.name),
                value = lowerValue(instruction.arg)
        )
    }

    private fun lowerCallInstruction(instruction: IRCall) {
        val callee = if (instruction.callee is IRMethodRef)
            IRVariable(
                    location = instruction.callee.location,
                    type = lowerType(instruction.callee.type),
                    name = instruction.callee.method)
            else lowerValue(instruction.callee)
        val args = buildList {
            if (instruction.callee is IRMethodRef) {
                add(lowerValue(instruction.callee.thisArg))
            }
            addAll(instruction.args.map { lowerValue(it) })
        }
        builder.buildCall(
                name = lowerLocalName(instruction.name),
                location = instruction.location,
                type = lowerType(instruction.type),
                typeArgs = instruction.typeArgs?.map { lowerType(it) },
                callee = callee,
                args = args
        )
    }

    private fun lowerBinOpInstruction(instruction: IRBinOp) {
        builder.buildBinOp(
                type = lowerType(instruction.type),
                name = lowerLocalName(instruction.name),
                lhs = lowerValue(instruction.lhs),
                operator = instruction.operator,
                rhs = lowerValue(instruction.rhs)
        )
    }

    private fun lowerLoadInstruction(instruction: IRLoad) {
        builder.buildLoad(
                type = lowerType(instruction.type),
                name = lowerLocalName(instruction.name),
                ptr = lowerValue(instruction.ptr)
        )
    }

    private fun lowerStoreInstruction(instruction: IRStore) {
        builder.buildStore(
                ptr = lowerValue(instruction.ptr),
                value = lowerValue(instruction.value)
        )
    }

    private fun lowerAllocaInstruction(instruction: IRAlloca) {
        builder.buildAlloca(
                type = lowerType(instruction.type),
                name = lowerLocalName(instruction.name)
        )
    }

    private fun lowerReturnInstruction(instruction: IRReturnInstruction) {
        builder.buildReturn(
                lowerValue(instruction.value)
        )
    }
}