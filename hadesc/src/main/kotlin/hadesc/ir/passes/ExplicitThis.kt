package hadesc.ir.passes

import hadesc.context.Context
import hadesc.ir.*
import hadesc.types.Type

@OptIn(ExperimentalStdlibApi::class)
class ExplicitThis(
    private val ctx: Context,
    override val inputModule: IRModule
): TransformationPass {
    override val builder = IRBuilder()
    override val module = IRModule()

    override fun lowerFunctionType(type: Type.Function): Type {
        val nonReceiverParams = type.from.map { lowerType(it) }
        val from =  if (type.receiver != null)
            listOf(lowerType(type.receiver)) + nonReceiverParams
        else nonReceiverParams
        require(type.constraints.isEmpty())
        return Type.Function(
                receiver = null,
                typeParams = type.typeParams,
                from = from,
                to = lowerType(type.to)
        )
    }

    override fun lowerFunctionDef(definition: IRFunctionDef) {
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
                params = params,
                constraints = listOf()
        )
        for (block in definition.blocks) {
            val newBlock = IRBlock(block.name)
            fn.appendBlock(newBlock)
            lowerBlock(oldBlock = block, newBlock = newBlock)
        }

    }

    override fun lowerCallInstruction(instruction: IRCall) {
        val callee = if (instruction.callee is IRMethodRef)
            lowerValue(instruction.callee.method)
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
}