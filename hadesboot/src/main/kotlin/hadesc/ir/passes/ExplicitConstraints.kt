package hadesc.ir.passes

import hadesc.ast.Binder
import hadesc.context.Context
import hadesc.ir.*
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type

class ExplicitConstraints(
        private val ctx: Context,
        override val inputModule: IRModule
): TransformationPass {
    override val builder: IRBuilder = IRBuilder()
    override val module: IRModule = IRModule()

    private fun typeOfInterfaceInstance(interfaceName: QualifiedName, thisType: Type, typeArgs: List<Type>): Type {
        val interfaceDecl = ctx.typer.getInterfaceDecl(interfaceName)
        require(interfaceDecl.typeParams?.size ?: 0 == typeArgs.size)
        val interfaceTypeParams = interfaceDecl.typeParams?.map {
            require(it.bound == null)
            Type.Param(it.binder)
        } ?: listOf()
        return Type.Application(
                callee = Type.Constructor(
                        binder = interfaceDecl.name,
                        name = interfaceName,
                        params = listOf(Type.Param(Binder(interfaceDecl.name.identifier.copy(name = ctx.makeName("This"))))) + interfaceTypeParams
                ),
                args = listOf(thisType) + typeArgs
        )
    }

    override fun lowerFunctionType(type: Type.Function): Type {
        val from = type.from.map { lowerType(it) } +
            type.constraints.map {
                typeOfInterfaceInstance(it.interfaceName, Type.ParamRef(it.param.binder), it.args)
            }
        return Type.Function(
                receiver = type.receiver?.let { lowerType(it) },
                typeParams = type.typeParams,
                from = from,
                to = lowerType(type.to),
                constraints = listOf()
        )
    }

    override fun lowerFunctionDef(definition: IRFunctionDef) {
        val constraintParams = definition.signature.constraints.mapIndexed { index, it ->
            IRParam(
                    name = it.name,
                    type = it.type,
                    location = it.location,
                    functionName = definition.name,
                    index = index + definition.params.size

            )
        }
        val fn = module.addGlobalFunctionDef(
                name = lowerGlobalName(definition.name),
                location = definition.signature.location,
                constraints = listOf(),
                typeParams = definition.typeParams?.map { lowerTypeParam(it) },
                params = definition.params.map { lowerParam(it) } + constraintParams,
                type = lowerType(definition.type) as Type.Function,
                entryBlock = lowerBlock(definition.entryBlock, IRBlock(definition.entryBlock.name)),
                receiverType = definition.signature.receiverType?.let { lowerType(it) }
        )
        for (it in definition.blocks) {
            fn.appendBlock(lowerBlock(it, IRBlock(it.name)))
        }
    }
}