package hadesc.hir.passes

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.context.Context
import hadesc.hir.HIRDefinition
import hadesc.hir.HIRExpression
import hadesc.hir.HIRParam
import hadesc.hir.HIRPropertyBinding
import hadesc.types.Type

@OptIn(ExperimentalStdlibApi::class)
class ReceiverElimination(
        private val ctx: Context
) : HIRTransformer {
    override fun transformFunctionDef(definition: HIRDefinition.Function): Collection<HIRDefinition> {
        if (definition.receiverType != null) {
            return listOf(HIRDefinition.Function(
                    location = definition.location,
                    name = transformGlobalName(definition.name),
                    typeParams = definition.typeParams?.map { transformTypeParam(it) },
                    receiverType = null,
                    returnType = lowerType(definition.returnType),
                    body = transformBlock(definition.body),
                    params = listOf(
                            HIRParam(
                                    definition.location,
                                    thisRefName(),
                                    lowerType(definition.receiverType))
                    ) + definition.params.map {
                        HIRParam(it.location, transformParamName(it.name), lowerType(it.type)) }
            ))
        }
        return super.transformFunctionDef(definition)
    }

    override fun transformThisRef(expression: HIRExpression.ThisRef): HIRExpression {
        return HIRExpression.ParamRef(
                expression.location,
                lowerType(expression.type),
                thisRefName()
        )
    }

    override fun lowerFunctionType(type: Type.Function): Type {
        require(type.constraints.isEmpty()) { TODO() }
        val from = if (type.receiver == null) {
            type.from.map { lowerType(it) }
        } else buildList {
            add(lowerType(type.receiver))
            addAll(type.from.map { lowerType(it) })
        }
        return Type.Function(
                from = from,
                to = lowerType(type.to),
                typeParams = type.typeParams,
                constraints = emptyList(),
                receiver = null
        )
    }

    override fun transformCall(expression: HIRExpression.Call): HIRExpression {
        return if (expression.callee is HIRExpression.MethodRef) {
            transformMethodCall(expression.callee, expression)
        } else {
            super.transformCall(expression)
        }
    }

    private fun transformMethodCall(callee: HIRExpression.MethodRef, expression: HIRExpression.Call): HIRExpression {
        val transformedReceiver = transformExpression(callee.thisValue)
        val transformedTypeArgs = expression.typeArgs?.map { lowerType(it) }
        val transformedArgs = listOf(transformedReceiver) +
            expression.args.map { transformExpression(it) }
        return when (callee.propertyBinding) {
            is HIRPropertyBinding.GlobalExtensionRef -> HIRExpression.Call(
                    expression.location,
                    lowerType(expression.type),
                    callee = HIRExpression.GlobalRef(
                            callee.location,
                            lowerType(callee.type),
                            callee.propertyBinding.functionName
                    ),
                    args = transformedArgs,
                    typeArgs = transformedTypeArgs
            )
            is HIRPropertyBinding.ImplementationMethodRef -> TODO()
        }
    }

    override fun transformMethodRef(expression: HIRExpression.MethodRef): HIRExpression {
        requireUnreachable()
    }

    private fun thisRefName(): Name {
        return ctx.makeName("this")
    }
}