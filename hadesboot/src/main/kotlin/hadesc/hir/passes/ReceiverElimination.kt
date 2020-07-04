package hadesc.hir.passes

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.context.Context
import hadesc.hir.*
import hadesc.types.Type

@OptIn(ExperimentalStdlibApi::class)
class ReceiverElimination(
        private val ctx: Context
) : HIRTransformer {
    override fun transformFunctionDef(definition: HIRDefinition.Function): Collection<HIRDefinition> {
        return listOf(HIRDefinition.Function(
                location = definition.location,
                signature = transformFunctionSignature(definition.signature),

                body = transformBlock(definition.body)
        ))
    }

    override fun transformFunctionSignature(signature: HIRFunctionSignature): HIRFunctionSignature {
        if (signature.receiverType == null) {
            return super.transformFunctionSignature(signature)
        }
        return HIRFunctionSignature(
                location = signature.location,
                name = transformGlobalName(signature.name),
                typeParams = signature.typeParams?.map { transformTypeParam(it) },
                constraintParams = signature.constraintParams?.map { transformConstraintParam(it) },
                receiverType = null,
                returnType = lowerType(signature.returnType),
                params = listOf(
                        HIRParam(
                                signature.location,
                                thisRefName(),
                                lowerType(signature.receiverType))
                ) + signature.params.map {
                    HIRParam(it.location, transformParamName(it.name), lowerType(it.type)) }
        )
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
        return HIRExpression.Call(
            expression.location,
            lowerType(expression.type),
            callee = transformExpression(callee.method),
            args = transformedArgs,
            typeArgs = transformedTypeArgs
        )
    }

    override fun transformMethodRef(expression: HIRExpression.MethodRef): HIRExpression {
        requireUnreachable()
    }

    private fun thisRefName(): Name {
        return ctx.makeName("this")
    }
}