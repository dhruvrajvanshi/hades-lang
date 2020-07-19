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

    private fun thisRefName(): Name {
        return ctx.makeName("this")
    }
}