package hadesc.hir.passes

import hadesc.Name
import hadesc.context.Context
import hadesc.hir.HIRDefinition
import hadesc.hir.HIRExpression
import hadesc.hir.HIRParam

class ExtensionMethodElimination(
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

    private fun thisRefName(): Name {
        return ctx.makeName("this")
    }
}