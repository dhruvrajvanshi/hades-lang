package hadesc.hir.passes

import hadesc.context.NamingContext
import hadesc.hir.*

class DesugarBlockExpressions(override val namingCtx: NamingContext): AbstractHIRTransformer() {

    override fun transformBlockExpression(expression: HIRExpression.BlockExpression): HIRExpression {
        val resultName = namingCtx.makeUniqueName()
        emitAlloca(resultName, expression.type)
        val initialBlock = transformBlock(expression.block)
        emitAll(initialBlock.statements)

        return HIRExpression.ValRef(
            expression.location,
            expression.type,
            resultName
        )
    }
}