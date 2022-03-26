package hadesc.hir.passes

import hadesc.context.NamingContext
import hadesc.hir.HIRBlock
import hadesc.hir.HIRExpression
import hadesc.hir.HIRStatement
import hadesc.hir.emitAlloca
import hadesc.types.Type

class DesugarBlockExpressions(override val namingCtx: NamingContext): AbstractHIRTransformer() {

    override fun transformBlockExpression(expression: HIRExpression.BlockExpression): HIRExpression {
        val resultName = namingCtx.makeUniqueName()
        emitAlloca(resultName, expression.type)
        checkNotNull(currentStatements).apply {
            val initialBlock = transformBlock(expression.block)
            val block = when (val lastMember = initialBlock.statements.lastOrNull()) {
                is HIRStatement.Expression -> {
                    val blockStatements = mutableListOf<HIRStatement>()
                    blockStatements.addAll(initialBlock.statements.dropLast(1))
                    blockStatements.add(HIRStatement.Assignment(
                        lastMember.location,
                        resultName,
                        transformExpression(lastMember.expression)
                    ))
                    HIRBlock(
                        initialBlock.location,
                        namingCtx.makeUniqueName(),
                        blockStatements
                    )
                }
                else -> initialBlock
            }
            addAll(block.statements)
        }

        return HIRExpression.ValRef(
            expression.location,
            expression.type,
            resultName
        )
    }
}