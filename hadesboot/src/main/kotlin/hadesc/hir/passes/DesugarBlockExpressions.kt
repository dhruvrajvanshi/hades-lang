package hadesc.hir.passes

import hadesc.context.NamingContext
import hadesc.hir.*
import hadesc.types.Type

class DesugarBlockExpressions(override val namingCtx: NamingContext): AbstractHIRTransformer() {

    override fun transformBlockExpression(expression: HIRExpression.BlockExpression): HIRExpression {
        val resultName = namingCtx.makeUniqueName()
        emitAlloca(resultName, expression.type)
        intoStatementList(checkNotNull(currentStatements)) {
            val initialBlock = transformBlock(expression.block)
            val block = when (val lastMember = initialBlock.statements.lastOrNull()) {
                is HIRStatement.Expression -> {
                    val blockStatements = mutableListOf<HIRStatement>()
                    intoStatementList(blockStatements) {
                        emitAll(initialBlock.statements.dropLast(1))
                        emit(HIRStatement.Assignment(
                            lastMember.location,
                            resultName,
                            transformExpression(lastMember.expression)
                        ))
                    }

                    HIRBlock(
                        initialBlock.location,
                        namingCtx.makeUniqueName(),
                        blockStatements
                    )
                }
                else -> initialBlock
            }
            emitAll(block.statements)
        }

        return HIRExpression.ValRef(
            expression.location,
            expression.type,
            resultName
        )
    }
}