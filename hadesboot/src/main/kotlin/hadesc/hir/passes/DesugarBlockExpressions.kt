package hadesc.hir.passes

import hadesc.context.Context
import hadesc.hir.HIRBlock
import hadesc.hir.HIRExpression
import hadesc.hir.HIRStatement
import hadesc.types.Type

class DesugarBlockExpressions(private val ctx: Context): HIRTransformer {
    override var statements: MutableList<HIRStatement>? = null

    override fun transformBlockExpression(expression: HIRExpression.BlockExpression): HIRExpression {
        val resultName = ctx.makeUniqueName()

        checkNotNull(statements).apply {
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
                        ctx.makeUniqueName(),
                        blockStatements
                    )
                }
                else -> initialBlock
            }
            if (expression.type !is Type.Void) {
                add(HIRStatement.ValDeclaration(
                    expression.location,
                    resultName,
                    isMutable = false,
                    type = expression.type
                ))
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