package hadesc.hir.passes

import hadesc.assertions.requireUnreachable
import hadesc.context.NamingContext
import hadesc.hir.*
import hadesc.hir.HIRStatement.Companion.ifStatement
import hadesc.types.Type

class SimplifyShortCircuitingOperators(override val namingCtx: NamingContext): AbstractHIRTransformer() {

    override fun transformBinOp(expression: HIRExpression.BinOp): HIRExpression {
        return when(expression.operator) {
            /**
             * a and b
             *
             * val result: Bool
             * if (a) {
             *  result = b
             * } else {
             *   result = false
             * }
             * result
             */
            BinaryOperator.AND -> requireUnreachable()
            /**
             * a or b
             * if a {
             *   result = true
             * } else {
             *   result = b
             * }
             */
            BinaryOperator.OR -> {
                val resultMem = emitAlloca("result", Type.Bool)
                currentLocation = expression.lhs.location
                emit(
                    ifStatement(
                        expression.location,
                        condition = transformExpression(expression.lhs),
                        trueBranch = buildBlock {
                            emitStore(resultMem.mutPtr(), trueValue())
                        },
                        falseBranch = buildBlock {
                            emitStore(resultMem.mutPtr(), transformExpression(expression.rhs))
                        }
                    )
                )
                resultMem.ptr().load()
            }
            else -> HIRExpression.BinOp(
                expression.location,
                lowerType(expression.type),
                lhs = transformExpression(expression.lhs),
                operator = expression.operator,
                rhs = transformExpression(expression.rhs),
            )
        }
    }
}