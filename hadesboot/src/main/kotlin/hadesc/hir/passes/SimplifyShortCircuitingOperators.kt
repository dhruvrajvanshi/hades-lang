package hadesc.hir.passes

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
            BinaryOperator.AND -> {
                val resultRef = declareVariable("result", Type.Bool)
                emit(
                    ifStatement(
                        expression.location,
                        condition = transformExpression(expression.lhs),
                        trueBranch = buildBlock(expression.lhs.location) {
                            emitAssign(resultRef, transformExpression(expression.rhs))
                        },
                        falseBranch = buildBlock(expression.rhs.location) {
                            emitAssign(resultRef, falseValue())
                        }
                    )
                )
                return resultRef
            }
            /**
             * a or b
             * if a {
             *   result = true
             * } else {
             *   result = b
             * }
             */
            BinaryOperator.OR -> {
                val resultRef = declareVariable("result", Type.Bool)
                currentLocation = expression.lhs.location
                emit(
                    ifStatement(
                        expression.location,
                        condition = transformExpression(expression.lhs),
                        trueBranch = buildBlock {
                            emitAssign(resultRef, trueValue())
                        },
                        falseBranch = buildBlock {
                            emitAssign(resultRef, transformExpression(expression.rhs))
                        }
                    )
                )
                resultRef
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