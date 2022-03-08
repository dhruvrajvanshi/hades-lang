package hadesc.hir.passes

import hadesc.context.Context
import hadesc.hir.HIRBlock
import hadesc.hir.HIRConstant.BoolValue
import hadesc.hir.HIRExpression
import hadesc.hir.HIRExpression.ValRef
import hadesc.hir.HIRStatement.*
import hadesc.hir.BinaryOperator
import hadesc.hir.HIRStatement
import hadesc.types.Type

class SimplifyShortCircuitingOperators(val ctx: Context): AbstractHIRTransformer() {

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
                val name = ctx.makeUniqueName()
                emit(
                    ValDeclaration(
                        expression.location,
                        name,
                        isMutable = false,
                        Type.Bool,
                    )
                )
                emit(
                    Companion.ifStatement(
                        expression.location,
                        condition = transformExpression(expression.lhs),
                        trueBranch = HIRBlock(expression.lhs.location, ctx.makeUniqueName(), mutableListOf(
                            Assignment(
                                expression.lhs.location,
                                name,
                                transformExpression(expression.rhs)
                            )
                        )),
                        falseBranch = HIRBlock(expression.rhs.location, ctx.makeUniqueName(), mutableListOf(
                            Assignment(
                                expression.rhs.location,
                                name,
                                BoolValue(
                                    expression.lhs.location,
                                    Type.Bool,
                                    false
                                )
                            )
                        ))
                    )
                )
                ValRef(
                    expression.location,
                    Type.Bool,
                    name
                )
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
                val name = ctx.makeUniqueName()
                emit(
                    ValDeclaration(
                        expression.location,
                        name,
                        isMutable = false,
                        type = Type.Bool,
                    )
                )
                emit(
                    HIRStatement.ifStatement(
                        expression.location,
                        condition = transformExpression(expression.lhs),
                        trueBranch = HIRBlock(
                            expression.lhs.location, ctx.makeUniqueName(), mutableListOf(
                                Assignment(
                                    expression.lhs.location,
                                    name,
                                    BoolValue(expression.lhs.location, Type.Bool, true)
                                )
                            )
                        ),
                        falseBranch = HIRBlock(
                            expression.rhs.location, ctx.makeUniqueName(), mutableListOf(
                                Assignment(
                                    expression.rhs.location,
                                    name,
                                    transformExpression(expression.rhs)
                                )
                            )
                        )
                    )
                )
                ValRef(
                    expression.location,
                    Type.Bool,
                    name
                )
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