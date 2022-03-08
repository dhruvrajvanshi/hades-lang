package hadesc.hir.passes

import hadesc.context.GlobalConstantContext
import hadesc.context.NamingContext
import hadesc.hir.*
import hadesc.types.Type

class DesugarWhenExpressions(
    override val namingCtx: NamingContext,
    private val constCtx: GlobalConstantContext
) : AbstractHIRTransformer() {
    override fun transformWhenExpression(expression: HIRExpression.When): HIRExpression {
        val discriminantRef = declareAndAssign("match_discriminant", transformExpression(expression.discriminant))
        val resultRef = declareVariable("match_result", expression.type)

        val discriminantTag = discriminantRef.getStructField("\$tag", 0, constCtx.enumTagType)

        val discriminantPtr = addressOf(discriminantRef)

        expression.cases.forEachIndexed { index, case ->
            val trueBranch = buildBlock(case.expression.location) {
                emitAll(mutableListOf(
                    HIRStatement.ValDeclaration(
                        case.expression.location,
                        case.valueBinder,
                        isMutable = false,
                        type = case.casePayloadType
                    ),
                    HIRStatement.Assignment(
                        case.expression.location,
                        case.valueBinder,
                        HIRExpression.Load(
                            case.expression.location,
                            case.casePayloadType,
                            HIRExpression.PointerCast(
                                case.expression.location,
                                case.casePayloadType,
                                discriminantPtr
                            )
                        )
                    ),
                    HIRStatement.Assignment(
                        case.expression.location,
                        resultRef.name,
                        transformExpression(case.expression)
                    )
                ))
            }
            emit(
                HIRStatement.ifStatement(
                    case.expression.location,
                    HIRExpression.BinOp(
                        case.expression.location,
                        Type.Bool,
                        lhs = discriminantTag,
                        operator = BinaryOperator.EQUALS,
                        rhs = HIRConstant.IntValue(
                            case.expression.location,
                            constCtx.enumTagType,
                            index
                        )
                    ),
                    trueBranch,
                    HIRBlock(location = case.expression.location, namingCtx.makeUniqueName())
                )
            )
        }
        return resultRef
    }
}