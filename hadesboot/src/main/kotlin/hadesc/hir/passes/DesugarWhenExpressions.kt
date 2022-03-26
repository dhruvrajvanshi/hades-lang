package hadesc.hir.passes

import hadesc.context.GlobalConstantContext
import hadesc.context.NamingContext
import hadesc.hir.*
import hadesc.types.Type
import hadesc.types.ptr

class DesugarWhenExpressions(
    override val namingCtx: NamingContext,
    private val constCtx: GlobalConstantContext
) : AbstractHIRTransformer() {
    override fun transformWhenExpression(expression: HIRExpression.When): HIRExpression {
        val discriminantLoc = allocaAssign("match_discriminant", transformExpression(expression.discriminant))
        val resultRef = declareVariable("match_result", expression.type)

        val discriminantTag = discriminantLoc.ptr().fieldPtr("\$tag", 0, constCtx.enumTagType.ptr()).load()

        val discriminantPtr = discriminantLoc.ptr()

        expression.cases.forEachIndexed { index, case ->
            val trueBranch = buildBlock(case.expression.location) {
                currentLocation = case.expression.location
                declareVariable(case.valueBinder, case.casePayloadType)
            }
            emitAssign(case.valueBinder, discriminantPtr.ptrCast(case.casePayloadType))
            emitAssign(resultRef, transformExpression(case.expression))
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