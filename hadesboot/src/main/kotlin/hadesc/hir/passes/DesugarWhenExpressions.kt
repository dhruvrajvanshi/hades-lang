package hadesc.hir.passes

import hadesc.context.Context
import hadesc.hir.HIRBlock
import hadesc.hir.HIRConstant
import hadesc.hir.HIRExpression
import hadesc.hir.HIRStatement
import hadesc.hir.BinaryOperator
import hadesc.types.Type

class DesugarWhenExpressions(private val ctx: Context) : AbstractHIRTransformer() {
    override fun transformWhenExpression(expression: HIRExpression.When): HIRExpression {
        val blockStatements = requireNotNull(currentStatements)
        val discriminantName = ctx.makeUniqueName()
        val resultName = ctx.makeUniqueName()
        blockStatements.addAll(
            listOfNotNull(
                HIRStatement.ValDeclaration(
                    expression.discriminant.location,
                    discriminantName,
                    isMutable = false,
                    expression.discriminant.type
                ),
                HIRStatement.ValDeclaration(
                    expression.location,
                    resultName,
                    isMutable = true,
                    expression.type
                ),
                HIRStatement.Assignment(
                    expression.discriminant.location,
                    discriminantName,
                    transformExpression(expression.discriminant)
                )
            )
        )

        val discriminant = HIRExpression.ValRef(
            expression.location,
            expression.discriminant.type,
            discriminantName
        )
        val discriminantTag = HIRExpression.GetStructField(
            expression.location,
            ctx.enumTagType(),
            lhs = discriminant,
            name = ctx.makeName("\$tag"),
            index = 0
        )

        val discriminantPtr = HIRExpression.AddressOf(
            expression.discriminant.location,
            Type.Ptr(discriminant.type, isMutable = false),
            discriminantName
        )
        expression.cases.forEachIndexed { index, case ->
            val trueBranch = HIRBlock(
                case.expression.location,
                ctx.makeUniqueName(),
                mutableListOf(
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
                            ))
                    ),
                    HIRStatement.Assignment(
                        case.expression.location,
                        resultName,
                        transformExpression(case.expression)
                    )
                )
            )
            blockStatements.add(
                HIRStatement.ifStatement(
                    case.expression.location,
                    HIRExpression.BinOp(
                        case.expression.location,
                        Type.Bool,
                        lhs = discriminantTag,
                        operator = BinaryOperator.EQUALS,
                        rhs = HIRConstant.IntValue(
                            case.expression.location,
                            ctx.enumTagType(),
                            index
                        )
                    ),
                    trueBranch,
                    HIRBlock(location = case.expression.location, ctx.makeUniqueName())
                )
            )
        }
        return HIRExpression.ValRef(
            expression.location,
            expression.type,
            resultName
        )
    }
}