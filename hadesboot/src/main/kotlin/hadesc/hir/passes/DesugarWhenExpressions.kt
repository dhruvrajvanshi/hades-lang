package hadesc.hir.passes

import hadesc.context.GlobalConstantContext
import hadesc.context.NamingContext
import hadesc.hir.HIRBlock
import hadesc.hir.HIRConstant
import hadesc.hir.HIRExpression
import hadesc.hir.HIRStatement
import hadesc.hir.BinaryOperator
import hadesc.types.Type

class DesugarWhenExpressions(
    override val namingCtx: NamingContext,
    private val constCtx: GlobalConstantContext
) : AbstractHIRTransformer() {
    override fun transformWhenExpression(expression: HIRExpression.When): HIRExpression {
        val blockStatements = requireNotNull(currentStatements)
        val discriminantName = namingCtx.makeUniqueName()
        val resultName = namingCtx.makeUniqueName()
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
            constCtx.enumTagType,
            lhs = discriminant,
            name = namingCtx.makeName("\$tag"),
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
                namingCtx.makeUniqueName(),
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
                            constCtx.enumTagType,
                            index
                        )
                    ),
                    trueBranch,
                    HIRBlock(location = case.expression.location, namingCtx.makeUniqueName())
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