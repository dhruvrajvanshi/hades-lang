package hadesc.hir.passes

import hadesc.context.Context
import hadesc.hir.HIRBlock
import hadesc.hir.HIRConstant
import hadesc.hir.HIRExpression
import hadesc.hir.HIRStatement
import hadesc.ir.BinaryOperator
import hadesc.types.Type

class DesugarWhenExpressions(val ctx: Context) : HIRTransformer {
    var statements: MutableList<HIRStatement>? = null

    override fun transformBlock(body: HIRBlock): HIRBlock {
        val oldStatements = statements
        val currentStatements = mutableListOf<HIRStatement>()
        statements = currentStatements
        body.statements.forEach {
            currentStatements.addAll(transformStatement(it))
        }
        statements = oldStatements
        return HIRBlock(
            location = body.location,
            statements = currentStatements
        )
    }

    override fun transformWhenExpression(expression: HIRExpression.When): HIRExpression {
        val blockStatements = requireNotNull(statements)
        val discriminantName = ctx.makeUniqueName()
        val resultName = ctx.makeUniqueName()
        blockStatements.addAll(listOf(
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
        ))

        val discriminant = HIRExpression.ValRef(
            expression.location,
            expression.discriminant.type,
            discriminantName
        )
        val discriminantTag = HIRExpression.GetStructField(
            expression.location,
            ctx.sealedTypeDiscriminantType(),
            lhs = discriminant,
            name = ctx.makeName("\$tag"),
            index = 0
        )

        val declaration = ctx.checker.getSealedTypeDeclaration(expression.discriminant.type)
        val discriminantPtr = HIRExpression.AddressOf(
            expression.discriminant.location,
            Type.Ptr(discriminant.type, isMutable = false),
            discriminantName
        )
        expression.cases.forEachIndexed { index, case ->
            val trueBranch = HIRBlock(
                case.expression.location,
                listOf(
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
                HIRStatement.If(
                    case.expression.location,
                    HIRExpression.BinOp(
                        case.expression.location,
                        Type.Bool,
                        lhs = discriminantTag,
                        operator = BinaryOperator.EQUALS,
                        rhs = HIRExpression.Constant(
                            HIRConstant.IntValue(
                                case.expression.location,
                                ctx.sealedTypeDiscriminantType(),
                                index
                            )
                        )

                    ),
                    trueBranch,
                    HIRBlock(location = case.expression.location, emptyList())
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