package hadesc.hirgen

import hadesc.assertions.requireUnreachable
import hadesc.ast.Expression
import hadesc.ast.IntrinsicType
import hadesc.ast.Pattern
import hadesc.context.ASTContext
import hadesc.context.Context
import hadesc.hir.*
import hadesc.resolver.Binding
import hadesc.types.Type
import hadesc.types.toSubstitution

internal class HIRGenExpression(
    private val ctx: Context,
    private val moduleContext: HIRGenModuleContext,
    private val functionContext: HIRGenFunctionContext,
) : HIRGenModuleContext by moduleContext,
    HIRGenFunctionContext by functionContext,
    ASTContext by ctx
{
    internal fun lowerVarExpression(expression: Expression.Var): HIRExpression {
        return when (val binding = ctx.resolver.resolve(expression.name)) {
            null -> requireUnreachable {
                "Found unresolved variable: ${expression.name} at ${expression.location}"
            }
            else -> lowerBinding(expression, binding)
        }
    }


    internal fun lowerBinding(
        expression: Expression,
        binding: Binding
    ): HIRExpression = when(binding) {
        is Binding.GlobalFunction -> HIRExpression.GlobalRef(
            expression.location,
            typeOfExpression(expression),
            lowerGlobalName(binding.declaration.name)
        )
        is Binding.ExternFunction -> {
            getExternDef(binding.declaration)
            HIRExpression.GlobalRef(
                expression.location,
                typeOfExpression(expression),
                lowerGlobalName(binding.declaration.binder)
            )
        }
        is Binding.FunctionParam -> HIRExpression.ParamRef(
            expression.location,
            typeOfExpression(expression),
            lowerLocalBinder(binding.param.binder),
            binding.binder
        )
        is Binding.ValBinding -> HIRExpression.ValRef(
            expression.location,
            typeOfExpression(expression),
            lowerLocalBinder(binding.statement.binder)
        )
        is Binding.Struct -> HIRExpression.GlobalRef(
            expression.location,
            typeOfExpression(expression),
            lowerGlobalName(binding.declaration.binder)
        )
        is Binding.GlobalConst -> HIRExpression.GlobalRef(
            expression.location,
            typeOfExpression(expression),
            lowerGlobalName(binding.declaration.name)
        )
        is Binding.ClosureParam -> HIRExpression.ParamRef(
            expression.location,
            typeOfExpression(expression),
            lowerLocalBinder(binding.param.binder),
            binding.binder,
        )
        is Binding.Enum -> TODO()
        is Binding.ExternConst -> HIRExpression.GlobalRef(
            expression.location,
            typeOfExpression(expression),
            lowerGlobalName(binding.declaration.name)
        )
        is Binding.MatchArmEnumCaseArg -> HIRExpression.ValRef(
            expression.location,
            typeOfExpression(expression),
            lowerLocalBinder(binding.arg.binder)
        )
    }

    private fun lowerIntrinsicCall(expression: Expression.Call): HIRExpression {
        check(isIntrinsicCall(expression))
        val intrinsic = if (expression.callee is Expression.TypeApplication) {
            val intrinsic = expression.callee.lhs
            check(intrinsic is Expression.Intrinsic)
            intrinsic
        } else {
            check(expression.callee is Expression.Intrinsic)
            expression.callee
        }
        return when (intrinsic.intrinsicType) {
            IntrinsicType.ADD, IntrinsicType.SUB, IntrinsicType.MUL -> {
                check(expression.args.size == 2)
                return HIRExpression.BinOp(
                    expression.location,
                    expression.type,
                    lowerExpression(expression.args[0].expression),
                    checkNotNull(INTRINSIC_TYPE_TO_BINOP[intrinsic.intrinsicType]),
                    lowerExpression(expression.args[1].expression),
                )
            }
            IntrinsicType.PTR_TO_INT -> {
                check(expression.type is Type.Size)
                check(expression.args.size == 1)
                return HIRExpression.IntegerConvert(
                    expression.location,
                    expression.type,
                    lowerExpression(expression.args[0].expression)
                )
            }
            IntrinsicType.INT_TO_PTR -> {
                check(expression.type is Type.Ptr)
                check(expression.args.size == 1)
                return HIRExpression.IntegerConvert(
                    expression.location,
                    expression.type,
                    lowerExpression(expression.args[0].expression)
                )
            }
            IntrinsicType.ERROR -> requireUnreachable()
        }
    }

    internal fun lowerCallExpression(expression: Expression.Call): HIRExpression {
        if (isIntrinsicCall(expression)) {
            return lowerIntrinsicCall(expression)
        }
        val callee = lowerExpression(expression.callee)
        if (callee.type is Type.Function) {
            return HIRExpression.InvokeClosure(
                location = expression.location,
                type = expression.type,
                closure = lowerExpression(expression.callee),
                args = expression.args.map { lowerExpression(it.expression)}
            )
        } else {
            val calleeType = callee.type
            check(calleeType is Type.Ptr && calleeType.to is Type.Function)
        }
        val receiver = ctx.analyzer.getCallReceiver(expression)?.let { lowerExpression(it) }
        val args =
            if (receiver != null) {
                listOf(receiver) + expression.args.map { lowerExpression(it.expression) }
            } else {
                expression.args.map { lowerExpression(it.expression) }
            }
        return HIRExpression.Call(
            location = expression.location,
            type = expression.type,
            callee = callee,
            args = args
        )
    }
    private fun isIntrinsicCall(expression: Expression.Call): Boolean {
        return expression.callee is Expression.Intrinsic
                || (expression.callee is Expression.TypeApplication && expression.callee.lhs is Expression.Intrinsic)
    }

    internal fun lowerEnumMatchExpression(expression: Expression.Match): HIRExpression {
        val discriminantType = expression.value.type
        val enumDef = ctx.analyzer.getEnumTypeDeclaration(discriminantType)
        checkNotNull(enumDef)

        // match value {
        //   X -> e1,
        //   Y -> e2,
        //   X -> e3
        // }
        //  ------------------ =>
        // val result: expression.type
        // val discriminant: expression.value.type
        // discriminant = expression.value
        // val tag      = expression.value.$tag
        // switch int [
        //    0 -> .0
        //    1 -> .1
        //    2 -> .2
        // ]

        val resultVar = declareVariable("match_result", expression.type)
        val discriminantVar = declareAndAssign("match_discriminant", lowerExpression(expression.value))

        val tagVar = declareAndAssign(
            "tag",
            discriminantVar.getStructField(
                enumTagFieldName,
                0,
                ctx.enumTagType()
            ),
        )

        val arms = expression.arms.mapNotNull { arm ->
            when (arm.pattern) {
                is Pattern.EnumCase -> {
                    val blockName = ctx.makeUniqueName(arm.pattern.identifier.name.text)
                    val (_, index) = checkNotNull(enumDef.getCase(arm.pattern.identifier.name))

                    MatchIntArm(
                        HIRConstant.IntValue(arm.pattern.location, ctx.enumTagType(), index),
                        buildBlock(arm.value.location, blockName) {
                            arm.pattern.args?.forEachIndexed { argIndex, arg ->
                                currentLocation = arg.location
                                when (arg) {
                                    is Pattern.Val -> {
                                        val type = ctx.analyzer.typeOfMatchArmEnumCaseArgBinding(Binding.MatchArmEnumCaseArg(
                                            arm.pattern,
                                            argIndex
                                        ))
                                        val argPatternValRef = declareVariable(arg.binder.name, type)
                                        val payloadUnionType = ctx.analyzer.getEnumPayloadType(enumDef)

                                        val unappliedPayloadType = payloadUnionType.members[index]
                                        val payloadType =
                                            if (enumDef.typeParams != null) {
                                                val typeArgs = discriminantType.typeArgs()
                                                check(typeArgs.size == enumDef.typeParams.size)
                                                unappliedPayloadType.applySubstitution(
                                                    enumDef.typeParams.zip(typeArgs).associate { (it, arg) ->
                                                        it.location to arg
                                                    }.toSubstitution()
                                                )
                                            } else {
                                                unappliedPayloadType
                                            }
                                        emitAssign(
                                            argPatternValRef,
                                            discriminantVar
                                                .getStructField("payload", 1, payloadType)
                                                .getStructField("$argIndex", argIndex, type)
                                        )
                                    }
                                    else -> {}
                                }
                            }
                            emitAssign(resultVar, lowerExpression(arm.value))
                        }
                    )
                }
                is Pattern.Wildcard -> null // handled separately as an otherwise block
                else -> requireUnreachable()
            }
        }

        val elseArm = expression.arms.find {
            it.pattern is Pattern.Wildcard
        }

        emit(
            HIRStatement.MatchInt(
            expression.location,
            tagVar,
            arms,
            otherwise = buildBlock(elseArm?.location ?: expression.location, ctx.makeUniqueName("else")) {
                if (elseArm != null) {
                    emit(
                        HIRStatement.Assignment(
                            elseArm.value.location,
                            resultVar.name,
                            lowerExpression(elseArm.value)
                        )
                    )
                }
            }
        ))

        return resultVar
    }
}

private val INTRINSIC_TYPE_TO_BINOP = mapOf(
    IntrinsicType.ADD to BinaryOperator.PLUS,
    IntrinsicType.SUB to BinaryOperator.MINUS,
    IntrinsicType.MUL to BinaryOperator.TIMES,
)
