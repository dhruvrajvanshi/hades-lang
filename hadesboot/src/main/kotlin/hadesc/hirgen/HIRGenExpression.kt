package hadesc.hirgen

import hadesc.analysis.PostAnalysisContext
import hadesc.assertions.requireUnreachable
import hadesc.ast.*
import hadesc.context.Context
import hadesc.frontend.PropertyBinding
import hadesc.hir.*
import hadesc.location.HasLocation
import hadesc.resolver.Binding
import hadesc.types.Type
import hadesc.types.ptr
import hadesc.types.toSubstitution

internal class HIRGenExpression(
    private val ctx: Context,
    private val moduleContext: HIRGenModuleContext,
    private val functionContext: HIRGenFunctionContext,
    private val closureGen: HIRGenClosure,
    private val postAnalysisContext: PostAnalysisContext,
) : HIRGenModuleContext by moduleContext,
    HIRGenFunctionContext by functionContext,
    PostAnalysisContext by postAnalysisContext {
    override val currentModule: HIRModule
        get() = moduleContext.currentModule
    internal fun lowerVarExpression(expression: Expression.Var): HIROperand {
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
    ): HIROperand = when (binding) {
        is Binding.Local -> {
            val name = when (expression) {
                is Expression.Move -> expression.name
                is Expression.Var -> expression.name
                else -> requireUnreachable()
            }
            if (ctx.analyzer.isClosureCapture(name)) {
                closureGen.lowerCaptureBinding(name, binding)
            } else {
                when (binding) {
                    is Binding.FunctionParam ->
                        lowerParamRef(expression, binding.param)
                    is Binding.ClosureParam ->
                        lowerParamRef(expression, binding.param)
                    is Binding.ValBinding -> HIRExpression.LocalRef(
                        expression.location,
                        lowerType(typeOfExpression(expression)).ptr(),
                        lowerLocalBinder(binding.statement.binder)
                    ).load()
                    is Binding.MatchArmEnumCaseArg ->
                        HIRExpression.LocalRef(
                            expression.location,
                            lowerType(typeOfExpression(expression)).ptr(),
                            lowerLocalBinder(binding.arg.binder)
                        ).load()
                }
            }
        }
        is Binding.GlobalFunction -> HIRExpression.GlobalRef(
            expression.location,
            lowerType(typeOfExpression(expression)),
            lowerGlobalName(binding.declaration.name)
        )
        is Binding.ExternFunction -> {
            getExternDef(binding.declaration)
            HIRExpression.GlobalRef(
                expression.location,
                lowerType(typeOfExpression(expression)),
                lowerGlobalName(binding.declaration.binder)
            )
        }
        is Binding.Struct -> HIRExpression.GlobalRef(
            expression.location,
            lowerType(typeOfExpression(expression)),
            lowerGlobalName(binding.declaration.binder)
        )
        is Binding.GlobalConst -> HIRExpression.GlobalRef(
            expression.location,
            typeOfExpression(expression),
            lowerGlobalName(binding.declaration.name)
        )
        is Binding.Enum -> TODO()
        is Binding.ExternConst -> HIRExpression.GlobalRef(
            expression.location,
            typeOfExpression(expression),
            lowerGlobalName(binding.declaration.name)
        )
    }

    private fun lowerParamRef(expression: Expression, param: Param): HIROperand {
        return HIRExpression.ParamRef(
            expression.location,
            lowerType(typeOfExpression(expression)),
            lowerLocalBinder(param.binder),
            param.binder
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
                return emit(
                    HIRStatement.BinOp(
                        expression.location,
                        ctx.makeUniqueName(),
                        expression.type,
                        lowerExpression(expression.args[0].expression),
                        checkNotNull(INTRINSIC_TYPE_TO_BINOP[intrinsic.intrinsicType]),
                        lowerExpression(expression.args[1].expression)
                    )
                ).result()
            }
            IntrinsicType.PTR_TO_INT -> {
                check(expression.type is Type.Size)
                check(expression.args.size == 1)
                return emitPtrToInt(
                    lowerExpression(expression.args[0].expression).asOperand(),
                    expression.type
                )
            }
            IntrinsicType.INT_TO_PTR -> {
                val exprTy = expression.type
                check(exprTy is Type.Ptr)
                check(expression.args.size == 1)
                return emitIntToPtr(
                    lowerExpression(expression.args[0].expression).asOperand(),
                    exprTy
                )
            }
            IntrinsicType.MEMCPY -> {
                val typeArgs = ctx.analyzer.getTypeArgs(expression.callee)
                check(typeArgs != null)
                check(typeArgs.size == 1)
                check(expression.args.size == 3)
                emit(
                    HIRStatement.Memcpy(
                        expression.location,
                        lowerExpression(expression.args[0].expression),
                        lowerExpression(expression.args[1].expression),
                        lowerExpression(expression.args[2].expression)
                    )
                )
                HIRConstant.Void(expression.location)
            }
            IntrinsicType.ERROR -> requireUnreachable()
        }
    }

    internal fun lowerCallExpression(expression: Expression.Call): HIRExpression {
        if (isIntrinsicCall(expression)) {
            return lowerIntrinsicCall(expression)
        }

        val calleeWithoutTypeArgs = expression.callee.withoutTypeArgs()
        val calleeStructDecl = ctx.analyzer.getStructDeclaration(calleeWithoutTypeArgs)
        if (calleeStructDecl != null && calleeStructDecl.isRef) {
            return lowerRefStructConstructorCall(calleeStructDecl, expression)
        }
        val callee = lowerExpression(expression.callee)
        if (callee.type is Type.Closure) {
            return emit(
                HIRStatement.InvokeClosure(
                    location = expression.location,
                    name = namingCtx.makeUniqueName(),
                    type = expression.type,
                    closureRef = lowerExpression(expression.callee),
                    args = expression.args.map { lowerExpression(it.expression) }
                )
            ).result()
        } else {
            val calleeType = callee.type
            check(calleeType is Type.FunctionPtr)
        }
        val receiver = ctx.analyzer.getCallReceiver(expression)?.let { lowerExpression(it) }
        val args =
            if (receiver != null) {
                listOf(receiver) + expression.args.map { lowerExpression(it.expression) }
            } else {
                expression.args.map { lowerExpression(it.expression) }
            }
        return emitCall(
            callee = callee,
            args = args
        ).result()
    }

    private fun lowerRefStructConstructorCall(structDecl: Declaration.Struct, expression: Expression.Call): HIRExpression {
        check(ctx.analyzer.isRefStructType(expression.type))
        val loweredType = lowerType(expression.type)
        check(loweredType is Type.Ref)
        val ref = emitAllocRef(ofType = loweredType.inner).ref()
        val fields = structDecl.fields
        check(fields.size == expression.args.size)

        for ((field, arg) in fields.zip(expression.args)) {
            ref.storeRefField(field.binder.name, lowerExpression(arg.expression))
        }

        return ref
    }

    private fun isIntrinsicCall(expression: Expression.Call): Boolean {
        return expression.callee is Expression.Intrinsic ||
            (expression.callee is Expression.TypeApplication && expression.callee.lhs is Expression.Intrinsic)
    }

    internal fun lowerEnumMatchExpression(expression: Expression.Match): HIROperand {
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

        val resultRef = emitAlloca("match_result", expression.type)
        val discriminantMem = allocaAssign("match_discriminant", lowerExpression(expression.value))

        val tagVar = discriminantMem.ptr().fieldPtr(enumTagFieldName).load()
        fun applyTypeArgs(type: Type) =
            if (enumDef.typeParams != null) {
                val typeArgs = discriminantType.typeArgs()
                check(typeArgs.size == enumDef.typeParams.size)
                type.applySubstitution(
                    enumDef.typeParams.zip(typeArgs).associate { (it, arg) ->
                        it.binder.id to arg
                    }.toSubstitution()
                )
            } else {
                type
            }

        val payloadUnionType = applyTypeArgs(ctx.analyzer.getEnumPayloadType(enumDef))
        check(payloadUnionType is Type.UntaggedUnion)
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
                                        val type = lowerType(
                                            ctx.analyzer.typeOfMatchArmEnumCaseArgBinding(
                                                Binding.MatchArmEnumCaseArg(
                                                    arm.pattern,
                                                    argIndex
                                                )
                                            )
                                        )
                                        val argPatternValRef = emitAlloca(arg.binder.name, type)

                                        val payloadType = payloadUnionType.members[index]
                                        emitStore(
                                            argPatternValRef.mutPtr(),
                                            discriminantMem.ptr()
                                                .fieldPtr(ctx.makeName("payload"))
                                                .ptrCast(payloadType)
                                                .fieldPtr(ctx.makeName("$argIndex"))
                                                .load()
                                        )
                                    }
                                    else -> {}
                                }
                            }
                            emitStore(resultRef.mutPtr(), lowerExpression(arm.value))
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
                        emitStore(
                            resultRef.mutPtr(),
                            lowerExpression(elseArm.value)
                        )
                    }
                }
            )
        )

        return resultRef.ptr().load()
    }

    private fun resolveStructConstructorBinding(callee: Expression): Binding.Struct? {
        return when (callee) {
            is Expression.Property ->
                when (val propBinding = callee.binding) {
                    is PropertyBinding.Global -> propBinding.binding as? Binding.Struct
                    else -> null
                }
            is Expression.Var -> ctx.resolver.resolve(callee.name) as? Binding.Struct
            else -> null
        }
    }

    fun lowerConstExpression(expression: Expression): HIRConstant =
        when (expression) {
            is Expression.IntLiteral,
            is Expression.FloatLiteral,
            is Expression.ByteString,
            is Expression.ByteCharLiteral,
                -> lowerExpression(expression) as HIRConstant
            is Expression.Var -> lowerConstVarExpression(expression)
            is Expression.Property -> lowerConstPropertyExpression(expression)
            is Expression.Call -> lowerConstCallExpression(expression)
            else -> requireUnreachable {
                "Expression: ${expression::class.simpleName} not supported as a constant"
            }
        }

    private fun lowerConstVarExpression(expression: Expression.Var): HIRConstant = when (val binding = ctx.resolver.resolve(expression.name)) {
        is Binding.GlobalConst -> lowerConstExpression(binding.declaration.initializer)
        is Binding.GlobalFunction -> HIRConstant.GlobalFunctionRef(expression.location, expression.type, ctx.resolver.qualifiedName(binding.declaration.name))
        else -> errorConstant(
            expression,
            expression.type,
            "Variable expression found in constant expression"
        )
    }
    private fun lowerConstPropertyExpression(expression: Expression.Property): HIRConstant {
        when (expression.binding) {
            else -> {
                return errorConstant(expression, expression.type, "Property expression found in constant expression")
            }

        }
    }

    private fun errorConstant(loc: HasLocation, type: Type, message: String): HIRConstant {
        ctx.diagnosticReporter.report(
            loc.location,
            message
        )
        return HIRConstant.Error(loc.location, type, message)
    }

    private fun lowerConstCallExpression(expression: Expression.Call): HIRConstant {
        val callee = expression.callee
        val calleeStructDecl = resolveStructConstructorBinding(callee)
        if (calleeStructDecl == null) {
            ctx.diagnosticReporter.report(
                expression.callee.location,
                "Only struct constructors can be called in constant expressions"
            )
            return HIRConstant.Error(expression.location, expression.type, "Function call found in constant expression")
        }
        return HIRConstant.StructValue(
            expression.location,
            expression.type,
            expression.args.map { lowerConstExpression(it.expression) }
        )

    }

}

private val INTRINSIC_TYPE_TO_BINOP = mapOf(
    IntrinsicType.ADD to BinaryOperator.PLUS,
    IntrinsicType.SUB to BinaryOperator.MINUS,
    IntrinsicType.MUL to BinaryOperator.TIMES
)
