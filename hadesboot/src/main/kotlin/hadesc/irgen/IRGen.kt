package hadesc.irgen

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.context.Context
import hadesc.hir.*
import hadesc.ir.*
import hadesc.ir.passes.TypeVisitor
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type
import kotlin.math.exp

class IRGen(
        private val ctx: Context
) {
    private val module = IRModule()
    private val builder = IRBuilder()
    private var currentFunction: IRFunctionDef? = null

    fun generate(hirModule: HIRModule): IRModule {
        for (definition in hirModule.definitions) {
            lowerDefinition(definition)
        }
        return module
    }

    private fun lowerDefinition(definition: HIRDefinition) = when(definition) {
        is HIRDefinition.Function -> lowerFunctionDef(definition)
        is HIRDefinition.ExternFunction -> lowerExternFunctionDef(definition)
        is HIRDefinition.Struct -> lowerStructDef(definition)
        is HIRDefinition.Const -> lowerConstDef(definition)
        is HIRDefinition.Implementation -> requireUnreachable()
        is HIRDefinition.ExternConst -> lowerExternConstDef(definition)
    }

    private fun lowerConstDef(definition: HIRDefinition.Const) {
        module.addConstDef(definition.location, lowerGlobalName(definition.name), definition.initializer.type, lowerExpression(definition.initializer))
    }

    private fun lowerStructDef(definition: HIRDefinition.Struct) {
        require(definition.typeParams == null)
        val instanceType = Type.Constructor(binder = null, name = definition.name)
        val constructorType = Type.Function(
                from = definition.fields.map { it.second },
                to = instanceType,
                traitRequirements = null
        )
        module.addStructDef(
            definition.location,
            constructorType,
            instanceType,
            name = lowerGlobalName(definition.name),
            typeParams = null,
            fields = definition.fields.toMap()
        )
    }

    private fun lowerExternFunctionDef(definition: HIRDefinition.ExternFunction) {
        module.addExternFunctionDef(
                definition.location,
                lowerGlobalName(definition.name),
                definition.type,
                definition.externName
        )
    }

    private fun lowerExternConstDef(definition: HIRDefinition.ExternConst) {
        module.addExternConstDef(
            definition.location,
            lowerGlobalName(definition.name),
            definition.type,
            definition.externName
        )
    }

    private fun lowerGlobalName(name: QualifiedName): IRGlobalName {
        return IRGlobalName(name)
    }

    private fun lowerFunctionDef(definition: HIRDefinition.Function) {

        val functionName = lowerGlobalName(definition.name)
        val entryBlock = IRBlock(definition.body.location)
        require(definition.typeParams == null)

        val fn = module.addGlobalFunctionDef(
                definition.location,
                functionName,
                typeParams = null,
                constraints = emptyList(),
                params = definition.params.mapIndexed { index, it -> lowerParam(functionName, index, it) },
                entryBlock = entryBlock,
                type = definition.type as Type.Function
        )

        currentFunction = fn
        lowerBlock(definition.body, entryBlock)
    }

    private fun lowerBlock(
            body: HIRBlock,
            block: IRBlock= IRBlock(body.location),
            cleanup: () -> Unit = {}
    ): IRBlock {
        builder.withinBlock(block) {
            for (statement in body.statements) {
                lowerStatement(statement)
            }
        }

        cleanup()
        return block

    }

    private fun lowerStatement(statement: HIRStatement): Unit = when(statement) {
        is HIRStatement.Expression -> lowerExpressionStatement(statement)
        is HIRStatement.ReturnVoid -> lowerReturnVoidStatement()
        is HIRStatement.Return -> lowerReturnStatement(statement)
        is HIRStatement.ValDeclaration -> lowerValStatement(statement)
        is HIRStatement.If -> lowerIfStatement(statement)
        is HIRStatement.Assignment -> lowerAssignmentStatement(statement)
        is HIRStatement.While -> lowerWhileStatement(statement)
        is HIRStatement.Store -> lowerStoreStatement(statement)
    }

    private fun lowerStoreStatement(statement: HIRStatement.Store) {
        builder.buildStore(
                ptr = lowerExpression(statement.ptr),
                value = lowerExpression(statement.value)
        )
    }

    private fun lowerWhileStatement(statement: HIRStatement.While) {
        val whileBody = buildBlock(statement.body.location)
        val whileExit = forkControlFlow(statement.location)

        builder.buildBranch(
                statement.condition.location,
                lowerExpression(statement.condition),
                whileBody.name,
                whileExit.name
        )

        lowerBlock(statement.body, whileBody) {
            terminateBlock(whileBody) {
                builder.buildBranch(
                        statement.condition.location,
                        lowerExpression(statement.condition),
                        whileBody.name,
                        whileExit.name
                )
            }
        }

        builder.positionAtEnd(whileExit)
    }

    private fun lowerAssignmentStatement(statement: HIRStatement.Assignment) {
        val ptrName = localValPtrName(statement.name)
        val ptr = builder.buildVariable(
                Type.Ptr(statement.value.type, isMutable = true),
                statement.location,
                ptrName
        )
        builder.buildStore(ptr = ptr, value = lowerExpression(statement.value))
    }

    private fun lowerIfStatement(statement: HIRStatement.If) {
        val ifTrue = buildBlock(statement.trueBranch.location)
        val ifFalse = buildBlock(statement.falseBranch.location)
        val end = forkControlFlow(statement.location)

        builder.buildBranch(
                statement.condition.location,
                lowerExpression(statement.condition),
                ifTrue = ifTrue.name,
                ifFalse = ifFalse.name
        )

        lowerBlock(statement.trueBranch, ifTrue) {
            terminateBlock(ifTrue) {
                builder.buildJump(statement.trueBranch.location, end.name)
            }
        }

        lowerBlock(statement.falseBranch, ifFalse) {
            terminateBlock(ifFalse) {
                builder.buildJump(statement.location, end.name)
            }
        }

        builder.positionAtEnd(end)
    }
    private fun terminateBlock(entryBlock: IRBlock, f: () -> IRInstruction) {
        val isVisited = mutableSetOf<IRLocalName>()
        fun visitBlock(branch: IRBlock) {
            if (isVisited.contains(branch.name)) {
                return
            }
            isVisited.add(branch.name)
            if (!branch.hasTerminator()) {
                builder.withinBlock(branch) {
                    f()
                }
            } else {
                when (val statement = branch.statements.last()) {
                    is IRReturnInstruction -> Unit
                    IRReturnVoidInstruction -> Unit
                    is IRSwitch -> requireUnreachable()
                    is IRBr -> {
                        val block1 = getBlock(statement.ifTrue)
                        val block2 = getBlock(statement.ifFalse)
                        visitBlock(block1)
                        visitBlock(block2)
                    }
                    is IRJump -> {
                        val block = getBlock(statement.label)
                        visitBlock(block)
                    }
                    else -> Unit
                }
            }
        }
        visitBlock(entryBlock)
    }

    private fun getBlock(name: IRLocalName): IRBlock {
        return requireNotNull(currentFunction?.getBlock(name))
    }

    private fun buildBlock(location: SourceLocation): IRBlock {
        val name = IRLocalName(ctx.makeUniqueName())
        val block = IRBlock(location, name)
        requireNotNull(currentFunction).appendBlock(block)
        return block
    }

    private fun forkControlFlow(location: SourceLocation): IRBlock {
        return buildBlock(location)
    }

    private fun lowerValStatement(statement: HIRStatement.ValDeclaration) {
        val ptrName = localValPtrName(statement.name)
        builder.buildAlloca(statement.type, ptrName)
    }

    private fun localValPtrName(name: Name): IRLocalName {
        return lowerLocalName(ctx.makeName(name.text + "\$ptr"))
    }

    private fun lowerReturnStatement(statement: HIRStatement.Return) {
        builder.buildReturn(lowerExpression(statement.expression))
    }

    private fun lowerReturnVoidStatement() {
        builder.buildRetVoid()
    }

    private fun lowerExpressionStatement(statement: HIRStatement.Expression) {
        lowerExpression(statement.expression)
    }

    private fun lowerExpression(expression: HIRExpression): IRValue = checkMonomorphicType(when(expression) {
        is HIRExpression.Call -> lowerCallExpression(expression)
        is HIRExpression.GlobalRef -> lowerGlobalRef(expression)
        is HIRExpression.Constant -> lowerConstant(expression.constant)
        is HIRExpression.ParamRef -> lowerLocalRef(expression)
        is HIRExpression.ValRef -> lowerValRef(expression)
        is HIRExpression.GetStructField -> lowerGetStructField(expression)
        is HIRExpression.Not -> lowerNotExpression(expression)
        is HIRExpression.BinOp -> lowerBinOpExpression(expression)
        is HIRExpression.NullPtr -> lowerNullPtrExpression(expression)
        is HIRExpression.SizeOf -> lowerSizeOfExpression(expression)
        is HIRExpression.AddressOf -> lowerAddressOfExpression(expression)
        is HIRExpression.Load -> lowerLoadExpression(expression)
        is HIRExpression.PointerCast -> lowerPointerCastExpression(expression)
        is HIRExpression.GetStructFieldPointer -> lowerGetStructFieldPointer(expression)
        is HIRExpression.TypeApplication -> requireUnreachable()
        is HIRExpression.TraitMethodRef -> requireUnreachable()
        is HIRExpression.UnsafeCast -> lowerUnsafeCast(expression)
        is HIRExpression.When -> requireUnreachable()
        is HIRExpression.Closure -> requireUnreachable()
        is HIRExpression.InvokeClosure -> requireUnreachable()
        is HIRExpression.IntegerConvert -> lowerIntegerConvert(expression)
    })

    private fun checkMonomorphicType(expression: IRValue): IRValue {
        object : TypeVisitor {
            override fun visitTypeFunction(type: Type.TypeFunction) {
                requireUnreachable {"Unexpected generic type for expression at ${expression.location}"}
            }

            override fun visitParamRefType(type: Type.ParamRef) {
                requireUnreachable {"Unexpected generic type for expression at ${expression.location}"}
            }

            override fun visitTypeApplication(type: Type.Application) {
                requireUnreachable {"Unexpected generic type for expression at ${expression.location}"}
            }

            override fun visitGenericInstance(type: Type.GenericInstance) {
                requireUnreachable {"Unexpected generic type for expression at ${expression.location}"}
            }
        }.visitType(expression.type)
        return expression
    }

    private fun lowerIntegerConvert(expression: HIRExpression.IntegerConvert): IRValue {
        val fromType = expression.value.type
        require(fromType is Type.Integral || fromType is Type.Size)
        val toType = expression.type
        require(toType is Type.Integral || toType is Type.Size)

        val fromSize = when (fromType) {
            is Type.Integral -> fromType.size
            is Type.Size -> sizeOfSizeInBits()
            else -> requireUnreachable()
        }

        val toSize = when (toType) {
            is Type.Integral -> toType.size
            is Type.Size -> sizeOfSizeInBits()
            else -> requireUnreachable()
        }

        return when {
            toSize > fromSize -> {
                IRZExt(
                    toType,
                    expression.location,
                    lowerExpression(expression.value)
                )
            }
            toSize < fromSize -> {
                IRTruncate(
                    toType,
                    expression.location,
                    lowerExpression(expression.value)
                )
            }
            else -> {
                lowerExpression(expression.value)
            }
        }
    }

    private fun sizeOfSizeInBits(): Int = 64

    private fun lowerUnsafeCast(expression: HIRExpression.UnsafeCast): IRValue {
        return IRUnsafeCast(
            expression.type,
            expression.location,
            lowerExpression(expression.value)
        )
    }

    private fun lowerGetStructFieldPointer(expression: HIRExpression.GetStructFieldPointer): IRValue {
        return IRGetElementPointer(
                expression.type,
                expression.location,
                ptr = lowerExpression(expression.lhs),
                offset = expression.memberIndex
        )
    }

    private fun lowerPointerCastExpression(expression: HIRExpression.PointerCast): IRValue {
        return IRPointerCast(
            expression.type,
            expression.location,
            toPointerOfType = expression.toPointerOfType,
            arg = lowerExpression(expression.value)
        )
    }

    private fun lowerLoadExpression(expression: HIRExpression.Load): IRValue {
        val name = makeLocalName()
        builder.buildLoad(name, expression.type, lowerExpression(expression.ptr))
        return IRVariable(expression.type, expression.location, name)
    }

    private fun lowerAddressOfExpression(expression: HIRExpression.AddressOf): IRValue {
        return builder.buildVariable(
                expression.type,
                expression.location,
                localValPtrName(expression.name)
        )
    }

    private fun lowerSizeOfExpression(expression: HIRExpression.SizeOf): IRValue {
        return IRSizeOf(
                expression.type,
                expression.location,
                ofType = expression.ofType
        )
    }

    private fun lowerNullPtrExpression(expression: HIRExpression.NullPtr): IRValue {
        return IRNullPtr(expression.type, expression.location)
    }

    private fun lowerBinOpExpression(expression: HIRExpression.BinOp): IRValue {
        return if (isShortCircuitingOperator(expression.operator)) {
            lowerShortCircuitingOperator(expression)
        } else {
            val ty = expression.type
            val lhs = lowerExpression(expression.lhs)
            val rhs = lowerExpression(expression.rhs)
            val name = makeLocalName()
            builder.buildBinOp(ty, name, lhs, expression.operator, rhs)
            builder.buildVariable(ty, expression.location, name)
        }
    }
    private fun isShortCircuitingOperator(operator: BinaryOperator): Boolean {
        return operator == BinaryOperator.AND || operator == BinaryOperator.OR
    }

    /**
     * %condition = alloca Bool
     * store %condition lhs
     * %lhs = load %condition

     * .done:
     * load %condition
     */
    private fun lowerShortCircuitingOperator(expression: HIRExpression.BinOp): IRValue {
        val conditionName = makeLocalName()
        val conditionPtr = IRVariable(Type.Ptr(Type.Bool, isMutable = true), expression.lhs.location, conditionName)
        val lhsName = makeLocalName()
        val lhs = IRVariable(Type.Bool, expression.location, lhsName)
        val done = forkControlFlow(expression.location)
        // %condition = alloca Bool
        // store %condition lhs
        // %lhs = load %condition
        alloca(Type.Bool, conditionName)
        builder.buildStore(ptr = conditionPtr, value = lowerExpression(expression.lhs))
        builder.buildLoad(name = lhsName, ptr = conditionPtr, type = Type.Bool)

        val (branch1, branch2) = when (expression.operator) {
            BinaryOperator.AND -> {
                // br %lhs if_true:.and_rhs if_false:.and_short_circuit
                // .and_rhs:
                //   store %condition rhs
                //   jmp .done
                // .and_short_circuit:
                //   jmp .done
                val andRHS = buildBlock(expression.rhs.location)
                val andShortCircuit = buildBlock(expression.lhs.location)
                builder.buildBranch(expression.location, lhs, ifTrue = andRHS.name, ifFalse = andShortCircuit.name)
                builder.withinBlock(andRHS) {
                    builder.buildStore(ptr = conditionPtr, value = lowerExpression(expression.rhs))
                }
                andRHS to andShortCircuit

            }
            BinaryOperator.OR -> {
                // br %lhs if_true:.or_short_circuit if_false:.or_rhs
                // .or_short_circuit:
                //   jmp .done
                // .or_rhs:
                //   store %condition rhs
                //   jmp .done
                val orShortCircuit = buildBlock(expression.location)
                val orRHS = buildBlock(expression.rhs.location)

                builder.buildBranch(expression.location, lhs, ifTrue = orShortCircuit.name, ifFalse = orRHS.name)

                builder.withinBlock(orRHS) {
                    builder.buildStore(ptr = conditionPtr, value = lowerExpression(expression.rhs))
                }
                orRHS to orShortCircuit
            }
            else -> {
                requireUnreachable()
            }
        }

        val resultName = makeLocalName()
        builder.withinBlock(done) {
            builder.buildLoad(resultName, Type.Bool, ptr = conditionPtr)
        }
        terminateBlock(branch1) {
            builder.buildJump(expression.lhs.location, done.name)
        }
        terminateBlock(branch2) {
            builder.buildJump(expression.rhs.location, done.name)
        }
        builder.positionAtEnd(done)

        return IRVariable(Type.Bool, expression.location, resultName)

    }

    private fun alloca(type: Type, name: IRLocalName) {
        builder.buildAlloca(type, name)
    }

    private fun makeLocalName(): IRLocalName {
        return IRLocalName(ctx.makeUniqueName())
    }

    private fun lowerNotExpression(expression: HIRExpression.Not): IRValue {
        val name = makeLocalName()
        builder.buildNot(expression.type, expression.location,
                name,
                lowerExpression(expression.expression))
        return builder.buildVariable(expression.type, expression.location, name)
    }

    private fun lowerGetStructField(expression: HIRExpression.GetStructField): IRValue {
        return builder.buildGetStructField(
                expression.type,
                expression.location,
                lowerExpression(expression.lhs),
                expression.name,
                expression.index
        )
    }

    private fun lowerValRef(expression: HIRExpression.ValRef): IRValue {
        val name = lowerLocalName(expression.name)
        val ptr = builder.buildVariable(
                Type.Ptr(expression.type, isMutable = false),
                expression.location,
                localValPtrName(expression.name)
        )
        builder.buildLoad(name, ptr = ptr, type = expression.type)
        return builder.buildVariable(expression.type, expression.location, name)
    }


    private fun lowerConstant(value: HIRConstant): IRValue = when(value) {
        is HIRConstant.ByteString -> builder.buildByteString(value.type, value.location, value.bytes)
        is HIRConstant.BoolValue -> builder.buildConstBool(value.type, value.location, value.value)
        is HIRConstant.IntValue -> IRCIntConstant(value.type, value.location, value.value)
        is HIRConstant.FloatValue -> IRFloatConstant(value.type, value.location, value.value)
    }

    private fun lowerGlobalRef(expression: HIRExpression.GlobalRef): IRValue {
        return builder.buildVariable(
                expression.type,
                expression.location,
                lowerGlobalName(expression.name)
        )
    }

    private fun lowerLocalRef(expression: HIRExpression.ParamRef): IRValue {
        return builder.buildVariable(
                expression.type,
                expression.location,
                lowerLocalName(expression.name)
        )
    }

    private fun lowerLocalName(name: Name): IRLocalName {
        return IRLocalName(name)
    }

    private fun lowerCallExpression(expression: HIRExpression.Call): IRValue {
        val name = declareLocalName()
        builder.buildCall(
                expression.type,
                location = expression.location,
                callee = lowerExpression(expression.callee),
                args = expression.args.map { lowerExpression(it) },
                name = name,
                typeArgs = null
        )

        return builder.buildVariable(
                expression.type,
                expression.location,
                name
        )
    }

    private fun declareLocalName(): IRLocalName {
        return IRLocalName(ctx.makeUniqueName())
    }

    private fun lowerParam(functionName: IRGlobalName, index: Int, param: HIRParam): IRParam {
        return IRParam(
                name = lowerLocalName(param.name),
                type = param.type,
                location = param.location,
                index = index,
                functionName =  functionName
        )
    }
}