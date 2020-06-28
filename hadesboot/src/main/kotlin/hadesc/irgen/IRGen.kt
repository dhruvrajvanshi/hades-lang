package hadesc.irgen

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.ast.Block
import hadesc.context.Context
import hadesc.hir.*
import hadesc.ir.*
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type

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
    }

    private fun lowerStructDef(definition: HIRDefinition.Struct) {
        require(definition.typeParams == null)
        val instanceType = Type.Constructor(binder = null, name = definition.name, params = null)
        val constructorType = Type.Function(
                receiver = null,
                from = definition.fields.map { it.second },
                to = instanceType,
                constraints = listOf(),
                typeParams = null
        )
        module.addStructDef(
            constructorType,
            instanceType,
            name = lowerGlobalName(definition.name),
            typeParams = null,
            fields = definition.fields.toMap()
        )
    }

    private fun lowerExternFunctionDef(definition: HIRDefinition.ExternFunction) {
        module.addExternFunctionDef(
                lowerGlobalName(definition.name),
                definition.type,
                definition.externName
        )
    }

    private fun lowerGlobalName(name: QualifiedName): IRGlobalName {
        return IRGlobalName(name)
    }

    private fun lowerFunctionDef(definition: HIRDefinition.Function) {
        require(definition.receiverType == null)

        val functionName = lowerGlobalName(definition.name)
        val entryBlock = IRBlock()

        val fn = module.addGlobalFunctionDef(
                definition.location,
                functionName,
                typeParams = definition.typeParams?.map { lowerTypeParam(it) },
                constraints = emptyList(),
                params = definition.params.mapIndexed { index, it -> lowerParam(functionName, index, it) },
                entryBlock = entryBlock,
                receiverType = null,
                type = definition.type
        )

        currentFunction = fn
        lowerBlock(definition.body, entryBlock, listOf())
    }

    private fun lowerBlock(
            body: HIRBlock,
            block: IRBlock= IRBlock(),
            cleanupBeforeBlocks: List<IRBlock>,
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
        is HIRStatement.ReturnVoid -> lowerReturnVoidStatement(statement)
        is HIRStatement.Return -> lowerReturnStatement(statement)
        is HIRStatement.Val -> lowerValStatement(statement)
        is HIRStatement.If -> lowerIfStatement(statement)
    }

    private fun lowerIfStatement(statement: HIRStatement.If) {
        val ifTrue = buildBlock()
        val ifFalse = buildBlock()
        val end = forkControlFlow()

        builder.buildBranch(
                statement.condition.location,
                lowerExpression(statement.condition),
                ifTrue = ifTrue.name,
                ifFalse = ifFalse.name
        )

        lowerBlock(statement.trueBranch, ifTrue, cleanupBeforeBlocks = listOf(end)) {
            terminateBlock(statement.trueBranch.location, ifTrue) {
                builder.buildJump(statement.trueBranch.location, end.name)
            }
        }

        lowerBlock(statement.falseBranch, ifFalse, cleanupBeforeBlocks = listOf(end)) {
            terminateBlock(statement.location, ifFalse) {
                builder.buildJump(statement.location, end.name)
            }
        }

        builder.positionAtEnd(end)
    }
    private fun terminateBlock(location: SourceLocation, entryBlock: IRBlock, f: () -> IRInstruction) {
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
                    is IRReturnInstruction -> {}
                    IRReturnVoidInstruction -> {}
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
                    else -> {}
                }
            }
        }
        visitBlock(entryBlock)
    }

    private fun getBlock(name: IRLocalName): IRBlock {
        return requireNotNull(currentFunction?.getBlock(name))
    }

    private fun buildBlock(): IRBlock {
        val name = IRLocalName(ctx.makeUniqueName())
        val block = IRBlock(name)
        requireNotNull(currentFunction).appendBlock(block)
        return block
    }

    private fun forkControlFlow(): IRBlock {
        return buildBlock()
    }

    private fun lowerValStatement(statement: HIRStatement.Val) {
        val ptrName = localValPtrName(statement.name)
        builder.buildAlloca(statement.type, ptrName)
        val ptr = builder.buildVariable(Type.Ptr(statement.type, isMutable = true), statement.location, ptrName)
        builder.buildStore(ptr = ptr, value = lowerExpression(statement.rhs))
    }

    private fun localValPtrName(name: Name): IRLocalName {
        return lowerLocalName(ctx.makeName(name.text + "\$ptr"))
    }

    private fun lowerReturnStatement(statement: HIRStatement.Return) {
        builder.buildReturn(lowerExpression(statement.expression))
    }

    private fun lowerReturnVoidStatement(statement: HIRStatement.ReturnVoid) {
        builder.buildRetVoid()
    }

    private fun lowerExpressionStatement(statement: HIRStatement.Expression) {
        lowerExpression(statement.expression)
    }

    private fun lowerExpression(expression: HIRExpression): IRValue = when(expression) {
        is HIRExpression.Call -> lowerCallExpression(expression)
        is HIRExpression.GlobalRef -> lowerGlobalRef(expression)
        is HIRExpression.Constant -> lowerConstant(expression.constant)
        is HIRExpression.ParamRef -> lowerLocalRef(expression)
        is HIRExpression.ValRef -> lowerValRef(expression)
        is HIRExpression.GetStructField -> lowerGetStructField(expression)
        is HIRExpression.ThisRef -> requireUnreachable()
        is HIRExpression.MethodRef -> requireUnreachable()
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
                typeArgs = expression.typeArgs


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

    private fun lowerTypeParam(it: HIRTypeParam): IRTypeParam {
        requireUnreachable()
    }
}