package hadesc.irgen

import hadesc.Name
import hadesc.assertions.requireUnreachable
import hadesc.context.Context
import hadesc.hir.*
import hadesc.ir.*
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type

class IRGen(
        private val ctx: Context
) {
    private val module = IRModule()
    private val builder = IRBuilder()

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
        module.addGlobalFunctionDef(
                definition.location,
                functionName,
                typeParams = definition.typeParams?.map { lowerTypeParam(it) },
                constraints = emptyList(),
                params = definition.params.mapIndexed { index, it -> lowerParam(functionName, index, it) },
                entryBlock = lowerBlock(definition.body),
                receiverType = null,
                type = definition.type
        )
    }

    private fun lowerBlock(body: HIRBlock): IRBlock {
        val block = IRBlock()
        builder.withinBlock(block) {
            for (statement in body.statements) {
                lowerStatement(statement)
            }
        }
        return block

    }

    private fun lowerStatement(statement: HIRStatement): Unit = when(statement) {
        is HIRStatement.Expression -> lowerExpressionStatement(statement)
        is HIRStatement.ReturnVoid -> lowerReturnVoidStatement(statement)
        is HIRStatement.Return -> lowerReturnStatement(statement)
        is HIRStatement.Val -> lowerValStatement(statement)
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
        TODO()
    }
}