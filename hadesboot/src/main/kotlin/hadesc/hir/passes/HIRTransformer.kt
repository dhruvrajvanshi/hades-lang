package hadesc.hir.passes

import hadesc.Name
import hadesc.hir.*
import hadesc.ir.passes.TypeTransformer
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type

interface HIRTransformer: TypeTransformer {
    fun transformModule(module: HIRModule): HIRModule {
        val definitions = mutableListOf<HIRDefinition>()
        for (definition in module.definitions) {
            definitions.addAll(transformDefinition(definition))
        }
        return HIRModule(definitions)
    }

    fun transformDefinition(definition: HIRDefinition): Collection<HIRDefinition> = when(definition) {
        is HIRDefinition.Function -> transformFunctionDef(definition)
        is HIRDefinition.ExternFunction -> transformExternFunctionDef(definition)
        is HIRDefinition.Struct -> transformStructDef(definition)
        is HIRDefinition.Implementation -> TODO()
        is HIRDefinition.Interface -> TODO()
    }

    fun transformStructDef(definition: HIRDefinition.Struct): Collection<HIRDefinition> {
        return listOf(
                HIRDefinition.Struct(
                        name = transformGlobalName(definition.name),
                        typeParams = definition.typeParams?.map { transformTypeParam(it) },
                        location = definition.location,
                        fields = definition.fields.map { it.first to lowerType(it.second) }
                )
        )
    }

    fun transformFunctionDef(definition: HIRDefinition.Function): Collection<HIRDefinition> {
        return listOf(HIRDefinition.Function(
                location = definition.location,
                signature = transformFunctionSignature(definition.signature),
                body = transformBlock(definition.body)
        ))
    }

    fun transformFunctionSignature(signature: HIRFunctionSignature): HIRFunctionSignature {
        return HIRFunctionSignature(
                location = signature.location,
                receiverType = signature.receiverType?.let { lowerType(it) },
                name = transformGlobalName(signature.name),
                returnType = lowerType(signature.returnType),
                constraintParams = signature.constraintParams?.map { transformConstraintParam(it) },
                params = signature.params.map { transformParam(it) },
                typeParams = signature.typeParams?.map { transformTypeParam(it) }
        )
    }

    fun transformConstraintParam(param: HIRConstraintParam): HIRConstraintParam {
        return HIRConstraintParam(
                type = lowerType(param.type),
                param = transformTypeParam(param.param),
                interfaceRef = transformInterfaceRef(param.interfaceRef)
        )
    }

    fun transformInterfaceRef(interfaceRef: HIRInterfaceRef): HIRInterfaceRef {
        return HIRInterfaceRef(
                interfaceName = transformGlobalName(interfaceRef.interfaceName),
                typeArgs = interfaceRef.typeArgs?.map { lowerType(it) }
        )
    }

    fun transformBlock(body: HIRBlock): HIRBlock {
        return HIRBlock(
                location = body.location,
                statements = body.statements.flatMap { transformStatement(it) }
        )
    }

    fun transformStatement(statement: HIRStatement): Collection<HIRStatement> = when(statement) {
        is HIRStatement.Expression -> transformExpressionStatement(statement)
        is HIRStatement.Return -> transformReturnStatement(statement)
        is HIRStatement.ReturnVoid -> transformRetVoidStatement(statement)
        is HIRStatement.ValDeclaration -> transformValDeclaration(statement)
        is HIRStatement.If -> transformIfStatement(statement)
        is HIRStatement.Assignment -> transformAssignmentStatement(statement)
        is HIRStatement.While -> transformWhileStatement(statement)
    }

    fun transformWhileStatement(statement: HIRStatement.While): Collection<HIRStatement> {
        return listOf(
                HIRStatement.While(
                        statement.location,
                        transformExpression(statement.condition),
                        transformBlock(statement.body)
                )
        )
    }

    fun transformAssignmentStatement(statement: HIRStatement.Assignment): Collection<HIRStatement> {
        return listOf(
                HIRStatement.Assignment(
                        statement.location,
                        transformValName(statement.name),
                        transformExpression(statement.value)
                )
        )
    }

    fun transformIfStatement(statement: HIRStatement.If): Collection<HIRStatement> {
        return listOf(
                HIRStatement.If(
                        statement.location,
                        transformExpression(statement.condition),
                        transformBlock(statement.trueBranch),
                        transformBlock(statement.falseBranch)
                )
        )
    }

    fun transformValDeclaration(statement: HIRStatement.ValDeclaration): Collection<HIRStatement> {
        return listOf(
                HIRStatement.ValDeclaration(
                        location = statement.location,
                        name = transformValName(statement.name),
                        isMutable = statement.isMutable,
                        type = lowerType(statement.type)
                )
        )
    }

    fun transformReturnStatement(statement: HIRStatement.Return): Collection<HIRStatement> {
        return listOf(
                HIRStatement.Return(
                        location = statement.location,
                        expression = transformExpression(statement.expression)
                )
        )
    }

    fun transformRetVoidStatement(statement: HIRStatement.ReturnVoid): Collection<HIRStatement> {
        return listOf(statement)
    }

    fun transformExpressionStatement(statement: HIRStatement.Expression): Collection<HIRStatement> {
        return listOf(HIRStatement.Expression(transformExpression(statement.expression)))
    }

    fun transformExpression(expression: HIRExpression): HIRExpression = when(expression) {
        is HIRExpression.Call -> transformCall(expression)
        is HIRExpression.GlobalRef -> transformGlobalRef(expression)
        is HIRExpression.Constant -> transformConstant(expression)
        is HIRExpression.ParamRef -> transformParamRef(expression)
        is HIRExpression.ValRef -> transformValRef(expression)
        is HIRExpression.GetStructField -> transformGetStructField(expression)
        is HIRExpression.ThisRef -> transformThisRef(expression)
        is HIRExpression.MethodRef -> transformMethodRef(expression)
        is HIRExpression.Not -> transformNotExpression(expression)
        is HIRExpression.BinOp -> transformBinOp(expression)
        is HIRExpression.NullPtr -> transformNullPtr(expression)
        is HIRExpression.SizeOf -> transformSizeOfExpression(expression)
        is HIRExpression.AddressOf -> transformAddressOfExpression(expression)
        is HIRExpression.BoundRef -> transformBoundRef(expression)
    }

    fun transformBoundRef(expression: HIRExpression.BoundRef): HIRExpression {
        return HIRExpression.BoundRef(
                location = expression.location,
                param = transformConstraintParam(expression.param)
        )
    }

    fun transformAddressOfExpression(expression: HIRExpression.AddressOf): HIRExpression {
        return HIRExpression.AddressOf(
                expression.location,
                lowerType(expression.type) as Type.Ptr,
                transformValName(expression.name)
        )
    }

    fun transformSizeOfExpression(expression: HIRExpression.SizeOf): HIRExpression {
        return HIRExpression.SizeOf(
                expression.location,
                type = lowerType(expression.type),
                ofType = lowerType(expression.ofType)
        )
    }

    fun transformNullPtr(expression: HIRExpression.NullPtr): HIRExpression {
        return HIRExpression.NullPtr(
                expression.location,
                lowerType(expression.type) as Type.Ptr
        )
    }

    fun transformBinOp(expression: HIRExpression.BinOp): HIRExpression {
        return HIRExpression.BinOp(
                expression.location,
                lowerType(expression.type),
                transformExpression(expression.lhs),
                expression.operator,
                transformExpression(expression.rhs)
        )
    }

    fun transformNotExpression(expression: HIRExpression.Not): HIRExpression {
        return HIRExpression.Not(transformExpression(expression.expression))
    }

    fun transformMethodRef(expression: HIRExpression.MethodRef): HIRExpression {
        return HIRExpression.MethodRef(
                expression.location,
                lowerType(expression.type),
                transformExpression(expression.thisValue),
                transformExpression(expression.method)
        )
    }

    fun transformImplementationMethodRef(binding: HIRPropertyBinding.ImplementationMethodRef): HIRPropertyBinding {
        return HIRPropertyBinding.ImplementationMethodRef(
                binding.location,
                implName = transformGlobalName(binding.implName),
                interfaceMemberIndex = binding.interfaceMemberIndex
        )
    }

    fun transformGlobalExtensionRef(binding: HIRPropertyBinding.GlobalExtensionRef): HIRPropertyBinding {
        return HIRPropertyBinding.GlobalExtensionRef(
                location = binding.location,
                functionName = binding.functionName
        )
    }

    fun transformThisRef(expression: HIRExpression.ThisRef): HIRExpression {
        return HIRExpression.ThisRef(location = expression.location, type = lowerType(expression.type))
    }

    fun transformGetStructField(expression: HIRExpression.GetStructField): HIRExpression {
        return HIRExpression.GetStructField(
                location = expression.location,
                name = expression.name,
                type = lowerType(expression.type),
                index = expression.index,
                lhs = transformExpression(expression.lhs)
        )
    }

    fun transformValRef(expression: HIRExpression.ValRef): HIRExpression {
        return HIRExpression.ValRef(
                expression.location,
                lowerType(expression.type),
                transformValName(expression.name)
        )
    }

    fun transformValName(name: Name): Name {
        return name
    }

    fun transformParamRef(expression: HIRExpression.ParamRef): HIRExpression {
        return HIRExpression.ParamRef(
                location = expression.location,
                type = lowerType(expression.type),
                name = transformParamName(expression.name)
        )
    }

    fun transformCall(expression: HIRExpression.Call): HIRExpression {
        return HIRExpression.Call(
                location = expression.location,
                type = lowerType(expression.type),
                typeArgs = expression.typeArgs?.map { lowerType(it) },
                callee = transformExpression(expression.callee),
                args = expression.args.map { transformExpression(it) }
        )
    }

    fun transformGlobalRef(expression: HIRExpression.GlobalRef): HIRExpression {
        return HIRExpression.GlobalRef(
                location = expression.location,
                type = lowerType(expression.type),
                name = transformGlobalName(expression.name)
        )
    }

    fun transformConstant(expression: HIRExpression.Constant): HIRExpression = when(expression.constant) {
        is HIRConstant.ByteString -> expression
        is HIRConstant.BoolValue -> expression
        is HIRConstant.IntValue -> expression
    }

    fun transformTypeParam(param: HIRTypeParam): HIRTypeParam {
        return HIRTypeParam(param.location, param.name)
    }

    fun transformParam(param: HIRParam): HIRParam {
        return HIRParam(
                location = param.location,
                name = transformParamName(param.name),
                type = lowerType(param.type)
        )
    }

    fun transformParamName(name: Name): Name {
        return name
    }

    fun transformExternFunctionDef(definition: HIRDefinition.ExternFunction): Collection<HIRDefinition> {
        return listOf(HIRDefinition.ExternFunction(
                location = definition.location,
                name = transformGlobalName(definition.name),
                params = definition.params.map { lowerType(it) },
                externName = definition.externName,
                returnType = lowerType(definition.returnType)
        ))
    }

    fun transformGlobalName(name: QualifiedName): QualifiedName {
        return name
    }
}