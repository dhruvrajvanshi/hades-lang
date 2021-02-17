package hadesc.hir.passes

import hadesc.Name
import hadesc.ast.Declaration
import hadesc.hir.*
import hadesc.ir.passes.TypeTransformer
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type
import kotlin.math.exp

interface HIRTransformer: TypeTransformer {
    fun transformModule(oldModule: HIRModule): HIRModule {
        val definitions = mutableListOf<HIRDefinition>()
        for (definition in oldModule.definitions) {
            definitions.addAll(transformDefinition(definition))
        }
        return HIRModule(definitions)
    }

    fun transformDefinition(definition: HIRDefinition): Collection<HIRDefinition> = when(definition) {
        is HIRDefinition.Function -> transformFunctionDef(definition)
        is HIRDefinition.ExternFunction -> transformExternFunctionDef(definition)
        is HIRDefinition.Struct -> transformStructDef(definition)
        is HIRDefinition.Const -> transformConstDef(definition)
        is HIRDefinition.Implementation -> transformImplementationDef(definition)
    }

    fun transformImplementationDef(definition: HIRDefinition.Implementation): Collection<HIRDefinition> {
        return listOf(
                HIRDefinition.Implementation(
                        typeParams = definition.typeParams?.map { transformTypeParam(it) },
                        traitName = transformGlobalName(definition.traitName),
                        traitArgs = definition.traitArgs.map { lowerType(it) },
                        functions = definition.functions.flatMap { transformDefinition(it) as Collection<HIRDefinition.Function> }.toList(),
                        location = definition.location,
                        traitRequirements = definition.traitRequirements.map { lowerTraitRequirement(it) }
                )
        )
    }

    fun transformConstDef(definition: HIRDefinition.Const): Collection<HIRDefinition> {
        return listOf(
                HIRDefinition.Const(
                        definition.location,
                        transformGlobalName(definition.name),
                        transformExpression(definition.initializer)
                )
        )
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

    fun transformFunctionDef(definition: HIRDefinition.Function, newName: QualifiedName? = null): Collection<HIRDefinition> {
        return listOf(HIRDefinition.Function(
                location = definition.location,
                signature = transformFunctionSignature(definition.signature, newName),
                body = transformBlock(definition.body)
        ))
    }

    fun transformFunctionSignature(signature: HIRFunctionSignature, newName: QualifiedName? = null): HIRFunctionSignature {
        return HIRFunctionSignature(
                location = signature.location,
                name = newName ?: transformGlobalName(signature.name),
                returnType = lowerType(signature.returnType),
                params = signature.params.map { transformParam(it) },
                typeParams = signature.typeParams?.map { transformTypeParam(it) }
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
        is HIRStatement.Store -> transformStoreStatement(statement)
    }

    fun transformStoreStatement(statement: HIRStatement.Store): Collection<HIRStatement> {
        return listOf(
                HIRStatement.Store(
                        statement.location,
                        transformExpression(statement.ptr),
                        transformExpression(statement.value)
                )
        )
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
        is HIRExpression.Not -> transformNotExpression(expression)
        is HIRExpression.BinOp -> transformBinOp(expression)
        is HIRExpression.NullPtr -> transformNullPtr(expression)
        is HIRExpression.SizeOf -> transformSizeOfExpression(expression)
        is HIRExpression.AddressOf -> transformAddressOfExpression(expression)
        is HIRExpression.TypeApplication -> transformTypeApplication(expression)
        is HIRExpression.Load -> transformLoadExpression(expression)
        is HIRExpression.PointerCast -> transformPointerCastExpression(expression)
        is HIRExpression.GetStructFieldPointer -> transformGetStructFieldPointer(expression)
        is HIRExpression.TraitMethodCall -> transformTraitMethodCall(expression)
        is HIRExpression.UnsafeCast -> transformUnsafeCast(expression)
        is HIRExpression.When -> transformWhenExpression(expression)
    }

    fun transformWhenExpression(expression: HIRExpression.When): HIRExpression {
        return HIRExpression.When(
            expression.location,
            lowerType(expression.type),
            transformExpression(expression.discriminant),
            expression.cases.map { HIRExpression.When.Case(
                caseName = it.caseName,
                valueBinder = it.valueBinder,
                expression = transformExpression(it.expression),
                casePayloadType = lowerType(it.casePayloadType)
            ) }
        )
    }

    fun transformUnsafeCast(expression: HIRExpression.UnsafeCast): HIRExpression {
        return HIRExpression.UnsafeCast(
            location = expression.location,
            lowerType(expression.type),
            transformExpression(expression.value)
        )
    }

    fun transformTraitMethodCall(expression: HIRExpression.TraitMethodCall): HIRExpression {
        return HIRExpression.TraitMethodCall(
                expression.location,
                expression.type,
                transformGlobalName(expression.traitName),
                methodName = expression.methodName,
                traitArgs = expression.traitArgs.map { lowerType(it) },
                args = expression.args.map { transformExpression(it) }
        )
    }

    fun transformGetStructFieldPointer(expression: HIRExpression.GetStructFieldPointer): HIRExpression {
        return HIRExpression.GetStructFieldPointer(
                expression.location,
                lowerType(expression.type),
                lhs = transformExpression(expression.lhs),
                memberIndex = expression.memberIndex,
                memberName = expression.memberName
        )
    }

    fun transformPointerCastExpression(expression: HIRExpression.PointerCast): HIRExpression {
        return HIRExpression.PointerCast(
            location = expression.location,
            toPointerOfType = lowerType(expression.toPointerOfType),
            value = transformExpression(expression.value)
        )
    }

    fun transformLoadExpression(expression: HIRExpression.Load): HIRExpression {
        return HIRExpression.Load(
            location = expression.location,
            type = lowerType(expression.type),
            ptr = transformExpression(expression.ptr)
        )
    }

    fun transformTypeApplication(expression: HIRExpression.TypeApplication): HIRExpression {
        return HIRExpression.TypeApplication(
                expression.location,
                lowerType(expression.type),
                transformExpression(expression.expression),
                expression.args.map { lowerType(it) }
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