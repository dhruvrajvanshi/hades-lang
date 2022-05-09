package hadesc.hir.passes

import hadesc.Name
import hadesc.analysis.TypeAnalyzer
import hadesc.ast.Binder
import hadesc.ast.Identifier
import hadesc.hir.*
import hadesc.hir.TypeTransformer
import hadesc.location.SourceLocation
import hadesc.qualifiedname.QualifiedName
import hadesc.types.Type

abstract class AbstractHIRTransformer: HIRTransformer {
    override val typeAnalyzer = TypeAnalyzer()
    override var currentStatements: MutableList<HIRStatement>? = null
    override lateinit var currentLocation: SourceLocation
    override val currentModule: HIRModule = HIRModule(mutableListOf())
    private var basicBlocks: MutableList<HIRBlock>? = null

    protected fun emit(statement: HIRStatement) {
        checkNotNull(currentStatements).add(statement)
    }

    override fun transformFunctionDef(
        definition: HIRDefinition.Function,
        newName: QualifiedName?
    ): Collection<HIRDefinition> {
        val oldBasicBlocks = basicBlocks
        val newBasicBlocks = mutableListOf<HIRBlock>()
        basicBlocks = newBasicBlocks

        val result = HIRDefinition.Function(
            location = definition.location,
            signature = transformFunctionSignature(definition.signature, newName),
            basicBlocks = newBasicBlocks
        )
        for (block in definition.basicBlocks) {
            newBasicBlocks.add(transformBlock(block))
        }
        basicBlocks = oldBasicBlocks

        return listOf(result)
    }
}

interface HIRTransformer: TypeTransformer, HIRBuilder {
    fun transformModule(oldModule: HIRModule): HIRModule {
        val definitions = currentModule.definitions
        val defns = oldModule.definitions.sortedBy {
            when (it) {
                is HIRDefinition.Struct -> 1
                else -> 2
            }
        }
        for (definition in defns) {
            definitions.addAll(transformDefinition(definition))
        }
        return currentModule
    }

    fun transformDefinition(definition: HIRDefinition): Collection<HIRDefinition> = when(definition) {
        is HIRDefinition.Function -> transformFunctionDef(definition)
        is HIRDefinition.ExternFunction -> transformExternFunctionDef(definition)
        is HIRDefinition.Struct -> transformStructDef(definition)
        is HIRDefinition.Const -> transformConstDef(definition)
        is HIRDefinition.Implementation -> transformImplementationDef(definition)
        is HIRDefinition.ExternConst -> transformExternConstDef(definition)
    }

    fun transformExternConstDef(definition: HIRDefinition.ExternConst): Collection<HIRDefinition> {
        return listOf(
            HIRDefinition.ExternConst(
                definition.location,
                transformGlobalName(definition.name),
                lowerType(definition.type),
                definition.externName
            )
        )
    }

    fun transformImplementationDef(definition: HIRDefinition.Implementation): Collection<HIRDefinition> {
        return listOf(
                HIRDefinition.Implementation(
                        typeParams = definition.typeParams?.map { transformTypeParam(it) },
                        traitName = transformGlobalName(definition.traitName),
                        traitArgs = definition.traitArgs.map { lowerType(it) },
                        functions = definition.functions.flatMap { transformDefinition(it) as Collection<HIRDefinition.Function> }.toList(),
                        typeAliases = definition.typeAliases.mapValues { lowerType(it.value) },
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
                basicBlocks = definition.basicBlocks.map { transformBlock(it) }.toMutableList()
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

    override var currentStatements: MutableList<HIRStatement>?
    fun transformBlock(body: HIRBlock): HIRBlock = buildBlock(body.location, body.name) {
        body.statements.forEach {
            emitAll(transformStatement(it))
        }
    }

    fun transformStatement(statement: HIRStatement): Collection<HIRStatement> {
        currentLocation = statement.location
        return transformStatementWorker(statement)
    }
    private fun transformStatementWorker(statement: HIRStatement): Collection<HIRStatement> = when(statement) {
        is HIRStatement.Return -> transformReturnStatement(statement)
        is HIRStatement.Alloca -> transformValDeclaration(statement)
        is HIRStatement.MatchInt -> transformMatchInt(statement)
        is HIRStatement.Assignment -> transformAssignmentStatement(statement)
        is HIRStatement.While -> transformWhileStatement(statement)
        is HIRStatement.Store -> transformStoreStatement(statement)
        is HIRStatement.SwitchInt -> transformSwitchInt(statement)
        is HIRStatement.Call -> transformCallStatement(statement)
        is HIRStatement.Load -> transformLoadStatement(statement)
        is HIRStatement.GetStructField -> transformGetStructField(statement)
        is HIRStatement.GetStructFieldPointer -> transformGetStructFieldPointer(statement)
        is HIRStatement.Not -> transformNotStatement(statement)
        is HIRStatement.Jump -> transformJump(statement)
        is HIRStatement.IntegerConvert -> listOf(transformIntegerConvert(statement))
        is HIRStatement.TypeApplication -> transformTypeApplication(statement)
        is HIRStatement.PointerCast -> transformPointerCast(statement)
        is HIRStatement.BinOp -> transformBinOp(statement)
    }

    fun transformJump(statement: HIRStatement.Jump): Collection<HIRStatement> {
        return listOf(statement)
    }

    fun transformLoadStatement(statement: HIRStatement.Load): Collection<HIRStatement> {
        return listOf(
            HIRStatement.Load(
                statement.location,
                name = statement.name,
                ptr = transformOperand(statement.ptr)
            )
        )
    }

    fun transformSwitchInt(statement: HIRStatement.SwitchInt): Collection<HIRStatement> {
        return listOf(
            HIRStatement.SwitchInt(
                statement.location,
                transformExpression(statement.condition),
                statement.cases,
                statement.otherwise,
            )
        )
    }

    fun transformStoreStatement(statement: HIRStatement.Store): Collection<HIRStatement> {
        return listOf(
                HIRStatement.Store(
                        statement.location,
                        transformOperand(statement.ptr),
                        transformExpression(statement.value)
                )
        )
    }

    fun transformWhileStatement(statement: HIRStatement.While): Collection<HIRStatement> {
        return listOf(
                HIRStatement.While(
                        statement.location,
                        statement.conditionName,
                        transformBlock(statement.conditionBlock),
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

    fun transformMatchInt(statement: HIRStatement.MatchInt): Collection<HIRStatement> {
        return listOf(
            HIRStatement.MatchInt(
                statement.location,
                transformExpression(statement.value),
                statement.arms.map {
                    MatchIntArm(
                        it.value,
                        transformBlock(it.block)
                    )
                },
                transformBlock(statement.otherwise)
            )
        )
    }

    fun transformValDeclaration(statement: HIRStatement.Alloca): Collection<HIRStatement> {
        return listOf(
                HIRStatement.Alloca(
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

    fun transformExpression(expression: HIRExpression): HIRExpression {
        currentLocation = expression.location
        return transformExpressionWorker(expression)
    }
    private fun transformExpressionWorker(expression: HIRExpression): HIRExpression = when(expression) {
        is HIROperand -> transformOperand(expression)

        is HIRExpression.Closure -> transformClosure(expression)
        is HIRExpression.InvokeClosure -> transformInvokeClosure(expression)

    }

    fun transformOperand(expression: HIROperand): HIROperand = when(expression) {
        is HIRExpression.GlobalRef -> transformGlobalRef(expression)
        is HIRExpression.ParamRef -> transformParamRef(expression)
        is HIRExpression.ValRef -> transformValRef(expression)
        is HIRExpression.TraitMethodRef -> transformTraitMethodRef(expression)
        is HIRConstant -> transformConstant(expression)
        is HIRExpression.LocalRef -> transformLocalRef(expression)
    }

    fun transformLocalRef(expression: HIRExpression.LocalRef): HIROperand {
        return HIRExpression.LocalRef(
            expression.location,
            lowerType(expression.type),
            expression.name
        )
    }

    fun transformIntegerConvert(statement: HIRStatement.IntegerConvert): HIRStatement.IntegerConvert {
        val type = lowerType(statement.type)
        require(type is Type.Integral || type is Type.Size || type is Type.Ptr)
        return HIRStatement.IntegerConvert(
            statement.location,
            statement.name,
            type,
            transformOperand(statement.value)
        )
    }

    fun transformInvokeClosure(expression: HIRExpression.InvokeClosure): HIRExpression {
        return HIRExpression.InvokeClosure(
            location = expression.location,
            type = expression.type,
            closure = transformOperand(expression.closure),
            args = expression.args.map { transformExpression(it) },
        )
    }

    fun transformClosure(expression: HIRExpression.Closure): HIRExpression {
        return HIRExpression.Closure(
            expression.location,
            lowerType(expression.type),
            expression.captures.copy(
                values = expression.captures.values.mapValues { it.value.first to lowerType(it.value.second) },
                types = expression.captures.types
            ),
            expression.params.map { transformParam(it) },
            lowerType(expression.returnType),
            transformBlock(expression.body)
        )
    }

    fun transformTraitMethodRef(expression: HIRExpression.TraitMethodRef): HIROperand {
        return HIRExpression.TraitMethodRef(
                expression.location,
                expression.type,
                transformGlobalName(expression.traitName),
                methodName = expression.methodName,
                traitArgs = expression.traitArgs.map { lowerType(it) },
        )
    }

    fun transformGetStructFieldPointer(statement: HIRStatement.GetStructFieldPointer): List<HIRStatement> {
        val type = lowerType(statement.type)
        require(type is Type.Ptr)
        return listOf(HIRStatement.GetStructFieldPointer(
            statement.location,
            name = statement.name,
            type = statement.type,
            lhs = transformExpression(statement.lhs),
            memberIndex = statement.memberIndex,
            memberName = statement.memberName
        ))
    }

    fun transformPointerCast(statement: HIRStatement.PointerCast): List<HIRStatement> {
        return listOf(HIRStatement.PointerCast(
            location = statement.location,
            name = statement.name,
            toPointerOfType = lowerType(statement.toPointerOfType),
            value = transformExpression(statement.value)
        ))
    }

    fun transformTypeApplication(statement: HIRStatement.TypeApplication): List<HIRStatement> {
        return listOf(HIRStatement.TypeApplication(
            statement.location,
            statement.name,
            lowerType(statement.type),
            transformOperand(statement.expression),
            statement.args.map { lowerType(it) }
        ))
    }

    fun transformSizeOfExpression(constant: HIRConstant.SizeOf): HIRConstant {
        return HIRConstant.SizeOf(
                constant.location,
                type = lowerType(constant.type),
                ofType = lowerType(constant.ofType)
        )
    }

    fun transformBinOp(statement: HIRStatement.BinOp): List<HIRStatement> {
        return listOf(HIRStatement.BinOp(
            statement.location,
            statement.name,
            lowerType(statement.type),
            transformExpression(statement.lhs),
            statement.operator,
            transformExpression(statement.rhs)
        ))
    }

    fun transformNotStatement(statement: HIRStatement.Not): List<HIRStatement> {
        return listOf(HIRStatement.Not(statement.name, transformExpression(statement.expression)))
    }

    fun transformGetStructField(expression: HIRStatement.GetStructField): List<HIRStatement> {
        return listOf(HIRStatement.GetStructField(
            location = expression.location,
            name = expression.name,
            type = lowerType(expression.type),
            index = expression.index,
            fieldName = expression.fieldName,
            lhs = transformExpression(expression.lhs)
        ))
    }

    fun transformValRef(expression: HIRExpression.ValRef): HIROperand {
        return HIRExpression.ValRef(
                expression.location,
                lowerType(expression.type),
                transformValName(expression.name)
        )
    }

    fun transformValName(name: Name): Name {
        return name
    }

    fun transformParamRef(expression: HIRExpression.ParamRef): HIROperand {
        return HIRExpression.ParamRef(
                location = expression.location,
                type = lowerType(expression.type),
                name = transformParamName(expression.name),
                binder = Binder(Identifier(expression.binder.location, transformParamName(expression.name)))
        )
    }

    fun transformCallStatement(statement: HIRStatement.Call): Collection<HIRStatement> {
        return listOf(
            HIRStatement.Call(
                location = statement.location,
                name = statement.name,
                resultType = lowerType(statement.resultType),
                callee = transformOperand(statement.callee),
                args = statement.args.map { transformExpression(it) },
            )
        )
    }

    fun transformGlobalRef(expression: HIRExpression.GlobalRef): HIROperand {
        return HIRExpression.GlobalRef(
                location = expression.location,
                type = lowerType(expression.type),
                name = transformGlobalName(expression.name)
        )
    }

    fun transformConstant(expression: HIRConstant): HIRConstant = when(expression) {
        is HIRConstant.ByteString -> expression
        is HIRConstant.BoolValue -> expression
        is HIRConstant.IntValue -> expression
        is HIRConstant.FloatValue -> expression
        is HIRConstant.Void -> expression
        is HIRConstant.NullPtr -> expression.copy(type = Type.Ptr(lowerType(expression.type.to), expression.type.isMutable))
        is HIRConstant.SizeOf -> transformSizeOfExpression(expression)
    }

    fun transformTypeParam(param: HIRTypeParam): HIRTypeParam {
        return HIRTypeParam(param.location, param.name)
    }

    fun transformParam(param: HIRParam): HIRParam {
        return HIRParam(
                location = param.location,
                binder = param.binder,
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